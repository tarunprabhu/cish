#include "SourceInfo.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"
#include "Options.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

static bool isChar(const Metadata* md) {
  if(const auto* diBasic = dyn_cast_or_null<DIBasicType>(md))
    return diBasic->getEncoding() == dwarf::DW_ATE_signed_char;
  return false;
}

static bool isConstChar(const Metadata* md) {
  if(const auto* diConst = dyn_cast_or_null<DIDerivedType>(md))
    if(diConst->getTag() == dwarf::DW_TAG_const_type)
      return isChar(diConst->getBaseType());
  return false;
}

static bool isCString(const Metadata* md) {
  if(const auto* diPtr = dyn_cast_or_null<DIDerivedType>(md))
    if(diPtr->getTag() == dwarf::DW_TAG_pointer_type)
      return isConstChar(diPtr->getBaseType());
  return false;
}

static bool isStringLiteral(const GlobalVariable& g) {
  if(const auto* init = dyn_cast_or_null<ConstantDataArray>(g.getInitializer()))
    if(init->getType()->getElementType()->isIntegerTy(8))
      if(g.hasGlobalUnnamedAddr() and g.hasPrivateLinkage())
        return true;
  return false;
}

SourceInfo::SourceInfo(const Module& m) : dl(m.getDataLayout()) {
  for(const char* fname : {"llvm.dbg.value", "llvm.dbg.declare"})
    if(const Function* f = m.getFunction(fname))
      dbgFns.push_back(f);
  runOnModule(m);
}

bool SourceInfo::isCString(const Value* val) const {
  return cstrings.contains(val);
}

bool SourceInfo::isCString(const Value& val) const {
  return isCString(&val);
}

bool SourceInfo::isStringLiteral(const Value* val) const {
  return stringLiterals.contains(val);
}

bool SourceInfo::isStringLiteral(const Value& val) const {
  return stringLiterals.contains(&val);
}

bool SourceInfo::hasName(const Value* val) const {
  return valueNames.contains(val);
}

bool SourceInfo::hasName(const Value& val) const {
  return hasName(&val);
}

bool SourceInfo::hasName(StructType* sty) const {
  return structNames.contains(sty) or classNames.contains(sty)
         or unionNames.contains(sty);
}

bool SourceInfo::hasElementName(StructType* sty, unsigned i) const {
  if(elemNames.contains(sty))
    return elemNames.at(sty).size() > i;
  return false;
}

const std::string& SourceInfo::getName(const Value* v) const {
  return valueNames.at(v);
}

const std::string& SourceInfo::getName(const Value& v) const {
  return getName(&v);
}

const std::string& SourceInfo::getName(StructType* sty) const {
  if(structNames.contains(sty))
    return structNames.at(sty);
  else if(classNames.contains(sty))
    return classNames.at(sty);
  else if(unionNames.contains(sty))
    return unionNames.at(sty);
  fatal(error() << "No source name for struct type: " << *sty);
}

const std::string& SourceInfo::getElementName(StructType* sty,
                                              unsigned i) const {
  return elemNames.at(sty).at(i);
}

SourceInfo::DbgValElements
SourceInfo::parseDbgValCall(const CallInst& call) const {
  Value* value = nullptr;
  if(auto* mdv = dyn_cast<MetadataAsValue>(call.getArgOperand(0)))
    if(auto* vdm = dyn_cast<ValueAsMetadata>(mdv->getMetadata()))
      value = vdm->getValue();

  DINode* di = nullptr;
  if(auto* mdv = dyn_cast<MetadataAsValue>(call.getArgOperand(1)))
    di = dyn_cast<DINode>(mdv->getMetadata());

  DIExpression* expr = nullptr;
  if(auto* mdv = dyn_cast<MetadataAsValue>(call.getArgOperand(2)))
    expr = dyn_cast<DIExpression>(mdv->getMetadata());

  return DbgValElements(value, di, expr);
}

void SourceInfo::collectStructs(PointerType* pty, const DIType* md) {
  if(const auto* derived = dyn_cast<DIDerivedType>(md)) {
    switch(derived->getTag()) {
    case dwarf::DW_TAG_pointer_type:
    case dwarf::DW_TAG_reference_type:
      return collectStructs(pty->getElementType(), derived->getBaseType());
    default:
      if(opts().verbose)
        warning() << "Unexpected tag for pointer type: " << *pty << "\n";
      break;
    }
  } else {
    if(opts().verbose)
      warning() << "Expected derived DI node for " << *pty << "\n";
  }
}

void SourceInfo::collectStructs(ArrayType* aty, const DIType* md) {
  if(const auto* comp = dyn_cast<DICompositeType>(md)) {
    switch(comp->getTag()) {
    case dwarf::DW_TAG_array_type:
      return collectStructs(getBaseType(aty), comp->getBaseType());
    default:
      if(opts().verbose)
        warning() << "Unexpected tag for array type: " << *aty << "\n";
    }
  } else {
    if(opts().verbose)
      warning() << "Expected composite DI node for " << *aty << "\n";
  }
}

void SourceInfo::collectStructs(FunctionType* fty, const DIType* md) {
  warning() << "UNIMPLEMENTED: Associating function types\n";
}

void SourceInfo::collectStructsFromStruct(StructType* sty,
                                          const DICompositeType* comp) {
  structs[sty] = comp;
  const StructLayout& sl = *dl.getStructLayout(sty);
  if(const auto& elements = comp->getElements()) {
    size_t numOps = elements->getNumOperands();
    size_t numElements = sty->getNumElements();
    structs[sty] = comp;
    // The LLVM structs sometimes have an extra padding field at the end.
    if((numOps == numElements) or (numOps == (numElements - 1))) {
      for(unsigned i = 0; i < elements->getNumOperands(); i++) {
        // This will be node with tag DW_TAG_member. To get the node containing
        // the actual type, we need to call getBaseType() on it
        const auto* derived = dyn_cast<DIDerivedType>(elements->getOperand(i));
        size_t offset = sl.getElementOffset(i) * 8;
        if(derived->getOffsetInBits() == offset) {
          elemNames[sty].push_back(derived->getName());
          collectStructs(sty->getElementType(i), derived->getBaseType());
        } else {
          if(opts().verbose)
            warning() << "Mismatched offsets of field " << i << " for struct "
                      << *sty << "\n";
          goto fail;
        }
      }
    } else {
      if(opts().verbose)
        warning() << "Mismatch in number of fields of struct "
                  << comp->getName() << " => " << sty->getNumElements() << ", "
                  << elements->getNumOperands() << "\n";
      goto fail;
    }
  }

  return;

fail:
  // Keep the struct in the typemap because otherwise we might infinitely
  // recurse in the case of a recursive struct
  structs[sty] = nullptr;
}

void SourceInfo::collectStructsFromClass(StructType* sty,
                                         const DICompositeType* comp) {
  const StructLayout& sl = *dl.getStructLayout(sty);
  structs[sty] = comp;
  unsigned i = 0;
  for(const MDOperand& op : comp->getElements()->operands()) {
    // This will be node with tag DW_TAG_member. To get the node containing
    // the actual type, we need to call getBaseType() on it
    if(const auto* derived = dyn_cast_or_null<DIDerivedType>(op)) {
      if((derived->getTag() == dwarf::DW_TAG_member)
         and (not derived->isStaticMember())) {
        size_t offset = sl.getElementOffset(i) * 8;
        if(derived->getOffsetInBits() == offset) {
          elemNames[sty].push_back(derived->getName());
          collectStructs(sty->getElementType(i), derived->getBaseType());
          i += 1;
        } else {
          if(opts().verbose)
            warning() << "Mismatched offsets of field " << i << " for struct "
                      << *sty << "\n";
          goto fail;
        }
      }
    }
  }

  // FIXME: This is not correct
  //
  // The check here is because LLVM may add a padding array to the end of a
  // struct. That would make it an extra field, but the presence of virtual
  // functions may also add an extra field at the front for the vtable
  // Either way, this condition is a bit wonky. At some point, when vtables
  // are handled correctly, this should be fixed
  if(elemNames.contains(sty)
     and ((elemNames[sty].size() == sty->getNumElements())
          or (elemNames[sty].size() == (sty->getNumElements() - 1))))
    return;

  if(opts().verbose)
    warning() << "Mismatch in number of elements for " << *sty << " => "
              << sty->getNumElements() << ", " << elemNames[sty].size() << "\n";

fail:
  // Keep the struct in the typemap because otherwise we might infinitely
  // recurse in the case of a recursive struct
  structs[sty] = nullptr;
}

void SourceInfo::collectStructsFromUnion(StructType*, const DICompositeType*) {
  // At some point, I might figure out if this can be used in any reasonable
  // way
}

void SourceInfo::collectStructs(StructType* sty, const DIType* md) {
  if(const auto* comp = dyn_cast<DICompositeType>(md)) {
    switch(comp->getTag()) {
    case dwarf::DW_TAG_structure_type:
      return collectStructsFromStruct(sty, comp);
    case dwarf::DW_TAG_class_type:
      return collectStructsFromClass(sty, comp);
    case dwarf::DW_TAG_union_type:
      return collectStructsFromUnion(sty, comp);
    default:
      break;
    }
  } else {
    if(opts().verbose)
      warning() << "Expected DIComposite type for struct: " << *sty << "\n";
  }
}

void SourceInfo::collectStructs(Type* type, const Metadata* md) {
  if(not md)
    return;

  if(const auto* di = dyn_cast<DIDerivedType>(md)) {
    switch(di->getTag()) {
    case dwarf::DW_TAG_restrict_type:
    case dwarf::DW_TAG_const_type:
    case dwarf::DW_TAG_typedef:
      return collectStructs(type, di->getBaseType());
    }
  }

  if(const auto* di = dyn_cast<DIType>(md)) {
    if(auto* pty = dyn_cast<PointerType>(type))
      return collectStructs(pty, di);
    else if(auto* aty = dyn_cast<ArrayType>(type))
      return collectStructs(aty, di);
    else if(auto* fty = dyn_cast<FunctionType>(type))
      return collectStructs(fty, di);
    else if(auto* sty = dyn_cast<StructType>(type))
      if(not structs.contains(sty))
        return collectStructs(sty, di);
  }
}

void SourceInfo::runOnFunction(const Function& f) {
  FunctionType* fty = f.getFunctionType();
  if(const DISubprogram* subp = f.getSubprogram()) {
    std::string buf;
    raw_string_ostream ss(buf);
    // FIXME: Track the scope beyond just one level because there may be
    // namespace information there too
    if(const auto* cls = dyn_cast_or_null<DICompositeType>(subp->getScope()))
      ss << cls->getName() << "::";
    ss << subp->getName();
    valueNames[&f] = ss.str();

    Map<unsigned, const DILocalVariable*> sourceArgs;
    Set<const DILocalVariable*> sourceLocals;
    Map<const DINode*, Vector<DbgValElements>> dbgElems;

    // If the function returns a struct by value, the first argument will be
    // the return argument
    bool sret = false;
    if(fty->getNumParams())
      sret = getArg(f, 0).hasStructRetAttr();

    for(const DINode* op : subp->getRetainedNodes()) {
      if(const auto* di = dyn_cast<DILocalVariable>(op)) {
        if(di->getArg())
          // The argument number in the debug information is 1-indexed
          // but the arguments in the function are 0-indexed as far as LLVM is
          // concerned. So the argument needs to be adjusted unless the function
          // returns a struct by value in which case the first argument of the
          // LLVM function will have an sret attribute and will the struct
          // will be returned through it
          sourceArgs[di->getArg() - (not sret)] = di;
        else
          sourceLocals.insert(di);
      }
    }

    // Look up the debug intrinsic function calls and see what metadata
    // was associated with the values. Some of these will be user variables
    // but others may be from inlined functions
    for(const Function* f : dbgFns)
      for(const Use& u : f->uses())
        if(const CallInst* call = dyn_cast<CallInst>(u.getUser()))
          if(DbgValElements p = parseDbgValCall(*call))
            dbgElems[p.di].push_back(p);

    // Associate names
    for(const auto& i : sourceArgs)
      valueNames[&getArg(f, i.first)] = i.second->getName();

    for(const DILocalVariable* di : sourceLocals)
      if(dbgElems.contains(di))
        for(const DbgValElements& p : dbgElems.at(di))
          if(p.expr->getNumElements() == 0)
            valueNames[p.value] = di->getName();

    // Associate types
    const auto types = cast<DISubroutineType>(subp->getType())->getTypeArray();

    // The first element of the types array is the return value which will
    // be null if the function returns void.
    if(types->getOperand(0)) {
      if(sret)
        collectStructs(
            cast<PointerType>(fty->getParamType(0))->getElementType(),
            types->getOperand(0));
      else
        collectStructs(fty->getReturnType(), types->getOperand(0));
    }
    for(unsigned i = 1, j = sret; i < types->getNumOperands(); i++, j++)
      collectStructs(fty->getParamType(j), types->getOperand(i));

    for(const DILocalVariable* di : sourceLocals) {
      if(dbgElems.contains(di)) {
        // Just check one of the DbgValElements because they will all have
        // the same type
        const Value* value = dbgElems.at(di).front().value;
        if(const auto* alloca = dyn_cast<AllocaInst>(value))
          collectStructs(alloca->getAllocatedType(), di->getType());
        else
          collectStructs(value->getType(), di->getType());
      }
    }
  }
}

void SourceInfo::runOnGlobal(const GlobalVariable& g) {
  SmallVector<DIGlobalVariableExpression*, 4> gexprs;
  g.getDebugInfo(gexprs);
  if(gexprs.size()) {
    if(gexprs.size() == 1) {
      const auto* di = dyn_cast<DIGlobalVariable>(gexprs[0]->getVariable());
      valueNames[&g] = di->getName();
      if(cish::isCString(di->getType()))
        cstrings.insert(&g);

      collectStructs(g.getType()->getElementType(), di->getType());
    } else {
      if(opts().verbose)
        warning() << "Got more than one expression for global: " << g << "\n";
    }
  }
  // Irrespective of whether or not there is debug info associated with a global
  // check if it is a string literal
  if(cish::isStringLiteral(g))
    stringLiterals.insert(&g);
}

void SourceInfo::runOnModule(const Module& m) {
  for(const Function& f : m.functions())
    runOnFunction(f);
  for(const GlobalVariable& g : m.globals())
    runOnGlobal(g);

  for(const auto& p : structs) {
    StructType* sty = p.first;
    const DICompositeType* di = p.second;

    // If there was an error in processing the type, it will still be
    // present in the map, but the node will be set to null
    if(not di)
      continue;

    // FIXME: There might be namespace and/or parent information in the struct
    // name. Make use of it
    if(di->getTag() == dwarf::DW_TAG_class_type)
      classNames[sty] = di->getName();
    else if(di->getTag() == dwarf::DW_TAG_structure_type)
      structNames[sty] = di->getName();
    else if(di->getTag() == dwarf::DW_TAG_union_type)
      structNames[sty] = di->getName();
    else if(opts().verbose)
      warning() << "Unexpected DI tag\n";
  }
}

} // namespace cish

SourceInfoWrapperPass::SourceInfoWrapperPass() : ModulePass(ID), si(nullptr) {
  ;
}

StringRef SourceInfoWrapperPass::getPassName() const {
  return "Cish Source Info Wrapper Pass";
}

void SourceInfoWrapperPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.setPreservesAll();
}

const cish::SourceInfo& SourceInfoWrapperPass::getSourceInfo() const {
  return *si;
}

bool SourceInfoWrapperPass::runOnModule(Module& m) {
  cish::message() << "Running " << getPassName() << "\n";

  si.reset(new cish::SourceInfo(m));

  return false;
}

char SourceInfoWrapperPass::ID = 0;

static RegisterPass<SourceInfoWrapperPass>
    X("cish-source-info", "Parses debug info", true, true);

Pass* createNewSourceInfoWrapperPass() {
  return new SourceInfoWrapperPass();
}
