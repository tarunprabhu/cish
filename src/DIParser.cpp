#include "DIParser.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

void DIParser::runOnFunction(const Function& f) {
  for(const Instruction& inst : instructions(f)) {
    if(const auto* call = dyn_cast<CallInst>(&inst)) {
      if(const Function* callee = call->getCalledFunction()) {
        if((callee->getName() == "llvm.dbg.value")
           or (callee->getName() == "llvm.dbg.declare")) {
          const auto* mdv = dyn_cast<MetadataAsValue>(call->getArgOperand(0));
          const auto* vdm = dyn_cast<ValueAsMetadata>(mdv->getMetadata());
          const auto* md = dyn_cast<MetadataAsValue>(call->getArgOperand(1))
                               ->getMetadata();
          if(not valueNames.contains(vdm->getValue())) {
            if(const auto* di = dyn_cast<DILocalVariable>(md)) {
              valueNames[vdm->getValue()] = di->getName();
              if(unsigned argNo = di->getArg()) {
                const Argument& arg = getArg(f, argNo - 1);
                if(not valueNames.contains(&arg)) {
                  valueNames[&arg] = di->getName();
                  valueNames[vdm->getValue()] += "_l";
                }
              }
            }
          }
        }
      }
    }
  }

  if(const DISubprogram* subp = f.getSubprogram()) {
    std::string buf;
    raw_string_ostream ss(buf);
    if(const auto* cls = dyn_cast_or_null<DICompositeType>(subp->getScope()))
      ss << cls->getName() << "::";
    ss << subp->getName();
    valueNames[&f] = ss.str();

    // In some cases, the function arguments may not be "registered"
    // with llvm.dbg.value. Not sure why this happens, but in that case,
    // look for it directly in the debug info
    for(const DINode* op : subp->getRetainedNodes()) {
      if(const auto* di = dyn_cast<DILocalVariable>(op)) {
        if(di->getArg()) {
          const Argument& arg = getArg(f, di->getArg() - 1);
          if(not valueNames.contains(&arg))
            valueNames[&arg] = di->getName();
        }
      }
    }
  }
}

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

static bool isStringLiteral(const llvm::GlobalVariable& g) {
  if(const auto* init = dyn_cast_or_null<ConstantDataArray>(g.getInitializer()))
    if(init->getType()->getElementType()->isIntegerTy(8))
      if(g.hasGlobalUnnamedAddr() and g.hasPrivateLinkage())
        return true;
  return false;
}

void DIParser::runOnGlobal(const GlobalVariable& g) {
  SmallVector<DIGlobalVariableExpression*, 4> gexprs;
  g.getDebugInfo(gexprs);
  if(gexprs.size()) {
    if(gexprs.size() == 1) {
      const auto* di = dyn_cast<DIGlobalVariable>(gexprs[0]->getVariable());
      valueNames[&g] = di->getName();
      if(cish::isCString(di->getType()))
        cstrings.insert(&g);
    } else {
      WithColor::warning(errs())
          << "Got more than one expression for global: " << g << "\n";
    }
  }
  // Irrespective of whether or not there is debug info associated with a global
  // check if it is a string literal
  if(cish::isStringLiteral(g))
    stringLiterals.insert(&g);
}

void DIParser::runOnStruct(StructType* sty,
                           const DICompositeType* di,
                           const StructLayout& sl) {
  // Don't need to check if the number of operands and the number of
  // elements or the offsets of the elements match up correctly because they
  // will have been checked when associating the types and if they didn't
  // match, the type would not be parsed
  for(const MDOperand& op : di->getElements()->operands())
    elemNames[sty].push_back(dyn_cast<DIDerivedType>(op)->getName());
}

void DIParser::runOnClass(StructType* sty,
                          const DICompositeType* di,
                          const StructLayout& sl) {
  // FIXME: Support for classes
  WithColor::warning(errs()) << "UNIMPLEMENTED:  information for classes\n";
}

void DIParser::runOnUnion(StructType* sty,
                          const DICompositeType* di,
                          const StructLayout& sl) {
  // Nothing to do here for now, but the function is here in case that
  // ever changes
}

static void
diWarning(Type* type, const Metadata* md, const std::string& expected) {
  WithColor::warning(errs())
      << "Expected " << expected << " debug info node. Got ";
  md->print(errs());
  errs() << " for type " << *type << "\n";
}

static void collectStructs(Type* type,
                           const Metadata* md,
                           const DataLayout& m,
                           Map<StructType*, const DICompositeType*>& types);

static void collectStructs(PointerType* pty,
                           const DIDerivedType* derived,
                           const DataLayout& dl,
                           Map<StructType*, const DICompositeType*>& types) {
  if(derived) {
    switch(derived->getTag()) {
    case dwarf::DW_TAG_pointer_type:
      return collectStructs(
          pty->getElementType(), derived->getBaseType(), dl, types);
    case dwarf::DW_TAG_restrict_type:
    case dwarf::DW_TAG_const_type:
      return collectStructs(pty, derived->getBaseType(), dl, types);
    }
    return diWarning(pty, derived, "Derived");
  }
  WithColor::warning(errs())
      << "Expected derived DI node for " << *pty << ". Got null\n";
}

static void collectStructs(ArrayType* aty,
                           const DICompositeType* comp,
                           const DataLayout& dl,
                           Map<StructType*, const DICompositeType*>& types) {
  if(comp) {
    if(comp->getTag() == dwarf::DW_TAG_array_type)
      return collectStructs(getBaseType(aty), comp->getBaseType(), dl, types);
    return diWarning(aty, comp, "Composite");
  }
  WithColor::warning(errs())
      << "Expected composite DI node for " << *aty << ". Got null\n";
}

static void collectStructs(FunctionType* fty,
                           const DIDerivedType* md,
                           const DataLayout& dl,
                           Map<StructType*, const DICompositeType*>& types) {
  WithColor::warning(errs()) << "UNIMPLEMENTED: Associating function types\n";
}

static void
collectStructsFromStruct(StructType* sty,
                         const DICompositeType* comp,
                         const DataLayout& dl,
                         Map<StructType*, const DICompositeType*>& types) {
  const StructLayout& sl = *dl.getStructLayout(sty);
  const auto& elements = comp->getElements();
  types[sty] = comp;
  if(elements->getNumOperands() == sty->getNumElements()) {
    for(unsigned i = 0; i < elements->getNumOperands(); i++) {
      // This will be node with tag DW_TAG_member. To get the node containing
      // the actual type, we need to call getBaseType() on it
      const auto* derived = dyn_cast<DIDerivedType>(elements->getOperand(i));
      size_t offset = sl.getElementOffset(i) * 8;
      if(derived->getOffsetInBits() == offset) {
        collectStructs(
            sty->getElementType(i), derived->getBaseType(), dl, types);
      } else {
        WithColor::warning(errs()) << "Mismatched offsets of field " << i
                                   << " for struct " << *sty << "\n";
        goto fail;
      }
    }
  } else {
    WithColor::warning(errs()) << "Mismatch in number of fields of struct "
                               << comp->getName() << " => " << *sty << "\n";
    goto fail;
  }

  return;

fail:
  // Keep the struct in the typemap because otherwise we might infinitely
  // recurse in the case of a recursive struct
  types[sty] = nullptr;
}

static void
collectStructsFromUnion(StructType* sty,
                        const DICompositeType* md,
                        const DataLayout& dl,
                        Map<StructType*, const DICompositeType*>& types) {
  // At some point, I might figure out if this can be used in any reasonable
  // way
}

static void
collectStructsFromClass(StructType* sty,
                        const DICompositeType* md,
                        const DataLayout& dl,
                        Map<StructType*, const DICompositeType*>& types) {
  WithColor::warning(errs()) << "UNIMPLEMENTED: Associating class types\n";
  types[sty] = nullptr;
}

static void collectStructs(Type* type,
                           const Metadata* md,
                           const DataLayout& dl,
                           Map<StructType*, const DICompositeType*>& types) {
  if(not md)
    return;

  if(auto* pty = dyn_cast<PointerType>(type)) {
    collectStructs(pty, dyn_cast<DIDerivedType>(md), dl, types);
  } else if(auto* aty = dyn_cast<ArrayType>(type)) {
    collectStructs(aty, dyn_cast<DICompositeType>(md), dl, types);
  } else if(auto* fty = dyn_cast<FunctionType>(type)) {
    collectStructs(fty, cast<DIDerivedType>(md), dl, types);
  } else if(auto* sty = dyn_cast<StructType>(type)) {
    if(not types.contains(sty)) {
      const auto comp = dyn_cast<DICompositeType>(md);
      // The struct types may contain other structs which may not be accessed
      // outside the struct. So recurse into the struct
      switch(comp->getTag()) {
      case dwarf::DW_TAG_structure_type:
        collectStructsFromStruct(sty, comp, dl, types);
        break;
      case dwarf::DW_TAG_class_type:
        collectStructsFromClass(sty, comp, dl, types);
        break;
      case dwarf::DW_TAG_union_type:
        collectStructsFromUnion(sty, comp, dl, types);
        break;
      default:
        break;
      }
    }
  }
}

static Map<StructType*, const DICompositeType*>
collectStructs(const Module& m) {
  const DataLayout& dl = m.getDataLayout();
  Map<StructType*, const DICompositeType*> structs;

  // There is no direct way to associate StructType's with some
  // debug info node. So try and find any function arguments, return types,
  // globals or locals that might have a struct type somewhere
  // and use those to try and associate the types
  for(const Function& f : m.functions()) {
    FunctionType* fty = f.getFunctionType();
    if(const DISubprogram* subp = f.getSubprogram()) {
      const auto* type = dyn_cast<DISubroutineType>(subp->getType());
      const auto types = type->getTypeArray();

      // The first argument could have an sret attribute which means that
      // it is an argument for a struct that was returned by value.
      unsigned start = 0;
      Type* retTy = fty->getReturnType();
      if(fty->getNumParams()) {
        if(getArg(f, 0).hasStructRetAttr()) {
          start += 1;
          retTy = getArg(f, 0).getType();
        }
      }

      if((types->getNumOperands() - 1) != (fty->getNumParams() - start)) {
        WithColor::warning(errs())
            << "Mismatch in number of arguments for function " << f.getName()
            << "\n";
        WithColor::warning(errs())
            << "Skipping type association for function: " << f.getName()
            << "\n";
        continue;
      }

      collectStructs(retTy, types->getOperand(0), dl, structs);
      for(unsigned i = 1, j = start; i < types->getNumOperands(); i++, j++)
        // FIXME: Newer versions of LLVM have a getArg() function in
        // Function
        collectStructs(
            getArg(f, j).getType(), types->getOperand(i), dl, structs);
    }

    for(const Instruction& inst : instructions(f)) {
      if(const auto* call = dyn_cast<CallInst>(&inst)) {
        if(const Function* f = call->getCalledFunction()) {
          if(f->getName() == "llvm.dbg.value") {
            const auto* mdv = dyn_cast<MetadataAsValue>(call->getArgOperand(0));
            const auto* vdm = dyn_cast<ValueAsMetadata>(mdv->getMetadata());
            const auto* md = dyn_cast<MetadataAsValue>(call->getArgOperand(1))
                                 ->getMetadata();
            if(const auto* alloca = dyn_cast<AllocaInst>(vdm->getValue()))
              collectStructs(alloca->getAllocatedType(),
                             dyn_cast<DILocalVariable>(md)->getType(),
                             dl,
                             structs);
          }
        }
      }
    }
  }

  for(const GlobalVariable& g : m.globals()) {
    SmallVector<DIGlobalVariableExpression*, 4> gexprs;
    g.getDebugInfo(gexprs);
    if(gexprs.size() == 1) {
      if(const auto* gv = dyn_cast<DIGlobalVariable>(gexprs[0]->getVariable()))
        collectStructs(
            g.getType()->getElementType(), gv->getType(), dl, structs);
    }
  }

  return structs;
}

void DIParser::runOnModule(const Module& m) {
  for(const Function& f : m.functions())
    runOnFunction(f);
  for(const GlobalVariable& g : m.globals())
    runOnGlobal(g);

  const DataLayout& dl = m.getDataLayout();
  for(const auto& p : collectStructs(m)) {
    StructType* sty = p.first;
    const DICompositeType* di = p.second;
    const StructLayout& sl = *dl.getStructLayout(sty);

    if(di->getTag() == dwarf::DW_TAG_structure_type)
      runOnStruct(sty, di, sl);
    else if(di->getTag() == dwarf::DW_TAG_class_type)
      runOnClass(sty, di, sl);
    else if(di->getTag() == dwarf::DW_TAG_union_type)
      runOnUnion(sty, di, sl);
    else
      WithColor::warning(errs()) << "Unexpected DI tag\n";

    // FIXME: There might be namespace and/or parent information in the struct
    // name. Make use of it
    structNames[sty] = di->getName();
  }
}

bool DIParser::isCString(const llvm::Value* val) const {
  return cstrings.contains(val);
}

bool DIParser::isCString(const llvm::Value& val) const {
  return isCString(&val);
}

bool DIParser::isStringLiteral(const llvm::Value* val) const {
  return stringLiterals.contains(val);
}

bool DIParser::isStringLiteral(const llvm::Value& val) const {
  return stringLiterals.contains(&val);
}

bool DIParser::hasName(const llvm::Value* val) const {
  return valueNames.contains(val);
}

bool DIParser::hasName(const llvm::Value& val) const {
  return hasName(&val);
}

bool DIParser::hasName(llvm::StructType* sty) const {
  return structNames.contains(sty);
}

bool DIParser::hasElementName(llvm::StructType* sty, unsigned i) const {
  if(structNames.contains(sty))
    return structNames.at(sty).size() > i;
  return false;
}

const std::string& DIParser::getName(const llvm::Value* v) const {
  return valueNames.at(v);
}

const std::string& DIParser::getName(const llvm::Value& v) const {
  return getName(&v);
}

const std::string& DIParser::getName(llvm::StructType* sty) const {
  return structNames.at(sty);
}

const std::string& DIParser::getElementName(llvm::StructType* sty,
                                            unsigned i) const {
  return elemNames.at(sty).at(i);
}

} // namespace cish
