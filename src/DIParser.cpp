#include "DIParser.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/Metadata.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

#include "LLVMUtils.h"

using namespace llvm;

namespace cish {

static bool mdHasName(const Metadata* md) {
  return isa<DILocalVariable>(md) or isa<DISubprogram>(md)
         or isa<DICompositeType>(md) or isa<DIGlobalVariable>(md);
}

static std::string mdGetName(const Metadata* md) {
  if(const auto* lv = dyn_cast<DILocalVariable>(md))
    return lv->getName();
  else if(const auto* gv = dyn_cast<DIGlobalVariable>(md))
    return gv->getName();

  WithColor::error(errs()) << "Name not found in Metadata\n";
  exit(1);
}

void DIParser::runOnFunction(const Function& f) {
  if(const DISubprogram* subp = f.getSubprogram()) {
    std::string buf;
    raw_string_ostream ss(buf);
    if(const auto* cls = dyn_cast_or_null<DICompositeType>(subp->getScope()))
      ss << cls->getName() << "::";
    ss << subp->getName();
    valueNames[&f] = ss.str();
  }

  for(const Instruction& inst : instructions(f)) {
    if(const auto* call = dyn_cast<CallInst>(&inst)) {
      if(const Function* f = call->getCalledFunction()) {
        if(f->getName() == "llvm.dbg.value") {
          const auto* mdv = dyn_cast<MetadataAsValue>(call->getArgOperand(0));
          const auto* vdm = dyn_cast<ValueAsMetadata>(mdv->getMetadata());
          const auto* md = dyn_cast<MetadataAsValue>(call->getArgOperand(1))
                               ->getMetadata();
          if((not valueNames.contains(vdm->getValue())) and mdHasName(md))
            valueNames[vdm->getValue()] = mdGetName(md);
        }
      }
    }
  }
}

void DIParser::runOnGlobal(const GlobalVariable& g) {
  SmallVector<DIGlobalVariableExpression*, 4> gexprs;
  g.getDebugInfo(gexprs);
  if(gexprs.size()) {
    if(gexprs.size() == 1) {
      const Metadata* md = gexprs[0]->getVariable();
      if(mdHasName(md))
        valueNames[&g] = mdGetName(md);
    } else {
      WithColor::warning(errs())
          << "Got more than one expression for global: " << g << "\n";
    }
  }
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
  WithColor::warning(errs())
      << "UNIMPLEMENTED: Source information for classes\n";
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

static void collectTypes(Type* type,
                         const Metadata* md,
                         const DataLayout& m,
                         Map<StructType*, const DICompositeType*>& types);

static void collectTypes(PointerType* pty,
                         const DIDerivedType* derived,
                         const DataLayout& dl,
                         Map<StructType*, const DICompositeType*>& types) {
  if(derived) {
    switch(derived->getTag()) {
    case dwarf::DW_TAG_pointer_type:
    case dwarf::DW_TAG_restrict_type:
      return collectTypes(
          pty->getElementType(), derived->getBaseType(), dl, types);
    }
    return diWarning(pty, derived, "Derived");
  }
  WithColor::warning(errs())
      << "Expected derived DI node for " << *pty << ". Got null\n";
}

static void collectTypes(ArrayType* aty,
                         const DICompositeType* comp,
                         const DataLayout& dl,
                         Map<StructType*, const DICompositeType*>& types) {
  if(comp) {
    if(comp->getTag() == dwarf::DW_TAG_array_type)
      return collectTypes(
          aty->getElementType(), comp->getBaseType(), dl, types);
    return diWarning(aty, comp, "Composite");
  }
  WithColor::warning(errs())
      << "Expected composite DI node for " << *aty << ". Got null\n";
}

static void collectTypes(FunctionType* fty,
                         const DIDerivedType* md,
                         const DataLayout& dl,
                         Map<StructType*, const DICompositeType*>& types) {
  WithColor::warning(errs()) << "UNIMPLEMENTED: Associating function types\n";
}

static void
collectTypesFromStruct(StructType* sty,
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
        collectTypes(sty->getElementType(i), derived->getBaseType(), dl, types);
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
collectTypesFromUnion(StructType* sty,
                      const DICompositeType* md,
                      const DataLayout& dl,
                      Map<StructType*, const DICompositeType*>& types) {
  // At some point, I might figure out if this can be used in any reasonable
  // way
}

static void
collectTypesFromClass(StructType* sty,
                      const DICompositeType* md,
                      const DataLayout& dl,
                      Map<StructType*, const DICompositeType*>& types) {
  WithColor::warning(errs()) << "UNIMPLEMENTED: Associating class types\n";
  types[sty] = nullptr;
}

static void collectTypes(Type* type,
                         const Metadata* md,
                         const DataLayout& dl,
                         Map<StructType*, const DICompositeType*>& types) {
  if(not md)
    return;

  if(auto* pty = dyn_cast<PointerType>(type)) {
    collectTypes(pty, dyn_cast<DIDerivedType>(md), dl, types);
  } else if(auto* aty = dyn_cast<ArrayType>(type)) {
    collectTypes(aty, dyn_cast<DICompositeType>(md), dl, types);
  } else if(auto* fty = dyn_cast<FunctionType>(type)) {
    collectTypes(fty, cast<DIDerivedType>(md), dl, types);
  } else if(auto* sty = dyn_cast<StructType>(type)) {
    if(not types.contains(sty)) {
      const auto comp = dyn_cast<DICompositeType>(md);
      // The struct types may contain other structs which may not be accessed
      // outside the struct. So recurse into the struct
      switch(comp->getTag()) {
      case dwarf::DW_TAG_structure_type:
        collectTypesFromStruct(sty, comp, dl, types);
      case dwarf::DW_TAG_class_type:
        collectTypesFromClass(sty, comp, dl, types);
      case dwarf::DW_TAG_union_type:
        collectTypesFromUnion(sty, comp, dl, types);
      default:
        break;
      }
    }
  }
}

static Map<StructType*, const DICompositeType*> collectTypes(const Module& m) {
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

      collectTypes(retTy, types->getOperand(0), dl, structs);
      for(unsigned i = 1, j = start; i < types->getNumOperands(); i++, j++)
        // FIXME: Newer versions of LLVM have a getArg() function in
        // Function
        collectTypes(getArg(f, j).getType(), types->getOperand(i), dl, structs);
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
              collectTypes(alloca->getAllocatedType(),
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
        collectTypes(g.getType()->getElementType(), gv->getType(), dl, structs);
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
  for(const auto& p : collectTypes(m)) {
    StructType* sty = p.first;
    const DICompositeType* di = p.second;
    const StructLayout& sl = *dl.getStructLayout(sty);
    std::string buf;
    raw_string_ostream sname(buf);

    if(di->getTag() == dwarf::DW_TAG_structure_type) {
      sname << "struct";
      runOnStruct(sty, di, sl);
    } else if(di->getTag() == dwarf::DW_TAG_class_type) {
      sname << "class";
      runOnClass(sty, di, sl);
    } else if(di->getTag() == dwarf::DW_TAG_union_type) {
      sname << "union";
      runOnUnion(sty, di, sl);
    } else {
      WithColor::warning(errs()) << "Unexpected DI tag\n";
    }

    // FIXME: There might be namespace and/or parent information in the struct
    // name. Make use of it
    sname << " " << di->getName();
    structNames[sty] = sname.str();
  }
}

bool DIParser::hasSourceName(const llvm::Value* val) const {
  return valueNames.contains(val);
}

bool DIParser::hasSourceName(const llvm::Value& val) const {
  return hasSourceName(&val);
}

bool DIParser::hasSourceName(llvm::StructType* sty) const {
  return structNames.contains(sty);
}

const std::string& DIParser::getSourceName(const llvm::Value* v) const {
  return valueNames.at(v);
}

const std::string& DIParser::getSourceName(const llvm::Value& v) const {
  return getSourceName(&v);
}

const std::string& DIParser::getSourceName(llvm::StructType* sty) const {
  return structNames.at(sty);
}

const std::string& DIParser::getSourceName(llvm::StructType* sty,
                                           unsigned i) const {
  return elemNames.at(sty).at(i);
}

} // namespace cish
