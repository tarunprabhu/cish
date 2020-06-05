#include "Context.h"
#include "LLVMUtils.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/BinaryFormat/Dwarf.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

namespace dwarf = llvm::dwarf;

namespace cish {

using llvm::cast;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;

Context::Context(const std::string& prefix) : varPrefix(prefix), varSuffix(0) {
  ;
}

std::string Context::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix;
  if(prefix.length())
    ss << prefix << "_";
  ss << varSuffix;

  varSuffix++;

  return ss.str();
}

const Type& Context::add(llvm::Type* type, const std::string& s) {
  types[type].reset(new Type(*this, type, s));
  return get(type);
}

void Context::addElementName(llvm::StructType* sty, const std::string& field) {
  elemNames[sty].push_back(field);
}

Expr& Context::overwrite(const llvm::Value* val, const std::string& name) {
  ovrs[val] = std::move(exprs.at(val));
  return add<Expr>(val, name);
}

Expr& Context::overwrite(const llvm::Value& val, const std::string& name) {
  return overwrite(&val, name);
}

template <>
Decl& Context::get(const llvm::Value* val) {
  return *decls.at(val);
}

template <>
Expr& Context::get(const llvm::Value* val) {
  return *exprs.at(val);
}

template <>
For& Context::get(const llvm::Loop* loop) {
  return *fors.at(loop);
}

template <>
If& Context::get(const llvm::Value* val) {
  return *ifs.at(val);
}

template <>
Stmt& Context::get(const llvm::Value* val) {
  return *stmts.at(val);
}

template <>
While& Context::get(const llvm::Loop* loop) {
  return *whiles.at(loop);
}

template <>
bool Context::has<Decl>(const llvm::Value* val) const {
  return decls.contains(val);
}

template <>
bool Context::has<Expr>(const llvm::Value* val) const {
  return exprs.contains(val);
}

template <>
bool Context::has<For>(const llvm::Loop* loop) const {
  return fors.contains(loop);
}

template <>
bool Context::has<If>(const llvm::Value* val) const {
  return ifs.contains(val);
}

template <>
bool Context::has<Stmt>(const llvm::Value* val) const {
  return stmts.contains(val);
}

bool Context::has(llvm::Type* type) const {
  return types.contains(type);
}

bool Context::hasElementNames(llvm::StructType* sty) const {
  return elemNames.contains(sty);
}

bool Context::hasSourceName(const llvm::Value& val) const {
  return hasSourceName(&val);
}

bool Context::hasSourceName(const llvm::Value* val) const {
  return valueNames.contains(val);
}

bool Context::hasSourceName(llvm::StructType* sty) const {
  return structNames.contains(sty);
}

template <>
bool Context::has<While>(const llvm::Loop* loop) const {
  return whiles.contains(loop);
}

bool Context::isOverwrite(const llvm::Value* val) const {
  return ovrs.contains(val);
}

bool Context::isOverwrite(const llvm::Value& val) const {
  return isOverwrite(&val);
}

const Expr& Context::getOverwrite(const llvm::Value* val) const {
  return *ovrs.at(val);
}

const Expr& Context::getOverwrite(const llvm::Value& val) const {
  return getOverwrite(&val);
}

template <>
const Decl& Context::get(const llvm::Value* val) const {
  return *decls.at(val);
}

template <>
const Expr& Context::get(const llvm::Value* val) const {
  return *exprs.at(val);
}

template <>
const For& Context::get(const llvm::Loop* loop) const {
  return *fors.at(loop);
}

template <>
const If& Context::get(const llvm::Value* val) const {
  return *ifs.at(val);
}

template <>
const Stmt& Context::get(const llvm::Value* val) const {
  return *stmts.at(val);
}

const Type& Context::get(llvm::Type* type) const {
  return *types.at(type);
}

template <>
const While& Context::get(const llvm::Loop* loop) const {
  return *whiles.at(loop);
}

const std::string& Context::getSourceName(const llvm::Value& val) const {
  return getSourceName(&val);
}

const std::string& Context::getSourceName(const llvm::Value* val) const {
  return valueNames.at(val);
}

const std::string& Context::getSourceName(llvm::StructType* sty) const {
  return structNames.at(sty);
}

const std::string& Context::getElementName(llvm::StructType* sty,
                                           unsigned i) const {
  return elemNames.at(sty).at(i);
}

static bool mdHasName(const llvm::Metadata* md) {
  return isa<llvm::DILocalVariable>(md) or isa<llvm::DISubprogram>(md)
         or isa<llvm::DICompositeType>(md) or isa<llvm::DIGlobalVariable>(md);
}

static std::string mdGetName(const llvm::Metadata* md) {
  if(const auto* lv = dyn_cast<llvm::DILocalVariable>(md))
    return lv->getName();
  else if(const auto* gv = dyn_cast<llvm::DIGlobalVariable>(md))
    return gv->getName();

  llvm::WithColor::error(llvm::errs()) << "Name not found in Metadata\n";
  exit(1);
}

void Context::parseFunctionSource(const llvm::Function& f) {
  if(const llvm::DISubprogram* subp = f.getSubprogram()) {
    std::string buf;
    llvm::raw_string_ostream ss(buf);
    if(const auto* cls
       = dyn_cast_or_null<llvm::DICompositeType>(subp->getScope()))
      ss << cls->getName() << "::";
    ss << subp->getName();
    valueNames[&f] = ss.str();
  }

  for(const llvm::Instruction& inst : llvm::instructions(f)) {
    if(const auto* call = dyn_cast<llvm::CallInst>(&inst)) {
      if(const llvm::Function* f = call->getCalledFunction()) {
        if(f->getName() == "llvm.dbg.value") {
          const auto* mdv
              = dyn_cast<llvm::MetadataAsValue>(call->getArgOperand(0));
          const auto* vdm = dyn_cast<llvm::ValueAsMetadata>(mdv->getMetadata());
          const auto* md
              = dyn_cast<llvm::MetadataAsValue>(call->getArgOperand(1))
                    ->getMetadata();
          if((not valueNames.contains(vdm->getValue())) and mdHasName(md))
            valueNames[vdm->getValue()] = mdGetName(md);
        }
      }
    }
  }
}

void Context::parseGlobalSource(const llvm::GlobalVariable& g) {
  llvm::SmallVector<llvm::DIGlobalVariableExpression*, 4> gexprs;
  g.getDebugInfo(gexprs);
  if(gexprs.size()) {
    if(gexprs.size() == 1) {
      const llvm::Metadata* md = gexprs[0]->getVariable();
      if(mdHasName(md))
        valueNames[&g] = mdGetName(md);
    } else {
      llvm::WithColor::warning(llvm::errs())
          << "Got more than one expression for global: " << g << "\n";
    }
  }
}

void Context::parseStructSource(llvm::StructType* sty,
                                const llvm::DICompositeType* di,
                                const llvm::StructLayout& sl) {
  // Don't need to check if the number of operands and the number of
  // elements or the offsets of the elements match up correctly because they
  // will have been checked when associating the types and if they didn't
  // match, the type would not be parsed
  for(const llvm::MDOperand& op : di->getElements()->operands())
    addElementName(sty, dyn_cast<llvm::DIDerivedType>(op)->getName());
}

void Context::parseClassSource(llvm::StructType* sty,
                               const llvm::DICompositeType* di,
                               const llvm::StructLayout& sl) {
  // FIXME: Support for classes
  llvm::WithColor::warning(llvm::errs())
      << "UNIMPLEMENTED: Source information for classes\n";
}

void Context::parseUnionSource(llvm::StructType* sty,
                               const llvm::DICompositeType* di,
                               const llvm::StructLayout& sl) {
  // Nothing to do here for now, but the function is here in case that
  // ever changes
}

static int diWarning(llvm::Type* type,
                     const llvm::Metadata* md,
                     const std::string& expected) {
  llvm::WithColor::warning(llvm::errs())
      << "Expected " << expected << " debug info node. Got ";
  md->print(llvm::errs());
  llvm::errs() << " for type " << *type << "\n";

  return 1;
}

static int
associateTypes(llvm::Type* type,
               const llvm::Metadata* md,
               const llvm::DataLayout& m,
               Map<llvm::StructType*, const llvm::DICompositeType*>& types);

static int
associateTypes(llvm::PointerType* pty,
               const llvm::DIDerivedType* derived,
               const llvm::DataLayout& dl,
               Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  if(derived) {
    if(derived->getTag() == dwarf::DW_TAG_pointer_type)
      return associateTypes(
          pty->getElementType(), derived->getBaseType(), dl, types);
    return diWarning(pty, derived, "Derived");
  }
  llvm::WithColor::warning(llvm::errs())
      << "Expected derived DI node for " << *pty << ". Got null\n";
  return 1;
}

static int
associateTypes(llvm::ArrayType* aty,
               const llvm::DICompositeType* comp,
               const llvm::DataLayout& dl,
               Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  if(comp) {
    if(comp->getTag() == dwarf::DW_TAG_array_type)
      return associateTypes(
          aty->getElementType(), comp->getBaseType(), dl, types);
    return diWarning(aty, comp, "Composite");
  }
  llvm::WithColor::warning(llvm::errs())
      << "Expected composite DI node for " << *aty << ". Got null\n";
  return 1;
}

static int
associateTypes(llvm::FunctionType* fty,
               const llvm::DIDerivedType* md,
               const llvm::DataLayout& dl,
               Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  llvm::WithColor::warning(llvm::errs())
      << "UNIMPLEMENTED: Associating function types\n";
  return 2;
}

static int associateStructTypes(
    llvm::StructType* sty,
    const llvm::DICompositeType* comp,
    const llvm::DataLayout& dl,
    Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  const llvm::StructLayout& sl = *dl.getStructLayout(sty);
  const auto& elements = comp->getElements();
  types[sty] = comp;
  if(elements->getNumOperands() == sty->getNumElements()) {
    for(unsigned i = 0; i < elements->getNumOperands(); i++) {
      // This will be node with tag DW_TAG_member. To get the node containing
      // the actual type, we need to call getBaseType() on it
      const auto* derived
          = dyn_cast<llvm::DIDerivedType>(elements->getOperand(i));
      size_t offset = sl.getElementOffset(i) * 8;
      if(derived->getOffsetInBits() == offset) {
        associateTypes(
            sty->getElementType(i), derived->getBaseType(), dl, types);
      } else {
        llvm::WithColor::warning(llvm::errs())
            << "Mismatched offsets of field " << i << " for struct " << *sty
            << "\n";
        goto fail;
      }
    }
  } else {
    llvm::WithColor::warning(llvm::errs())
        << "Mismatch in number of fields of struct " << comp->getName()
        << " => " << *sty << "\n";
    goto fail;
  }

  return 0;

fail:
  // Keep the struct in the typemap because otherwise we might infinitely
  // recurse in the case of a recursive struct
  types[sty] = nullptr;
  return 1;
}

static int associateUnionTypes(
    llvm::StructType* sty,
    const llvm::DICompositeType* md,
    const llvm::DataLayout& dl,
    Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  // At some point, I might figure out if this can be used in any reasonable
  // way
  return 0;
}

static int associateClassTypes(
    llvm::StructType* sty,
    const llvm::DICompositeType* md,
    const llvm::DataLayout& dl,
    Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  llvm::WithColor::warning(llvm::errs())
      << "UNIMPLEMENTED: Associating class types\n";
  types[sty] = nullptr;
  return 2;
}

static const llvm::Module* g_m = nullptr;

static int
associateTypes(llvm::Type* type,
               const llvm::Metadata* md,
               const llvm::DataLayout& dl,
               Map<llvm::StructType*, const llvm::DICompositeType*>& types) {
  if(not md)
    return 1;

  if(auto* pty = dyn_cast<llvm::PointerType>(type)) {
    return associateTypes(pty, dyn_cast<llvm::DIDerivedType>(md), dl, types);
  } else if(auto* aty = dyn_cast<llvm::ArrayType>(type)) {
    return associateTypes(aty, dyn_cast<llvm::DICompositeType>(md), dl, types);
  } else if(auto* fty = dyn_cast<llvm::FunctionType>(type)) {
    return associateTypes(fty, cast<llvm::DIDerivedType>(md), dl, types);
  } else if(auto* sty = dyn_cast<llvm::StructType>(type)) {
    if(not types.contains(sty)) {
      const auto comp = dyn_cast<llvm::DICompositeType>(md);
      // The struct types may contain other structs which may not be accessed
      // outside the struct. So recurse into the struct
      switch(comp->getTag()) {
      case dwarf::DW_TAG_structure_type:
        return associateStructTypes(sty, comp, dl, types);
      case dwarf::DW_TAG_class_type:
        return associateClassTypes(sty, comp, dl, types);
      case dwarf::DW_TAG_union_type:
        return associateUnionTypes(sty, comp, dl, types);
      default:
        break;
      }
    }
  }
  return 0;
}

void Context::parseSourceInformation(const llvm::Module& m) {
  g_m = &m;

  for(const llvm::Function& f : m.functions())
    parseFunctionSource(f);
  for(const llvm::GlobalVariable& g : m.globals())
    parseGlobalSource(g);

  const llvm::DataLayout& dl = m.getDataLayout();
  Map<llvm::StructType*, const llvm::DICompositeType*> structs;

  // There is no direct way to associate llvm::StructType's with some
  // debug info node. So try and find any function arguments, return types,
  // globals or locals that might have a struct type somewhere
  // and use those to try and associate the types
  for(const llvm::Function& f : m.functions()) {
    llvm::FunctionType* fty = f.getFunctionType();
    if(const llvm::DISubprogram* subp = f.getSubprogram()) {
      const auto* type = dyn_cast<llvm::DISubroutineType>(subp->getType());
      const auto types = type->getTypeArray();

      // The first argument could have an sret attribute which means that
      // it is an argument for a struct that was returned by value.
      unsigned start = 0;
      llvm::Type* retTy = fty->getReturnType();
      if(fty->getNumParams()) {
        if(getArg(f, 0).hasStructRetAttr()) {
          start += 1;
          retTy = getArg(f, 0).getType();
        }
      }

      if((types->getNumOperands() - 1) != (fty->getNumParams() - start)) {
        llvm::WithColor::warning(llvm::errs())
            << "Mismatch in number of arguments for function " << f.getName()
            << "\n";
        llvm::WithColor::warning(llvm::errs())
            << "Skipping type association for function: " << f.getName()
            << "\n";
        continue;
      }

      associateTypes(retTy, types->getOperand(0), dl, structs);
      for(unsigned i = 1, j = start; i < types->getNumOperands(); i++, j++)
        // FIXME: Newer versions of LLVM have a getArg() function in
        // llvm::Function
        associateTypes(
            getArg(f, j).getType(), types->getOperand(i), dl, structs);
    }

    for(const llvm::Instruction& inst : llvm::instructions(f)) {
      if(const auto* call = dyn_cast<llvm::CallInst>(&inst)) {
        if(const llvm::Function* f = call->getCalledFunction()) {
          if(f->getName() == "llvm.dbg.value") {
            const auto* mdv
                = dyn_cast<llvm::MetadataAsValue>(call->getArgOperand(0));
            const auto* vdm
                = dyn_cast<llvm::ValueAsMetadata>(mdv->getMetadata());
            const auto* md
                = dyn_cast<llvm::MetadataAsValue>(call->getArgOperand(1))
                      ->getMetadata();
            if(const auto* alloca = dyn_cast<llvm::AllocaInst>(vdm->getValue()))
              associateTypes(alloca->getAllocatedType(),
                             dyn_cast<llvm::DILocalVariable>(md)->getType(),
                             dl,
                             structs);
          }
        }
      }
    }
  }

  for(const llvm::GlobalVariable& g : m.globals()) {
    llvm::SmallVector<llvm::DIGlobalVariableExpression*, 4> gexprs;
    g.getDebugInfo(gexprs);
    if(gexprs.size() == 1) {
      if(const auto* gv
         = dyn_cast<llvm::DIGlobalVariable>(gexprs[0]->getVariable()))
        associateTypes(
            g.getType()->getElementType(), gv->getType(), dl, structs);
    }
  }

  for(const auto& p : structs) {
    llvm::StructType* sty = p.first;
    const llvm::DICompositeType* di = p.second;
    const llvm::StructLayout& sl = *dl.getStructLayout(sty);
    std::string buf;
    llvm::raw_string_ostream sname(buf);

    if(di->getTag() == dwarf::DW_TAG_structure_type) {
      sname << "struct";
      parseStructSource(sty, di, sl);
    } else if(di->getTag() == dwarf::DW_TAG_class_type) {
      sname << "class";
      parseClassSource(sty, di, sl);
    } else if(di->getTag() == dwarf::DW_TAG_union_type) {
      sname << "union";
      parseUnionSource(sty, di, sl);
    } else {
      llvm::WithColor::warning(llvm::errs()) << "Unexpected DWARF tag\n";
    }

    // FIXME: There might be namespace and/or parent information in the struct
    // name. Make use of it
    sname << " " << di->getName();
    structNames[sty] = sname.str();
  }
}

} // namespace cish
