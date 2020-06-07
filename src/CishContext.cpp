#include "CishContext.h"
#include "LLVMUtils.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

using llvm::cast;
using llvm::dyn_cast;
using llvm::dyn_cast_or_null;
using llvm::isa;

static FullSourceLoc invLoc;

CishContext::CishContext(const std::string& prefix, ASTContext& astContext)
    : currFunc(nullptr), varPrefix(prefix), varSuffix(0),
      astContext(astContext) {
  ;
}

Decl* CishContext::getCurrentDecl() {
  if(currFunc)
    return currFunc;
  else
    return astContext.getTranslationUnitDecl();
}

std::string CishContext::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix;
  if(prefix.length())
    ss << prefix << "_";
  ss << varSuffix;

  varSuffix++;

  return ss.str();
}

DeclRefExpr& CishContext::add(const llvm::AllocaInst& alloca,
                              const std::string& name) {
  QualType type = get(alloca.getAllocatedType());
  VarDecl* var = VarDecl::Create(astContext,
                                 currFunc,
                                 invLoc,
                                 invLoc,
                                 &astContext.Idents.get(name),
                                 type,
                                 nullptr,
                                 SC_None);
  return add(alloca,
             DeclRefExpr::Create(astContext,
                                 NestedNameSpecifierLoc(),
                                 invLoc,
                                 var,
                                 false,
                                 invLoc,
                                 type,
                                 VK_LValue,
                                 var));
}

Stmt& CishContext::add(const llvm::BranchInst& br) {
  if(llvm::Value* cond = br.getCondition()) {
    auto* thn = new(astContext)
        GotoStmt(&get<LabelDecl>(br.getSuccessor(0)), invLoc, invLoc);
    auto* els = new(astContext)
        GotoStmt(&get<LabelDecl>(br.getSuccessor(1)), invLoc, invLoc);
    return add(br,
               IfStmt::Create(astContext,
                              invLoc,
                              false,
                              nullptr,
                              nullptr,
                              &get<Expr>(cond),
                              thn,
                              invLoc,
                              els));
  } else {
    return add(br,
               new(astContext) GotoStmt(
                   &get<LabelDecl>(br.getSuccessor(0)), invLoc, invLoc));
  }
}

BinaryOperator& CishContext::add(const llvm::BinaryOperator& inst,
                                 BinaryOperator::Opcode opc) {
  return add(inst,
             new(astContext) BinaryOperator(&get<Expr>(inst.getOperand(0)),
                                            &get<Expr>(inst.getOperand(1)),
                                            opc,
                                            get(inst.getType()),
                                            VK_LValue,
                                            OK_Ordinary,
                                            invLoc,
                                            FPOptions()));
}

BinaryOperator& CishContext::add(const llvm::CmpInst& cmp,
                                 BinaryOperator::Opcode opc) {
  return add(cmp,
             new(astContext) BinaryOperator(&get<Expr>(cmp.getOperand(0)),
                                            &get<Expr>(cmp.getOperand(1)),
                                            opc,
                                            get(cmp.getType()),
                                            VK_LValue,
                                            OK_Ordinary,
                                            invLoc,
                                            FPOptions()));
}

UnaryOperator& CishContext::add(const llvm::UnaryOperator& inst,
                                UnaryOperator::Opcode opc) {
  return add(inst,
             new(astContext) UnaryOperator(&get<Expr>(inst.getOperand(0)),
                                           opc,
                                           get(inst.getType()),
                                           VK_LValue,
                                           OK_Ordinary,
                                           invLoc,
                                           false));
}

CallExpr& CishContext::add(const llvm::CallInst& call) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << call << "\n";
  exit(1);
}

Expr& CishContext::add(const llvm::LoadInst& load) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << load << "\n";
  exit(1);
}

BinaryOperator& CishContext::add(const llvm::StoreInst& store) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << store << "\n";
  exit(1);
}

CastExpr& CishContext::add(const llvm::CastInst& cst) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cst << "\n";
  exit(1);
}

ConditionalOperator& CishContext::add(const llvm::SelectInst& select) {
  return add(select,
             new(astContext)
                 ConditionalOperator(&get<Expr>(select.getCondition()),
                                     invLoc,
                                     &get<Expr>(select.getTrueValue()),
                                     invLoc,
                                     &get<Expr>(select.getFalseValue()),
                                     get(select.getType()),
                                     VK_LValue,
                                     OK_Ordinary));
}

ReturnStmt& CishContext::add(const llvm::ReturnInst& ret) {
  if(const llvm::Value* val = ret.getReturnValue()) {
    return add(
        ret, ReturnStmt::Create(astContext, invLoc, &get<Expr>(val), nullptr));
  } else {
    return add(ret, ReturnStmt::CreateEmpty(astContext, false));
  }
}

DeclRefExpr& CishContext::add(const llvm::PHINode& phi,
                              const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << phi << "\n";
  exit(1);
}

DeclRefExpr& CishContext::add(const llvm::Function& f,
                              const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << f << "\n";
  exit(1);
}

DeclRefExpr& CishContext::add(const llvm::GlobalVariable& g,
                              const std::string& name) {
  TranslationUnitDecl* tu = astContext.getTranslationUnitDecl();
  QualType type = get(g.getType()->getElementType());
  VarDecl* decl = VarDecl::Create(astContext,
                                  tu,
                                  invLoc,
                                  invLoc,
                                  &astContext.Idents.get(name),
                                  type,
                                  nullptr,
                                  SC_None);
  tu->addDecl(decl);
  if(const llvm::Constant* init = g.getInitializer())
    decl->setInit(&get<Expr>(init));
  return add(g,
             DeclRefExpr::Create(astContext,
                                 NestedNameSpecifierLoc(),
                                 invLoc,
                                 decl,
                                 false,
                                 invLoc,
                                 type,
                                 VK_LValue,
                                 decl));
}

DeclRefExpr& CishContext::add(const llvm::Argument& arg,
                              const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << arg << "\n";
  exit(1);
}

DeclRefExpr& CishContext::add(const llvm::BasicBlock& bb,
                              const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << bb << "\n";
  exit(1);
}

QualType CishContext::add(llvm::StructType* sty,
                          const std::string& name,
                          const Vector<std::string>& elements) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << *sty << "\n";
  exit(1);
}

IntegerLiteral& CishContext::add(const llvm::ConstantInt& cint) {
  return add(cint,
             IntegerLiteral::Create(
                 astContext, cint.getValue(), get(cint.getType()), invLoc));
}

FloatingLiteral& CishContext::add(const llvm::ConstantFP& cfp) {
  return add(
      cfp,
      FloatingLiteral::Create(
          astContext, cfp.getValueAPF(), false, get(cfp.getType()), invLoc));
}

CXXNullPtrLiteralExpr&
CishContext::add(const llvm::ConstantPointerNull& cnull) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cnull << "\n";
  exit(1);
}

Stmt& CishContext::add(const llvm::ConstantExpr& cexpr,
                       const llvm::Instruction& inst) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cexpr << "\n";
  exit(1);
}

CXXStdInitializerListExpr&
CishContext::add(const llvm::ConstantDataArray& cda) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cda << "\n";
  exit(1);
}

CXXStdInitializerListExpr&
CishContext::add(const llvm::ConstantStruct& cstruct) {
  llvm::WithColor::error(llvm::errs())
      << "NOT IMPLEMENTED: " << cstruct << "\n";
  exit(1);
}

Expr& CishContext::add(const llvm::ConstantArray& carray) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << carray << "\n";
  exit(1);
}

GNUNullExpr& CishContext::add(const llvm::UndefValue& cundef) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cundef << "\n";
  exit(1);
}

QualType CishContext::add(llvm::IntegerType* ity) {
  switch(ity->getBitWidth()) {
  case 1:
    return types[ity] = astContext.BoolTy;
  case 8:
    return types[ity] = astContext.CharTy;
  case 16:
    return types[ity] = astContext.ShortTy;
  case 32:
    return types[ity] = astContext.IntTy;
  case 64:
    return types[ity] = astContext.LongTy;
  case 128:
    return types[ity] = astContext.Int128Ty;
  }

  llvm::WithColor::error(llvm::errs())
      << "UNKNOWN INTEGER TYPE: " << *ity << "\n";
  exit(1);
}

QualType CishContext::add(llvm::PointerType* pty) {
  return types[pty] = astContext.getPointerType(add(pty->getElementType()));
}

QualType CishContext::add(llvm::ArrayType* aty) {
  llvm::APInt elems(64, aty->getNumElements(), false);
  QualType ety = add(aty->getElementType());

  return types[aty]
         = astContext.getConstantArrayType(ety, elems, ArrayType::Normal, 0);
}

QualType CishContext::add(llvm::FunctionType* fty) {
  QualType ret = add(fty->getReturnType());
  std::vector<QualType> args;
  for(llvm::Type* param : fty->params())
    args.push_back(add(param));
  FunctionProtoType::ExtProtoInfo proto;
  proto.Variadic = fty->isVarArg();

  return types[fty] = astContext.getFunctionType(
             ret, llvm::ArrayRef<QualType>(args), proto);
}

QualType CishContext::add(llvm::VectorType* vty) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << *vty << "\n";
  exit(1);
}

QualType CishContext::add(llvm::Type* type) {
  if(has(type))
    return get(type);

  if(type->isVoidTy())
    return types[type] = astContext.VoidTy;
  else if(auto* ity = dyn_cast<llvm::IntegerType>(type))
    return add(ity);
  else if(type->isFloatTy())
    return types[type] = astContext.FloatTy;
  else if(type->isDoubleTy())
    return types[type] = astContext.DoubleTy;
  else if(type->isX86_FP80Ty())
    return types[type] = astContext.LongDoubleTy;
  else if(type->isFP128Ty())
    return types[type] = astContext.Float128Ty;
  else if(auto* pty = dyn_cast<llvm::PointerType>(type))
    return add(pty);
  else if(auto* aty = dyn_cast<llvm::ArrayType>(type))
    return add(aty);

  llvm::WithColor::error(llvm::errs())
      << "COULD NOT ADD TYPE: " << *type << "\n";
  exit(1);
}

DeclRefExpr& CishContext::addTemp(const llvm::Value& val,
                                  const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: "
                                       << "addTemp()"
                                       << "\n";
  exit(1);
}

bool CishContext::has(llvm::Type* type) const {
  return types.contains(type);
}

QualType CishContext::get(llvm::Type* type) const {
  return types.at(type);
}

} // namespace cish
