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

// We obviously don't have any SourceLocations, so just use this wherever
// we need a clang::SourceLocation
static FullSourceLoc invLoc;

CishContext::CishContext(const std::string& prefix, ASTContext& astContext)
    : varPrefix(prefix), varSuffix(0), astContext(astContext) {
  ;
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

void CishContext::beginFunction(const llvm::Function& f) {
  stmts.clear();
}

void CishContext::endFunction(const llvm::Function& f) {
  CompoundStmt* body = CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmts), invLoc, invLoc);
  funcs.at(&f)->setBody(body);
}

void CishContext::beginBlock(const llvm::BasicBlock& bb) {
  stmts.push_back(new(astContext) LabelStmt(invLoc, blocks.at(&bb), nullptr));
}

void CishContext::endBlock(const llvm::BasicBlock& bb) {
  // Nothing to do here
}

Stmt& CishContext::add(const llvm::AllocaInst& alloca,
                       const std::string& name) {
  QualType type = get(alloca.getAllocatedType());
  VarDecl* var = VarDecl::Create(astContext,
                                 funcs.at(&getFunction(alloca)),
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
        GotoStmt(blocks.at(br.getSuccessor(0)), invLoc, invLoc);
    auto* els = new(astContext)
        GotoStmt(blocks.at(br.getSuccessor(1)), invLoc, invLoc);
    add(br,
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
    add(br,
        new(astContext)
            GotoStmt(blocks.at(br.getSuccessor(0)), invLoc, invLoc));
  }
  stmts.push_back(&get(br));
  return *stmts.back();
}

Stmt& CishContext::add(const llvm::BinaryOperator& inst,
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

Stmt& CishContext::add(const llvm::CmpInst& cmp, BinaryOperator::Opcode opc) {
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

Stmt& CishContext::add(const llvm::UnaryOperator& inst,
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

Stmt& CishContext::add(const llvm::CallInst& call) {
  std::vector<Expr*> exprs;
  for(const llvm::Value* arg : call.arg_operands())
    exprs.push_back(&get<Expr>(arg));

  add(call,
      CallExpr::Create(astContext,
                       &get<Expr>(call.getCalledValue()),
                       llvm::ArrayRef<Expr*>(exprs),
                       get(call.getType()),
                       VK_RValue,
                       invLoc));
  if(call.getType()->isVoidTy())
    stmts.push_back(&get<CallExpr>(call));

  return get(call);
}

Stmt& CishContext::add(const llvm::LoadInst& load) {
  return add(load,
             new(astContext) UnaryOperator(&get<Expr>(load.getPointerOperand()),
                                           UO_Deref,
                                           get(load.getType()),
                                           VK_RValue,
                                           OK_Ordinary,
                                           invLoc,
                                           false));
}

Stmt& CishContext::add(const llvm::StoreInst& store) {
  const llvm::Value* ptr = store.getPointerOperand();
  const llvm::Value* val = store.getValueOperand();
  auto* assign = new(astContext) BinaryOperator(&get<Expr>(ptr),
                                                &get<Expr>(val),
                                                BO_Assign,
                                                get(val->getType()),
                                                VK_LValue,
                                                OK_Ordinary,
                                                invLoc,
                                                FPOptions());
  stmts.push_back(assign);
  return add(store, assign);
}

Stmt& CishContext::add(const llvm::CastInst& cst) {
  return add(cst,
             CStyleCastExpr::Create(astContext,
                                    get(cst.getType()),
                                    VK_RValue,
                                    CastKind::CK_BitCast,
                                    &get<Expr>(cst.getOperand(0)),
                                    nullptr,
                                    nullptr,
                                    invLoc,
                                    invLoc));
}

Stmt& CishContext::add(const llvm::SelectInst& select) {
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

Stmt& CishContext::add(const llvm::ReturnInst& ret) {
  if(const llvm::Value* val = ret.getReturnValue()) {
    add(ret, ReturnStmt::Create(astContext, invLoc, &get<Expr>(val), nullptr));
  } else {
    add(ret, ReturnStmt::CreateEmpty(astContext, false));
  }
  stmts.push_back(&get(ret));
  return *stmts.back();
}

Stmt& CishContext::add(const llvm::PHINode& phi, const std::string& name) {
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << phi << "\n";
  exit(1);
}

Stmt& CishContext::add(const llvm::Argument& arg, const std::string& name) {
  // We could look for dereferenceable bytes in the argument and then display
  // it as a byref parameter, but that gets problematic because then we would
  // also have to go in and handle anything that was originally a reference
  // in the original code. The point is not really to get back the original
  // code but make it easier to understand what the compiler has done to the
  // code. In this case, it is good to understand that what is a reference in
  // C++ is just a non-null pointer under the hood.
  //
  // FIXME: Add argument attributes like const, restrict, non-null etc. that
  // were inferred by the compiler and/or were present in the original code
  QualType type = get(arg.getType());
  FunctionDecl* func = funcs.at(arg.getParent());
  ParmVarDecl* param = ParmVarDecl::Create(astContext,
                                           func,
                                           invLoc,
                                           invLoc,
                                           &astContext.Idents.get(name),
                                           type,
                                           nullptr,
                                           SC_None,
                                           nullptr);

  return add(arg,
             DeclRefExpr::Create(astContext,
                                 NestedNameSpecifierLoc(),
                                 invLoc,
                                 param,
                                 false,
                                 invLoc,
                                 type,
                                 VK_LValue,
                                 param));
}

Stmt& CishContext::add(const llvm::Function& f,
                       const std::string& name,
                       const Vector<std::string>& argNames) {
  QualType type = get(f.getFunctionType());
  funcs[&f]
      = FunctionDecl::Create(astContext,
                             astContext.getTranslationUnitDecl(),
                             invLoc,
                             invLoc,
                             DeclarationName(&astContext.Idents.get(name)),
                             type,
                             nullptr,
                             SC_None);
  astContext.getTranslationUnitDecl()->addDecl(funcs.at(&f));

  std::vector<ParmVarDecl*> args;
  for(unsigned i = 0; i < f.getFunctionType()->getNumParams(); i++) {
    const llvm::Argument& arg = getArg(f, i);
    if(argNames.size())
      add(arg, argNames[i]);
    else
      add(arg, "");
    args.push_back(&get<ParmVarDecl>(arg));
  }
  funcs.at(&f)->setParams(ArrayRef<ParmVarDecl*>(args));

  return add(f,
             DeclRefExpr::Create(astContext,
                                 NestedNameSpecifierLoc(),
                                 invLoc,
                                 funcs.at(&f),
                                 false,
                                 invLoc,
                                 type,
                                 VK_LValue,
                                 funcs.at(&f)));
}

Stmt& CishContext::add(const llvm::GlobalVariable& g, const std::string& name) {
  TranslationUnitDecl* tu = astContext.getTranslationUnitDecl();
  QualType type = get(g.getType()->getElementType());
  if(g.isConstant())
    type.addConst();
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

void CishContext::add(const llvm::BasicBlock& bb, const std::string& name) {
  blocks[&bb] = LabelDecl::Create(astContext,
                                  funcs.at(bb.getParent()),
                                  invLoc,
                                  &astContext.Idents.get(name));
}

QualType CishContext::add(llvm::StructType* sty, const std::string& name) {
  RecordDecl* decl = RecordDecl::Create(astContext,
                                        TTK_Struct,
                                        astContext.getTranslationUnitDecl(),
                                        invLoc,
                                        invLoc,
                                        &astContext.Idents.get(name));

  astContext.getTranslationUnitDecl()->addDecl(decl);
  udts[sty] = decl;
  return types[sty] = QualType(udts.at(sty)->getTypeForDecl(), 0);
}

QualType CishContext::add(llvm::StructType* sty,
                          const Vector<std::string>& elements) {
  RecordDecl* record = udts.at(sty);
  for(unsigned i = 0; i < sty->getNumElements(); i++) {
    FieldDecl* field = FieldDecl::Create(astContext,
                                         udts.at(sty),
                                         invLoc,
                                         invLoc,
                                         &astContext.Idents.get(elements[i]),
                                         get(sty->getElementType(i)),
                                         nullptr,
                                         nullptr,
                                         true,
                                         ICIS_NoInit);
    record->addDecl(field);
  }

  // Not sure if adding a body to the struct will result in the underlying
  // RecordType changing. Just in case, add the type back to the map
  return types[sty] = QualType(udts.at(sty)->getTypeForDecl(), 0);
}

Stmt& CishContext::add(const llvm::ConstantInt& cint) {
  llvm::Type* type = cint.getType();
  if(type->isIntegerTy(1))
    return add(cint,
               new(astContext) CXXBoolLiteralExpr(
                   (bool)cint.getLimitedValue(), get(type), invLoc));
  else
    return add(cint,
               IntegerLiteral::Create(
                   astContext, cint.getValue(), get(cint.getType()), invLoc));
}

Stmt& CishContext::add(const llvm::ConstantFP& cfp) {
  return add(
      cfp,
      FloatingLiteral::Create(
          astContext, cfp.getValueAPF(), false, get(cfp.getType()), invLoc));
}

Stmt& CishContext::add(const llvm::ConstantPointerNull& cnull) {
  return add(cnull,
             new(astContext) CXXNullPtrLiteralExpr(get(cnull.getType()), invLoc)

  );
}

Stmt& CishContext::add(const llvm::ConstantAggregateZero& czero) {
  // FIXME: Implement this properly. It's not that hard. Just recursively
  // build an init list with zeros in it.
  llvm::Type* i64 = llvm::Type::getInt64Ty(czero.getContext());
  add(i64);
  return add(
      czero,
      IntegerLiteral::Create(astContext, llvm::APInt(64, 0), get(i64), invLoc));
}

Stmt& CishContext::add(const llvm::ConstantExpr& cexpr,
                       const llvm::Value& val) {
  return *(exprs[&cexpr] = &get(val));
}

Stmt& CishContext::add(const llvm::ConstantDataArray& cda) {
  if(cda.isCString()) {
    return add(cda,
               StringLiteral::Create(astContext,
                                     cda.getAsString(),
                                     StringLiteral::Ascii,
                                     false,
                                     get(cda.getType()),
                                     invLoc));
  } else if(cda.isString()) {
    return add(cda,
               StringLiteral::Create(astContext,
                                     cda.getAsString(),
                                     StringLiteral::Ascii,
                                     false,
                                     get(cda.getType()),
                                     invLoc));
  } else {
    std::vector<Expr*> exprs;
    llvm::ArrayRef<Expr*> aref(exprs);
    for(unsigned i = 0; i < cda.getNumElements(); i++)
      exprs.push_back(&get<Expr>(cda.getElementAsConstant(i)));
    return add(cda,
               new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
  }
  llvm::WithColor::error(llvm::errs()) << "NOT IMPLEMENTED: " << cda << "\n";
  exit(1);
}

Stmt& CishContext::add(const llvm::ConstantStruct& cstruct) {
  std::vector<Expr*> exprs;
  llvm::ArrayRef<Expr*> aref(exprs);
  for(const llvm::Use& op : cstruct.operands())
    exprs.push_back(&get<Expr>(op.get()));
  return add(cstruct,
             new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
}

Stmt& CishContext::add(const llvm::ConstantArray& carray) {
  std::vector<Expr*> exprs;
  llvm::ArrayRef<Expr*> aref(exprs);
  for(const llvm::Use& op : carray.operands())
    exprs.push_back(&get<Expr>(op.get()));
  return add(carray,
             new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
}

Stmt& CishContext::add(const llvm::UndefValue& cundef) {
  return add(cundef,
             new(astContext) GNUNullExpr(get(cundef.getType()), invLoc));
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
  else if(auto* fty = dyn_cast<llvm::FunctionType>(type))
    return add(fty);

  llvm::WithColor::error(llvm::errs())
      << "COULD NOT ADD TYPE: " << *type << "\n";
  exit(1);
}

void CishContext::addTemp(const llvm::Instruction& inst,
                          const std::string& name) {
  // QualType type = get(inst.getType());
  // auto* var = VarDecl::Create(astContext,
  //                             funcs.at(&getFunction(inst)),
  //                             invLoc,
  //                             invLoc,
  //                             &astContext.Idents.get(name),
  //                             type,
  //                             nullptr,
  //                             SC_None);
  // auto* ref = DeclRefExpr::Create(astContext,
  //                                 NestedNameSpecifierLoc(),
  //                                 invLoc,
  //                                 var,
  //                                 false,
  //                                 invLoc,
  //                                 type,
  //                                 VK_LValue,
  //                                 var);
  // auto* temp = new(astContext) BinaryOperator(ref,
  //                                             &get<Expr>(inst),
  //                                             BO_Assign,
  //                                             type,
  //                                             VK_LValue,
  //                                             OK_Ordinary,
  //                                             invLoc,
  //                                             FPOptions());
  // return add(inst, temp);
}

bool CishContext::has(llvm::Type* type) const {
  return types.contains(type);
}

QualType CishContext::get(llvm::Type* type) const {
  return types.at(type);
}

} // namespace cish
