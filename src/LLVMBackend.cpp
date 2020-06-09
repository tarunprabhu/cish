#include "LLVMBackend.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

LLVMBackend::LLVMBackend(CishContext& context) : BackendBase(context) {
  ;
}

void LLVMBackend::beginFunction(const llvm::Function& f) {
  BackendBase::beginFunction(funcs.at(&f));
}

void LLVMBackend::endFunction(const llvm::Function& f) {
  BackendBase::endFunction(funcs.at(&f));
}

void LLVMBackend::beginBlock(const llvm::BasicBlock& bb) {
  BackendBase::beginBlock(blocks.at(&bb));
}

void LLVMBackend::endBlock(const llvm::BasicBlock& bb) {
  BackendBase::endBlock(blocks.at(&bb));
}

void LLVMBackend::add(const llvm::AllocaInst& alloca, const std::string& name) {
  QualType type = get(alloca.getAllocatedType());
  VarDecl* var = VarDecl::Create(astContext,
                                 funcs.at(&getFunction(alloca)),
                                 invLoc,
                                 invLoc,
                                 &astContext.Idents.get(name),
                                 type,
                                 nullptr,
                                 SC_None);
  add(alloca,
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

void LLVMBackend::add(const llvm::BranchInst& br) {
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
}

void LLVMBackend::add(const llvm::BinaryOperator& inst) {
  BinaryOperator::Opcode opc = (BinaryOperator::Opcode)-1;

  switch(inst.getOpcode()) {
  case llvm::BinaryOperator::Add:
  case llvm::BinaryOperator::FAdd:
    opc = BO_Add;
    break;
  case llvm::BinaryOperator::Sub:
  case llvm::BinaryOperator::FSub:
    opc = BO_Sub;
    break;
  case llvm::BinaryOperator::Mul:
  case llvm::BinaryOperator::FMul:
    opc = BO_Mul;
    break;
  case llvm::BinaryOperator::UDiv:
  case llvm::BinaryOperator::FDiv:
  case llvm::BinaryOperator::SDiv:
    opc = BO_Div;
    break;
  case llvm::BinaryOperator::URem:
  case llvm::BinaryOperator::SRem:
  case llvm::BinaryOperator::FRem:
    opc = BO_Rem;
    break;
  case llvm::BinaryOperator::Shl:
    opc = BO_Shl;
    break;
  case llvm::BinaryOperator::LShr:
    opc = BO_Shr;
    break;
  case llvm::BinaryOperator::And:
    opc = BO_And;
    break;
  case llvm::BinaryOperator::Or:
    opc = BO_Or;
    break;
  case llvm::BinaryOperator::Xor:
    opc = BO_Xor;
    break;
  default:
    fatal(llvm::errs() << "Unknown binary operator: " << inst);
    break;
  }

  add(inst,
      new(astContext) BinaryOperator(&get<Expr>(inst.getOperand(0)),
                                     &get<Expr>(inst.getOperand(1)),
                                     opc,
                                     get(inst.getType()),
                                     VK_LValue,
                                     OK_Ordinary,
                                     invLoc,
                                     FPOptions()));
}

void LLVMBackend::add(const llvm::CmpInst& cmp) {
  BinaryOperator::Opcode opc = (BinaryOperator::Opcode)-1;
  switch(cmp.getPredicate()) {
  case llvm::CmpInst::FCMP_OEQ:
  case llvm::CmpInst::FCMP_UEQ:
  case llvm::CmpInst::ICMP_EQ:
    opc = BO_EQ;
    break;
  case llvm::CmpInst::FCMP_ONE:
  case llvm::CmpInst::FCMP_UNE:
  case llvm::CmpInst::ICMP_NE:
    opc = BO_NE;
    break;
  case llvm::CmpInst::FCMP_OGT:
  case llvm::CmpInst::FCMP_UGT:
  case llvm::CmpInst::ICMP_UGT:
  case llvm::CmpInst::ICMP_SGT:
    opc = BO_GT;
    break;
  case llvm::CmpInst::FCMP_OGE:
  case llvm::CmpInst::FCMP_UGE:
  case llvm::CmpInst::ICMP_UGE:
  case llvm::CmpInst::ICMP_SGE:
    opc = BO_GE;
    break;
  case llvm::CmpInst::FCMP_OLT:
  case llvm::CmpInst::FCMP_ULT:
  case llvm::CmpInst::ICMP_ULT:
  case llvm::CmpInst::ICMP_SLT:
    opc = BO_LT;
    break;
  case llvm::CmpInst::FCMP_OLE:
  case llvm::CmpInst::FCMP_ULE:
  case llvm::CmpInst::ICMP_ULE:
  case llvm::CmpInst::ICMP_SLE:
    opc = BO_LE;
    break;
  default:
    fatal(error() << "Unknown compare predicate: " << cmp);
    break;
  }

  add(cmp,
      new(astContext) BinaryOperator(&get<Expr>(cmp.getOperand(0)),
                                     &get<Expr>(cmp.getOperand(1)),
                                     opc,
                                     get(cmp.getType()),
                                     VK_LValue,
                                     OK_Ordinary,
                                     invLoc,
                                     FPOptions()));
}

void LLVMBackend::add(const llvm::UnaryOperator& inst) {
  UnaryOperator::Opcode opc = (UnaryOperator::Opcode)-1;
  switch(inst.getOpcode()) {
  case llvm::UnaryOperator::FNeg:
    opc = UO_Minus;
    break;
  default:
    fatal(error() << "Unknown unary operator: " << inst);
    break;
  }

  add(inst,
      new(astContext) UnaryOperator(&get<Expr>(inst.getOperand(0)),
                                    opc,
                                    get(inst.getType()),
                                    VK_LValue,
                                    OK_Ordinary,
                                    invLoc,
                                    false));
}

void LLVMBackend::add(const llvm::CallInst& call) {
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
}

void LLVMBackend::handleIndices(llvm::Type* ty,
                                unsigned idx,
                                const Vector<const llvm::Value*>& indices,
                                const llvm::Instruction& inst) {
  const llvm::Value* op = indices[idx];
  llvm::Type* next = nullptr;
  // if(auto* pty = dyn_cast<PointerType>(ty)) {
  //   ss << "[" << handle(op) << "]";
  //   next = pty->getElementType();
  // } else if(auto* aty = dyn_cast<ArrayType>(ty)) {
  //   ss << "[" << handle(op) << "]";
  //   next = aty->getElementType();
  // } else if(auto* sty = dyn_cast<StructType>(ty)) {
  //   if(auto* cint = dyn_cast<ConstantInt>(op)) {
  //     unsigned field = cint->getLimitedValue();
  //     ss << "." << cg.getElementName(sty, field);
  //     next = sty->getElementType(field);
  //   } else {
  //     WithColor::error(errs()) << "Expected constant index in GEP\n"
  //                              << "          idx: " << idx << "\n"
  //                              << "           op: " << *op << "\n"
  //                              << "         type: " << *ty << "\n"
  //                              << "         inst: " << inst << "\n";
  //     exit(1);
  //   }
  // } else {
  //   WithColor::error(errs())
  //       << "GEP Indices not implemented for type: " << *ty << "\n";
  //   exit(1);
  // }

  if((idx + 1) < indices.size())
    handleIndices(next, idx + 1, indices, inst);
}

void LLVMBackend::add(const llvm::GetElementPtrInst& gep) {
  fatal(error() << "NOT IMPLEMENTED: " << gep);
}

void LLVMBackend::add(const llvm::LoadInst& load) {
  add(load,
      new(astContext) UnaryOperator(&get<Expr>(load.getPointerOperand()),
                                    UO_Deref,
                                    get(load.getType()),
                                    VK_RValue,
                                    OK_Ordinary,
                                    invLoc,
                                    false));
}

void LLVMBackend::add(const llvm::StoreInst& store) {
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
  add(store, assign);
}

void LLVMBackend::add(const llvm::CastInst& cst) {
  add(cst,
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

void LLVMBackend::add(const llvm::SelectInst& select) {
  add(select,
      new(astContext) ConditionalOperator(&get<Expr>(select.getCondition()),
                                          invLoc,
                                          &get<Expr>(select.getTrueValue()),
                                          invLoc,
                                          &get<Expr>(select.getFalseValue()),
                                          get(select.getType()),
                                          VK_LValue,
                                          OK_Ordinary));
}

void LLVMBackend::add(const llvm::ReturnInst& ret) {
  if(const llvm::Value* val = ret.getReturnValue()) {
    add(ret, ReturnStmt::Create(astContext, invLoc, &get<Expr>(val), nullptr));
  } else {
    add(ret, ReturnStmt::CreateEmpty(astContext, false));
  }
  stmts.push_back(&get(ret));
}

void LLVMBackend::add(const llvm::PHINode& phi, const std::string& name) {
  fatal(error() << "NOT IMPLEMENTED: " << phi);
}

void LLVMBackend::add(const llvm::Argument& arg, const std::string& name) {
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

  add(arg,
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

void LLVMBackend::add(const llvm::Function& f,
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

  add(f,
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

void LLVMBackend::add(const llvm::GlobalVariable& g, const std::string& name) {
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
  add(g,
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

void LLVMBackend::add(const llvm::BasicBlock& bb, const std::string& name) {
  blocks[&bb] = LabelDecl::Create(astContext,
                                  funcs.at(bb.getParent()),
                                  invLoc,
                                  &astContext.Idents.get(name));
}

void LLVMBackend::add(llvm::StructType* sty, const std::string& name) {
  RecordDecl* decl = RecordDecl::Create(astContext,
                                        TTK_Struct,
                                        astContext.getTranslationUnitDecl(),
                                        invLoc,
                                        invLoc,
                                        &astContext.Idents.get(name));

  astContext.getTranslationUnitDecl()->addDecl(decl);
  udts[sty] = decl;
  types[sty] = QualType(udts.at(sty)->getTypeForDecl(), 0);
}

void LLVMBackend::add(llvm::StructType* sty,
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
  types[sty] = QualType(udts.at(sty)->getTypeForDecl(), 0);
}

void LLVMBackend::add(const llvm::ConstantInt& cint) {
  llvm::Type* type = cint.getType();
  if(type->isIntegerTy(1))
    add(cint,
        new(astContext) CXXBoolLiteralExpr(
            (bool)cint.getLimitedValue(), get(type), invLoc));
  else
    add(cint,
        IntegerLiteral::Create(
            astContext, cint.getValue(), get(cint.getType()), invLoc));
}

void LLVMBackend::add(const llvm::ConstantFP& cfp) {
  add(cfp,
      FloatingLiteral::Create(
          astContext, cfp.getValueAPF(), false, get(cfp.getType()), invLoc));
}

void LLVMBackend::add(const llvm::ConstantPointerNull& cnull) {
  add(cnull,
      new(astContext) CXXNullPtrLiteralExpr(get(cnull.getType()), invLoc));
}

void LLVMBackend::add(const llvm::ConstantAggregateZero& czero) {
  // FIXME: Implement this properly. It's not that hard. Just recursively
  // build an init list with zeros in it.
  llvm::Type* i64 = llvm::Type::getInt64Ty(czero.getContext());
  add(i64);
  add(czero,
      IntegerLiteral::Create(astContext, llvm::APInt(64, 0), get(i64), invLoc));
}

void LLVMBackend::add(const llvm::ConstantExpr& cexpr, const llvm::Value& val) {
  exprs[&cexpr] = &get(val);
}

void LLVMBackend::add(const llvm::ConstantDataArray& cda) {
  if(cda.isCString()) {
    add(cda,
        StringLiteral::Create(astContext,
                              cda.getAsString(),
                              StringLiteral::Ascii,
                              false,
                              get(cda.getType()),
                              invLoc));
  } else if(cda.isString()) {
    add(cda,
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
    add(cda, new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
  }
}

void LLVMBackend::add(const llvm::ConstantStruct& cstruct) {
  std::vector<Expr*> exprs;
  llvm::ArrayRef<Expr*> aref(exprs);
  for(const llvm::Use& op : cstruct.operands())
    exprs.push_back(&get<Expr>(op.get()));
  add(cstruct, new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
}

void LLVMBackend::add(const llvm::ConstantArray& carray) {
  std::vector<Expr*> exprs;
  llvm::ArrayRef<Expr*> aref(exprs);
  for(const llvm::Use& op : carray.operands())
    exprs.push_back(&get<Expr>(op.get()));
  add(carray, new(astContext) InitListExpr(astContext, invLoc, exprs, invLoc));
}

void LLVMBackend::add(const llvm::UndefValue& cundef) {
  add(cundef, new(astContext) GNUNullExpr(get(cundef.getType()), invLoc));
}

void LLVMBackend::add(llvm::IntegerType* ity) {
  switch(ity->getBitWidth()) {
  case 1:
    types[ity] = astContext.BoolTy;
    break;
  case 8:
    types[ity] = astContext.CharTy;
    break;
  case 16:
    types[ity] = astContext.ShortTy;
    break;
  case 32:
    types[ity] = astContext.IntTy;
    break;
  case 64:
    types[ity] = astContext.LongTy;
    break;
  case 128:
    types[ity] = astContext.Int128Ty;
    break;
  default:
    fatal(error() << "UNKNOWN INTEGER TYPE: " << *ity);
    break;
  }
}

void LLVMBackend::add(llvm::PointerType* pty) {
  types[pty] = astContext.getPointerType(get(pty->getElementType()));
}

void LLVMBackend::add(llvm::ArrayType* aty) {
  llvm::APInt elems(64, aty->getNumElements(), false);
  QualType ety = get(aty->getElementType());

  types[aty]
      = astContext.getConstantArrayType(ety, elems, ArrayType::Normal, 0);
}

void LLVMBackend::add(llvm::FunctionType* fty) {
  QualType ret = get(fty->getReturnType());
  std::vector<QualType> args;
  for(llvm::Type* param : fty->params())
    args.push_back(get(param));
  FunctionProtoType::ExtProtoInfo proto;
  proto.Variadic = fty->isVarArg();

  types[fty]
      = astContext.getFunctionType(ret, llvm::ArrayRef<QualType>(args), proto);
}

void LLVMBackend::add(llvm::VectorType* vty) {
  fatal(error() << "NOT IMPLEMENTED: " << *vty);
}

void LLVMBackend::add(llvm::Type* type) {
  if(has(type))
    return;

  if(type->isVoidTy())
    types[type] = astContext.VoidTy;
  else if(auto* ity = dyn_cast<llvm::IntegerType>(type))
    add(ity);
  else if(type->isFloatTy())
    types[type] = astContext.FloatTy;
  else if(type->isDoubleTy())
    types[type] = astContext.DoubleTy;
  else if(type->isX86_FP80Ty())
    types[type] = astContext.LongDoubleTy;
  else if(type->isFP128Ty())
    types[type] = astContext.Float128Ty;
  else if(auto* pty = dyn_cast<llvm::PointerType>(type))
    add(pty);
  else if(auto* aty = dyn_cast<llvm::ArrayType>(type))
    add(aty);
  else if(auto* fty = dyn_cast<llvm::FunctionType>(type))
    add(fty);
  else
    fatal(error() << "COULD NOT ADD TYPE: " << *type);
}

void LLVMBackend::addTemp(const llvm::Instruction& inst,
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

bool LLVMBackend::has(llvm::Type* type) const {
  return types.contains(type);
}

QualType LLVMBackend::get(llvm::Type* type) const {
  return types.at(type);
}

} // namespace cish
