#include "LLVMBackend.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

#define UNIMPLEMENTED(TYPE)                        \
  void LLVMBackend::add(const llvm::TYPE& inst) {  \
    fatal(error() << "NOT IMPLEMENTED: " << inst); \
  }

LLVMBackend::LLVMBackend(CishContext& context) : BackendBase(context) {
  ;
}

void LLVMBackend::beginFunction(const llvm::Function& f) {
  BackendBase::beginFunction(funcs.at(&f));
}

void LLVMBackend::endFunction(const llvm::Function& f) {
  clang::FunctionDecl* cf = funcs.at(&f);
  cf->setBody(createCompoundStmt(stmts.pop()));
  BackendBase::endFunction(cf);
}

void LLVMBackend::beginBlock() {
  stmts.emplace();
}

void LLVMBackend::endBlock() {
  BackendBase::add(CompoundStmt::Create(
      astContext, makeArrayRef(stmts.pop()), invLoc, invLoc));
}

void LLVMBackend::beginBlock(const llvm::BasicBlock& bb) {
  BackendBase::add(createLabelStmt(blocks.at(&bb)));
}

void LLVMBackend::endBlock(const llvm::BasicBlock& bb) {
  // Nothing to do
}

DeclRefExpr* LLVMBackend::createDeclRefExpr(ValueDecl* decl) {
  return DeclRefExpr::Create(astContext,
                             NestedNameSpecifierLoc(),
                             invLoc,
                             decl,
                             false,
                             invLoc,
                             decl->getType(),
                             VK_LValue,
                             decl);
}

LabelStmt* LLVMBackend::createLabelStmt(LabelDecl* label) {
  return new(astContext) LabelStmt(invLoc, label, nullptr);
}

LabelDecl* LLVMBackend::createLabelDecl(FunctionDecl* f,
                                        const std::string& name) {
  return LabelDecl::Create(astContext, f, invLoc, &astContext.Idents.get(name));
}

DeclRefExpr* LLVMBackend::createVariable(const std::string& name,
                                         QualType type,
                                         DeclContext* parent) {
  return createDeclRefExpr(VarDecl::Create(astContext,
                                           parent,
                                           invLoc,
                                           invLoc,
                                           &astContext.Idents.get(name),
                                           type,
                                           nullptr,
                                           SC_None));
}

BinaryOperator* LLVMBackend::createBinaryOperator(Expr& lhs,
                                                  Expr& rhs,
                                                  BinaryOperator::Opcode opc,
                                                  QualType type) {
  return new(astContext) BinaryOperator(
      &lhs, &rhs, opc, type, VK_RValue, OK_Ordinary, invLoc, FPOptions());
}

UnaryOperator* LLVMBackend::createUnaryOperator(Expr& lhs,
                                                UnaryOperator::Opcode opc,
                                                QualType type) {
  return new(astContext)
      UnaryOperator(&lhs, opc, type, VK_RValue, OK_Ordinary, invLoc, false);
}

ArraySubscriptExpr*
LLVMBackend::createArraySubscriptExpr(Expr& base, Expr& idx, QualType type) {
  return new(astContext)
      ArraySubscriptExpr(&base, &idx, type, VK_RValue, OK_Ordinary, invLoc);
}

void LLVMBackend::add(const llvm::AllocaInst& alloca, const std::string& name) {
  FunctionDecl* f = funcs.at(&getFunction(alloca));

  DeclRefExpr* ref = createVariable(name, get(alloca.getAllocatedType()), f);
  f->addDecl(ref->getDecl());

  // The statement to declare the variable in the function
  auto* group = new(astContext) DeclGroupRef(ref->getDecl());
  auto* stmt = new(astContext) DeclStmt(*group, invLoc, invLoc);
  BackendBase::add(stmt);

  // The alloca statement in LLVM is always a pointer type, so where the
  // variable will be used, we actually need the pointer to it to be
  // consisten
  add(alloca, createUnaryOperator(*ref, UO_AddrOf, get(alloca.getType())));
}

CStyleCastExpr* LLVMBackend::createCastExpr(Expr& expr, QualType type) {
  return CStyleCastExpr::Create(astContext,
                                type,
                                VK_RValue,
                                CastKind::CK_BitCast,
                                &expr,
                                nullptr,
                                nullptr,
                                invLoc,
                                invLoc);
}

IfStmt* LLVMBackend::createIfStmt(Expr& cond, Stmt* thn, Stmt* els) {
  return IfStmt::Create(
      astContext, invLoc, false, nullptr, nullptr, &cond, thn, invLoc, els);
}

GotoStmt* LLVMBackend::createGoto(LabelDecl* label) {
  return new(astContext) GotoStmt(label, invLoc, invLoc);
}

CompoundStmt* LLVMBackend::createCompoundStmt(const Vector<Stmt*>& stmts) {
  return CompoundStmt::Create(astContext, makeArrayRef(stmts), invLoc, invLoc);
}

CompoundStmt* LLVMBackend::createCompoundStmt(Stmt* stmt) {
  return CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmt), invLoc, invLoc);
}

BreakStmt* LLVMBackend::createBreakStmt() {
  return new(astContext) BreakStmt(invLoc);
}

ContinueStmt* LLVMBackend::createContinueStmt() {
  return new(astContext) ContinueStmt(invLoc);
}

ReturnStmt* LLVMBackend::createReturnStmt(Expr* retExpr) {
  return ReturnStmt::Create(astContext, invLoc, retExpr, nullptr);
}

CallExpr* LLVMBackend::createCallExpr(Expr& callee,
                                      const Vector<Expr*>& args,
                                      QualType type) {
  return CallExpr::Create(
      astContext, &callee, makeArrayRef(args), type, VK_RValue, invLoc);
}

DoStmt* LLVMBackend::createDoStmt(Stmt* body, Expr& cond) {
  return new(astContext) DoStmt(body, &cond, invLoc, invLoc, invLoc);
}

WhileStmt* LLVMBackend::createWhileStmt(Expr& cond, Stmt* body) {
  return WhileStmt::Create(astContext, nullptr, &cond, body, invLoc);
}

CXXBoolLiteralExpr* LLVMBackend::createBoolLiteral(bool b, QualType type) {
  return new(astContext) CXXBoolLiteralExpr(b, type, invLoc);
}

IntegerLiteral* LLVMBackend::createIntLiteral(const llvm::APInt& i,
                                              QualType type) {
  return IntegerLiteral::Create(astContext, i, type, invLoc);
}

FloatingLiteral* LLVMBackend::createFloatLiteral(const llvm::APFloat& f,
                                                 QualType type) {
  return FloatingLiteral::Create(astContext, f, false, type, invLoc);
}

CXXNullPtrLiteralExpr* LLVMBackend::createNullptr(QualType type) {
  return new(astContext) CXXNullPtrLiteralExpr(type, invLoc);
}

InitListExpr* LLVMBackend::createInitListExpr(const Vector<Expr*>& exprs) {
  return new(astContext)
      InitListExpr(astContext, invLoc, makeArrayRef(exprs), invLoc);
}

UNIMPLEMENTED(AtomicCmpXchgInst)
UNIMPLEMENTED(AtomicRMWInst)

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
    fatal(error() << "Unknown binary operator: " << inst);
    break;
  }

  add(inst,
      createBinaryOperator(get<Expr>(inst.getOperand(0)),
                           get<Expr>(inst.getOperand(1)),
                           opc,
                           get(inst.getType())));
}

void LLVMBackend::add(const llvm::BranchInst& br) {
  if(br.isConditional()) {
    llvm::Value* cond = br.getCondition();
    auto* thn = createCompoundStmt(createGoto(blocks.at(br.getSuccessor(0))));
    auto* els = createCompoundStmt(createGoto(blocks.at(br.getSuccessor(1))));
    add(br, createIfStmt(get<Expr>(cond), thn, els));
  } else {
    add(br, createGoto(blocks.at(br.getSuccessor(0))));
  }
  BackendBase::add(get(br));
}

void LLVMBackend::add(const llvm::CastInst& cst) {
  add(cst, createCastExpr(get<Expr>(cst.getOperand(0)), get(cst.getType())));
}

UNIMPLEMENTED(InvokeInst)

void LLVMBackend::add(const llvm::CallInst& call) {
  Vector<Expr*> args;
  for(const llvm::Value* arg : call.arg_operands())
    args.push_back(&get<Expr>(arg));

  add(call,
      createCallExpr(
          get<Expr>(call.getCalledValue()), args, get(call.getType())));
  if(call.getType()->isVoidTy())
    BackendBase::add(get<CallExpr>(call));
}

UNIMPLEMENTED(CatchReturnInst)
UNIMPLEMENTED(CatchSwitchInst)
UNIMPLEMENTED(CleanupReturnInst)

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
      createBinaryOperator(get<Expr>(cmp.getOperand(0)),
                           get<Expr>(cmp.getOperand(1)),
                           opc,
                           get(cmp.getType())));
}

UNIMPLEMENTED(ExtractElementInst)
UNIMPLEMENTED(ExtractValueInst)
UNIMPLEMENTED(FenceInst)
UNIMPLEMENTED(CatchPadInst)

static ArraySubscriptExpr* getAsArraySubscriptExpr(clang::Expr* expr) {
  if(auto* un = dyn_cast<UnaryOperator>(stripCasts(expr)))
    return dyn_cast<ArraySubscriptExpr>(un->getSubExpr());
  return nullptr;
}

clang::Expr*
LLVMBackend::handleIndexOperand(llvm::PointerType* pty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const llvm::Value*>& indices,
                                const llvm::Instruction& inst) {
  const llvm::Value* op = indices[idx];
  if(idx == 0) {
    // If the first pointer operand to the instruction is an ArraySubscriptExpr,
    // then this is continuing an index computation. For the high-level
    // language's point of view, it may very well be an additional dimension
    // of an array being accessed, but we can't do much about that right now
    // anyway. In most cases, this is also the result of a cast intervening
    // between this and the previous ArraySubscriptExpr that was computed.
    // In that case, add the current offset computation to the previous
    // subscript expr.
    if(auto* arrExpr = getAsArraySubscriptExpr(currExpr)) {
      Expr* idxExpr = arrExpr->getIdx();
      Expr* newIdx = createBinaryOperator(
          *idxExpr, get<Expr>(op), BO_Add, idxExpr->getType());
      Expr* newArr = createArraySubscriptExpr(
          *arrExpr->getBase(), *newIdx, arrExpr->getType());
      Expr* addrOf = createUnaryOperator(*newArr, UO_AddrOf, get(pty));
      if(auto* cst = dyn_cast<CStyleCastExpr>(currExpr))
        return createCastExpr(*addrOf, cst->getType());
      else
        return addrOf;
    }
  } else if(const auto* cint = dyn_cast<llvm::ConstantInt>(op)) {
    // If the current offset is zero, don't add it because it is unlikely to
    // be "useful" and just ends up complicating the resulting expression
    if(cint->getLimitedValue() == 0)
      return currExpr;
  }
  return createArraySubscriptExpr(*currExpr, get<Expr>(op), get(pty));
}

clang::Expr*
LLVMBackend::handleIndexOperand(llvm::ArrayType* aty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const llvm::Value*>& indices,
                                const llvm::Instruction& inst) {
  return createArraySubscriptExpr(
      *currExpr, get<Expr>(indices.at(idx)), get(aty));
}

clang::Expr* LLVMBackend::handleIndexOperand(llvm::StructType* sty,
                                             clang::Expr* currExpr,
                                             unsigned field,
                                             const llvm::Instruction& inst) {

  return createBinaryOperator(*currExpr,
                              *fields.at(sty)[field],
                              BO_PtrMemD,
                              get(sty->getElementType(field)));
}

void LLVMBackend::add(const llvm::GetElementPtrInst& gep) {
  const llvm::Value* ptr = gep.getPointerOperand();
  llvm::Type* type = ptr->getType();
  clang::Expr* expr = &get<Expr>(ptr);
  Vector<const llvm::Value*> indices(gep.idx_begin(), gep.idx_end());
  for(unsigned i = 0; i < indices.size(); i++) {
    if(auto* pty = dyn_cast<llvm::PointerType>(type)) {
      expr = handleIndexOperand(pty, expr, i, indices, gep);
      type = pty->getElementType();
    } else if(auto* aty = dyn_cast<llvm::ArrayType>(type)) {
      expr = handleIndexOperand(aty, expr, i, indices, gep);
      type = aty->getElementType();
    } else if(auto* sty = dyn_cast<llvm::StructType>(type)) {
      if(auto* cint = dyn_cast<llvm::ConstantInt>(indices[i])) {
        unsigned field = cint->getLimitedValue();
        expr = handleIndexOperand(sty, expr, field, gep);
        type = sty->getElementType(field);
      } else {
        fatal(error() << "Expected constant index in GEP\n"
                      << "           op: " << *indices.at(i) << "\n"
                      << "         type: " << *type << "\n"
                      << "         inst: " << gep);
      }
    } else {
      fatal(error() << "GEP Indices not implemented for type: " << *type);
      exit(1);
    }
  }

  // If the operand to the GEP is itself a GEP, then the AddrOf operator
  // will already be present
  if(isa<llvm::GetElementPtrInst>(stripCasts(gep.getPointerOperand())))
    add(gep, expr);
  else
    add(gep, createUnaryOperator(*expr, UO_AddrOf, get(gep.getType())));
}

UNIMPLEMENTED(IndirectBrInst)
UNIMPLEMENTED(InsertElementInst)
UNIMPLEMENTED(InsertValueInst)
UNIMPLEMENTED(LandingPadInst)

void LLVMBackend::add(const llvm::LoadInst& load) {
  add(load,
      createUnaryOperator(
          get<Expr>(load.getPointerOperand()), UO_Deref, get(load.getType())));
}

UNIMPLEMENTED(ResumeInst)

void LLVMBackend::add(const llvm::ReturnInst& ret) {
  Expr* retExpr = nullptr;
  if(const llvm::Value* val = ret.getReturnValue())
    retExpr = &get<Expr>(val);

  add(ret, BackendBase::add(createReturnStmt(retExpr)));
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

UNIMPLEMENTED(ShuffleVectorInst)

void LLVMBackend::add(const llvm::StoreInst& store) {
  const llvm::Value* val = store.getValueOperand();
  QualType type = get(val->getType());
  Expr* ptr = createUnaryOperator(
      get<Expr>(store.getPointerOperand()), UO_Deref, type);
  auto* assign = createBinaryOperator(*ptr, get<Expr>(val), BO_Assign, type);
  BackendBase::add(assign);
  add(store, assign);
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
      createUnaryOperator(
          get<Expr>(inst.getOperand(0)), opc, get(inst.getType())));
}

UNIMPLEMENTED(UnreachableInst)

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

  add(arg, createDeclRefExpr(param));
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

  add(f, createDeclRefExpr(funcs.at(&f)));
}

void LLVMBackend::add(const llvm::GlobalAlias& alias, const std::string& name) {
  fatal(error() << "NOT IMPLEMENTED: " << alias);
}

void LLVMBackend::add(const llvm::GlobalVariable& g, const std::string& name) {
  TranslationUnitDecl* tu = astContext.getTranslationUnitDecl();
  QualType type = get(g.getType()->getElementType());
  if(g.isConstant())
    type.addConst();
  DeclRefExpr* ref = createVariable(name, type, tu);
  VarDecl* decl = dyn_cast<VarDecl>(ref->getDecl());
  tu->addDecl(decl);
  if(const llvm::Constant* init = g.getInitializer())
    decl->setInit(&get<Expr>(init));
  add(g, ref);
}

void LLVMBackend::add(const llvm::BasicBlock& bb, const std::string& name) {
  blocks[&bb] = createLabelDecl(funcs.at(bb.getParent()), name);
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
    fields[sty].push_back(DeclRefExpr::Create(astContext,
                                              NestedNameSpecifierLoc(),
                                              invLoc,
                                              field,
                                              false,
                                              invLoc,
                                              get(sty->getElementType(i)),
                                              VK_RValue,
                                              field));
  }

  // Not sure if adding a body to the struct will result in the underlying
  // RecordType changing. Just in case, add the type back to the map
  types[sty] = QualType(udts.at(sty)->getTypeForDecl(), 0);
}

void LLVMBackend::add(const llvm::ConstantInt& cint) {
  llvm::Type* type = cint.getType();
  if(type->isIntegerTy(1))
    add(cint, createBoolLiteral((bool)cint.getLimitedValue(), get(type)));
  else
    add(cint, createIntLiteral(cint.getValue(), get(type)));
}

void LLVMBackend::add(const llvm::ConstantFP& cfp) {
  add(cfp, createFloatLiteral(cfp.getValueAPF(), get(cfp.getType())));
}

void LLVMBackend::add(const llvm::ConstantPointerNull& cnull) {
  add(cnull, createNullptr(get(cnull.getType())));
}

void LLVMBackend::add(const llvm::ConstantAggregateZero& czero) {
  // FIXME: Implement this properly. It's not that hard. Just recursively
  // build an init list with zeros in it.
  add(czero,
      createIntLiteral(llvm::APInt(64, 0),
                       get(llvm::Type::getInt64Ty(czero.getContext()))));
}

void LLVMBackend::add(const llvm::ConstantExpr& cexpr, const llvm::Value& val) {
  exprs[&cexpr] = &get(val);
}

void LLVMBackend::add(const llvm::ConstantDataSequential& cseq) {
  if(cseq.isCString()) {
    add(cseq,
        StringLiteral::Create(astContext,
                              cseq.getAsString(),
                              StringLiteral::Ascii,
                              false,
                              get(cseq.getType()),
                              invLoc));
  } else if(cseq.isString()) {
    add(cseq,
        StringLiteral::Create(astContext,
                              cseq.getAsString(),
                              StringLiteral::Ascii,
                              false,
                              get(cseq.getType()),
                              invLoc));
  } else {
    Vector<Expr*> exprs;
    for(unsigned i = 0; i < cseq.getNumElements(); i++)
      exprs.push_back(&get<Expr>(cseq.getElementAsConstant(i)));
    add(cseq, createInitListExpr(exprs));
  }
}

void LLVMBackend::add(const llvm::ConstantStruct& cstruct) {
  Vector<Expr*> exprs;
  for(const llvm::Use& op : cstruct.operands())
    exprs.push_back(&get<Expr>(op.get()));
  add(cstruct, createInitListExpr(exprs));
}

void LLVMBackend::add(const llvm::ConstantArray& carray) {
  Vector<Expr*> exprs;
  for(const llvm::Use& op : carray.operands())
    exprs.push_back(&get<Expr>(op.get()));
  add(carray, createInitListExpr(exprs));
}

void LLVMBackend::add(const llvm::UndefValue& cundef) {
  add(cundef,
      createVariable("__undefined__",
                     get(cundef.getType()),
                     astContext.getTranslationUnitDecl()));
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
  types[vty] = astContext.getVectorType(get(vty->getElementType()),
                                        vty->getNumElements(),
                                        VectorType::GenericVector);
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
  QualType type = get(inst.getType());
  DeclRefExpr* var = createVariable(name, type, funcs.at(&getFunction(inst)));
  BinaryOperator* temp
      = createBinaryOperator(*var, get<Expr>(inst), BO_Assign, type);
  BackendBase::add(temp);
  add(inst, var);
}

void LLVMBackend::addIfThen(const llvm::BranchInst& br, bool invert) {
  Stmt* thn = stmts.top().pop_back();
  Expr* cond = &get<Expr>(br.getCondition());
  if(invert)
    cond = createUnaryOperator(*cond, UO_LNot, cond->getType());
  BackendBase::add(createIfStmt(*cond, thn));
}

void LLVMBackend::addIfThenElse(const llvm::BranchInst& br) {
  Stmt* els = stmts.top().pop_back();
  Stmt* thn = stmts.top().pop_back();
  BackendBase::add(createIfStmt(get<Expr>(br.getCondition()), thn, els));
}

void LLVMBackend::addIfThenBreak(const llvm::BranchInst& br) {
  Stmt* thn = createCompoundStmt(createBreakStmt());
  BackendBase::add(createIfStmt(get<Expr>(br.getCondition()), thn));
}

void LLVMBackend::addIfThenGoto(const std::string& name,
                                const llvm::BranchInst& br,
                                bool invert) {
  if(not labels.contains(name))
    labels[name] = createLabelDecl(funcs.at(br.getParent()->getParent()), name);

  Stmt* stmt = createCompoundStmt(createGoto(labels.at(name)));
  Expr* cond = &get<Expr>(br.getCondition());
  if(invert)
    cond = createUnaryOperator(*cond, UO_LNot, cond->getType());

  BackendBase::add(createIfStmt(*cond, stmt));
}

void LLVMBackend::addLabel(const std::string& name, const llvm::Function& f) {
  if(not labels.contains(name))
    labels[name] = createLabelDecl(funcs.at(&f), name);
  BackendBase::add(createLabelStmt(labels.at(name)));
}

void LLVMBackend::addBreak() {
  BackendBase::add(createBreakStmt());
}

void LLVMBackend::addContinue() {
  BackendBase::add(createContinueStmt());
}

void LLVMBackend::addEndlessLoop() {
  Stmt* body = stmts.top().pop_back();
  BackendBase::add(
      createDoStmt(body, *createBoolLiteral(true, astContext.BoolTy)));
}

bool LLVMBackend::has(llvm::Type* type) const {
  return types.contains(type);
}

QualType LLVMBackend::get(llvm::Type* type) const {
  return types.at(type);
}

} // namespace cish
