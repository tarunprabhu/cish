#include "LLVMBackend.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

#include <llvm/ADT/APInt.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

#define UNIMPLEMENTED(TYPE)                        \
  void LLVMBackend::add(const TYPE& inst) {        \
    fatal(error() << "NOT IMPLEMENTED: " << inst); \
  }

LLVMBackend::LLVMBackend(CishContext& context) : BackendBase(context) {
  ;
}

void LLVMBackend::beginFunction(const Function& f) {
  BackendBase::beginFunction(funcs.at(&f));
}

void LLVMBackend::endFunction(const Function& f) {
  clang::FunctionDecl* cf = funcs.at(&f);
  cf->setBody(createCompoundStmt(stmts.pop()));
  BackendBase::endFunction(cf);
}

void LLVMBackend::beginBlock() {
  stmts.emplace();
}

void LLVMBackend::endBlock() {
  BackendBase::add(createCompoundStmt(stmts.pop()));
}

void LLVMBackend::beginBlock(const BasicBlock& bb) {
  BackendBase::add(createLabelStmt(blocks.at(&bb)));
}

void LLVMBackend::endBlock(const BasicBlock& bb) {
  // Nothing to do
}

void LLVMBackend::add(const AllocaInst& alloca, const std::string& name) {
  clang::FunctionDecl* f = funcs.at(&getFunction(alloca));
  clang::DeclRefExpr* ref
      = createLocalVariable(name, get(alloca.getAllocatedType()), f);

  // The statement to declare the variable in the function
  BackendBase::add(createDeclStmt(ref->getDecl()));

  // The alloca statement in LLVM is always a pointer type, so where the
  // variable will be used, we actually need the pointer to it to be
  // consistent
  add(alloca,
      createUnaryOperator(ref, clang::UO_AddrOf, get(alloca.getType())));
}

UNIMPLEMENTED(AtomicCmpXchgInst)
UNIMPLEMENTED(AtomicRMWInst)

void LLVMBackend::add(const BinaryOperator& inst) {
  clang::BinaryOperator::Opcode opc;

  switch(inst.getOpcode()) {
  case BinaryOperator::Add:
  case BinaryOperator::FAdd:
    opc = clang::BO_Add;
    break;
  case BinaryOperator::Sub:
  case BinaryOperator::FSub:
    opc = clang::BO_Sub;
    break;
  case BinaryOperator::Mul:
  case BinaryOperator::FMul:
    opc = clang::BO_Mul;
    break;
  case BinaryOperator::UDiv:
  case BinaryOperator::FDiv:
  case BinaryOperator::SDiv:
    opc = clang::BO_Div;
    break;
  case BinaryOperator::URem:
  case BinaryOperator::SRem:
  case BinaryOperator::FRem:
    opc = clang::BO_Rem;
    break;
  case BinaryOperator::Shl:
    opc = clang::BO_Shl;
    break;
  case BinaryOperator::LShr:
    opc = clang::BO_Shr;
    break;
  case BinaryOperator::And:
    opc = clang::BO_And;
    break;
  case BinaryOperator::Or:
    opc = clang::BO_Or;
    break;
  case BinaryOperator::Xor:
    opc = clang::BO_Xor;
    break;
  default:
    fatal(error() << "Unknown binary operator: " << inst);
    break;
  }

  add(inst,
      createBinaryOperator(get<clang::Expr>(inst.getOperand(0)),
                           get<clang::Expr>(inst.getOperand(1)),
                           opc,
                           get(inst.getType())));
}

void LLVMBackend::add(const BranchInst& br) {
  if(br.isConditional()) {
    Value* cond = br.getCondition();
    auto* thn
        = createCompoundStmt(createGotoStmt(blocks.at(br.getSuccessor(0))));
    auto* els
        = createCompoundStmt(createGotoStmt(blocks.at(br.getSuccessor(1))));
    add(br, createIfStmt(get<clang::Expr>(cond), thn, els));
  } else {
    add(br, createGotoStmt(blocks.at(br.getSuccessor(0))));
  }
  BackendBase::add(get(br));
}

void LLVMBackend::add(const CastInst& cst) {
  add(cst,
      createCastExpr(get<clang::Expr>(cst.getOperand(0)), get(cst.getType())));
}

UNIMPLEMENTED(InvokeInst)

void LLVMBackend::add(const CallInst& call) {
  Vector<clang::Expr*> args;
  for(const Value* arg : call.arg_operands())
    args.push_back(get<clang::Expr>(arg));

  add(call,
      createCallExpr(
          get<clang::Expr>(call.getCalledValue()), args, get(call.getType())));
  if(call.getType()->isVoidTy())
    BackendBase::add(get<clang::CallExpr>(call));
}

UNIMPLEMENTED(CatchReturnInst)
UNIMPLEMENTED(CatchSwitchInst)
UNIMPLEMENTED(CleanupReturnInst)

void LLVMBackend::add(const CmpInst& cmp) {
  clang::BinaryOperator::Opcode opc;
  switch(cmp.getPredicate()) {
  case CmpInst::FCMP_OEQ:
  case CmpInst::FCMP_UEQ:
  case CmpInst::ICMP_EQ:
    opc = clang::BO_EQ;
    break;
  case CmpInst::FCMP_ONE:
  case CmpInst::FCMP_UNE:
  case CmpInst::ICMP_NE:
    opc = clang::BO_NE;
    break;
  case CmpInst::FCMP_OGT:
  case CmpInst::FCMP_UGT:
  case CmpInst::ICMP_UGT:
  case CmpInst::ICMP_SGT:
    opc = clang::BO_GT;
    break;
  case CmpInst::FCMP_OGE:
  case CmpInst::FCMP_UGE:
  case CmpInst::ICMP_UGE:
  case CmpInst::ICMP_SGE:
    opc = clang::BO_GE;
    break;
  case CmpInst::FCMP_OLT:
  case CmpInst::FCMP_ULT:
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT:
    opc = clang::BO_LT;
    break;
  case CmpInst::FCMP_OLE:
  case CmpInst::FCMP_ULE:
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE:
    opc = clang::BO_LE;
    break;
  default:
    fatal(error() << "Unknown compare predicate: " << cmp);
    break;
  }

  add(cmp,
      createBinaryOperator(get<clang::Expr>(cmp.getOperand(0)),
                           get<clang::Expr>(cmp.getOperand(1)),
                           opc,
                           get(cmp.getType())));
}

UNIMPLEMENTED(ExtractElementInst)
UNIMPLEMENTED(ExtractValueInst)
UNIMPLEMENTED(FenceInst)
UNIMPLEMENTED(CatchPadInst)

static clang::ArraySubscriptExpr* getAsArraySubscriptExpr(clang::Expr* expr) {
  if(auto* un = dyn_cast<clang::UnaryOperator>(stripCasts(expr)))
    return dyn_cast<clang::ArraySubscriptExpr>(un->getSubExpr());
  return nullptr;
}

clang::Expr*
LLVMBackend::handleIndexOperand(PointerType* pty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const Value*>& indices,
                                const Instruction& inst) {
  const Value* op = indices[idx];
  if(const auto* cint = dyn_cast<ConstantInt>(op)) {
    // If the current offset is zero, don't add it because it is unlikely to
    // be "useful" and just ends up complicating the resulting expression
    if(cint->getLimitedValue() == 0)
      return currExpr;
  }
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
      clang::Expr* idxExpr = arrExpr->getIdx();
      clang::Expr* newIdx = createBinaryOperator(
          idxExpr, get<clang::Expr>(op), clang::BO_Add, idxExpr->getType());
      clang::Expr* newArr = createArraySubscriptExpr(
          arrExpr->getBase(), newIdx, arrExpr->getType());
      clang::Expr* addrOf
          = createUnaryOperator(newArr, clang::UO_AddrOf, get(pty));
      if(auto* cst = dyn_cast<clang::CStyleCastExpr>(currExpr))
        return createCastExpr(addrOf, cst->getType());
      else
        return addrOf;
    }
  }
  return createArraySubscriptExpr(currExpr, get<clang::Expr>(op), get(pty));
}

clang::Expr*
LLVMBackend::handleIndexOperand(ArrayType* aty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const Value*>& indices,
                                const Instruction& inst) {
  return createArraySubscriptExpr(
      currExpr, get<clang::Expr>(indices[idx]), get(aty));
}

clang::Expr* LLVMBackend::handleIndexOperand(StructType* sty,
                                             clang::Expr* currExpr,
                                             unsigned field,
                                             const Instruction& inst) {

  return createBinaryOperator(currExpr,
                              fields.at(sty)[field],
                              clang::BO_PtrMemD,
                              get(sty->getElementType(field)));
}

void LLVMBackend::add(const GetElementPtrInst& gep) {
  const Value* ptr = gep.getPointerOperand();
  Type* type = ptr->getType();
  clang::Expr* expr = get<clang::Expr>(ptr);
  Vector<const Value*> indices(gep.idx_begin(), gep.idx_end());
  for(unsigned i = 0; i < indices.size(); i++) {
    if(auto* pty = dyn_cast<PointerType>(type)) {
      expr = handleIndexOperand(pty, expr, i, indices, gep);
      type = pty->getElementType();
    } else if(auto* aty = dyn_cast<ArrayType>(type)) {
      expr = handleIndexOperand(aty, expr, i, indices, gep);
      type = aty->getElementType();
    } else if(auto* sty = dyn_cast<StructType>(type)) {
      if(auto* cint = dyn_cast<ConstantInt>(indices[i])) {
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
  if(isa<GetElementPtrInst>(stripCasts(gep.getPointerOperand())))
    add(gep, expr);
  else
    add(gep, createUnaryOperator(expr, clang::UO_AddrOf, get(gep.getType())));
}

UNIMPLEMENTED(IndirectBrInst)
UNIMPLEMENTED(InsertElementInst)
UNIMPLEMENTED(InsertValueInst)
UNIMPLEMENTED(LandingPadInst)

void LLVMBackend::add(const LoadInst& load) {
  add(load,
      createUnaryOperator(get<clang::Expr>(load.getPointerOperand()),
                          clang::UO_Deref,
                          get(load.getType())));
}

UNIMPLEMENTED(ResumeInst)

void LLVMBackend::add(const ReturnInst& ret) {
  clang::Expr* retExpr = nullptr;
  if(const Value* val = ret.getReturnValue())
    retExpr = get<clang::Expr>(val);

  add(ret, BackendBase::add(createReturnStmt(retExpr)));
}

void LLVMBackend::add(const SelectInst& select) {
  add(select,
      createConditionalOperator(get<clang::Expr>(select.getCondition()),
                                get<clang::Expr>(select.getTrueValue()),
                                get<clang::Expr>(select.getFalseValue()),
                                get(select.getType())));
}

UNIMPLEMENTED(ShuffleVectorInst)

void LLVMBackend::add(const StoreInst& store) {
  const Value* val = store.getValueOperand();
  clang::QualType type = get(val->getType());
  clang::Expr* ptr = createUnaryOperator(
      get<clang::Expr>(store.getPointerOperand()), clang::UO_Deref, type);
  auto* assign = createBinaryOperator(
      ptr, get<clang::Expr>(val), clang::BO_Assign, type);
  BackendBase::add(assign);
  add(store, assign);
}

void LLVMBackend::add(const SwitchInst& swtch) {
  addSwitchStmt(swtch);
  for(const auto& i : swtch.cases()) {
    BackendBase::add(
        createCompoundStmt(createGotoStmt(blocks.at(i.getCaseSuccessor()))));
    addSwitchCase(i.getCaseValue());
  };
  if(const BasicBlock* deflt = swtch.getDefaultDest()) {
    BackendBase::add(createCompoundStmt(createGotoStmt(blocks.at(deflt))));
    addSwitchDefault();
  }
}

void LLVMBackend::add(const UnaryOperator& inst) {
  clang::UnaryOperator::Opcode opc;
  switch(inst.getOpcode()) {
  case UnaryOperator::FNeg:
    opc = clang::UO_Minus;
    break;
  default:
    fatal(error() << "Unknown unary operator: " << inst);
    break;
  }

  add(inst,
      createUnaryOperator(
          get<clang::Expr>(inst.getOperand(0)), opc, get(inst.getType())));
}

void LLVMBackend::add(const UnreachableInst&) {
  // Nothing to do here
}

void LLVMBackend::add(const Argument& arg, const std::string& name) {
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
  clang::QualType type = get(arg.getType());
  clang::ParmVarDecl* param
      = createParam(name, type, funcs.at(arg.getParent()));

  add(arg, createDeclRefExpr(param));
}

void LLVMBackend::add(const Function& f,
                      const std::string& name,
                      const Vector<std::string>& argNames) {
  clang::QualType type = get(f.getFunctionType());
  clang::FunctionDecl* decl = createFunction(name, type);

  funcs[&f] = decl;
  Vector<clang::ParmVarDecl*> args;
  for(const Argument& arg : f.args()) {
    add(arg, argNames[arg.getArgNo()]);
    args.push_back(get<clang::ParmVarDecl>(arg));
  }
  decl->setParams(makeArrayRef<clang::ParmVarDecl*>(args));

  add(f, createDeclRefExpr(decl));
}

void LLVMBackend::add(const GlobalAlias& alias, const std::string& name) {
  fatal(error() << "NOT IMPLEMENTED: " << alias);
}

void LLVMBackend::add(const GlobalVariable& g, const std::string& name) {
  clang::QualType type = get(g.getType()->getElementType());
  if(g.isConstant())
    type.addConst();
  clang::DeclRefExpr* ref = createGlobalVariable(name, type);
  clang::VarDecl* decl = cast<clang::VarDecl>(ref->getDecl());
  if(const Constant* init = g.getInitializer())
    decl->setInit(get<clang::Expr>(init));
  add(g, ref);
}

void LLVMBackend::add(const BasicBlock& bb, const std::string& name) {
  blocks[&bb] = createLabelDecl(funcs.at(bb.getParent()), name);
}

void LLVMBackend::add(StructType* sty, const std::string& name) {
  clang::RecordDecl* decl = createStruct(name);
  udts[sty] = decl;
  types[sty] = clang::QualType(decl->getTypeForDecl(), 0);
}

void LLVMBackend::add(StructType* sty, const Vector<std::string>& elements) {
  for(unsigned i = 0; i < sty->getNumElements(); i++)
    fields[sty].push_back(createDeclRefExpr(
        createField(elements[i], get(sty->getElementType(i)), udts.at(sty))));

  // Not sure if adding a body to the struct will result in the underlying
  // RecordType changing. Jus in case, add the type back to the map
  types[sty] = clang::QualType(udts.at(sty)->getTypeForDecl(), 0);
}

void LLVMBackend::add(const ConstantInt& cint) {
  Type* type = cint.getType();
  if(type->isIntegerTy(1))
    add(cint, createBoolLiteral((bool)cint.getLimitedValue(), get(type)));
  else
    add(cint, createIntLiteral(cint.getValue(), get(type)));
}

void LLVMBackend::add(const ConstantFP& cfp) {
  add(cfp, createFloatLiteral(cfp.getValueAPF(), get(cfp.getType())));
}

void LLVMBackend::add(const ConstantPointerNull& cnull) {
  add(cnull, createNullptr(get(cnull.getType())));
}

void LLVMBackend::add(const ConstantAggregateZero& czero) {
  // FIXME: Implement this properly. It's not that hard. Just recursively
  // build an init list with zeros in it.
  add(czero,
      createIntLiteral(APInt(64, 0),
                       get(Type::getInt64Ty(czero.getContext()))));
}

void LLVMBackend::add(const ConstantExpr& cexpr, const Value& val) {
  add(cexpr, get(val));
}

void LLVMBackend::add(const ConstantDataSequential& cseq) {
  clang::QualType type = get(cseq.getType());
  if(cseq.isCString()) {
    add(cseq, createStringLiteral(cseq.getAsString(), type));
  } else if(cseq.isString()) {
    add(cseq, createStringLiteral(cseq.getAsString(), type));
  } else {
    Vector<clang::Expr*> exprs;
    for(unsigned i = 0; i < cseq.getNumElements(); i++)
      exprs.push_back(get<clang::Expr>(cseq.getElementAsConstant(i)));
    add(cseq, createInitListExpr(exprs));
  }
}

void LLVMBackend::add(const ConstantStruct& cstruct) {
  Vector<clang::Expr*> exprs;
  for(const Use& op : cstruct.operands())
    exprs.push_back(get<clang::Expr>(op.get()));
  add(cstruct, createInitListExpr(exprs));
}

void LLVMBackend::add(const ConstantArray& carray) {
  Vector<clang::Expr*> exprs;
  for(const Use& op : carray.operands())
    exprs.push_back(get<clang::Expr>(op.get()));
  add(carray, createInitListExpr(exprs));
}

void LLVMBackend::add(const UndefValue& cundef) {
  add(cundef,
      createVariable("__undefined__",
                     get(cundef.getType()),
                     astContext.getTranslationUnitDecl()));
}

void LLVMBackend::add(IntegerType* ity) {
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

void LLVMBackend::add(PointerType* pty) {
  types[pty] = astContext.getPointerType(get(pty->getElementType()));
}

void LLVMBackend::add(ArrayType* aty) {
  APInt elems(64, aty->getNumElements(), false);
  clang::QualType ety = get(aty->getElementType());

  types[aty] = astContext.getConstantArrayType(
      ety, elems, clang::ArrayType::Normal, 0);
}

void LLVMBackend::add(FunctionType* fty) {
  clang::QualType ret = get(fty->getReturnType());
  std::vector<clang::QualType> args;
  for(Type* param : fty->params())
    args.push_back(get(param));
  clang::FunctionProtoType::ExtProtoInfo proto;
  proto.Variadic = fty->isVarArg();

  types[fty]
      = astContext.getFunctionType(ret, ArrayRef<clang::QualType>(args), proto);
}

void LLVMBackend::add(VectorType* vty) {
  types[vty] = astContext.getVectorType(get(vty->getElementType()),
                                        vty->getNumElements(),
                                        clang::VectorType::GenericVector);
}

void LLVMBackend::add(Type* type) {
  if(has(type))
    return;

  if(type->isVoidTy())
    types[type] = astContext.VoidTy;
  else if(auto* ity = dyn_cast<IntegerType>(type))
    add(ity);
  else if(type->isFloatTy())
    types[type] = astContext.FloatTy;
  else if(type->isDoubleTy())
    types[type] = astContext.DoubleTy;
  else if(type->isX86_FP80Ty())
    types[type] = astContext.LongDoubleTy;
  else if(type->isFP128Ty())
    types[type] = astContext.Float128Ty;
  else if(auto* pty = dyn_cast<PointerType>(type))
    add(pty);
  else if(auto* aty = dyn_cast<ArrayType>(type))
    add(aty);
  else if(auto* fty = dyn_cast<FunctionType>(type))
    add(fty);
  else
    fatal(error() << "COULD NOT ADD TYPE: " << *type);
}

void LLVMBackend::addTemp(const Instruction& inst, const std::string& name) {
  clang::QualType type = get(inst.getType());
  clang::DeclRefExpr* var
      = createVariable(name, type, funcs.at(&getFunction(inst)));
  clang::BinaryOperator* temp = createBinaryOperator(
      var, get<clang::Expr>(inst), clang::BO_Assign, type);
  BackendBase::add(temp);
  add(inst, var);
}

void LLVMBackend::addIfThen(const BranchInst& br, bool invert) {
  clang::Stmt* thn = stmts.top().pop_back();
  clang::Expr* cond = get<clang::Expr>(br.getCondition());
  if(invert)
    cond = createUnaryOperator(cond, clang::UO_LNot, cond->getType());
  BackendBase::add(createIfStmt(cond, thn));
}

void LLVMBackend::addIfThenElse(const BranchInst& br) {
  clang::Stmt* els = stmts.top().pop_back();
  clang::Stmt* thn = stmts.top().pop_back();
  BackendBase::add(createIfStmt(get<clang::Expr>(br.getCondition()), thn, els));
}

void LLVMBackend::addIfThenBreak(const BranchInst& br) {
  clang::Stmt* thn = createCompoundStmt(createBreakStmt());
  BackendBase::add(createIfStmt(get<clang::Expr>(br.getCondition()), thn));
}

void LLVMBackend::addIfThenGoto(const std::string& name,
                                const BranchInst& br,
                                bool invert) {
  if(not labels.contains(name))
    labels[name] = createLabelDecl(funcs.at(br.getParent()->getParent()), name);

  clang::Stmt* stmt = createCompoundStmt(createGotoStmt(labels.at(name)));
  clang::Expr* cond = get<clang::Expr>(br.getCondition());
  if(invert)
    cond = createUnaryOperator(cond, clang::UO_LNot, cond->getType());

  BackendBase::add(createIfStmt(cond, stmt));
}

void LLVMBackend::addLabel(const std::string& name, const Function& f) {
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
  clang::Stmt* body = stmts.top().pop_back();
  BackendBase::add(
      createDoStmt(body, createBoolLiteral(true, astContext.BoolTy)));
}

void LLVMBackend::addSwitchStmt(const SwitchInst& sw) {
  BackendBase::add(createSwitchStmt(get<clang::Expr>(sw.getCondition())));
}

void LLVMBackend::addSwitchCase(const ConstantInt* value) {
  clang::Stmt* body = stmts.top().pop_back();
  clang::SwitchCase* newCase = nullptr;
  if(value) {
    clang::CaseStmt* kase = createCaseStmt(get<clang::Expr>(*value));
    kase->setSubStmt(body);
    newCase = kase;
  } else {
    newCase = createDefaultStmt(body);
  }

  // Calling SwitchStmt->addSwitchCase() ends up with the newly added case
  // becoming the first case in the switch statement. Better to maintain the
  // order here
  auto* stmt = cast<clang::SwitchStmt>(stmts.top().back());
  clang::SwitchCase* first = stmt->getSwitchCaseList();
  if(not first) {
    stmt->addSwitchCase(newCase);
  } else {
    clang::SwitchCase* last = first;
    while(last->getNextSwitchCase())
      last = last->getNextSwitchCase();
    last->setNextSwitchCase(newCase);
  }
}

void LLVMBackend::addSwitchCase(const ConstantInt& value) {
  addSwitchCase(&value);
}

void LLVMBackend::addSwitchDefault() {
  addSwitchCase(nullptr);
}

void LLVMBackend::addGoto(const BasicBlock& bb) {
  BackendBase::add(createGotoStmt(blocks.at(&bb)));
}

void LLVMBackend::addGoto(const std::string& dest, const Function& f) {
  if(not labels.contains(dest))
    labels[dest] = createLabelDecl(funcs.at(&f), dest);
  BackendBase::add(createGotoStmt(labels.at(dest)));
}

bool LLVMBackend::has(const Value* val) const {
  return l2c.contains(val);
}

bool LLVMBackend::has(const Value& val) const {
  return has(&val);
}

bool LLVMBackend::has(Type* type) const {
  return types.contains(type);
}

clang::QualType LLVMBackend::get(Type* type) const {
  return types.at(type);
}

} // namespace cish
