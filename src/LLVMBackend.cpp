//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

#include "LLVMBackend.h"
#include "CishLLVMContext.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "LLVMClangMap.h"
#include "LLVMUtils.h"
#include "LLVMSourceInfo.h"

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

LLVMBackend::LLVMBackend(CishLLVMContext& cishContext)
    : BackendBase(cishContext), si(cishContext.getLLVMSourceInfo()),
      llvmClangMap(cishContext.getLLVMClangMap()) {
  ;
}

std::string LLVMBackend::getName(const Value& val, const std::string& prefix) {
  if(si.hasName(val))
    return si.getName(val);
  else if(val.hasName())
    return LLVM::formatName(val.getName());
  else if(prefix.length())
    return getNewVar(prefix);
  else
    return getNewVar();
}

std::string LLVMBackend::getName(const Value* val, const std::string& prefix) {
  return getName(*val, prefix);
}

void LLVMBackend::beginFunction(const Function& f) {
  BackendBase::beginFunction(funcs.at(&f));
}

void LLVMBackend::endFunction(const Function& f) {
  BackendBase::endFunction(funcs.at(&f));
}

void LLVMBackend::beginBlock() {
  stmts.emplace();
}

void LLVMBackend::endBlock() {
  BackendBase::add(ast->createCompoundStmt(stmts.pop()));
}

void LLVMBackend::beginBlock(const BasicBlock& bb) {
  BackendBase::add(blocks.at(&bb)->getStmt());
}

void LLVMBackend::endBlock(const BasicBlock&) {
  // Nothing to do
}

const Instruction&
LLVMBackend::getInstructionForConstantExpr(const ConstantExpr& cexpr) {
  if(not cexprs.contains(&cexpr))
    // ********************* HORRENDOUS EVIL AHEAD *************************
    //
    // FIXME: Make this go away ASAP!
    //
    // ConstantExpr for LLVM 8.0 (that this was originally developed with)
    // is not marked const, but it is in later versions. I don't think
    // anything has actually changed in the code between the two versions.
    // And I really don't want to "un-const" everything here. So just
    // cast away the const and forge ahead.
    //
    // ---------------------------------------------------------------------
    cexprs[&cexpr]
        = std::unique_ptr<Instruction, std::function<void(Instruction*)>>(
            const_cast<ConstantExpr&>(cexpr).getAsInstruction(),
            [](Instruction* inst) { inst->deleteValue(); });

  return *cexprs.at(&cexpr);
}

const ConstantDataArray*
LLVMBackend::getStringLiteral(const llvm::Instruction& inst) {
  if(const auto* gep = dyn_cast<GetElementPtrInst>(&inst))
    if(si.isStringLiteral(gep->getPointerOperand())
       and gep->hasAllZeroIndices())
      return dyn_cast<ConstantDataArray>(
          dyn_cast<GlobalVariable>(gep->getPointerOperand())->getInitializer());
  return nullptr;
}

void LLVMBackend::add(const AllocaInst& alloca, const std::string& name) {
  clang::FunctionDecl* f = funcs.at(&LLVM::getFunction(alloca));
  locals[&alloca]
      = ast->createLocalVariable(name, get(alloca.getAllocatedType()), f);
}

clang::Expr* LLVMBackend::get(const AllocaInst& alloca) {
  // The alloca statement in LLVM is always a pointer type, so where the
  // variable will be used, we actually need the pointer to it to be
  // consistent
  return ast->createUnaryOperator(ast->createDeclRefExpr(locals.at(&alloca)),
                                  clang::UO_AddrOf,
                                  get(alloca.getType()));
}

// UNIMPLEMENTED(AtomicCmpXchgInst)
// UNIMPLEMENTED(AtomicRMWInst)

clang::Expr* LLVMBackend::get(const BinaryOperator& inst) {
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

  return ast->createBinaryOperator(get(inst.getOperand(0)),
                                   get(inst.getOperand(1)),
                                   opc,
                                   get(inst.getType()));
}

clang::Expr* LLVMBackend::get(const CastInst& cst) {
  return ast->createCastExpr(get(cst.getOperand(0)), get(cst.getType()));
}

void LLVMBackend::add(const CallInst& call) {
  BackendBase::add(get(call));
}

void LLVMBackend::add(const InvokeInst& invoke) {
  BackendBase::add(get(invoke));
}

clang::Expr* LLVMBackend::get(const CallInst& call) {
  Vector<clang::Expr*> args;
  for(const Value* arg : call.arg_operands())
    args.push_back(get(arg));

  clang::CallExpr* callExpr = ast->createCallExpr(
      get(call.getCalledValue()), args, get(call.getType()));

  return callExpr;
}

// UNIMPLEMENTED(CatchReturnInst)
// UNIMPLEMENTED(CatchSwitchInst)
// UNIMPLEMENTED(CleanupReturnInst)

clang::Expr* LLVMBackend::get(const CmpInst& cmp) {
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

  return ast->createBinaryOperator(
      get(cmp.getOperand(0)), get(cmp.getOperand(1)), opc, get(cmp.getType()));
}

clang::Expr* LLVMBackend::get(const ExtractElementInst& extract) {
  fatal(error() << "NOT IMPLEMENTED: " << extract);
  return nullptr;
}

clang::Expr* LLVMBackend::get(const ExtractValueInst& extract) {
  fatal(error() << "NOT IMPLEMENTED: " << extract);
  return nullptr;
}

clang::Expr*
LLVMBackend::handleIndexOperand(PointerType* pty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const Value*>& indices,
                                const Instruction&) {
  const Value* op = indices[idx];
  if(const auto* cint = dyn_cast<ConstantInt>(op)) {
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
    clang::ArraySubscriptExpr* arrExpr = nullptr;
    if(auto* un = dyn_cast<clang::UnaryOperator>(Clang::stripCasts(currExpr)))
      arrExpr = dyn_cast<clang::ArraySubscriptExpr>(un->getSubExpr());
    if(arrExpr) {
      clang::Expr* idxExpr = arrExpr->getIdx();
      clang::Expr* newIdx = ast->createBinaryOperator(
          ast->cloneExpr(idxExpr), get(op), clang::BO_Add, idxExpr->getType());
      arrExpr->setRHS(newIdx);
      return currExpr;
    }
  }
  return ast->createArraySubscriptExpr(
      currExpr, get(op), get(pty->getElementType()));
}

clang::Expr*
LLVMBackend::handleIndexOperand(ArrayType* aty,
                                clang::Expr* currExpr,
                                unsigned idx,
                                const Vector<const Value*>& indices,
                                const Instruction&) {
  return ast->createArraySubscriptExpr(
      currExpr, get(indices[idx]), get(aty->getElementType()));
}

clang::Expr* LLVMBackend::handleIndexOperand(StructType* sty,
                                             clang::Expr* currExpr,
                                             unsigned field,
                                             const Instruction&) {
  return ast->createMemberExpr(
      currExpr, fields.at(sty)[field], get(sty->getElementType(field)));
}

clang::Expr* LLVMBackend::get(const GetElementPtrInst& gep) {
  const Value* ptr = gep.getPointerOperand();
  Type* type = ptr->getType();
  clang::Expr* expr = get(ptr);
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
  if(isa<GetElementPtrInst>(LLVM::stripCasts(gep.getPointerOperand())))
    return expr;
  else
    return ast->createUnaryOperator(expr, clang::UO_AddrOf, get(gep.getType()));
}

// UNIMPLEMENTED(IndirectBrInst)
// UNIMPLEMENTED(InsertElementInst)
// UNIMPLEMENTED(InsertValueInst)

clang::Expr* LLVMBackend::get(const InvokeInst& invoke) {
  fatal(error() << "NOT IMPLEMENTED: " << invoke);
  return nullptr;
}

// UNIMPLEMENTED(LandingPadInst)

clang::Expr* LLVMBackend::get(const LoadInst& load) {
  return ast->createUnaryOperator(
      get(load.getPointerOperand()), clang::UO_Deref, get(load.getType()));
}

clang::Expr* LLVMBackend::get(const PHINode&) {
  fatal(error() << "PHI nodes should have been removed");

  return nullptr;
}

UNIMPLEMENTED(ResumeInst)

clang::Expr* LLVMBackend::get(const SelectInst& select) {
  return ast->createConditionalOperator(get(select.getCondition()),
                                        get(select.getTrueValue()),
                                        get(select.getFalseValue()),
                                        get(select.getType()));
}

clang::Expr* LLVMBackend::get(const ShuffleVectorInst& shuffle) {
  fatal(error() << "NOT IMPLEMENTED: " << shuffle);
  return nullptr;
}

clang::Expr* LLVMBackend::get(const UnaryOperator& inst) {
  clang::UnaryOperator::Opcode opc;
  switch(inst.getOpcode()) {
  case UnaryOperator::FNeg:
    opc = clang::UO_Minus;
    break;
  default:
    fatal(error() << "Unknown unary operator: " << inst);
    break;
  }

  return ast->createUnaryOperator(
      get(inst.getOperand(0)), opc, get(inst.getType()));
}

clang::Expr* LLVMBackend::get(const Value* v) {
  if(const auto* f = dyn_cast<Function>(v))
    return get(*f);
  else if(const auto* arg = dyn_cast<Argument>(v))
    return get(*arg);
  else if(const auto* g = dyn_cast<GlobalVariable>(v))
    return get(*g);
  else if(const auto* alias = dyn_cast<GlobalAlias>(v))
    return get(*alias);
  else if(const auto* alloca = dyn_cast<AllocaInst>(v))
    return get(*alloca);
  else if(const auto* cst = dyn_cast<CastInst>(v))
    return get(*cst);
  else if(const auto* invoke = dyn_cast<InvokeInst>(v))
    return get(*invoke);
  else if(const auto* call = dyn_cast<CallInst>(v))
    return get(*call);
  else if(const auto* gep = dyn_cast<GetElementPtrInst>(v))
    return get(*gep);
  else if(const auto* load = dyn_cast<LoadInst>(v))
    return get(*load);
  else if(const auto* shuffle = dyn_cast<ShuffleVectorInst>(v))
    return get(*shuffle);
  else if(const auto* select = dyn_cast<SelectInst>(v))
    return get(*select);
  else if(const auto* cmp = dyn_cast<CmpInst>(v))
    return get(*cmp);
  // else if(const auto* axchg = dyn_cast<AtomicCmpXchgInst>(v))
  //   handle(*axchg);
  // else if(const auto* rmw = dyn_cast<AtomicRMWInst>(v))
  //   handle(*rmw);
  // else if(const auto* fence = dyn_cast<FenceInst>(v))
  //   handle(*fence);
  else if(const auto* extractElem = dyn_cast<ExtractElementInst>(v))
    return get(*extractElem);
  else if(const auto* extractVal = dyn_cast<ExtractValueInst>(v))
    return get(*extractVal);
  // else if(const auto* insertElem = dyn_cast<InsertElementInst>(v))
  //   handle(*insertElem);
  // else if(const auto* insertVal = dyn_cast<InsertValueInst>(v))
  //   handle(*insertVal);
  else if(const auto* binop = dyn_cast<BinaryOperator>(v))
    return get(*binop);
  else if(const auto* unop = dyn_cast<UnaryOperator>(v))
    return get(*unop);
  else if(const auto* phi = dyn_cast<PHINode>(v))
    return get(*phi);
  else if(const auto* cint = dyn_cast<ConstantInt>(v))
    return get(*cint);
  else if(const auto* cfp = dyn_cast<ConstantFP>(v))
    return get(*cfp);
  else if(const auto* cnull = dyn_cast<ConstantPointerNull>(v))
    return get(*cnull);
  else if(const auto* cundef = dyn_cast<UndefValue>(v))
    return get(*cundef);
  else if(const auto* cseq = dyn_cast<ConstantDataSequential>(v))
    return get(*cseq);
  else if(const auto* carray = dyn_cast<ConstantArray>(v))
    return get(*carray);
  else if(const auto* cstruct = dyn_cast<ConstantStruct>(v))
    return get(*cstruct);
  else if(const auto* cvec = dyn_cast<ConstantVector>(v))
    return get(cvec);
  else if(const auto* cexpr = dyn_cast<ConstantExpr>(v))
    return get(*cexpr);
  else
    fatal(error() << "Unknown LLVM value to get: " << *v);

  return nullptr;
}

void LLVMBackend::add(const BranchInst& br) {
  if(br.isConditional()) {
    auto* thn = ast->createCompoundStmt(
        ast->createGotoStmt(blocks.at(br.getSuccessor(0))));
    auto* els = ast->createCompoundStmt(
        ast->createGotoStmt(blocks.at(br.getSuccessor(1))));
    BackendBase::add(ast->createIfStmt(get(br.getCondition()), thn, els));
  } else {
    BackendBase::add(ast->createGotoStmt(blocks.at(br.getSuccessor(0))));
  }
}

UNIMPLEMENTED(CatchPadInst)
UNIMPLEMENTED(FenceInst)

void LLVMBackend::add(const ReturnInst& ret) {
  clang::Expr* retExpr = nullptr;
  if(const Value* val = ret.getReturnValue())
    retExpr = get(val);

  BackendBase::add(ast->createReturnStmt(retExpr));
}

void LLVMBackend::add(const StoreInst& store) {
  const Value* val = store.getValueOperand();
  clang::QualType type = get(val->getType());
  clang::Expr* ptr = ast->createUnaryOperator(
      get(store.getPointerOperand()), clang::UO_Deref, type);
  auto* assign
      = ast->createBinaryOperator(ptr, get(val), clang::BO_Assign, type);
  BackendBase::add(assign);
}

void LLVMBackend::add(const SwitchInst& swtch) {
  beginBlock();
  for(const auto& i : swtch.cases()) {
    BackendBase::add(ast->createCompoundStmt(
        ast->createGotoStmt(blocks.at(i.getCaseSuccessor()))));
    addSwitchCase(i.getCaseValue());
  };
  if(const BasicBlock* deflt = swtch.getDefaultDest()) {
    BackendBase::add(
        ast->createCompoundStmt(ast->createGotoStmt(blocks.at(deflt))));
    addSwitchDefault();
  }
  endBlock();
  addSwitchStmt(swtch);
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
  // TODO: Add argument attributes like const, restrict, non-null etc. that
  // were inferred by the compiler and/or were present in the original code
  clang::QualType type = get(arg.getType());
  args[&arg] = ast->createParam(name, type, funcs.at(arg.getParent()));
}

void LLVMBackend::add(const Function& f,
                      const std::string& name,
                      const Vector<std::string>& argNames) {
  clang::QualType type = get(f.getFunctionType());
  clang::FunctionDecl* decl = ast->createFunction(name, type);
  llvmClangMap.add(f, decl);

  funcs[&f] = decl;
  Vector<clang::ParmVarDecl*> params;
  for(const Argument& arg : f.args()) {
    add(arg, argNames[arg.getArgNo()]);
    params.push_back(args[&arg]);
  }
  decl->setParams(LLVM::makeArrayRef<clang::ParmVarDecl*>(params));
}

void LLVMBackend::add(const GlobalAlias& alias, const std::string&) {
  fatal(error() << "NOT IMPLEMENTED: " << alias);
}

void LLVMBackend::add(const GlobalVariable& g, const std::string& name) {
  clang::QualType type = get(g.getType()->getElementType());
  if(g.isConstant())
    type.addConst();
  clang::VarDecl* decl
      = ast->createGlobalVariable(name, get(g.getType()->getElementType()));
  llvmClangMap.add(g, decl);
  if(const Constant* init = g.getInitializer())
    decl->setInit(get(init));
}

clang::Expr* LLVMBackend::get(const Argument& arg) {
  return ast->createDeclRefExpr(args.at(&arg));
}

clang::Expr* LLVMBackend::get(const Function& f) {
  return ast->createDeclRefExpr(funcs.at(&f));
}

clang::Expr* LLVMBackend::get(const GlobalVariable& g) {
  return ast->createDeclRefExpr(globals.at(&g));
}

clang::Expr* LLVMBackend::get(const GlobalAlias& alias) {
  return ast->createDeclRefExpr(aliases.at(&alias));
}

void LLVMBackend::add(const BasicBlock& bb, const std::string& name) {
  blocks[&bb] = ast->createLabelDecl(funcs.at(bb.getParent()), name);
}

void LLVMBackend::add(StructType* sty, const std::string& name) {
  clang::RecordDecl* decl = ast->createStruct(name);
  udts[sty] = decl;
  types[sty] = clang::QualType(decl->getTypeForDecl(), 0);
}

void LLVMBackend::add(StructType* sty, const Vector<std::string>& elements) {
  for(unsigned i = 0; i < sty->getNumElements(); i++)
    fields[sty].push_back(ast->createField(
        elements[i], get(sty->getElementType(i)), udts.at(sty)));

  // Not sure if adding a body to the struct will result in the underlying
  // RecordType changing. Jus in case, add the type back to the map
  types[sty] = clang::QualType(udts.at(sty)->getTypeForDecl(), 0);
}

clang::Expr* LLVMBackend::get(const ConstantInt& cint) {
  Type* type = cint.getType();
  if(type->isIntegerTy(1))
    return ast->createBoolLiteral((bool)cint.getLimitedValue(), get(type));
  else
    return ast->createIntLiteral(cint.getValue(), get(type));
}

clang::Expr* LLVMBackend::get(const ConstantFP& cfp) {
  return ast->createFloatLiteral(cfp.getValueAPF(), get(cfp.getType()));
}

clang::Expr* LLVMBackend::get(const ConstantPointerNull& cnull) {
  return ast->createNullptr(get(cnull.getType()));
}

clang::Expr* LLVMBackend::get(const ConstantAggregateZero& czero) {
  // FIXME: Implement this properly. It's not that hard. Just recursively
  // build an init list with zeros in it.
  return ast->createIntLiteral(APInt(64, 0),
                               get(Type::getInt64Ty(czero.getContext())));
}

clang::Expr* LLVMBackend::get(const ConstantDataSequential& cseq) {
  clang::QualType type = get(cseq.getType());
  if(cseq.isCString()) {
    return ast->createStringLiteral(cseq.getAsString(), type);
  } else if(cseq.isString()) {
    return ast->createStringLiteral(cseq.getAsString(), type);
  } else {
    Vector<clang::Expr*> exprs;
    for(unsigned i = 0; i < cseq.getNumElements(); i++)
      exprs.push_back(get(cseq.getElementAsConstant(i)));
    return ast->createInitListExpr(exprs);
  }
}

clang::Expr* LLVMBackend::get(const ConstantStruct& cstruct) {
  Vector<clang::Expr*> exprs;
  for(const Use& op : cstruct.operands())
    exprs.push_back(get(op.get()));
  return ast->createInitListExpr(exprs);
}

clang::Expr* LLVMBackend::get(const ConstantArray& carray) {
  Vector<clang::Expr*> exprs;
  for(const Use& op : carray.operands())
    exprs.push_back(get(op.get()));
  return ast->createInitListExpr(exprs);
}

clang::Expr* LLVMBackend::get(const UndefValue& cundef) {
  return ast->createVariable("__undefined__",
                             get(cundef.getType()),
                             astContext.getTranslationUnitDecl());
}

clang::Expr* LLVMBackend::get(const ConstantExpr& cexpr) {
  const llvm::Instruction& inst = getInstructionForConstantExpr(cexpr);

  // If we are looking up a string literal, just use the literal because
  // that is almost always what we want, I think. I am not sure there is a lot
  // of value in exposing the intricacies of string literals being created
  // in "constant" memory and having their addresses taken when being used.
  if(const ConstantDataArray* lit = getStringLiteral(inst))
    return get(*lit);
  else
    return get(&inst);
}

clang::QualType LLVMBackend::get(IntegerType* ity) {
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
  default:
    fatal(error() << "UNKNOWN INTEGER TYPE: " << *ity);
    break;
  }

  return clang::QualType();
}

clang::QualType LLVMBackend::get(PointerType* pty) {
  return types[pty] = astContext.getPointerType(get(pty->getElementType()));
}

clang::QualType LLVMBackend::get(ArrayType* aty) {
  APInt elems(64, aty->getNumElements(), false);
  clang::QualType ety = get(aty->getElementType());

  return types[aty] = astContext.getConstantArrayType(
             ety, elems, clang::ArrayType::Normal, 0);
}

clang::QualType LLVMBackend::get(FunctionType* fty) {
  clang::QualType ret = get(fty->getReturnType());
  std::vector<clang::QualType> args;
  for(Type* param : fty->params())
    args.push_back(get(param));
  clang::FunctionProtoType::ExtProtoInfo proto;
  proto.Variadic = fty->isVarArg();

  return types[fty] = astContext.getFunctionType(
             ret, ArrayRef<clang::QualType>(args), proto);
}

clang::QualType LLVMBackend::get(VectorType* vty) {
  return types[vty]
         = astContext.getVectorType(get(vty->getElementType()),
                                    vty->getNumElements(),
                                    clang::VectorType::GenericVector);
}

clang::QualType LLVMBackend::get(Type* type) {
  if(not has(type)) {
    if(type->isVoidTy())
      return types[type] = astContext.VoidTy;
    else if(auto* ity = dyn_cast<IntegerType>(type))
      return get(ity);
    else if(type->isFloatTy())
      return types[type] = astContext.FloatTy;
    else if(type->isDoubleTy())
      return types[type] = astContext.DoubleTy;
    else if(type->isX86_FP80Ty())
      return types[type] = astContext.LongDoubleTy;
    else if(type->isFP128Ty())
      return types[type] = astContext.Float128Ty;
    else if(auto* pty = dyn_cast<PointerType>(type))
      return get(pty);
    else if(auto* aty = dyn_cast<ArrayType>(type))
      return get(aty);
    else if(auto* fty = dyn_cast<FunctionType>(type))
      return get(fty);
    else if(auto* vty = dyn_cast<VectorType>(type))
      return get(vty);
    else
      fatal(error() << "COULD NOT GET TYPE: " << *type);
  }

  return types.at(type);
}

void LLVMBackend::addTemp(const Instruction& inst, const std::string& name) {
  clang::QualType type = get(inst.getType());
  clang::DeclRefExpr* var
      = ast->createVariable(name, type, funcs.at(&LLVM::getFunction(inst)));
  clang::BinaryOperator* temp = ast->createBinaryOperator(
      var, get(&inst), clang::BO_Assign, type);
  BackendBase::add(temp);
}

void LLVMBackend::addIfThen(const BranchInst& br, bool invert) {
  clang::Stmt* thn = stmts.top().pop_back();
  clang::Expr* cond = get(br.getCondition());
  if(invert)
    cond = ast->createUnaryOperator(cond, clang::UO_LNot, cond->getType());
  BackendBase::add(ast->createIfStmt(cond, thn));
}

void LLVMBackend::addIfThenElse(const BranchInst& br) {
  clang::Stmt* els = stmts.top().pop_back();
  clang::Stmt* thn = stmts.top().pop_back();
  clang::Expr* cond = get(br.getCondition());
  BackendBase::add(ast->createIfStmt(cond, thn, els));
}

void LLVMBackend::addIfThenBreak(const BranchInst& br, bool invert) {
  clang::Stmt* thn = ast->createCompoundStmt(ast->createBreakStmt());
  clang::Expr* cond = get(br.getCondition());
  if(invert)
    cond = ast->createUnaryOperator(cond, clang::UO_LNot, cond->getType());
  BackendBase::add(ast->createIfStmt(cond, thn));
}

void LLVMBackend::addIfThenGoto(const std::string& name,
                                const BranchInst& br,
                                bool invert) {
  if(not labels.contains(name))
    labels[name]
        = ast->createLabelDecl(funcs.at(br.getParent()->getParent()), name);

  clang::Stmt* stmt
      = ast->createCompoundStmt(ast->createGotoStmt(labels.at(name)));
  clang::Expr* cond = get(br.getCondition());
  if(invert)
    cond = ast->createUnaryOperator(cond, clang::UO_LNot, cond->getType());

  BackendBase::add(ast->createIfStmt(cond, stmt));
}

void LLVMBackend::addLabel(const std::string& name, const Function& f) {
  if(not labels.contains(name))
    labels[name] = ast->createLabelDecl(funcs.at(&f), name);
  BackendBase::add(labels.at(name)->getStmt());
}

void LLVMBackend::addBreak() {
  BackendBase::add(ast->createBreakStmt());
}

void LLVMBackend::addContinue() {
  BackendBase::add(ast->createContinueStmt());
}

void LLVMBackend::addEndlessLoop() {
  clang::Stmt* body = stmts.top().pop_back();
  BackendBase::add(ast->createDoStmt(
      body, ast->createBoolLiteral(true, astContext.BoolTy)));
}

void LLVMBackend::addSwitchStmt(const SwitchInst& sw) {
  clang::CompoundStmt* body = cast<clang::CompoundStmt>(stmts.top().pop_back());
  clang::SwitchStmt* switchStmt
      = ast->createSwitchStmt(get(sw.getCondition()), body);

  for(clang::Stmt* kase : body->body()) {
    // Calling SwitchStmt->addSwitchCase() ends up with the newly added case
    // becoming the first case in the switch statement. Better to maintain the
    // order here
    clang::SwitchCase* first = switchStmt->getSwitchCaseList();
    if(not first) {
      switchStmt->addSwitchCase(cast<clang::SwitchCase>(kase));
    } else {
      clang::SwitchCase* last = first;
      while(last->getNextSwitchCase())
        last = last->getNextSwitchCase();
      last->setNextSwitchCase(cast<clang::SwitchCase>(kase));
    }
  }

  BackendBase::add(switchStmt);
}

void LLVMBackend::addSwitchCase(const ConstantInt* value) {
  clang::Stmt* body = stmts.top().pop_back();
  if(value)
    BackendBase::add(ast->createCaseStmt(get(*value), body));
  else
    BackendBase::add(ast->createDefaultStmt(body));
}

void LLVMBackend::addSwitchCase(const ConstantInt& value) {
  addSwitchCase(&value);
}

void LLVMBackend::addSwitchDefault() {
  addSwitchCase(nullptr);
}

void LLVMBackend::addGoto(const BasicBlock& bb) {
  BackendBase::add(ast->createGotoStmt(blocks.at(&bb)));
}

void LLVMBackend::addGoto(const std::string& dest, const Function& f) {
  if(not labels.contains(dest))
    labels[dest] = ast->createLabelDecl(funcs.at(&f), dest);
  BackendBase::add(ast->createGotoStmt(labels.at(dest)));
}

// bool LLVMBackend::has(const Value* val) const {
//   return l2c.contains(val);
// }

// bool LLVMBackend::has(const Value& val) const {
//   return has(&val);
// }

bool LLVMBackend::has(Type* type) const {
  return types.contains(type);
}

clang::QualType LLVMBackend::get(Type* type) const {
  return types.at(type);
}

} // namespace cish
