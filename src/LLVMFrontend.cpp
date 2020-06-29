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

#include "LLVMFrontend.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "IRSourceInfo.h"
#include "LLVMBackend.h"
#include "LLVMUtils.h"
#include "Set.h"
#include "Vector.h"

#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/LoopPass.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/PassManager.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

#define UNIMPLEMENTED(TYPE)                        \
  void LLVMFrontend::handle(const TYPE& inst) {    \
    fatal(error() << "NOT IMPLEMENTED: " << inst); \
  }

static std::string formatLLVMName(const std::string& s) {
  static const Set<char> repl = {'.', ':', '"'};
  std::string name = s;
  for(size_t i = 0; i < name.length(); i++)
    if(repl.contains(name[i]))
      name[i] = '_';
  return name;
}

LLVMFrontend::LLVMFrontend(CishContext& context, const SourceInfo& si)
    : context(context), si(si), be(context.getLLVMBackend()) {
  ;
}

void LLVMFrontend::addIgnoreValue(const llvm::Value* v) {
  ignoreValues.insert(v);
}

void LLVMFrontend::addIgnoreValue(const llvm::Value& v) {
  addIgnoreValue(&v);
}

bool LLVMFrontend::isIgnoreValue(const llvm::Value* v) const {
  return ignoreValues.contains(v);
}

bool LLVMFrontend::isIgnoreValue(const llvm::Value& v) const {
  return isIgnoreValue(&v);
}

const Instruction&
LLVMFrontend::getInstructionForConstantExpr(const ConstantExpr& cexpr) {
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
LLVMFrontend::getStringLiteral(const llvm::Instruction& inst) {
  if(const auto* gep = dyn_cast<GetElementPtrInst>(&inst))
    if(si.isStringLiteral(gep->getPointerOperand())
       and gep->hasAllZeroIndices())
      return dyn_cast<ConstantDataArray>(
          dyn_cast<GlobalVariable>(gep->getPointerOperand())->getInitializer());
  return nullptr;
}

void LLVMFrontend::handle(const ConstantInt& cint) {
  be.add(cint);
}

void LLVMFrontend::handle(const ConstantFP& cfp) {
  be.add(cfp);
}

void LLVMFrontend::handle(const ConstantPointerNull& cnull) {
  be.add(cnull);
}

void LLVMFrontend::handle(const ConstantAggregateZero& czero) {
  be.add(czero);
}

// This is a somewhat grotesque abuse of things. But there isn't an equivalent
// clang Expr for LLVM's undef value. Until (if) I actually make one, just
// use the GNUNullExpr instead to refer to this undef value
void LLVMFrontend::handle(const UndefValue& undef) {
  be.add(undef);
}

void LLVMFrontend::handle(const ConstantArray& carray) {
  for(const Use& op : carray.operands())
    handle(op.get());

  be.add(carray);
}

void LLVMFrontend::handle(const ConstantDataSequential& cseq) {
  if(not(cseq.isCString() or cseq.isString()))
    for(unsigned i = 0; i < cseq.getNumElements(); i++)
      handle(cseq.getElementAsConstant(i));
  be.add(cseq);
}

void LLVMFrontend::handle(const ConstantStruct& cstruct) {
  for(const Use& op : cstruct.operands())
    handle(op.get());

  be.add(cstruct);
}

void LLVMFrontend::handle(const ConstantVector& cvec) {
  // We may be able to use something else to explicitly represent a vector
  // since obviously nothing precisely like it exists in C/C++, but an
  // initializer list ought to be good enough and once annotations are
  // implemented, a comment could be added near where these are used
  // indicating that they are actually vectors
  fatal(error() << "UNIMPLEMENTED: CONSTANT VECTOR");
}

void LLVMFrontend::handle(const ConstantExpr& cexpr) {
  const llvm::Instruction& inst = getInstructionForConstantExpr(cexpr);
  // If we are looking up a string literal, just use the literal because
  // that is almost always what we want, I think. I am not sure there is a lot
  // of value in exposing the intricacies of string literals being created
  // in "constant" memory and having their addresses taken when being used.
  if(const ConstantDataArray* lit = getStringLiteral(inst)) {
    handle(lit);
    be.add(cexpr, *lit);
  } else {
    handle(&inst);
    be.add(cexpr, inst);
  }
}

void LLVMFrontend::handle(const AllocaInst& alloca) {
  handle(alloca.getAllocatedType());
  be.add(alloca, getName(alloca, "local"));
}

UNIMPLEMENTED(AtomicCmpXchgInst)
UNIMPLEMENTED(AtomicRMWInst)

void LLVMFrontend::handle(const BinaryOperator& inst) {
  handle(inst.getOperand(0));
  handle(inst.getOperand(1));

  be.add(inst);
}

void LLVMFrontend::handle(const BranchInst& br) {
  // Don't need to handle the successors because all blocks will be handled
  // separately
  if(br.isConditional())
    handle(br.getCondition());
  be.add(br);
}

void LLVMFrontend::handle(const CastInst& cst) {
  handle(cst.getDestTy());
  handle(cst.getOperand(0));
  be.add(cst);
}

UNIMPLEMENTED(InvokeInst)

void LLVMFrontend::handle(const CallInst& call) {
  handle(call.getCalledValue());
  for(const Value* arg : call.arg_operands())
    handle(arg);
  be.add(call);
}

UNIMPLEMENTED(CatchReturnInst)
UNIMPLEMENTED(CatchSwitchInst)
UNIMPLEMENTED(CleanupReturnInst)

void LLVMFrontend::handle(const CmpInst& cmp) {
  handle(cmp.getOperand(0));
  handle(cmp.getOperand(1));
  be.add(cmp);
}

UNIMPLEMENTED(ExtractElementInst)
UNIMPLEMENTED(ExtractValueInst)
UNIMPLEMENTED(FenceInst)
UNIMPLEMENTED(CatchPadInst)

void LLVMFrontend::handle(const GetElementPtrInst& gep) {
  handle(gep.getPointerOperand());
  for(const Value* op : gep.indices())
    handle(op);
  be.add(gep);
}

UNIMPLEMENTED(IndirectBrInst)
UNIMPLEMENTED(InsertElementInst)
UNIMPLEMENTED(InsertValueInst)
UNIMPLEMENTED(LandingPadInst)

void LLVMFrontend::handle(const LoadInst& load) {
  handle(load.getPointerOperand());
  be.add(load);
}

void LLVMFrontend::handle(const PHINode& phi) {
  // PHINodes should have been removed before getting here
  fatal(error() << "Unexpected PHI nodes");
}

UNIMPLEMENTED(ResumeInst)

void LLVMFrontend::handle(const ReturnInst& ret) {
  if(Value* val = ret.getReturnValue())
    handle(ret.getReturnValue());
  be.add(ret);
}

void LLVMFrontend::handle(const SelectInst& select) {
  handle(select.getCondition());
  handle(select.getTrueValue());
  handle(select.getFalseValue());
  be.add(select);
}

UNIMPLEMENTED(ShuffleVectorInst)

void LLVMFrontend::handle(const StoreInst& store) {
  handle(store.getPointerOperand());
  handle(store.getValueOperand());
  be.add(store);
}

void LLVMFrontend::handle(const SwitchInst& sw) {
  // Don't need to handle the basic blocks because they will already have been
  // handled before the instructions are processed
  handle(sw.getCondition());
  for(const auto& i : sw.cases())
    handle(i.getCaseValue());
  be.add(sw);
}

void LLVMFrontend::handle(const UnaryOperator& inst) {
  handle(inst.getOperand(0));
  be.add(inst);
}

void LLVMFrontend::handle(const UnreachableInst&) {
  // Nothing to do here
}

bool LLVMFrontend::shouldUseTemporary(const Instruction& inst) const {
  // In the case of function calls, if there is more than one use, save the
  // result to a temporary variable and use that temporary instead of inserting
  // the call everywhere. This is important because it otherwise gives the
  // impression that the call is made several times which is not correct
  size_t uses = inst.getNumUses();
  if(isa<PHINode>(inst) or isa<AllocaInst>(inst))
    return false;
  else if((isa<CallInst>(inst) or isa<InvokeInst>(inst))
          and (not inst.getType()->isVoidTy()) and ((uses == 0) or (uses > 1)))
    return true;
  else if(auto* load = dyn_cast<LoadInst>(&inst))
    if(inst.getMetadata("cish.notemp"))
      return false;
  return false;
}

void LLVMFrontend::handle(const Instruction* inst) {
  if(const auto* alloca = dyn_cast<AllocaInst>(inst))
    handle(*alloca);
  else if(const auto* sw = dyn_cast<SwitchInst>(inst))
    handle(*sw);
  else if(const auto* br = dyn_cast<BranchInst>(inst))
    handle(*br);
  else if(const auto* indirectBr = dyn_cast<IndirectBrInst>(inst))
    handle(*indirectBr);
  else if(const auto* cst = dyn_cast<CastInst>(inst))
    handle(*cst);
  else if(const auto* invoke = dyn_cast<InvokeInst>(inst))
    handle(*invoke);
  else if(const auto* call = dyn_cast<CallInst>(inst))
    handle(*call);
  else if(const auto* gep = dyn_cast<GetElementPtrInst>(inst))
    handle(*gep);
  else if(const auto* load = dyn_cast<LoadInst>(inst))
    handle(*load);
  else if(const auto* store = dyn_cast<StoreInst>(inst))
    handle(*store);
  else if(const auto* returnInst = dyn_cast<ReturnInst>(inst))
    handle(*returnInst);
  else if(const auto* shuffle = dyn_cast<ShuffleVectorInst>(inst))
    handle(*shuffle);
  else if(const auto* select = dyn_cast<SelectInst>(inst))
    handle(*select);
  else if(const auto* cmp = dyn_cast<CmpInst>(inst))
    handle(*cmp);
  else if(const auto* axchg = dyn_cast<AtomicCmpXchgInst>(inst))
    handle(*axchg);
  else if(const auto* rmw = dyn_cast<AtomicRMWInst>(inst))
    handle(*rmw);
  else if(const auto* fence = dyn_cast<FenceInst>(inst))
    handle(*fence);
  else if(const auto* extractElem = dyn_cast<ExtractElementInst>(inst))
    handle(*extractElem);
  else if(const auto* extractVal = dyn_cast<ExtractValueInst>(inst))
    handle(*extractVal);
  else if(const auto* insertElem = dyn_cast<InsertElementInst>(inst))
    handle(*insertElem);
  else if(const auto* insertVal = dyn_cast<InsertValueInst>(inst))
    handle(*insertVal);
  else if(const auto* catchPad = dyn_cast<CatchPadInst>(inst))
    handle(*catchPad);
  else if(const auto* catchReturn = dyn_cast<CatchReturnInst>(inst))
    handle(*catchReturn);
  else if(const auto* cleanupReturn = dyn_cast<CleanupReturnInst>(inst))
    handle(*cleanupReturn);
  else if(const auto* landingPad = dyn_cast<LandingPadInst>(inst))
    handle(*landingPad);
  else if(const auto* resume = dyn_cast<ResumeInst>(inst))
    handle(*resume);
  else if(const auto* unreachable = dyn_cast<UnreachableInst>(inst))
    handle(*unreachable);
  else if(const auto* binop = dyn_cast<BinaryOperator>(inst))
    handle(*binop);
  else if(const auto* unop = dyn_cast<UnaryOperator>(inst))
    handle(*unop);
  else if(const auto* phi = dyn_cast<PHINode>(inst))
    handle(*phi);
  else
    fatal(error() << "UNKNOWN INSTRUCTION: " << *inst);

  if(shouldUseTemporary(*inst))
    be.addTemp(*inst, getName(inst));
}

void LLVMFrontend::handle(const Value* v) {
  if(be.has(*v))
    return;

  handle(v->getType());
  // The order of the statements is important. All globals are constants
  // but they need to be handled differently from the other constants
  if(const auto* inst = dyn_cast<Instruction>(v))
    return handle(inst);
  else if(const auto* ga = dyn_cast<GlobalAlias>(v))
    return handle(*ga);
  else if(const auto* f = dyn_cast<Function>(v))
    return handle(*f);
  else if(const auto* g = dyn_cast<GlobalVariable>(v))
    return handle(*g);
  else if(const auto* cint = dyn_cast<ConstantInt>(v))
    return handle(*cint);
  else if(const auto* cfp = dyn_cast<ConstantFP>(v))
    return handle(*cfp);
  else if(const auto* cnull = dyn_cast<ConstantPointerNull>(v))
    return handle(*cnull);
  else if(const auto* czero = dyn_cast<ConstantAggregateZero>(v))
    return handle(*czero);
  else if(const auto* cexpr = dyn_cast<ConstantExpr>(v))
    return handle(*cexpr);
  else if(const auto* undef = dyn_cast<UndefValue>(v))
    return handle(*undef);
  else if(const auto* carray = dyn_cast<ConstantArray>(v))
    return handle(*carray);
  else if(const auto* cda = dyn_cast<ConstantDataArray>(v))
    return handle(*cda);
  else if(const auto* cdv = dyn_cast<ConstantDataVector>(v))
    return handle(*cdv);
  else if(const auto* cstruct = dyn_cast<ConstantStruct>(v))
    return handle(*cstruct);
  else if(const auto* cvec = dyn_cast<ConstantVector>(v))
    return handle(*cvec);
  else if(const auto* bb = dyn_cast<BasicBlock>(v))
    return handle(*bb);
  else
    fatal(error() << "UNHANDLED: " << *v);
}

void LLVMFrontend::handle(const Value& v) {
  handle(&v);
}

void LLVMFrontend::handle(const BasicBlock& bb) {
  if(bb.hasNPredecessors(0))
    be.add(bb);
  else
    be.add(bb, be.getNewVar("bb"));
}

void LLVMFrontend::handle(const Function& f) {
  std::string fname = getName(f);
  cish::Vector<std::string> argNames(f.getFunctionType()->getNumParams());
  if(f.size()) {
    for(const Argument& arg : f.args()) {
      unsigned i = arg.getArgNo();
      if(si.hasName(arg))
        argNames[i] = si.getName(arg);
      else if(arg.hasName())
        argNames[i] = formatLLVMName(arg.getName());
      else
        argNames[i] = "arg_" + std::to_string(i);
    }
    be.add(f, fname, argNames);
    for(const BasicBlock& bb : f)
      handle(bb);
  } else if(not isIgnoreValue(&f)) {
    be.add(f, fname, argNames);
  }
}

void LLVMFrontend::handle(const GlobalVariable& g) {
  if(const Constant* init = g.getInitializer())
    handle(init);

  if(not si.isStringLiteral(g))
    be.add(g, getName(g, "g"));
}

UNIMPLEMENTED(GlobalAlias)

void LLVMFrontend::handle(PointerType* pty) {
  handle(pty->getElementType());
  be.add(pty);
}

void LLVMFrontend::handle(ArrayType* aty) {
  handle(aty->getElementType());
  be.add(aty);
}

void LLVMFrontend::handle(FunctionType* fty) {
  handle(fty->getReturnType());
  for(Type* param : fty->params())
    handle(param);
  be.add(fty);
}

void LLVMFrontend::handle(VectorType* vty) {
  handle(vty->getElementType());
  be.add(vty);
}

void LLVMFrontend::handle(Type* type) {
  if(be.has(type))
    return;

  if(auto* pty = dyn_cast<PointerType>(type))
    handle(pty);
  else if(auto* aty = dyn_cast<ArrayType>(type))
    handle(aty);
  else if(auto* fty = dyn_cast<FunctionType>(type))
    handle(fty);
  else if(auto* vty = dyn_cast<VectorType>(type))
    handle(vty);
  else if(isa<IntegerType>(type) or type->isFloatingPointTy()
          or type->isVoidTy())
    be.add(type);
}

std::string LLVMFrontend::getName(llvm::StructType* sty) {
  if(si.hasName(sty))
    return si.getName(sty);
  else if(sty->hasName())
    if(sty->getName().find("struct.") == 0)
      return formatLLVMName(sty->getName().substr(7));
    else if(sty->getName().find("class.") == 0)
      return formatLLVMName(sty->getName().substr(6));
    else if(sty->getName().find("union.") == 0)
      return formatLLVMName(sty->getName().substr(6));
    else
      return formatLLVMName(sty->getName());
  else
    return be.getNewVar("struct");
}

std::string LLVMFrontend::getName(const llvm::Value& val,
                                  const std::string& prefix) {
  if(si.hasName(val))
    return si.getName(val);
  else if(val.hasName())
    return formatLLVMName(val.getName());
  else if(prefix.length())
    return be.getNewVar(prefix);
  else
    return be.getNewVar();
}

std::string LLVMFrontend::getName(const llvm::Value* val,
                                  const std::string& prefix) {
  return getName(*val, prefix);
}

bool LLVMFrontend::allUsesIgnored(const Value* v) const {
  if(v->getNumUses() == 0)
    return false;
  for(const Use& u : v->uses())
    if(not ignoreValues.contains(u.getUser()))
      return false;
  return true;
}

void LLVMFrontend::expandIgnoredValues() {
  // Grow the ignore list to include anything that is only used by values in
  // the ignore list
  cish::Set<const Value*> wl = ignoreValues;
  while(wl.size()) {
    cish::Set<const Value*> next;
    for(const Value* v : wl)
      if(const auto* user = dyn_cast<User>(v))
        for(const Use& op : user->operands())
          if(allUsesIgnored(op.get()))
            next.insert(op.get());
    for(const Value* v : next)
      ignoreValues.insert(v);
    wl = std::move(next);
  }
}

} // namespace cish
