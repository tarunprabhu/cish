#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Pass.h>

#include "CishContext.h"
#include "Diagnostics.h"
#include "Set.h"
#include "SourceInfo.h"
#include "StructureAnalysis.h"
#include "Vector.h"

using namespace llvm;

static llvm::LLVMContext* llvmContext = nullptr;

class Walker {
protected:
  const cish::SourceInfo& si;
  cish::LLVMFrontend& fe;
  cish::LLVMBackend& be;

protected:
  void walk(const cish::Block& block);
  void walk(const cish::Sequence& seq);
  void walk(const cish::IfThen& ift);
  void walk(const cish::IfThenElse& ifte);
  void walk(const cish::IfThenBreak& iftb);
  void walk(const cish::IfThenContinue& iftc);
  void walk(const cish::DoWhileLoop& loop);
  void walk(const cish::EndlessLoop& loop);
  void walk(const cish::NaturalLoop& loop);
  void walk(const cish::Latch& latch);
  void walk(const cish::Switch& sw);

public:
  Walker(const cish::SourceInfo& si,
         cish::LLVMFrontend& fe,
         cish::LLVMBackend& be);
  Walker(const Walker&) = delete;
  Walker(Walker&&) = delete;
  virtual ~Walker() = default;

  void walk(const cish::StructNode* node);
  void walk(const cish::StructNode& node);
};

Walker::Walker(const cish::SourceInfo& si,
               cish::LLVMFrontend& fe,
               cish::LLVMBackend& be)
  : si(si), fe(fe), be(be) {
  ;
}

void Walker::walk(const cish::Block& block) {
  // Because this will only get called when a complete structure could be
  // computed. all branching will be handled by the structure nodes, so
  // ignore any branch instruction.
  //
  // If at some point, we allow a "hybrid" situation where only a part of the
  // structure may have been constructed, we need to check if this is a basic
  // block for which we need to handle the branch
  for(const Instruction& inst : block.getLLVM()) {
    if(fe.isIgnoreValue(inst)) {
      continue;
    } else if(const BranchInst* br = dyn_cast<BranchInst>(&inst)) {
      if(br->isConditional())
        fe.handle(br->getCondition());
    } else if(isa<AllocaInst>(inst) or isa<CallInst>(inst)
              or isa<InvokeInst>(inst) or isa<StoreInst>(inst)
              or isa<ReturnInst>(inst)) {
      fe.handle(static_cast<const Value&>(inst));
    }
  }
}

void Walker::walk(const cish::Sequence& seq) {
  for(const cish::StructNode* next : seq)
    walk(next);
}

void Walker::walk(const cish::IfThen& ift) {
  walk(ift.getCond());
  be.beginBlock();
  walk(ift.getThen());
  be.endBlock();

  be.addIfThen(ift.getLLVMBranchInst());
}

void Walker::walk(const cish::IfThenElse& ifte) {
  walk(ifte.getCond());

  be.beginBlock();
  walk(ifte.getThen());
  be.endBlock();

  be.beginBlock();
  walk(ifte.getElse());
  be.endBlock();

  be.addIfThenElse(ifte.getLLVMBranchInst());
}

void Walker::walk(const cish::IfThenBreak& iftb) {
  walk(iftb.getCond());

  be.beginBlock();
  walk(iftb.getThen());
  be.addBreak();
  be.endBlock();

  be.addIfThen(iftb.getLLVMBranchInst());
}

void Walker::walk(const cish::IfThenContinue& iftc) {
  walk(iftc.getCond());

  be.beginBlock();
  walk(iftc.getThen());
  be.addContinue();
  be.endBlock();

  be.addIfThen(iftc.getLLVMBranchInst());
}

void Walker::walk(const cish::Switch& sw) {
  ;
}

void Walker::walk(const cish::DoWhileLoop& loop) {
  cish::warning() << "DoWhileLoop (UNIMPLEMNETED)\n";
}

void Walker::walk(const cish::EndlessLoop& loop) {
  be.beginBlock();
  for(const cish::StructNode* node : loop)
    walk(node);
  be.endBlock();

  be.addEndlessLoop();
}

void Walker::walk(const cish::NaturalLoop& loop) {
  cish::warning() << "Natural Loop (UNIMPLEMENTED)\n";
}

void Walker::walk(const cish::Latch& latch) {
  errs() << "walk: Latch (UNIMPLEMENTED)\n";
}

void Walker::walk(const cish::StructNode* node) {
  if(const auto* block = dyn_cast<cish::Block>(node))
    walk(*block);
  else if(const auto* seq = dyn_cast<cish::Sequence>(node))
    walk(*seq);
  else if(const auto* ift = dyn_cast<cish::IfThen>(node))
    walk(*ift);
  else if(const auto* ifte = dyn_cast<cish::IfThenElse>(node))
    walk(*ifte);
  else if(const auto* iftb = dyn_cast<cish::IfThenBreak>(node))
    walk(*iftb);
  else if(const auto* iftc = dyn_cast<cish::IfThenContinue>(node))
    walk(*iftc);
  else if(const auto* loop = dyn_cast<cish::DoWhileLoop>(node))
    walk(*loop);
  else if(const auto* endless = dyn_cast<cish::EndlessLoop>(node))
    walk(*endless);
  else if(const auto* natural = dyn_cast<cish::NaturalLoop>(node))
    walk(*natural);
  else if(const auto* latch = dyn_cast<cish::Latch>(node))
    walk(*latch);
  else if(const auto* sw = dyn_cast<cish::Switch>(node))
    walk(*sw);
  else
    cish::fatal(cish::error()
                << "Unknown structure node: " << node->getKindName());
}

void Walker::walk(const cish::StructNode& node) {
  walk(&node);
}

class CishFunctionPass : public FunctionPass {
public:
  static char ID;

public:
  CishFunctionPass();

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnFunction(Function& f) override;
};

CishFunctionPass::CishFunctionPass() : FunctionPass(ID) {
  ;
}

StringRef CishFunctionPass::getPassName() const {
  return "Cish Function Pass";
}

void CishFunctionPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<CishContextWrapperPass>();
  AU.addRequired<StructureAnalysisWrapperPass>();
  AU.setPreservesAll();
}

bool CishFunctionPass::runOnFunction(Function& f) {
  const cish::CishContext& context
      = getAnalysis<CishContextWrapperPass>().getCishContext();
  const cish::SourceInfo& si = context.getSourceInfo();
  cish::LLVMFrontend& fe = context.getLLVMFrontend();
  cish::LLVMBackend& be = context.getLLVMBackend();

  errs() << f << "\n";

  llvmContext = &f.getContext();
  be.beginFunction(f);

  const auto& analysis = getAnalysis<StructureAnalysisWrapperPass>();
  if(analysis.isStructured()) {
    Walker(si, fe, be).walk(&analysis.getStructured());
  } else {
    for(const BasicBlock& bb : f) {
      be.beginBlock(bb);
      for(const Instruction& inst : bb) {
        // There are only certain instructions that we "care" about. Most LLVM
        // instructions correspond to C "expressions" - arithmetic operations,
        // address computations etc. Only a few correspond to those that are
        // more likely to be "statements". The expressions are typically
        // operands to the statement. So we only explicitly handle those
        // instructions that are most likely to be statements.
        //
        // StoreInst and ReturnInst are obviously statements
        //
        // Calls to functions that do not return a value are always statements,
        // but other calls may be too because moving calls around is not a good
        // idea because it may look like a violation of semantics if the calls
        // are side-effecting, so just handle all calls as statements
        //
        // AllocaInst are local variable declarations and get treated like
        // statements.
        if(fe.isIgnoreValue(inst))
          continue;
        else if(isa<AllocaInst>(inst) or isa<CallInst>(inst)
                or isa<InvokeInst>(inst) or isa<StoreInst>(inst)
                or isa<BranchInst>(inst) or isa<ReturnInst>(inst))
          fe.handle(static_cast<const Value&>(inst));
      }
      be.endBlock(bb);
    }
  }

  be.endFunction(f);

  return false;
}

char CishFunctionPass::ID = 0;

static RegisterPass<CishFunctionPass>
    X("cish-functions", "Cish Function Pass", true, true);

Pass* createCishFunctionPass() {
  return new CishFunctionPass();
}
