#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Pass.h>

#include "CishContext.h"
#include "Set.h"
#include "SourceInfo.h"
#include "Vector.h"

using namespace llvm;

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
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<ScalarEvolutionWrapperPass>();
  AU.setPreservesAll();
}

bool CishFunctionPass::runOnFunction(Function& f) {
  const cish::CishContext& context
      = getAnalysis<CishContextWrapperPass>().getCishContext();
  const cish::SourceInfo& si = context.getSourceInfo();
  cish::LLVMFrontend& fe = context.getLLVMFrontend();
  cish::LLVMBackend& be = context.getLLVMBackend();

  cish::Set<const llvm::Value*> phiValues;
  for(const Instruction& inst : instructions(f))
    if(const PHINode* phi = dyn_cast<PHINode>(&inst))
      for(const llvm::Value* v : phi->incoming_values())
        phiValues.insert(v);

  fe.handle(f.getFunctionType());

  cish::Vector<std::string> argNames;
  for(const Argument& arg : f.args())
    if(si.hasName(arg))
      argNames.push_back(si.getName(arg));
    else if(arg.hasName())
      argNames.push_back(arg.getName());
    else
      argNames.push_back("arg_" + std::to_string(arg.getArgNo()));
  if(si.hasName(f))
    be.add(f, si.getName(f), argNames);
  else
    be.add(f, f.getName(), argNames);
  for(const BasicBlock& bb : f)
    fe.handle(bb);

  be.beginFunction(f);

  // FIXME: Instead of just running through the basic blocks in function order,
  // find an ordering so that the generated code is more structured

  for(const BasicBlock& bb : f) {
    be.beginBlock(bb);
    for(const Instruction& inst : bb) {
      // There are only certain instructions that we "care" about. Most LLVM
      // instructions correspond to C "expressions" - arithmetic operations,
      // address computations etc. Only a few correspond to those that are
      // more likely to be "statements". The expressions are typically operands
      // to the statement. So we only explicitly handle those instructions
      // that are most likely to be statements.
      //
      // StoreInst and ReturnInst are obviously statements
      //
      // BranchInst and PHINode both appear in conditionals and loops. While
      // they could be handled there, they are dealt with explicitly because
      // it may not be possible to resolve the CFG in the LLVM IR back into
      // something neatly structured. In that case, things are done with goto's
      // in which case the branches and phi's need to be handled explicitly.
      // Anything that is an incoming value of a PHI node is explicitly treated
      // as a statement because we cannot deal with PHI nodes neatly in C,
      // so we may just have to throw our hands up there. So as not to be
      // utterly useless, the incoming values of a PHI should appear somewhere
      // in the output program.
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
      else if(phiValues.contains(&inst))
        fe.handle(&inst);
      else if(isa<AllocaInst>(inst) or isa<CallInst>(inst)
              or isa<InvokeInst>(inst) or isa<StoreInst>(inst)
              or isa<PHINode>(inst) or isa<BranchInst>(inst)
              or isa<ReturnInst>(inst))
        fe.handle(&inst);
    }
    be.endBlock(bb);
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
