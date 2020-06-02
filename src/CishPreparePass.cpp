#include <llvm/Pass.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Instructions.h>

#include <vector>

using namespace llvm;

/// This LLVM pass moves all branch instructions into their own basic blocks
/// The intention is to make it to try and recreate a reasonable approximation
/// of the control flow in the result C-ish program without having to resort
/// to gotos everwhere
class CishPreparePass : public FunctionPass {
public:
  static char ID;

public:
  CishPreparePass();

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnFunction(Function& f) override;
};

CishPreparePass::CishPreparePass() : FunctionPass(ID) {
  ;
}

StringRef CishPreparePass::getPassName() const {
  return "CISH Prepare Pass";
}

void CishPreparePass::getAnalysisUsage(AnalysisUsage& AU) const {
  ;
}

bool CishPreparePass::runOnFunction(Function& f) {
  bool changed = false;

  std::vector<BranchInst*> brs;
  for(llvm::Instruction& inst : llvm::instructions(f))
    if(auto* br = dyn_cast<BranchInst>(&inst))
      brs.push_back(br);

  for(BranchInst* br : brs) {
    BasicBlock* bb = br->getParent();
    if(bb->getInstList().size() > 1)
      bb->splitBasicBlock(br);
  }

  return changed;
}

char CishPreparePass::ID = 0;

static RegisterPass<CishPreparePass> X("cish-prepare",
                                       "Prepare code for cish",
                                       false,
                                       false);
