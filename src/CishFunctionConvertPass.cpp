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

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Pass.h>

#include "CishContext.h"
#include "Diagnostics.h"
#include "LLVMBackend.h"
#include "LLVMFrontend.h"
#include "Set.h"
#include "StructureAnalysis.h"
#include "Vector.h"

using namespace llvm;

namespace cish {

class WalkerBase {
protected:
  LLVMFrontend& fe;
  LLVMBackend& be;
  const Function& func;

protected:
  WalkerBase(LLVMFrontend& fe, LLVMBackend& be, const Function& func)
      : fe(fe), be(be), func(func) {
    ;
  }

  virtual void handle(const Instruction& inst) = 0;

public:
  WalkerBase(const WalkerBase&) = delete;
  WalkerBase(WalkerBase&&) = delete;
  virtual ~WalkerBase() = default;

  virtual void walk(const StructNode*) = 0;
};

class WalkStructured : public WalkerBase {
protected:
  virtual void walk(const Block& block);
  virtual void walk(const Sequence& seq);
  virtual void walk(const Label& label);
  virtual void walk(const IfThen& ift);
  virtual void walk(const IfThenElse& ifte);
  virtual void walk(const IfThenBreak& iftb);
  virtual void walk(const IfThenGoto& iftg);
  virtual void walk(const LoopHeader& header);
  virtual void walk(const EndlessLoop& loop);
  virtual void walk(const SimplifiedLoop& loop);
  virtual void walk(const Switch& sw);
  virtual void walk(const StructNode& node);

  virtual void handle(const Instruction& inst) override;

public:
  WalkStructured(LLVMFrontend& fe, LLVMBackend& be, const Function& func);
  WalkStructured(const WalkStructured&) = delete;
  WalkStructured(WalkStructured&&) = delete;
  virtual ~WalkStructured() = default;

  virtual void walk(const StructNode* node) override;
};

WalkStructured::WalkStructured(LLVMFrontend& fe,
                               LLVMBackend& be,
                               const Function& func)
    : WalkerBase(fe, be, func) {
  ;
}

void WalkStructured::handle(const Instruction& inst) {
  // Because this will only get called when a complete structure could be
  // computed. all branching will be handled by the structure nodes, so
  // ignore any branch instruction. But the conditions of the branches still
  // need to be handled because the expression there should be converted
  //
  if(not fe.isIgnoreValue(inst)) {
    if(isa<AllocaInst>(inst) or isa<CallInst>(inst) or isa<InvokeInst>(inst)
       or isa<StoreInst>(inst) or isa<ReturnInst>(inst)) {
      fe.handle(static_cast<const Value&>(inst));
    } else if(const auto* swtch = dyn_cast<SwitchInst>(&inst)) {
      fe.handle(swtch->getCondition());
      for(const auto& i : swtch->cases())
        fe.handle(i.getCaseValue());
    } else if(const auto* br = dyn_cast<BranchInst>(&inst)) {
      if(br->isConditional())
        fe.handle(br->getCondition());
    }
  }
}

void WalkStructured::walk(const Block& block) {
  for(const Instruction& inst : block.getLLVM())
    handle(inst);
}

void WalkStructured::walk(const Sequence& seq) {
  for(const StructNode& next : seq)
    walk(next);
}

void WalkStructured::walk(const IfThen& ift) {
  walk(ift.getCond());
  be.beginBlock();
  walk(ift.getThen());
  be.endBlock();

  be.addIfThen(ift.getLLVMBranchInst(), ift.isInverted());
}

void WalkStructured::walk(const IfThenElse& ifte) {
  walk(ifte.getCond());

  be.beginBlock();
  walk(ifte.getThen());
  be.endBlock();

  be.beginBlock();
  walk(ifte.getElse());
  be.endBlock();

  be.addIfThenElse(ifte.getLLVMBranchInst());
}

void WalkStructured::walk(const IfThenBreak& iftb) {
  const BranchInst& br = iftb.getLLVMBranchInst();
  handle(br);
  be.addIfThenBreak(br);
}

void WalkStructured::walk(const IfThenGoto& iftg) {
  walk(iftg.getBlock());

  be.addIfThenGoto(
      iftg.getTarget().getName(), iftg.getLLVMBranchInst(), iftg.isInverted());
}

void WalkStructured::walk(const Label& label) {
  be.addLabel(label.getName(), func);
}

void WalkStructured::walk(const Switch& swtch) {
  const SwitchInst& sw = swtch.getLLVM();
  handle(sw);
  be.addSwitchStmt(sw);

  for(const Switch::Case& kase : swtch.getCases()) {
    be.beginBlock();
    if(not kase.isEmpty())
      walk(kase.getBody());
    if(not kase.isFallthrough())
      be.addBreak();
    be.endBlock();
    be.addSwitchCase(kase.getValue());
  }
  if(swtch.hasDefault()) {
    be.beginBlock();
    walk(swtch.getDefault());
    be.addBreak();
    be.endBlock();
    be.addSwitchDefault();
  }
}

void WalkStructured::walk(const LoopHeader&) {
  // Loop headers are empty by design
}

void WalkStructured::walk(const EndlessLoop& loop) {
  be.beginBlock();
  for(const StructNode& node : loop)
    walk(node);
  be.endBlock();

  be.addEndlessLoop();
}

void WalkStructured::walk(const SimplifiedLoop& loop) {
  be.beginBlock();
  for(const StructNode& node : loop)
    walk(node);
  be.endBlock();

  be.addEndlessLoop();
}

void WalkStructured::walk(const StructNode* node) {
  if(const auto* block = dyn_cast<Block>(node))
    walk(*block);
  else if(const auto* seq = dyn_cast<Sequence>(node))
    walk(*seq);
  else if(const auto* label = dyn_cast<Label>(node))
    walk(*label);
  else if(const auto* ift = dyn_cast<IfThen>(node))
    walk(*ift);
  else if(const auto* ifte = dyn_cast<IfThenElse>(node))
    walk(*ifte);
  else if(const auto* iftb = dyn_cast<IfThenBreak>(node))
    walk(*iftb);
  else if(const auto* iftg = dyn_cast<IfThenGoto>(node))
    walk(*iftg);
  else if(const auto* endless = dyn_cast<EndlessLoop>(node))
    walk(*endless);
  else if(const auto* natural = dyn_cast<SimplifiedLoop>(node))
    walk(*natural);
  else if(const auto* header = dyn_cast<LoopHeader>(node))
    walk(*header);
  else if(const auto* sw = dyn_cast<Switch>(node))
    walk(*sw);
  else
    fatal(error() << "Unknown structure node: " << node->getKindName());
}

void WalkStructured::walk(const StructNode& node) {
  walk(&node);
}

class WalkSemiStructured : public WalkStructured {
private:
  Map<const StructNode*, std::string> nodeNames;

private:
  void setNodeNames(const StructNode* node);

protected:
  Set<const StructNode*> seen;

protected:
  bool isUnstructured(const StructNode& node) const;
  virtual void walk(const IfThenBreak& iftb) override;
  virtual void walk(const Block& block) override;
  virtual void walk(const StructNode& node) override;

public:
  WalkSemiStructured(LLVMFrontend& fe, LLVMBackend& be, const Function& f);
  WalkSemiStructured(const WalkSemiStructured&) = delete;
  WalkSemiStructured(WalkSemiStructured&&) = delete;
  virtual ~WalkSemiStructured() = default;

  virtual void walk(const StructNode* node) override;
};

WalkSemiStructured::WalkSemiStructured(LLVMFrontend& fe,
                                       LLVMBackend& be,
                                       const Function& func)
    : WalkStructured(fe, be, func) {
  ;
}

void WalkSemiStructured::setNodeNames(const StructNode* node) {
  if(not nodeNames.contains(node)) {
    nodeNames[node] = be.getNewVar("bb");
    for(const StructNode& succ : node->successors())
      setNodeNames(&succ);
  }
}

bool WalkSemiStructured::isUnstructured(const StructNode& node) const {
  return node.getNumPredecessors() or node.getNumSuccessors();
}

void WalkSemiStructured::walk(const Block& block) {
  WalkStructured::walk(block);

  if(isUnstructured(block) and (block.getNumSuccessors() > 1)) {
    const BasicBlock& bb = block.getLLVM();
    if(const auto* swtch = dyn_cast<SwitchInst>(&bb.back())) {
      be.addSwitchStmt(*swtch);
      for(const auto& i : swtch->cases()) {
        be.beginBlock();
        be.addGoto(nodeNames.at(&block.getSuccessor(i.getCaseValue())), func);
        be.endBlock();

        be.addSwitchCase(*i.getCaseValue());
      };
      if(const BasicBlock* deflt = swtch->getDefaultDest()) {
        be.beginBlock();
        be.addGoto(nodeNames.at(&block.getSuccessor(nullptr)), func);
        be.endBlock();

        be.addSwitchDefault();
      }
    } else if(const auto* br = dyn_cast<BranchInst>(&bb.back())) {
      LLVMContext& llvmContext = func.getContext();
      be.beginBlock();
      be.addGoto(
          nodeNames.at(&block.getSuccessor(ConstantInt::getTrue(llvmContext))),
          func);
      be.endBlock();

      be.beginBlock();
      be.addGoto(
          nodeNames.at(&block.getSuccessor(ConstantInt::getFalse(llvmContext))),
          func);
      be.endBlock();

      be.addIfThenElse(*br);
    } else {
      fatal(error()
            << "Unexpected terminator instruction in semi structured block: "
            << bb.back());
    }
  }
}

void WalkSemiStructured::walk(const IfThenBreak& iftb) {
  if(isUnstructured(iftb)) {
    const auto& br = iftb.getLLVMBranchInst();
    handle(br);

    be.beginBlock();
    be.addGoto(nodeNames.at(&iftb.getExit()), func);
    be.endBlock();

    be.beginBlock();
    be.addGoto(nodeNames.at(&iftb.getContinue()), func);
    be.endBlock();

    be.addIfThenElse(br);
  } else {
    WalkStructured::walk(iftb);
  }
}

void WalkSemiStructured::walk(const StructNode& node) {
  walk(&node);
}

void WalkSemiStructured::walk(const StructNode* node) {
  if(seen.contains(node))
    return;
  if(nodeNames.empty())
    setNodeNames(node);

  // In the semi-structured case, the nodes may have a predecessor and
  // successor. Any disconnected nodes are structured
  if(isUnstructured(*node))
    be.addLabel(nodeNames.at(node), func);

  WalkStructured::walk(node);
  seen.insert(node);

  if(isUnstructured(*node)) {
    if(node->getNumSuccessors() == 1) {
      be.addGoto(nodeNames.at(&node->getSuccessor()), func);
    }
  }

  for(StructNode& succ : node->successors())
    walk(succ);
}

class WalkUnstructured : public WalkerBase {
protected:
  virtual void handle(const Instruction&) override;

public:
  WalkUnstructured(LLVMFrontend& fe, LLVMBackend& be, const Function& f);
  WalkUnstructured(const WalkUnstructured&) = delete;
  WalkUnstructured(WalkUnstructured&&) = delete;
  virtual ~WalkUnstructured() = default;

  virtual void walk(const StructNode*) override;
};

WalkUnstructured::WalkUnstructured(LLVMFrontend& fe,
                                   LLVMBackend& be,
                                   const Function& func)
    : WalkerBase(fe, be, func) {
  ;
}

void WalkUnstructured::handle(const Instruction& inst) {
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
  if(not fe.isIgnoreValue(inst)) {
    if(isa<AllocaInst>(inst) or isa<CallInst>(inst) or isa<InvokeInst>(inst)
       or isa<StoreInst>(inst) or isa<SwitchInst>(inst) or isa<BranchInst>(inst)
       or isa<ReturnInst>(inst))
      fe.handle(static_cast<const Value&>(inst));
  }
}

void WalkUnstructured::walk(const StructNode*) {
  for(const BasicBlock& bb : func) {
    be.beginBlock(bb);
    for(const Instruction& inst : bb)
      handle(inst);
    be.endBlock(bb);
  }
}

} // namespace cish

class CishFunctionConvertPass : public FunctionPass {
public:
  static char ID;

public:
  CishFunctionConvertPass() : FunctionPass(ID) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish Function Pass";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.addRequired<CishContextWrapperPass>();
    AU.addRequired<StructureAnalysisWrapperPass>();
    AU.setPreservesAll();
  }

  virtual bool runOnFunction(Function& f) override {
    cish::message() << "Running " << getPassName() << " on " << f.getName()
                    << "\n";

    const cish::CishContext& context
        = getAnalysis<CishContextWrapperPass>().getCishContext();
    cish::LLVMFrontend& fe = context.getLLVMFrontend();
    cish::LLVMBackend& be = context.getLLVMBackend();

    const auto& analysis = getAnalysis<StructureAnalysisWrapperPass>();

    be.beginFunction(f);

    switch(analysis.getStructureKind()) {
    case cish::StructureKind::Unstructured:
      cish::WalkUnstructured(fe, be, f).walk(nullptr);
      break;
    case cish::StructureKind::SemiStructured:
      cish::WalkSemiStructured(fe, be, f).walk(&analysis.getStructured());
      break;
    case cish::StructureKind::PerfectlyStructured:
    case cish::StructureKind::Structured:
      cish::WalkStructured(fe, be, f).walk(&analysis.getStructured());
      break;
    }

    be.endFunction(f);

    return false;
  }
};

char CishFunctionConvertPass::ID = 0;

static RegisterPass<CishFunctionConvertPass>
    X("cish-functions", "Cish Function Pass", true, true);

Pass* createCishFunctionConvertPass() {
  return new CishFunctionConvertPass();
}
