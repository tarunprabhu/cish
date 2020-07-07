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

#include "LLVMStructureAnalysis.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

using namespace llvm;

namespace cish {

LLVMStructureAnalysis::LLVMStructureAnalysis(const Function& f,
                                             const LoopInfo& li)
    : StructureAnalysis(f.getContext(), f.getName()), func(f), li(li) {
  ;
}

static void getBlocksInPostOrder(const BasicBlock* bb,
                                 Set<const BasicBlock*>& seen,
                                 List<const BasicBlock*>& blocks) {
  if(not seen.contains(bb)) {
    seen.insert(bb);
    for(const BasicBlock* succ : successors(bb))
      getBlocksInPostOrder(succ, seen, blocks);
    blocks.push_back(bb);
  }
}

static List<const BasicBlock*> getBlocksInPostOrder(const Function& f) {
  List<const BasicBlock*> blocks;
  Set<const BasicBlock*> seen;

  getBlocksInPostOrder(&f.getEntryBlock(), seen, blocks);

  return blocks;
}

void LLVMStructureAnalysis::buildTree() {
  message() << "Build analysis tree\n";
  List<const BasicBlock*> blocks = getBlocksInPostOrder(func);
  for(const BasicBlock* bb : blocks)
    nodeMap[bb] = &newNode<Block>(*bb);

  root.reset(new Entry(func.getContext(), getNodeFor(&func.getEntryBlock())));
  for(const BasicBlock* bb : blocks) {
    StructNode& node = getNodeFor(bb);
    const Instruction& inst = bb->back();
    if(const auto* br = dyn_cast<BranchInst>(&inst)) {
      if(br->isConditional()) {
        node.addSuccessor(ConstantInt::getTrue(llvmContext),
                          getNodeFor(br->getSuccessor(0)));
        node.addSuccessor(ConstantInt::getFalse(llvmContext),
                          getNodeFor(br->getSuccessor(1)));
      } else {
        node.addSuccessor(ConstantInt::getTrue(llvmContext),
                          getNodeFor(br->getSuccessor(0)));
      }
    } else if(const auto* sw = dyn_cast<SwitchInst>(&inst)) {
      for(const auto& i : sw->cases()) {
        const ConstantInt* kase = i.getCaseValue();
        const BasicBlock* succ = i.getCaseSuccessor();
        node.addSuccessor(kase, getNodeFor(succ));
      }
      if(BasicBlock* def = sw->getDefaultDest())
        node.addSuccessor(nullptr, getNodeFor(def));
    } else if(not(isa<ReturnInst>(inst) or isa<UnreachableInst>(inst))) {
      fatal(error() << "Unexpected instruction in CFG construction: "
                    << bb->back());
    }
  }
  log("init");
}

StructureAnalysis::Result LLVMStructureAnalysis::calculate() {
  Map<const Loop*, Set<StructNode*>> loopExitBlocks;

  buildTree();

  for(const Loop* loop : LLVM::collectLoops(li)) {
    SmallVector<Loop::Edge, 4> edges;
    loop->getExitEdges(edges);
    for(const Loop::Edge& edge : edges) {
      const BasicBlock* inside = edge.first;

      // The exiting blocks should only contain a single branch instruction
      if(inside->size() == 1)
        loopExitingBlocks.insert(&getNodeFor(inside));

      // There are cases where the exit block of a loop may be empty.
      // So a loop may "effectively" have a unique exit block if one looks past
      // the empty blocks, but LLVM will still report it as not having a unique
      // exit block. So don't rely on LLVM and try to be a little cleverer
      // about it
      const BasicBlock* outside = edge.second;
      while((outside->size() == 1) and outside->getSingleSuccessor())
        outside = outside->getSingleSuccessor();
      loopExitBlocks[loop].insert(&getNodeFor(outside));
    }
    loopHeaders[&getNodeFor(loop->getHeader())] = loop->getLoopDepth();
  }


  message() << "Optimize analysis tree\n";
  stripEmptyBlocks();

  // Do these first because it is guaranteed that everything in the tree is
  // still a Block. For loops with a unique exit block, the conditional
  // breaks out of them can be replaced with an IfThenBreak node. This makes
  // it more likely that the loop can be reduced even if it has multiple exits
  message() << "  Identify loop exit blocks\n";
  for(const BasicBlock* bb : getBlocksInPostOrder(func)) {
    if(loopExitingBlocks.contains(&getNodeFor(bb))) {
      const Loop& loop = *li.getLoopFor(bb);
      if(loopExitBlocks[&loop].size() == 1) {
        StructNode& old = getNodeFor(bb);

        // Start by assuming that the loop is exited when the condition is true.
        // If that turns out not to be the case, swap the exit and the
        // successor and mark the IfThenBreak node as having to invert the
        // condition. This is because the loop must always be exited when the
        // condition is true
        StructNode* exit = &old.getSuccessor(ConstantInt::getTrue(llvmContext));
        StructNode* cont
            = &old.getSuccessor(ConstantInt::getFalse(llvmContext));
        bool invert = false;
        if(auto* block = dyn_cast<Block>(exit)) {
          if(loop.contains(&block->getLLVM())) {
            std::swap(exit, cont);
            invert = true;
          }
        } else if(auto* ift = dyn_cast<IfThenBreak>(exit)) {
          if(loop.contains(ift->getLLVMBranchInst().getParent())) {
            std::swap(exit, cont);
            invert = true;
          }
        } else {
          fatal(
              error() << "Expected successor to be a block or if-then-break\n");
        }

        Vector<std::pair<const ConstantInt*, StructNode*>> preds
            = old.getIncoming();
        IfThenBreak& neew
            = newNode<IfThenBreak>(*exit,
                                   cast<BranchInst>(bb->back()),
                                   invert,
                                   loopExitBlocks.contains(&loop));

        old.disconnect();
        for(const auto& i : preds) {
          const ConstantInt* kase = i.first;
          StructNode& pred = *i.second;
          pred.addSuccessor(kase, neew);
        }
        neew.addSuccessor(ConstantInt::getTrue(llvmContext), *exit);
        neew.addSuccessor(ConstantInt::getFalse(llvmContext), *cont);
      }
    }
  }

  // Specialize nodes
  specializeLoopHeaders();
  log("specialized");

  return StructureAnalysis::calculate();
}

} // namespace cish
