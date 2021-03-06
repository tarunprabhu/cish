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

#include "StructureAnalysis.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "Logging.h"
#include "Map.h"
#include "Options.h"
#include "StructureTree.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

StructureAnalysis::StructureAnalysis(LLVMContext& llvmContext,
                                     const std::string& func)
    : llvmContext(llvmContext), func(func), hasGoto(false), reductions(0),
      labelId(0) {
  ;
}

const StructNode& StructureAnalysis::getStructured() const {
  return root->getEntry();
}

void StructureAnalysis::log(const std::string& tag) {
  if(not opts().has(LogCategory::Structure))
    return;

  if(Logger log = tag.length() ? Logger::openFile(func, tag, "dot")
                               : Logger::openFile(func, reductions, "dot")) {
    log() << "digraph {\n";

    const StructNode& entry = root->getEntry();
    log() << "  " << entry.getId() << " [label=\"" << entry.getKindName() << "("
          << entry.getId() << ")\"]\n";
    Set<const StructNode*> seen = {&root->getEntry()};
    List<const StructNode*> wl = {&root->getEntry()};
    do {
      List<const StructNode*> next;
      for(const StructNode* node : wl) {
        for(const auto& edge : node->outgoing()) {
          const ConstantInt* kase = edge.first;
          const StructNode& child = *edge.second;
          uint64_t childId = child.getId();
          log() << "  " << node->getId() << " -> " << childId << " [label="
                << (kase ? std::to_string(kase->getLimitedValue())
                         : std::string("default"))
                << "]\n";
          if(not seen.contains(&child)) {
            log() << "  " << childId << " [label=\"" << child.getKindName()
                  << "(" << childId << ")";
            if(const auto* block = dyn_cast<Block>(&child))
              log() << " |" << block->getLLVM().size() << "|";
            log() << "\"]\n";
            seen.insert(&child);
            next.push_back(&child);
          }
        }
      }
      wl = std::move(next);
    } while(wl.size());

    log() << "}";
  }
}

void StructureAnalysis::dfs(StructNode& node,
                            Set<StructNode*>& seen,
                            List<StructNode*>& postorder) const {
  if(not seen.contains(&node)) {
    seen.insert(&node);
    for(StructNode& succ : node.successors())
      dfs(succ, seen, postorder);
    postorder.push_back(&node);
  }
}

List<StructNode*> StructureAnalysis::dfs() const {
  List<StructNode*> postorder;
  Set<StructNode*> seen;

  dfs(root->getEntry(), seen, postorder);

  return postorder;
}

static bool isReducibleSESEBlock(const StructNode& node) {
  // TODO: Do something slightly more clever than simply checking
  // for a single predecessor and successor. IfThenBreak nodes from loops with
  // a unique exit node can also be treated as effectively having only a single
  // entry and single exit because the break statement is guaranteed to jump
  // to the right place. Something like this perhaps
  //
  // if(const auto* iftb = dyn_cast<IfThenBreak>(&node)) {
  //   if(iftb->isInLoopWithUniqueExit() and (iftb->getNumPredecessors() == 1))
  //     return true;
  // }
  //
  if((node.getNumSuccessors() <= 1) and (node.getNumPredecessors() == 1)) {
    return true;
  }
  return false;
}

bool StructureAnalysis::reduceSequence(const List<StructNode*>& nodes) {
  StructNode& head = *nodes.front();
  StructNode& tail = *nodes.back();
  StructNode::Edges headEdges = head.getIncoming();
  StructNode::Edges tailEdges = tail.getOutgoing();
  for(StructNode* node : nodes)
    node->disconnect();

  Sequence& seq = newNode<Sequence>(nodes);
  for(const auto& i : headEdges) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, seq);
  }
  for(const auto& i : tailEdges) {
    const ConstantInt* kase = i.first;
    StructNode& succ = *i.second;
    seq.addSuccessor(kase, succ);
  }

  message() << ++reductions << ": Reducing Sequence |" << nodes.size()
            << "| => " << seq.getId() << "\n";
  return true;
}

bool StructureAnalysis::tryReduceSequence(const List<StructNode*>& postorder) {
  //
  // node-->--node-->--node
  //
  // All nodes in the sequence must have exactly one predecessor and exactly one
  // successor
  for(StructNode* node : postorder) {
    List<StructNode*> seq;
    StructNode* curr = node;
    while(isReducibleSESEBlock(*curr)) {
      seq.push_front(curr);
      curr = &curr->getPredecessor();
    }

    if(seq.size() > 1)
      return reduceSequence(seq);
  }

  return false;
}

bool StructureAnalysis::reduceIfThen(Block& cond,
                                     StructNode& thn,
                                     StructNode& succ) {
  bool invert = cond.getSuccessorCase(thn)->isZero();
  Vector<std::pair<const ConstantInt*, StructNode*>> preds = cond.getIncoming();

  cond.disconnect();
  thn.disconnect();
  succ.removePredecessor(cond);
  succ.removePredecessor(thn);

  IfThen& iff = newNode<IfThen>(cond, thn, invert);
  for(auto i : preds) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, iff);
  }
  iff.addSuccessor(ConstantInt::getTrue(llvmContext), succ);

  message() << ++reductions << ": Reducing IfThen => " << iff.getId() << "\n";
  return true;
}

bool StructureAnalysis::tryReduceIfThen(const List<StructNode*>& postorder) {
  //
  // cond --> then --> node ->
  //  |                  |
  //  ------>-------->----
  //
  // The "then" branch must contain exactly one node
  //
  for(StructNode* node : postorder) {
    unsigned n = node->getNumPredecessors();
    if(n >= 2) {
      for(unsigned i = 0; i < n; i++) {
        for(unsigned j = i + 1; j < n; j++) {
          StructNode& pi = node->getPredecessorAt(i);
          StructNode& pj = node->getPredecessorAt(j);

          // From the post order, it's not clear which path contains the "then"
          if(isReducibleSESEBlock(pi) and (pi.getPredecessor() == pj)) {
            // pi is the "then" block and pj is the condition block
            if(auto* cond = dyn_cast<Block>(&pj))
              if(not isa<SwitchInst>(cond->getLLVM().back()))
                return reduceIfThen(*cond, pi, *node);
          } else if(isReducibleSESEBlock(pj) and (pj.getPredecessor() == pi)) {
            // pj is the "then" block. Do the same as for pi
            if(auto* cond = dyn_cast<Block>(&pi))
              if(not isa<SwitchInst>(cond->getLLVM().back()))
                return reduceIfThen(*cond, pj, *node);
          }
        }
      }
    }
  }

  return false;
}

bool StructureAnalysis::reduceIfThenElse(Block& cond,
                                         StructNode& pi,
                                         StructNode& pj,
                                         StructNode& succ) {
  StructNode& thn = (cond.getSuccessorCase(pi)->isOne()) ? pi : pj;
  StructNode& els = (thn == pi) ? pj : pi;
  Vector<std::pair<const ConstantInt*, StructNode*>> preds = cond.getIncoming();

  cond.disconnect();
  thn.disconnect();
  els.disconnect();
  succ.removePredecessor(thn);
  succ.removePredecessor(els);

  IfThenElse& iff = newNode<IfThenElse>(cond, thn, els);
  for(auto i : preds) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, iff);
  }
  iff.addSuccessor(ConstantInt::getTrue(llvmContext), succ);

  message() << ++reductions << ": Reducing IfThenElse => " << iff.getId()
            << "\n";
  return true;
}

bool StructureAnalysis::tryReduceIfThenElse(
    const List<StructNode*>& postorder) {
  //
  // cond -> then -> node ->
  //  |                |
  //  --> -> else -> ---
  //
  // Both branches must contain exactly one node each and each of them must
  // have exactly one predecessor and successor each. The predecessor on
  // each branch must be the condition node and the successor must be the
  // current node
  //
  for(StructNode* node : postorder) {
    size_t n = node->getNumPredecessors();
    if(n >= 2) {
      for(unsigned i = 0; i < n - 1; i++) {
        for(unsigned j = i + 1; j < n; j++) {
          StructNode& pi = node->getPredecessorAt(i);
          StructNode& pj = node->getPredecessorAt(j);
          if(isReducibleSESEBlock(pi) and isReducibleSESEBlock(pj)
             and (pi.getPredecessor() == pj.getPredecessor()))
            if(auto* cond = dyn_cast<Block>(&pi.getPredecessor()))
              if(not isa<SwitchInst>(cond->getLLVM().back()))
                return reduceIfThenElse(*cond, pi, pj, *node);
        }
      }
    }
  }

  return false;
}

bool StructureAnalysis::reduceIfThenGoto(StructNode& dest) {
  // Add a label node as the new target of the IfThenGotos
  Vector<std::pair<const ConstantInt*, StructNode*>> preds = dest.getIncoming();
  Label& label = newNode<Label>("label_" + std::to_string(labelId++));
  for(const auto& i : preds) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    pred.removeSuccessor(dest);
    pred.addSuccessor(kase, label);
  }
  label.addSuccessor(ConstantInt::getTrue(llvmContext), dest);

  for(const auto& i : preds) {
    StructNode& pred = *i.second;
    if(pred.getNumSuccessors() == 2) {
      const ConstantInt* kase = i.first;
      const ConstantInt* invKase = kase->isZero()
                                       ? ConstantInt::getTrue(llvmContext)
                                       : ConstantInt::getFalse(llvmContext);
      auto* block = cast<Block>(&pred);

      IfThenGoto& ifg = newNode<IfThenGoto>(label, *block, kase->isZero());
      Vector<std::pair<const ConstantInt*, StructNode*>> preds
          = block->getIncoming();
      StructNode& succ = block->getSuccessor(invKase);

      block->disconnect();
      for(const auto& j : preds) {
        const ConstantInt* kase = j.first;
        StructNode& pred = *j.second;
        pred.addSuccessor(kase, ifg);
      }
      ifg.addSuccessor(invKase, succ);
    }
  }

  message() << ++reductions << ": Reducing to IfThenGoto with target "
            << dest.getId() << "\n";
  hasGoto = true;

  return true;
}

bool StructureAnalysis::tryReduceIfThenGoto(
    const List<StructNode*>& postorder) {
  // This will be called last when there is nothing else that can be done.
  // It's basically a Hail Mary hoping that adding the goto's will help
  // uncover something else

  // Add the goto to the node with the largest indegree. Hopefully this will
  // minimize the number of gotos but who knows
  Vector<StructNode*> sorted(postorder.begin(), postorder.end());
  std::sort(sorted.begin(),
            sorted.end(),
            [](const StructNode* a, const StructNode* b) {
              return a->getNumPredecessors() < b->getNumPredecessors();
            });
  std::reverse(sorted.begin(), sorted.end());

  for(StructNode* node : sorted) {
    // Of the predecessors of the node, at least one must be a block with
    // more than one successor and that predecessor cannot be a switch
    if(node->getNumPredecessors() >= 2) {
      bool candidate = not isa<Label>(node);
      unsigned blocks = 0;
      if(candidate) {
        for(StructNode& pred : node->predecessors()) {
          if(isa<Block>(pred) and (pred.getNumSuccessors() == 2))
            blocks += 1;
          else if(pred.getNumSuccessors() != 1)
            candidate = false;
        }
      }
      if(candidate and (blocks > 1))
        return reduceIfThenGoto(*node);
    }
  }

  return false;
}

bool StructureAnalysis::reduceEndlessLoop(StructNode& node) {
  StructNode& header = [&]() -> StructNode& {
    if(node.hasSuccessor(node))
      // If this is a single node loop, the header is itself
      return node;
    else if(node.getNumPredecessors() > 1)
      // If the header has more than one predecessor, then one of the
      // predecessors is a backedge and the rest are from outside the loop
      return node;
    else if(node.getSuccessor().getNumPredecessors() > 1)
      return node.getSuccessor();
    else
      fatal(error() << "Could not determine header for endless loop");
  }();

  EndlessLoop& loop = newNode<EndlessLoop>(header);
  Map<const ConstantInt*, StructNode*> preds;
  for(const auto& i : header.getIncoming()) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    if(not loop.isInLoop(pred))
      preds[kase] = &pred;
  }

  for(StructNode& node : loop)
    node.disconnect();
  for(auto i : preds)
    i.second->addSuccessor(i.first, loop);

  message() << ++reductions << ": Reducing endless loop => " << loop.getId()
            << "\n";
  ;
  return true;
}

bool StructureAnalysis::tryReduceEndlessLoop(
    const List<StructNode*>& postorder) {
  for(StructNode* node : postorder) {
    if(node->getNumSuccessors() == 1) {
      // node-->--
      //   |      |
      //    --<---
      if(node->getSuccessor() == *node)
        return reduceEndlessLoop(*node);

      // node-->--succ
      //   |       |
      //    ---<---
      StructNode& succ = node->getSuccessor();
      if((succ.getNumSuccessors() == 1) and (succ.getSuccessor() == *node))
        return reduceEndlessLoop(*node);
    }
  }

  return false;
}

bool StructureAnalysis::reduceSimplifiedLoop(LoopHeader& header,
                                             const List<IfThenBreak*>& exits) {
  Vector<std::pair<const ConstantInt*, StructNode*>> preds
      = header.getIncoming();
  Vector<StructNode*> succs;
  for(IfThenBreak* exit : exits) {
    StructNode& succ = exit->getExit();
    succs.push_back(&succ);
    succ.removePredecessor(*exit);
  }

  // The node should be made before disconnecting everything because we need
  // the connections to correctly construct the sequence of nodes in the loop
  // body
  SimplifiedLoop& loop = newNode<SimplifiedLoop>(header, exits);
  header.disconnect();
  for(IfThenBreak* exit : exits)
    exit->disconnect();

  for(auto& i : preds) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    if(not loop.isInLoop(pred))
      pred.addSuccessor(kase, loop);
  }
  for(StructNode* succ : succs)
    loop.addSuccessor(ConstantInt::getTrue(llvmContext), *succ);

  message() << ++reductions << ": Reducing Simplified Loop => " << loop.getId()
            << "\n";
  return true;
}

bool StructureAnalysis::tryReduceSimplifiedLoop(
    const List<StructNode*>& postorder) {
  // Find deepest loop
  LoopHeader* header = nullptr;
  for(StructNode* node : postorder) {
    if(auto* h = dyn_cast<LoopHeader>(node)) {
      if(not header)
        header = h;
      else if(h->getLoopDepth() > header->getLoopDepth())
        header = h;
    }
  }
  if(not header)
    return false;

  // Only IfThenBreaks can appear successively in the loop.
  // Any two non-IfThenBreak nodes must be separated by at least one IfThenBreak
  // All non-IfThenBreak nodes must have exactly one  successor. If not, it
  // may be a backedge
  List<IfThenBreak*> exits;
  StructNode* curr = &header->getSuccessor();
  StructNode* prev = header;
  do {
    if(auto* iftb = dyn_cast<IfThenBreak>(curr)) {
      // If the current node is an IfThenBreak, doesn't matter what the previous
      // one was
      prev = curr;
      curr = &iftb->getContinue();
      exits.push_back(iftb);
    } else if(not isa<LoopHeader>(prev) and not isa<IfThenBreak>(prev)) {
      // If the previous node was not the header and it was not an IfThenBreak,
      // then this is not a natural loop
      return false;
    } else if(curr->getNumSuccessors() != 1) {
      // The current node is not an IfThenBreak, and the previous node was
      // either the loop header or an IfThenBreak. If this has more than one
      // successor, one of the successors may be a backedge or else the loop
      // is not yet reduced. In either case, we cannot reduce it
      return false;
    } else {
      prev = curr;
      curr = &curr->getSuccessor();
    }
  } while(curr != header);

  // The loop may have multiple exiting blocks but it should have a dedicated
  // exit block
  if(exits.size()) {
    StructNode& exit = exits.front()->getExit();
    for(const IfThenBreak* iftb : exits)
      if(exit != iftb->getExit())
        return false;
    return reduceSimplifiedLoop(*header, exits);
  }

  return false;
}

bool StructureAnalysis::reduceSwitch(StructNode::Edges edges,
                                     Block& block,
                                     StructNode& succ) {
  Vector<std::pair<const ConstantInt*, StructNode*>> preds
      = block.getIncoming();
  const auto& switchInst = cast<SwitchInst>(block.getLLVM().back());
  for(StructNode::Edge& edge : edges) {
    StructNode& node = *edge.second;
    if(node != succ)
      node.disconnect();
  }
  block.disconnect();

  Vector<Switch::Case> cases;
  StructNode* deflt = nullptr;
  for(const auto& i : edges) {
    const ConstantInt* value = i.first;
    StructNode* dest = i.second;
    if(value)
      if(*dest != succ)
        cases.emplace_back(*value, dest, false);
      else
        cases.emplace_back(*value, nullptr, false);
    else if(*dest != succ)
      deflt = dest;
  }

  Switch& sw = newNode<Switch>(switchInst, cases, deflt);
  for(const auto& i : preds) {
    const ConstantInt* kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, sw);
  }
  sw.addSuccessor(ConstantInt::getTrue(llvmContext), succ);

  message() << ++reductions << ": Reducing Switch |" << edges.size() << "| => "
            << sw.getId() << "\n";

  return true;
}

static bool isSwitchStrict(const Block& cond, const StructNode& succ) {
  for(StructNode& kase : cond.successors())
    if(not((kase == succ)
           or ((kase.getNumSuccessors() == 1)
               and (kase.getNumPredecessors() == 1)
               and (kase.getSuccessor() == succ))))
      return false;
  return true;
}

static bool isSwitchFallthrough(const Block&, const StructNode&) {
  // FIXME: Implement this
  return false;
}

bool StructureAnalysis::tryReduceSwitch(const List<StructNode*>& postorder) {
  for(StructNode* i : postorder) {
    if(auto* block = dyn_cast<Block>(i)) {
      if(const auto* sw = dyn_cast<SwitchInst>(&block->getLLVM().back())) {
        // Because the nodes are visited in postorder, the successor must be
        // before this the postorder list
        for(StructNode* j : postorder) {
          if(j == i)
            break;
          if(isSwitchStrict(*block, *j))
            return reduceSwitch(block->getOutgoing(), *block, *j);
          else if(isSwitchFallthrough(*block, *j))
            // FIXME: Support switches with fallthroughs
            return false;
        }
      }
    }
  }
  return false;
}

bool StructureAnalysis::tryReduce(const List<StructNode*>& postorder) {
  // Short-circuits. The order in which each is tried matters to an extent
  return (tryReduceEndlessLoop(postorder) || tryReduceSimplifiedLoop(postorder)
          || tryReduceSequence(postorder) || tryReduceSwitch(postorder)
          || tryReduceIfThen(postorder) || tryReduceIfThenElse(postorder)
          || tryReduceIfThenGoto(postorder));
}

void StructureAnalysis::stripEmptyBlocks() {
  message() << "  Strip empty blocks\n";

  auto shouldStrip = [&](StructNode* node) {
    if(auto* block = dyn_cast<Block>(node))
      return (block->getLLVM().size() == 1)
             and (not(loopHeaders.contains(node)
                      or loopExitingBlocks.contains(node)))
             and (node->getNumSuccessors() == 1);
    return false;
  };

  // Get rid of any empty blocks but don't get rid of anything that might
  // be specialized
  bool changed = false;
  do {
    changed = false;
    for(StructNode* node : dfs()) {
      if(shouldStrip(node)) {
        StructNode::Edges headEdges = node->getIncoming();
        StructNode& succ = node->getSuccessor();
        if(not std::any_of(
               headEdges.begin(),
               headEdges.end(),
               [&](const StructNode::Edge& e) { return *e.second == *node; })) {
          node->disconnect();
          for(const StructNode::Edge& e : headEdges) {
            const ConstantInt* kase = e.first;
            StructNode& pred = *e.second;
            pred.addSuccessor(kase, succ);
            changed |= true;
          }
        }
      }
    }
  } while(changed);

  log("stripped");
}

void StructureAnalysis::specializeLoopHeaders() {
  message() << "  Identify loop headers\n";
  Vector<StructNode*> headers(loopHeaders.keys().begin(),
                              loopHeaders.keys().end());
  for(StructNode* old : headers) {
    StructNode& neew = newNode<LoopHeader>(loopHeaders.at(old));
    Vector<std::pair<const ConstantInt*, StructNode*>> preds
        = old->getIncoming();
    StructNode::Edges succs = old->getOutgoing();

    old->disconnect();
    for(auto& i : preds) {
      const ConstantInt* kase = i.first;
      StructNode& pred = *i.second;
      pred.addSuccessor(kase, neew);
    }
    for(auto& i : succs) {
      const ConstantInt* kase = i.first;
      StructNode& succ = *i.second;
      neew.addSuccessor(kase, succ);
    }
  }
}

StructureAnalysis::Result StructureAnalysis::calculate() {
  // TODO: One case that precludes structural analysis is when loops have
  // break statements inside non-empty conditionals. For example
  //
  // for(i = 0; i < n; i++) {
  //   if(condition) {
  //     do_something();
  //     break;
  //   } else if(other_condition) {
  //     do_something_else();
  //     break;
  //   }
  //   do_work();
  // }
  //
  // In this case, the two exit blocks containing do_something() and
  // do_something_else() are outside the loop. Those blocks themselves flow
  // to the same block - so in essence, the "dedicated" exit is deferred.
  // In any case, the analysis detecting a simplified loop fails for this
  // case even though the loop looks like it ought to have a dedicated exit.
  // One workaround would be to push these exit blocks back inside the loop
  // if certain conditions are met.

  bool reduced = true;
  List<StructNode*> postorder = dfs();
  while(postorder.size() > 1 and reduced) {
    reduced = tryReduce(postorder);
    if(reduced)
      postorder = dfs();
    log();
  }

  // FIXME: Right now, if there was anything that could not be reduced in the
  // CFG, just return false. This is too extreme because there is no reason
  // why we need to resort to an all or nothing approach. For the moment,
  // this will be a known limitation that can be addressed later
  if((not reductions) and (postorder.size() != 1)) {
    message() << "No strcuture could be deduced\n";
    return Unstructured;
  } else if(postorder.size() != 1) {
    message()
        << "Structure analysis incomplete. Partial structure determined\n";
    return SemiStructured;
  } else if(hasGoto) {
    message() << "Structure analysis completed with gotos\n";
    return Structured;
  } else {
    message() << "Structure analysis successful\n";
    return PerfectlyStructured;
  }
}

} // namespace cish
