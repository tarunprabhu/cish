#include "StructureAnalysis.h"
#include "Diagnostics.h"
#include "Map.h"
#include "Structure.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/InstIterator.h>

using namespace llvm;

namespace cish {

// This a special node whose only purpose is to serve as the root of the
// control tree. This will be the sole predecessor of the StructNode
// corresponding to the function entry block. The function entry block is also
// its sole successor
class Entry : protected StructNode {
public:
  Entry(StructNode& entry) : StructNode(StructNode::S_Entry) {
    addSuccessor(true, entry);
  }
  virtual ~Entry() = default;

  StructNode& getEntry() const {
    return getSuccessor();
  }
};

class StructureAnalysis {
private:
  const LoopInfo& li;
  const DominatorTree& dt;
  const PostDominatorTree& pdt;

  unsigned labelId;
  std::unique_ptr<Entry> root;
  List<std::unique_ptr<StructNode>> nodes;
  Map<const BasicBlock*, StructNode*> nodeMap;

private:
  template <
      typename ClassT,
      typename... ArgsT,
      std::enable_if_t<std::is_base_of<StructNode, ClassT>::value, int> = 0>
  ClassT& newNode(ArgsT&&... args) {
    return cast<ClassT>(*nodes.emplace_back(new ClassT(args...)));
  }

  template <typename T = StructNode>
  T& getNodeFor(const llvm::BasicBlock& bb) const {
    return *dyn_cast<T>(nodeMap.at(&bb));
  }

  template <typename T = StructNode>
  T& getNodeFor(const llvm::BasicBlock* bb) const {
    return getNodeFor<T>(*bb);
  }

  void dfs(StructNode& node,
           Set<StructNode*>& seen,
           List<StructNode*>& postorder) const;
  List<StructNode*> dfs(StructNode& node) const;

  bool reduceEndlessLoop(StructNode& header);
  bool reduceNaturalLoop(LoopHeader& header, const List<IfThenBreak*>& exits);
  bool reduceSequence(const List<StructNode*>& nodes);
  bool reduceIfThen(Block& cond,
                    StructNode& thn,
                    StructNode& succ,
                    bool invert = false);
  bool reduceIfThenElse(Block& cond,
                        StructNode& thn,
                        StructNode& els,
                        StructNode& succ);
  bool reduceIfThenGoto(StructNode& dest);
  bool reduceSwitch();

  bool tryReduce(const List<StructNode*>& postorder);
  bool tryReduceSequence(const List<StructNode*>& postorder);
  bool tryReduceIfThen(const List<StructNode*>& postorder);
  bool tryReduceIfThenElse(const List<StructNode*>& postorder);
  bool tryReduceIfThenGoto(const List<StructNode*>& postorder);
  bool tryReduceEndlessLoop(const List<StructNode*>& postorder);
  bool tryReduceNaturalLoop(const List<StructNode*>& postorder);
  bool tryReduceSwitch(const List<StructNode*>& postorder);

public:
  StructureAnalysis(const LoopInfo& li,
                    const DominatorTree& dt,
                    const PostDominatorTree& pdt);
  StructureAnalysis(const StructureAnalysis&) = delete;
  StructureAnalysis(const StructureAnalysis&&) = delete;

  void dot(raw_ostream&) const;
  const StructNode& getStructured() const;
  bool runOnFunction(const Function&);
};

StructureAnalysis::StructureAnalysis(const LoopInfo& li,
                                     const DominatorTree& dt,
                                     const PostDominatorTree& pdt)
    : li(li), dt(dt), pdt(pdt), labelId(0) {
  ;
}

const StructNode& StructureAnalysis::getStructured() const {
  return root->getEntry();
}

void StructureAnalysis::dot(raw_ostream& os) const {
  os << "digraph {\n";

  os << "  " << (uint64_t)&root->getEntry() << " [label=\""
     << root->getEntry().getKindName() << "("
     << (unsigned long)&root->getEntry() << ")\"]\n";
  Set<const StructNode*> seen = {&root->getEntry()};
  List<const StructNode*> wl = {&root->getEntry()};
  do {
    List<const StructNode*> next;
    for(const StructNode* node : wl) {
      for(const auto& edge : node->outgoing()) {
        long kase = edge.first;
        const StructNode& child = *edge.second;
        uint64_t childId = child.getId();
        os << "  " << node->getId() << " -> " << childId << " [label=" << kase
           << "]\n";
        if(not seen.contains(&child)) {
          os << "  " << child.getId() << " [label=\"" << child.getKindName()
             << "(" << child.getId() << ")\"]\n";
          seen.insert(&child);
          next.push_back(&child);
        }
      }
    }
    wl = std::move(next);
  } while(wl.size());

  os << "}";
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

List<StructNode*> StructureAnalysis::dfs(StructNode& node) const {
  List<StructNode*> postorder;
  Set<StructNode*> seen;

  dfs(node, seen, postorder);

  return postorder;
}

bool StructureAnalysis::reduceSequence(const List<StructNode*>& nodes) {
  errs() << "Reducing Sequence at " << nodes.front()->getId() << " |"
         << nodes.size() << "|\n";
  StructNode& head = *nodes.front();
  StructNode& tail = *nodes.back();
  Vector<std::pair<long, StructNode*>> headEdges = head.getIncoming();
  Map<long, StructNode*> tailEdges = tail.getOutgoing();
  for(StructNode* node : nodes)
    node->disconnect();

  Sequence& seq = newNode<Sequence>(nodes);
  for(const auto& i : headEdges) {
    long kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, seq);
  }
  for(const auto& i : tailEdges) {
    long kase = i.first;
    StructNode& succ = *i.second;
    seq.addSuccessor(kase, succ);
  }

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
    while((curr->getNumSuccessors() <= 1)
          and (curr->getNumPredecessors() == 1)) {
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
                                     StructNode& succ,
                                     bool invert) {
  errs() << "Reducing IfThen at " << cond.getId() << "\n";
  Vector<std::pair<long, StructNode*>> preds = cond.getIncoming();
  cond.disconnect();
  thn.disconnect();
  succ.removePredecessor(cond);
  succ.removePredecessor(thn);

  IfThen& iff = newNode<IfThen>(cond, thn, invert);
  for(auto i : preds) {
    long kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, iff);
  }
  iff.addSuccessor(true, succ);

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
          if((pi.getNumPredecessors() == 1) and (pi.getPredecessor() == pj)) {
            // pi is the "then" block and pj is the condition block
            // Now, determine whether the condition should be inverted so
            // the "then" block is always encountered if the condition is true
            if(auto* cond = dyn_cast<Block>(&pj)) {
              if(cond->getSuccessorCase(pi))
                return reduceIfThen(*cond, pi, *node);
              else
                return reduceIfThen(*cond, pi, *node, true);
            }
            fatal(error() << "Condition of an IfThen must be a block");
          } else if((pj.getNumPredecessors() == 1)
                    and (pj.getPredecessor() == pi)) {
            // pj is the "then" block. Do the same as for pi
            if(auto* cond = dyn_cast<Block>(&pi)) {
              if(cond->getSuccessorCase(pj))
                return reduceIfThen(*cond, pj, *node);
              else
                return reduceIfThen(*cond, pj, *node, true);
            }
            fatal(error() << "Condition of an IfThen must be a block");
          }
        }
      }
    }
  }

  return false;
}

bool StructureAnalysis::reduceIfThenElse(Block& cond,
                                         StructNode& thn,
                                         StructNode& els,
                                         StructNode& succ) {
  errs() << "Reducing IfThenElse at " << cond.getId() << "\n";
  Vector<std::pair<long, StructNode*>> preds = cond.getIncoming();
  cond.disconnect();
  thn.disconnect();
  els.disconnect();
  succ.removePredecessor(thn);
  succ.removePredecessor(els);

  IfThenElse& iff = newNode<IfThenElse>(cond, thn, els);
  for(auto i : preds) {
    long kase = i.first;
    StructNode& pred = *i.second;
    pred.addSuccessor(kase, iff);
  }
  iff.addSuccessor(true, succ);

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
          StructNode& thn = node->getPredecessorAt(i);
          StructNode& els = node->getPredecessorAt(j);
          if((thn.getNumPredecessors() == 1) and (els.getNumPredecessors() == 1)
             and (thn.getPredecessor() == els.getPredecessor())) {
            if(auto* cond = dyn_cast<Block>(&thn.getPredecessor())) {
              if(cond->getSuccessorCase(thn))
                return reduceIfThenElse(*cond, thn, els, *node);
              else
                return reduceIfThenElse(*cond, els, thn, *node);
            }
            fatal(error() << "Condition of an IfThenElse must be a block");
          }
        }
      }
    }
  }

  return false;
}

bool StructureAnalysis::reduceIfThenGoto(StructNode& dest) {
  errs() << "Reducing to gotos with target: " << dest.getId() << "\n";

  // Add a label node as the new target of the IfThenGotos
  Vector<std::pair<long, StructNode*>> preds = dest.getIncoming();
  Label& label = newNode<Label>("label_" + std::to_string(labelId++));
  for(const auto& i : preds) {
    long kase = i.first;
    StructNode& pred = *i.second;
    pred.removeSuccessor(dest);
    pred.addSuccessor(kase, label);
  }
  label.addSuccessor(true, dest);

  for(const auto& i : preds) {
    long kase = i.first;
    StructNode& pred = *i.second;
    if(pred.getNumSuccessors() == 2) {
      if(auto* block = dyn_cast<Block>(&pred)) {
        IfThenGoto& ifg = newNode<IfThenGoto>(label, *block, not kase);
        Vector<std::pair<long, StructNode*>> preds = block->getIncoming();
        StructNode& succ = block->getSuccessor(not kase);

        block->disconnect();
        for(const auto& j : preds) {
          long kase = j.first;
          StructNode& pred = *j.second;
          pred.addSuccessor(kase, ifg);
        }
        ifg.addSuccessor(not kase, succ);
      } else {
        fatal(error() << "Expected block when reducing goto");
      }
    }
  }

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
  std::sort(sorted.begin(), sorted.end(),
            [](const StructNode* a, const StructNode* b) {
              return a->getNumPredecessors() < b->getNumPredecessors();
            });
  std::reverse(sorted.begin(), sorted.end());

  for(StructNode* node : sorted) {
    if(node->getNumPredecessors() > 2) {
      bool candidate = true;
      unsigned blocks = 0;
      for(StructNode& pred : node->predecessors()) {
        if(isa<Block>(pred) and (pred.getNumSuccessors() > 1))
          blocks += 1;
        else if(pred.getNumSuccessors() != 1)
          candidate = false;
      }
      // Of the predecessors of the node, at least one must be a block with
      // more than one successor
      if(candidate and (blocks > 1))
        return reduceIfThenGoto(*node);
    }
  }

  return false;
}

bool StructureAnalysis::reduceEndlessLoop(StructNode& node) {
  errs() << "Reducing endless loop at " << node.getId() << "\n";

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
  Map<long, StructNode*> preds;
  for(const auto& i : header.getIncoming()) {
    long kase = i.first;
    StructNode& pred = *i.second;
    if(not loop.isInLoop(pred))
      preds[kase] = &pred;
  }

  for(StructNode& node : loop)
    node.disconnect();
  for(auto i : preds)
    i.second->addSuccessor(i.first, loop);

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

bool StructureAnalysis::reduceNaturalLoop(LoopHeader& header,
                                          const List<IfThenBreak*>& exits) {
  // FIXME: Remove this limitation
  if(exits.size() > 1) {
    warning() << "Loops with multiple exits not supported\n";
    return false;
  }

  errs() << "Reducing Natural Loop at " << header.getId() << "\n";

  Vector<std::pair<long, StructNode*>> preds = header.getIncoming();
  Vector<StructNode*> succs;
  for(IfThenBreak* exit : exits) {
    StructNode& succ = exit->getExit();
    succs.push_back(&succ);
    succ.removePredecessor(*exit);
  }

  // The node should be made before disconnecting everything because we need
  // the connections to correctly construct the sequence of nodes in the loop
  // body
  NaturalLoop& loop = newNode<NaturalLoop>(header, exits);
  header.disconnect();
  for(IfThenBreak* exit : exits)
    exit->disconnect();

  for(auto& i : preds) {
    long kase = i.first;
    StructNode& pred = *i.second;
    if(not loop.isInLoop(pred))
      pred.addSuccessor(kase, loop);
  }
  for(StructNode* succ : succs)
    loop.addSuccessor(true, *succ);

  return true;
}

bool StructureAnalysis::tryReduceNaturalLoop(
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

  // The natural loop must contain alternating IfThenBreak and other nodes
  if(header) {
    List<IfThenBreak*> exits;
    StructNode* curr = &header->getSuccessor();
    bool expectingBreak = isa<IfThenBreak>(curr);
    do {
      if(expectingBreak) {
        if(auto* iftb = dyn_cast<IfThenBreak>(curr)) {
          curr = &iftb->getContinue();
          exits.push_back(iftb);
        } else {
          return false;
        }
      } else if(isa<IfThenBreak>(curr)) {
        return false;
      } else if(curr->getNumSuccessors() != 1) {
        return false;
      } else {
        curr = &curr->getSuccessor();
      }
      expectingBreak = not expectingBreak;
    } while(curr != header);

    return reduceNaturalLoop(*header, exits);
  }
  return false;
}

bool StructureAnalysis::reduceSwitch() {
  fatal(error() << "NOT IMPLEMENTED: reduceSwitch()\n");
  return false;
}

bool StructureAnalysis::tryReduceSwitch(const List<StructNode*>& postorder) {
  return false;
}

bool StructureAnalysis::tryReduce(const List<StructNode*>& postorder) {
  // Short-circuits. The order in which each is tried matters to an extent
  return (tryReduceEndlessLoop(postorder) || tryReduceNaturalLoop(postorder)
          || tryReduceSequence(postorder) || tryReduceSwitch(postorder)
          || tryReduceIfThen(postorder) || tryReduceIfThenElse(postorder)
          || tryReduceIfThenGoto(postorder));
}

static void collectLoops(const Loop* loop, List<const Loop*>& loops) {
  loops.push_back(loop);
  for(const Loop* subLoop : *loop)
    collectLoops(subLoop, loops);
}

static List<const Loop*> collectLoops(const LoopInfo& li) {
  List<const Loop*> loops;
  for(const Loop* loop : li)
    collectLoops(loop, loops);
  return loops;
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

static void getBlocksInPreOrder(const BasicBlock* bb,
                                Set<const BasicBlock*>& seen,
                                List<const BasicBlock*>& blocks) {
  if(not seen.contains(bb)) {
    seen.insert(bb);
    blocks.push_back(bb);
    for(const BasicBlock* succ : successors(bb))
      getBlocksInPreOrder(succ, seen, blocks);
  }
}

static List<const BasicBlock*> getBlocksInPreOrder(const Function& f) {
  List<const BasicBlock*> blocks;
  Set<const BasicBlock*> seen;

  getBlocksInPreOrder(&f.getEntryBlock(), seen, blocks);

  return blocks;
}

bool StructureAnalysis::runOnFunction(const Function& f) {
  List<const Loop*> loops = collectLoops(li);
  Map<const BasicBlock*, unsigned> exiting;
  Map<const BasicBlock*, unsigned> headers;
  for(const Loop* loop : loops) {
    SmallVector<Loop::Edge, 4> edges;
    loop->getExitEdges(edges);
    for(const Loop::Edge& edge : edges) {
      const BasicBlock* inside = edge.first;
      const BasicBlock* outside = edge.second;
      // The exiting blocks should only contain a single branch instruction
      if(inside->size() == 1) {
        exiting[inside] = loop->getLoopDepth();
      }
    }
    headers[loop->getHeader()] = loop->getLoopDepth();
  }

  // Construct initial tree
  LLVMContext& llvmContext = f.getContext();
  List<const BasicBlock*> blocks = getBlocksInPostOrder(f);
  for(const BasicBlock* bb : blocks)
    nodeMap[bb] = &newNode<Block>(*bb);

  // Add edges with attributes if required
  for(const BasicBlock* bb : blocks) {
    StructNode& node = getNodeFor(bb);
    const Instruction& inst = bb->back();
    if(const auto* br = dyn_cast<BranchInst>(&inst)) {
      if(br->isConditional()) {
        node.addSuccessor(true, getNodeFor(br->getSuccessor(0)));
        node.addSuccessor(false, getNodeFor(br->getSuccessor(1)));
      } else {
        node.addSuccessor(true, getNodeFor(br->getSuccessor(0)));
      }
    } else if(const auto* sw = dyn_cast<SwitchInst>(&inst)) {
      fatal(error() << "UNIMPLEMENTED SwitchInst in CFG contstruction");
    } else if(not(isa<ReturnInst>(inst) or isa<UnreachableInst>(inst))) {
      fatal(error() << "Unexpected instruction in CFG construction: "
                    << bb->back());
    }
  }

  root.reset(new Entry(getNodeFor(&f.getEntryBlock())));

  unsigned cfg = 0;

  {
    std::error_code ec;
    raw_fd_ostream fs("cfg." + std::to_string(cfg) + ".dot", ec);
    dot(fs);
    fs.close();
    cfg++;
  }

  // Get rid of any empty blocks but don't get rid of anything that might
  // be specialized
  bool changed = false;
  do {
    changed = false;
    for(const BasicBlock* bb : blocks) {
      StructNode& node = getNodeFor(bb);
      if((bb->size() == 1) and isa<Block>(node)
         and (not (headers.contains(bb) or exiting.contains(bb)))
         and (node.getNumPredecessors() == 1)
         and (node.getNumSuccessors() == 1)) {
        StructNode& pred = node.getPredecessor();
        long kase = pred.getSuccessorCase(node);
        StructNode& succ = node.getSuccessor();
        if(pred != node) {
          node.disconnect();
          pred.addSuccessor(kase, succ);
          changed |= true;
        }
      }
    }
  } while(changed);

  {
    std::error_code ec;
    raw_fd_ostream fs("cfg." + std::to_string(cfg) + ".dot", ec);
    dot(fs);
    fs.close();
    cfg++;
  }


  // Do these first because it is guaranteed that everything in the tree is
  // still a Block.
  for(const BasicBlock* bb : blocks) {
    if(exiting.contains(bb)) {
      StructNode& old = getNodeFor(bb);
      const Loop& loop = *li.getLoopFor(bb);
      StructNode& succ0 = old.getSuccessor(0);
      long exitCase = 0;
      if(auto* block = dyn_cast<Block>(&succ0)) {
        // Break when the condition is true
        if(loop.contains(&block->getLLVM()))
          exitCase = 1;
      } else if(auto* ift = dyn_cast<IfThenBreak>(&succ0)) {
        // Break when the condition is true
        if(loop.contains(ift->getLLVMBranchInst().getParent()))
          exitCase = 1;
      } else {
        fatal(error() << "Expected successor to be a block or if-then-break\n");
      }
      StructNode& exit = old.getSuccessor(exitCase);
      StructNode& cont = old.getSuccessor(not exitCase);
      IfThenBreak& neew = newNode<IfThenBreak>(
          exit, cast<BranchInst>(bb->back()), exiting.at(bb));
      Vector<std::pair<long, StructNode*>> preds = old.getIncoming();

      old.disconnect();
      for(const auto& i : preds) {
        long kase = i.first;
        StructNode& pred = *i.second;
        pred.addSuccessor(kase, neew);
      }
      neew.addSuccessor(exitCase, exit);
      neew.addSuccessor(not exitCase, cont);
    }
  }
  // Specialize nodes
  for(const BasicBlock* bb : blocks) {
    if(headers.contains(bb)) {
      StructNode& old = getNodeFor(bb);
      StructNode& neew = newNode<LoopHeader>(headers.at(bb));
      Vector<std::pair<long, StructNode*>> preds = old.getIncoming();
      Map<long, StructNode*> succs = old.getOutgoing();

      old.disconnect();
      for(auto& i : preds) {
        long kase = i.first;
        StructNode& pred = *i.second;
        pred.addSuccessor(kase, neew);
      }
      for(auto& i : succs) {
        long kase = i.first;
        StructNode& succ = *i.second;
        neew.addSuccessor(kase, succ);
      }
    }
  }

  {
    std::error_code ec;
    raw_fd_ostream fs("cfg." + std::to_string(cfg) + ".dot", ec);
    dot(fs);
    fs.close();
    cfg++;
  }

  bool reduced = true;
  List<StructNode*> postorder = dfs(root->getEntry());
  while(postorder.size() > 1 and reduced) {
    reduced = tryReduce(postorder);
    if(reduced)
      postorder = dfs(root->getEntry());
    {
      std::error_code ec;
      raw_fd_ostream fs("cfg." + std::to_string(cfg) + ".dot", ec);
      dot(fs);
      fs.close();
      cfg++;
    }
  }

  // FIXME: Right now, if there was anything that could not be reduced in the
  // CFG, just return false. This is too extreme because there is no reason
  // why we need to resort to an all or nothing approach. For the moment,
  // this will be a known limitation that can be addressed later
  return postorder.size() == 1;
}

} // namespace cish

StructureAnalysisWrapperPass::StructureAnalysisWrapperPass()
    : FunctionPass(ID), structured(false), ctree(nullptr) {
  ;
}

StringRef StructureAnalysisWrapperPass::getPassName() const {
  return "Cish Structure Analysis Pass";
}

void StructureAnalysisWrapperPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.addRequired<PostDominatorTreeWrapperPass>();
  AU.addRequired<ScalarEvolutionWrapperPass>();
  AU.setPreservesAll();
}

bool StructureAnalysisWrapperPass::isStructured() const {
  return structured;
}

const cish::StructNode& StructureAnalysisWrapperPass::getStructured() const {
  return ctree->getStructured();
}

bool StructureAnalysisWrapperPass::runOnFunction(Function& f) {
  const LoopInfo& li = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  const DominatorTree& dt
      = getAnalysis<DominatorTreeWrapperPass>().getDomTree();
  const PostDominatorTree& pdt
      = getAnalysis<PostDominatorTreeWrapperPass>().getPostDomTree();
  const ScalarEvolution& se = getAnalysis<ScalarEvolutionWrapperPass>().getSE();

  ctree.reset(new cish::StructureAnalysis(li, dt, pdt));
  structured = ctree->runOnFunction(f);

  return false;
}

char StructureAnalysisWrapperPass::ID = 0;

static RegisterPass<StructureAnalysisWrapperPass>
    X("cish-program-structure",
      "Creates a high-level program outline",
      true,
      true);

Pass* createStructureAnalysisWrapperPass() {
  return new StructureAnalysisWrapperPass();
}
