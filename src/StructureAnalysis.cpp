#include "StructureAnalysis.h"
#include "Diagnostics.h"

#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/IR/CFG.h>
#include <llvm/IR/InstIterator.h>

using namespace llvm;

namespace cish {

StructNode::StructNode(Kind kind) : kind(kind), flags(0) {
  ;
}

StructNode::Kind StructNode::getKind() const {
  return kind;
}

StringRef StructNode::getKindName() const {
  switch(getKind()) {
  case S_Entry:
    return "Entry";
  case S_Exit:
    return "Exit";
  case S_Block:
    return "Block";
  case S_Sequence:
    return "Sequence";
  case S_IfThen:
    return "IfThen";
  case S_IfThenElse:
    return "IfThenElse";
  case S_IfThenBreak:
    return "IfThenBreak";
  case S_IfThenContinue:
    return "IfThenContinue";
  case S_DoWhileLoop:
    return "DoWhileLoop";
  case S_EndlessLoop:
    return "EndlessLoop";
  case S_NaturalLoop:
    return "NaturalLoop";
  case S_Latch:
    return "Latch";
  case S_Switch:
    return "Switch";
  default:
    return "Other";
  };
}

std::string StructNode::getFlagNames() const {
  std::string buf;
  raw_string_ostream ss(buf);
  if(hasPreheader())
    ss << "P";
  if(hasHeader())
    ss << "H";
  if(hasExiting())
    ss << "G";
  if(hasExit())
    ss << "X";
  return ss.str();
}

void StructNode::setFlag(StructNode::Flags flag) {
  flags |= (unsigned)flag;
}

void StructNode::setFlags(unsigned flags) {
  this->flags |= flags;
}

void StructNode::setPreheader() {
  setFlag(StructNode::F_Preheader);
}

void StructNode::setHeader() {
  setFlag(StructNode::F_Header);
}

void StructNode::setExiting() {
  setFlag(StructNode::F_Exiting);
}

void StructNode::setExit() {
  setFlag(StructNode::F_Exit);
}

StructNode& StructNode::addPredecessor(StructNode& pred,
                                       const Instruction* inst,
                                       const ConstantInt* cond) {
  if(not hasPredecessor(pred)) {
    in.emplace_back(pred, *this, inst, cond);
    pred.addSuccessor(*this, inst, cond);
  }
  return *this;
}

StructNode& StructNode::addSuccessor(StructNode& succ,
                                     const Instruction* inst,
                                     const ConstantInt* cond) {
  if(not hasSuccessor(succ)) {
    out.emplace_back(*this, succ, inst, cond);
    succ.addPredecessor(*this, inst, cond);
  }

  return *this;
}

StructNode& StructNode::removePredecessor(StructNode& pred) {
  if(hasPredecessor(pred)) {
    for(auto i = in.begin(); i != in.end(); i++) {
      if(i->head == pred) {
        in.erase(i);
        break;
      }
    }
    pred.removeSuccessor(*this);
  }
  return *this;
}

StructNode& StructNode::removeSuccessor(StructNode& succ) {
  if(hasSuccessor(succ)) {
    for(auto i = out.begin(); i != out.end(); i++) {
      if(i->tail == succ) {
        out.erase(i);
        break;
      }
    }
    succ.removePredecessor(*this);
  }
  return *this;
}

StructNode& StructNode::replacePredecessor(StructNode& old, StructNode& neew) {
  removePredecessor(old);
  addPredecessor(neew);
  return *this;
}

StructNode& StructNode::replaceSuccessor(StructNode& old, StructNode& neew) {
  removeSuccessor(old);
  addSuccessor(neew);
  return *this;
}

StructNode& StructNode::disconnect() {
  for(StructNode* pred : getPredecessors())
    removePredecessor(*pred);
  for(StructNode* succ : getSuccessors())
    removeSuccessor(*succ);
  return *this;
}

unsigned StructNode::getFlags() const {
  return flags;
}

bool StructNode::hasFlag(StructNode::Flags flag) const {
  return (flags & (unsigned)flag) == flag;
}

bool StructNode::hasPreheader() const {
  return hasFlag(StructNode::F_Preheader);
}

bool StructNode::hasHeader() const {
  return hasFlag(StructNode::F_Header);
}

bool StructNode::hasExiting() const {
  return hasFlag(StructNode::F_Exiting);
}

bool StructNode::hasExit() const {
  return hasFlag(StructNode::F_Exit);
}

bool StructNode::hasPredecessor(StructNode& pred) const {
  for(const Edge& edge : in)
    if(edge.head == pred)
      return true;
  return false;
}

bool StructNode::hasSuccessor(StructNode& succ) const {
  for(const Edge& edge : out)
    if(edge.tail == succ)
      return true;
  return false;
}

size_t StructNode::getNumPredecessors() const {
  return in.size();
}

size_t StructNode::getNumSuccessors() const {
  return out.size();
}

StructNode& StructNode::getPredecessor(size_t n) const {
  return in[n].head;
}

StructNode& StructNode::getSuccessor(size_t n) const {
  return out[n].tail;
}

const Edge& StructNode::getIncomingEdge(StructNode& pred) const {
  for(const Edge& e : in)
    if(e.head == pred)
      return e;
  fatal(error() << "No incoming edge found for predecessor");
}

const Edge& StructNode::getOutgoingEdge(StructNode& succ) const {
  for(const Edge& e : out)
    if(e.tail == succ)
      return e;
  fatal(error() << "No outgoing edge found for successor");
}

Vector<StructNode*> StructNode::getPredecessors() const {
  Vector<StructNode*> ret;
  for(const Edge& e : in)
    ret.push_back(&e.head);
  return ret;
}

Vector<StructNode*> StructNode::getSuccessors() const {
  Vector<StructNode*> ret;
  for(const Edge& e : out)
    ret.push_back(&e.tail);
  return ret;
}

iterator_range<StructNode::const_edge_iterator> StructNode::incoming() const {
  return iterator_range<const_edge_iterator>(in.begin(), in.end());
}

iterator_range<StructNode::const_edge_iterator> StructNode::outgoing() const {
  return iterator_range<const_edge_iterator>(out.begin(), out.end());
}

iterator_range<StructNode::const_succ_iterator> StructNode::successors() const {
  return iterator_range<const_succ_iterator>(out.begin(), out.end());
}

iterator_range<StructNode::const_pred_iterator>
StructNode::predecessors() const {
  return iterator_range<const_pred_iterator>(in.begin(), in.end());
}

bool StructNode::operator==(const StructNode& other) const {
  return this == &other;
}

bool StructNode::operator!=(const StructNode& other) const {
  return this != &other;
}

Block::Block(const BasicBlock& bb) : StructNode(StructNode::S_Block), bb(bb) {
  ;
}

const BasicBlock& Block::getLLVM() const {
  return bb;
}

Sequence::Sequence(const List<StructNode*>& seq)
    : StructNode(StructNode::S_Sequence) {
  for(StructNode* node : seq) {
    setFlags(node->getFlags());
    add(*node);
  }
}

void Sequence::add(StructNode& elem) {
  seq.push_back(&elem);
}

Sequence::const_iterator Sequence::begin() const {
  return seq.begin();
}

Sequence::const_iterator Sequence::end() const {
  return seq.end();
}

IfThenBase::IfThenBase(StructNode::Kind kind, Block& cond, StructNode& thn)
    : StructNode(kind), cond(cond), thn(thn) {
  setFlags(cond.getFlags());
}

const BranchInst& IfThenBase::getLLVMBranchInst() const {
  return cast<BranchInst>(getCond().getLLVM().back());
}

const Block& IfThenBase::getCond() const {
  return cond;
}

const StructNode& IfThenBase::getThen() const {
  return thn;
}

IfThen::IfThen(Block& cond, StructNode& thn, bool inverted)
    : IfThenBase(StructNode::S_IfThen, cond, thn), inverted(inverted) {
  ;
}

bool IfThen::isInverted() const {
  return inverted;
}

IfThenElse::IfThenElse(Block& cond, StructNode& thn, StructNode& els)
    : IfThenBase(StructNode::S_IfThenElse, cond, thn), els(els) {
  ;
}

const StructNode& IfThenElse::getElse() const {
  return els;
}

IfThenBreak::IfThenBreak(Block& cond,
                         StructNode& thn,
                         bool inverted)
    : IfThenBase(StructNode::S_IfThenBreak, cond, thn),
      inverted(inverted) {
  ;
}

bool IfThenBreak::isInverted() const {
  return inverted;
}

IfThenContinue::IfThenContinue(Block& cond, StructNode& thn, DoWhileLoop& loop)
    : IfThenBase(StructNode::S_IfThenContinue, cond, thn), loop(loop) {
  ;
}

const class DoWhileLoop& IfThenContinue::getLoop() const {
  return loop;
}

Switch::Switch() : StructNode(StructNode::S_Switch) {
  ;
}

LoopBase::LoopBase(StructNode::Kind kind, StructNode& header)
    : StructNode(kind), header(header) {
  nodes.push_back(&header);
}

StructNode& LoopBase::getHeader() const {
  return header;
}

bool LoopBase::isInLoop(StructNode& node) const {
  return nodes.contains(&node);
}

LoopBase::const_iterator LoopBase::begin() const {
  return nodes.begin();
}

LoopBase::const_iterator LoopBase::end() const {
  return nodes.end();
}

DoWhileLoop::DoWhileLoop(StructNode& header, StructNode& exit)
    : cish::LoopBase(StructNode::S_DoWhileLoop, header), cond(nullptr) {
  for(const Edge& e : header.outgoing())
    if(e.tail == exit)
      cond = dyn_cast<CmpInst>(cast<BranchInst>(e.inst)->getCondition());
  if(not cond)
    fatal(error() << "Expected CmpInst in exit edge of while loop\n");

  for(StructNode& node : header.successors())
    if(node != exit)
      nodes.push_back(&node);
}

const CmpInst& DoWhileLoop::getCondition() const {
  return *cond;
}

EndlessLoop::EndlessLoop(StructNode& header)
    : cish::LoopBase(StructNode::S_EndlessLoop, header) {
  for(StructNode& node : header.successors())
    nodes.push_back(&node);
}

// This a special node whose only purpose is to serve as the root of the
// control tree. This will be the sole predecessor of the StructNode
// corresponding to the function entry block. The function entry block is also
// its sole successor
class Entry : protected StructNode {
public:
  Entry(StructNode& entry) : StructNode(StructNode::S_Entry) {
    addSuccessor(entry);
  }
  virtual ~Entry() = default;

  StructNode& getEntry() const {
    return getSuccessor(0);
  }
};

// This is a special node whose only purpose is to serve as a successor to
// the lone exit node of the function
class Exit : protected StructNode {
public:
  Exit(StructNode& exit) : StructNode(StructNode::S_Exit) {
    addPredecessor(exit);
  }
  virtual ~Exit() = default;

  StructNode& getExit() const {
    return getPredecessor(0);
  }
};

class StructureAnalysis {
private:
  const LoopInfo& li;
  const DominatorTree& dt;
  const PostDominatorTree& pdt;

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
  bool reduceWhileLoop(StructNode& header, StructNode& exit);
  bool reduceNaturalLoop(StructNode& header, const List<StructNode*>& exits);
  bool reduceSequence(const List<StructNode*>& nodes);
  bool reduceIfThen(Block& cond,
                    StructNode& thn,
                    StructNode& succ,
                    bool invert = false);
  bool reduceIfThenElse(Block& cond,
                        StructNode& thn,
                        StructNode& els,
                        StructNode& succ);
  bool reduceIfThenBreak(Block& cond,
                         StructNode& thn,
                         StructNode& succ);
  bool reduceIfThenContinue(Block& cond,
                            StructNode& thn,
                            DoWhileLoop& loop,
                            StructNode& succ);
  bool reduceSwitch();

  bool tryReduce(const List<StructNode*>& postorder);
  bool tryReduceSequence(const List<StructNode*>& postorder);
  bool tryReduceIfThen(const List<StructNode*>& postorder);
  bool tryReduceIfThenBreak(const List<StructNode*>& postorder);
  bool tryReduceIfThenElse(const List<StructNode*>& postorder);
  bool tryReduceEndlessLoop(const List<StructNode*>& postorder);
  bool tryReduceWhileLoop(const List<StructNode*>& postorder);
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
    : li(li), dt(dt), pdt(pdt) {
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
      uint64_t nodeId = (uint64_t)node;
      for(const Edge& edge : node->outgoing()) {
        const StructNode& child = edge.tail;
        uint64_t childId = (uint64_t)&child;
        os << "  " << nodeId << " -> " << childId;
        if(const llvm::ConstantInt* cond = edge.condition)
          os << " [label=" << cond->getLimitedValue() << "]";
        os << "\n";
        if(not seen.contains(&child)) {
          os << "  " << (uint64_t)&child << " [label=\"" << child.getKindName()
             << "(" << (unsigned long)&child << ")\"]\n";
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
  errs() << "Reducing sequence " << nodes.size() << "\n";
  StructNode& head = *nodes.front();
  StructNode& tail = *nodes.back();
  Vector<Edge> headEdges(head.incoming().begin(), head.incoming().end());
  Vector<Edge> tailEdges(tail.outgoing().begin(), tail.outgoing().end());
  for(StructNode* node : nodes)
    node->disconnect();

  Sequence& seq = newNode<Sequence>(nodes);
  for(const Edge& edge : headEdges)
    edge.head.addSuccessor(seq, edge.inst, edge.condition);
  for(const Edge& edge : tailEdges)
    seq.addSuccessor(edge.tail, edge.inst, edge.condition);

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
      curr = &curr->getPredecessor(0);
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
  errs() << "Reducing IfThen\n";
  Vector<StructNode*> preds = cond.getPredecessors();
  cond.disconnect();
  thn.disconnect();
  succ.removePredecessor(cond);
  succ.removePredecessor(thn);

  IfThen& iff = newNode<IfThen>(cond, thn, invert);
  for(StructNode* pred : preds)
    pred->addSuccessor(iff);
  succ.addPredecessor(iff);

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
          StructNode& p0 = node->getPredecessor(i);
          StructNode& p1 = node->getPredecessor(j);

          // From the post order, it's not clear which path contains the "then"
          if((p0.getNumPredecessors() == 1)
             and (&p0.getPredecessor(0) == &p1)) {
            if(auto* cond = dyn_cast<Block>(&p1)) {
              if(cond->getOutgoingEdge(p0).condition->getLimitedValue())
                return reduceIfThen(*cond, p0, *node);
              else
                return reduceIfThen(*cond, p0, *node, true);
            }
            fatal(error() << "Condition of an IfThen must be a block");
          } else if((p1.getNumPredecessors() == 1)
                    and (&p1.getPredecessor(1) == &p0)) {
            if(auto* cond = dyn_cast<Block>(&p0)) {
              if(cond->getOutgoingEdge(p0).condition->getLimitedValue())
                return reduceIfThen(*cond, p1, *node);
              else
                return reduceIfThen(*cond, p1, *node, true);
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
  errs() << "Reducing IfThenElse\n";
  Vector<StructNode*> preds = cond.getPredecessors();
  cond.disconnect();
  thn.disconnect();
  els.disconnect();
  succ.removePredecessor(thn);
  succ.removePredecessor(els);

  IfThenElse& iff = newNode<IfThenElse>(cond, thn, els);
  for(StructNode* pred : preds)
    pred->addSuccessor(iff);
  succ.addPredecessor(iff);

  return true;
}

bool StructureAnalysis::tryReduceIfThenElse(
    const List<StructNode*>& postorder) {
  // IfThenElse
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
          StructNode& thn = node->getPredecessor(i);
          StructNode& els = node->getPredecessor(j);
          if((thn.getNumPredecessors() == 1) and (els.getNumPredecessors() == 1)
             and (&thn.getPredecessor(0) == &els.getPredecessor(0))) {
            if(auto* cond = dyn_cast<Block>(&thn.getPredecessor(0))) {
              if(cond->getOutgoingEdge(thn).condition->getLimitedValue())
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

bool StructureAnalysis::reduceIfThenBreak(Block& cond,
                                          StructNode& then,
                                          StructNode& succ) {
  errs() << "Redeucing IfThenBreak\n";
  return false;
}

bool StructureAnalysis::tryReduceIfThenBreak(
    const List<StructNode*>& postorder) {
  return false;
}

bool StructureAnalysis::reduceEndlessLoop(StructNode& node) {
  errs() << "Reducing endless loop: " << (uint64_t)&node << "\n";

  StructNode& header = [&]() -> StructNode& {
    if(node.hasSuccessor(node))
      // If this is a single node loop, the header is itself
      return node;
    else if(node.getNumPredecessors() > 1)
      // If the header has more than one predecessor, then one of the
      // predecessors is a backedge and the rest are from outside the loop
      return node;
    else if(node.getSuccessor(0).getNumPredecessors() > 1)
      return node.getSuccessor(0);
    else
      fatal(error() << "Could not determine header for endless loop");
  }();

  EndlessLoop& loop = newNode<EndlessLoop>(header);
  Vector<StructNode*> preds;
  for(StructNode& pred : header.predecessors())
    if(not loop.isInLoop(pred))
      preds.push_back(&pred);

  for(StructNode& node : loop)
    node.disconnect();
  for(StructNode* pred : preds)
    pred->addSuccessor(loop);

  return true;
}

bool StructureAnalysis::tryReduceEndlessLoop(
    const List<StructNode*>& postorder) {
  for(StructNode* node : postorder) {
    if(node->getNumSuccessors() == 1) {
      // node-->--
      //   |      |
      //    --<---
      if(&node->getSuccessor(0) == node)
        return reduceEndlessLoop(*node);

      // node-->--succ
      //   |       |
      //    ---<---
      StructNode& succ = node->getSuccessor(0);
      if((succ.getNumSuccessors() == 1) and (&succ.getSuccessor(0) == node))
        return reduceEndlessLoop(*node);
    }
  }

  return false;
}

bool StructureAnalysis::reduceWhileLoop(StructNode& header, StructNode& exit) {
  errs() << "Reducing while loop: " << (uint64_t)&header << "\n";

  DoWhileLoop& loop = newNode<DoWhileLoop>(header, exit);
  Vector<StructNode*> preds;
  for(StructNode& pred : header.predecessors())
    if(not loop.isInLoop(pred))
      preds.push_back(&pred);

  for(StructNode& node : loop)
    node.disconnect();
  for(StructNode* pred : preds)
    pred->addSuccessor(loop);
  loop.addSuccessor(exit);

  return true;
}

bool StructureAnalysis::tryReduceWhileLoop(
    const List<StructNode*>& postorder) {
  // While loops
  //
  for(StructNode* node : postorder) {
    if(node->getNumSuccessors() == 2) {
      //    --->--exit
      //   |
      // node-->--
      //   |      |
      //    --<---
      if(&node->getSuccessor(0) == node)
        return reduceWhileLoop(*node, node->getSuccessor(1));
      else if(&node->getSuccessor(1) == node)
        return reduceWhileLoop(node->getSuccessor(1), *node);

      //    --->--exit
      //   |
      // node-->--succ
      //   |       |
      //    ---<---
      StructNode& succ0 = node->getSuccessor(0);
      StructNode& succ1 = node->getSuccessor(1);
      if((succ0.getNumSuccessors() == 1) and (&succ0.getSuccessor(0) == node))
        return reduceWhileLoop(*node, succ1);
      else if((succ1.getNumSuccessors() == 1)
              and (&succ1.getSuccessor(0) == node))
        return reduceWhileLoop(*node, succ0);
    }
  }

  return false;
}

bool StructureAnalysis::reduceNaturalLoop(StructNode& header,
                                          const List<StructNode*>& exits) {
  return false;
}

bool StructureAnalysis::tryReduceNaturalLoop(
    const List<StructNode*>& postorder) {
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
  return (tryReduceEndlessLoop(postorder) || tryReduceWhileLoop(postorder)
          || tryReduceNaturalLoop(postorder) || tryReduceIfThenBreak(postorder)
          || tryReduceSequence(postorder) || tryReduceSwitch(postorder)
          || tryReduceIfThen(postorder) || tryReduceIfThenElse(postorder));
}

static void collectLoops(const Loop* loop, Set<const Loop*>& loops) {
  loops.insert(loop);
  for(Loop* subLoop : *loop)
    collectLoops(subLoop, loops);
}

bool StructureAnalysis::runOnFunction(const Function& f) {
  // Construct initial tree
  LLVMContext& llvmContext = f.getContext();
  for(const BasicBlock& bb : f)
    nodeMap[&bb] = &newNode<Block>(bb);

  for(const BasicBlock& bb : f) {
    StructNode& node = getNodeFor(bb);
    const Instruction& inst = bb.back();
    if(const auto* br = dyn_cast<BranchInst>(&inst)) {
      if(br->isConditional()) {
        node.addSuccessor(getNodeFor(br->getSuccessor(0)),
                          &inst,
                          llvm::ConstantInt::getTrue(llvmContext));
        node.addSuccessor(getNodeFor(br->getSuccessor(1)),
                          &inst,
                          llvm::ConstantInt::getFalse(llvmContext));
      } else {
        node.addSuccessor(getNodeFor(br->getSuccessor(0)));
      }
    } else if(const auto* sw = dyn_cast<SwitchInst>(&inst)) {
      fatal(error() << "UNIMPLEMENTED SwitchInst in CFG contstruction");
    } else if(not(isa<ReturnInst>(inst) or isa<UnreachableInst>(inst))) {
      fatal(error() << "Unexpected instruction in CFG construction: "
                    << bb.back());
    }
  }

  root.reset(new Entry(getNodeFor(&f.getEntryBlock())));
  unsigned cfg = 0;

  Set<const Loop*> loops;
  for(const Loop* loop : li) {
    collectLoops(loop, loops);
    // loops.push_back(loop);
    // for(const Loop* subLoop : loop->getSubLoops())
    //   loops.push_back(subLoop);
  }

  for(const Loop* loop : loops) {
    if(loop->isLoopSimplifyForm()) {
      getNodeFor(loop->getLoopPreheader()).setPreheader();
      getNodeFor(loop->getHeader()).setHeader();
      SmallVector<Loop::Edge, 4> exitEdges;
      loop->getExitEdges(exitEdges);
      for(const Loop::Edge& edge : exitEdges) {
        getNodeFor(edge.first).setExiting();
        getNodeFor(edge.second).setExit();
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
      errs() << "#" << cfg << "\n";
      std::error_code ec;
      raw_fd_ostream fs("cfg." + std::to_string(cfg) + ".dot", ec);
      dot(fs);
      fs.close();
      dot(errs());
      errs() << "\n";
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
