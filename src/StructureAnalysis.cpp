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

StructNode& StructNode::addPredecessor(StructNode& pred) {
  if(not preds.contains(&pred)) {
    preds.push_back(&pred);
    pred.addSuccessor(*this);
  }
  return *this;
}

StructNode& StructNode::addSuccessor(StructNode& succ) {
  if(not succs.contains(&succ)) {
    succs.push_back(&succ);
    succ.addPredecessor(*this);
  }

  return *this;
}

StructNode& StructNode::removePredecessor(StructNode& pred) {
  if(hasPredecessor(pred)) {
    preds.erase_all(&pred);
    pred.removeSuccessor(*this);
  }
  return *this;
}

StructNode& StructNode::removeSuccessor(StructNode& succ) {
  if(hasSuccessor(succ)) {
    succs.erase_all(&succ);
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
  return preds.contains(&pred);
}

bool StructNode::hasSuccessor(StructNode& succ) const {
  return succs.contains(&succ);
}

size_t StructNode::getNumPredecessors() const {
  return preds.size();
}

size_t StructNode::getNumSuccessors() const {
  return succs.size();
}

StructNode& StructNode::getPredecessor(size_t n) const {
  return *preds[n];
}

StructNode& StructNode::getSuccessor(size_t n) const {
  return *succs[n];
}

Vector<StructNode*> StructNode::getPredecessors() const {
  return Vector<StructNode*>(preds.begin(), preds.end());
}

Vector<StructNode*> StructNode::getSuccessors() const {
  return Vector<StructNode*>(succs.begin(), succs.end());
}

StructNode::succ_iterator StructNode::succ_begin() {
  return succs.begin();
}

StructNode::succ_iterator StructNode::succ_end() {
  return succs.end();
}

StructNode::pred_iterator StructNode::pred_begin() {
  return preds.begin();
}

StructNode::pred_iterator StructNode::pred_end() {
  return preds.end();
}

iterator_range<StructNode::succ_iterator> StructNode::successors() {
  return iterator_range<succ_iterator>(succs.begin(), succs.end());
}

iterator_range<StructNode::const_succ_iterator> StructNode::successors() const {
  return iterator_range<const_succ_iterator>(succs.begin(), succs.end());
}

iterator_range<StructNode::pred_iterator> StructNode::predecessors() {
  return iterator_range<pred_iterator>(preds.begin(), preds.end());
}

iterator_range<StructNode::const_pred_iterator>
StructNode::predecessors() const {
  return iterator_range<const_pred_iterator>(preds.begin(), preds.end());
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

IfThen::IfThen(Block& cond, StructNode& thn)
    : IfThenBase(StructNode::S_IfThen, cond, thn) {
  ;
}

IfThenElse::IfThenElse(Block& cond, StructNode& thn, StructNode& els)
    : IfThenBase(StructNode::S_IfThenElse, cond, thn), els(els) {
  ;
}

const StructNode& IfThenElse::getElse() const {
  return els;
}

IfThenBreak::IfThenBreak(Block& cond, StructNode& thn, DoWhileLoop& loop)
    : IfThenBase(StructNode::S_IfThenBreak, cond, thn), loop(loop) {
  ;
}

const class DoWhileLoop& IfThenBreak::getLoop() const {
  return loop;
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
    : StructNode(kind), header(nullptr) {
  ;
}

StructNode& LoopBase::getHeader() const {
  return *header;
}

bool LoopBase::isInLoop(StructNode* node) const {
  return nodes.contains(node);
}

bool LoopBase::isInLoop(StructNode& node) const {
  return isInLoop(&node);
}

LoopBase::const_iterator LoopBase::begin() const {
  return nodes.begin();
}

LoopBase::const_iterator LoopBase::end() const {
  return nodes.end();
}

DoWhileLoop::DoWhileLoop(StructNode& header)
    : cish::LoopBase(StructNode::S_DoWhileLoop, header) {
  ;
}

EndlessLoop::EndlessLoop(StructNode& header)
    : cish::LoopBase(StructNode::S_EndlessLoop, header) {
  // There is a possibility that the "header" that is passed here is not
  // actually the header. Just make sure
  if(header.getSuccessors().contains(&header))
    // If this is a single node loop, the header is itself
    this->header = &header;
  else if(header.getNumPredecessors() > 1)
    // If the header has more than one predecessor, then one of the predecessors
    // is a backedge and the rest are from outside the loop
    this->header = &header;
  else if(header.getSuccessor(0).getNumPredecessors() > 1)
    this->header = &header.getSuccessor(0);
  else
    fatal(error() << "Could not determine header for endless loop");

  nodes.push_back(this->header);
  for(StructNode* node : this->header->getSuccessors())
    nodes.push_back(node);
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
           const Set<StructNode*>& exits,
           Set<StructNode*>& seen,
           List<StructNode*>& postorder) const;
  List<StructNode*> dfs(StructNode& node) const;
  List<StructNode*> dfs(const Loop& loop) const;

  bool reduceEndlessLoop(StructNode& header);

  bool reduceAcyclicRegion(const List<StructNode*>& postorder);
  bool reduceSequence(const List<StructNode*>& nodes);
  bool reduceIfThen(Block& cond, StructNode& thn, StructNode& succ);
  bool reduceIfThenElse(Block& cond,
                        StructNode& thn,
                        StructNode& els,
                        StructNode& succ);
  bool reduceIfThenBreak(Block& cond,
                         StructNode& thn,
                         DoWhileLoop& loop,
                         StructNode& succ);
  bool reduceIfThenContinue(Block& cond,
                            StructNode& thn,
                            DoWhileLoop& loop,
                            StructNode& succ);
  bool reduceSwitch();
  void replaceNodes(StructNode& head, StructNode& tail, StructNode& repl);

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
     << root->getEntry().getFlagNames() << ")\"]";
  Set<const StructNode*> seen = {&root->getEntry()};
  List<const StructNode*> wl = {&root->getEntry()};
  do {
    List<const StructNode*> next;
    for(const StructNode* node : wl) {
      uint64_t nodeId = (uint64_t)node;
      for(const StructNode* child : node->successors()) {
        uint64_t childId = (uint64_t)child;
        os << "  " << nodeId << " -> " << childId << "\n";
        if(not seen.contains(child)) {
          os << "  " << (uint64_t)child << " [label=\""
             << child->getKindName() << "(" << child->getFlagNames() << ")\"]";
          seen.insert(child);
          next.push_back(child);
        }
      }
    }
    wl = std::move(next);
  } while(wl.size());

  os << "}";
}

void StructureAnalysis::dfs(StructNode& node,
                            const Set<StructNode*>& exits,
                            Set<StructNode*>& seen,
                            List<StructNode*>& postorder) const {
  if(not seen.contains(&node)) {
    seen.insert(&node);
    if(not exits.contains(&node))
      for(StructNode* succ : node.successors())
        dfs(*succ, exits, seen, postorder);
    postorder.push_back(&node);
  }
}

List<StructNode*> StructureAnalysis::dfs(StructNode& node) const {
  List<StructNode*> postorder;
  Set<StructNode*> seen;

  dfs(node, {}, seen, postorder);

  return postorder;
}

List<StructNode*> StructureAnalysis::dfs(const Loop& loop) const {
  List<StructNode*> postorder;
  Set<StructNode*> seen;
  Set<StructNode*> exits;
  SmallVector<BasicBlock*, 4> exitBlocks;
  loop.getExitBlocks(exitBlocks);
  for(BasicBlock* bb : exitBlocks)
    exits.insert(&getNodeFor(bb));

  dfs(getNodeFor(loop.getHeader()), exits, seen, postorder);

  return postorder;
}

bool StructureAnalysis::reduceSequence(const List<StructNode*>& nodes) {
  errs() << "Reducing sequence " << nodes.size() << " => ";
  StructNode& head = *nodes.front();
  StructNode& tail = *nodes.back();
  Vector<StructNode*> preds = head.getPredecessors();
  Vector<StructNode*> succs = tail.getSuccessors();
  for(StructNode* node : nodes)
    node->disconnect();

  Sequence& seq = newNode<Sequence>(nodes);
  for(StructNode* pred : preds)
    pred->addSuccessor(seq);
  for(StructNode* succ : succs)
    succ->addPredecessor(seq);

  return true;
}

bool StructureAnalysis::reduceIfThen(Block& cond,
                                     StructNode& thn,
                                     StructNode& succ) {
  errs() << "Reducing IfThen\n";
  Vector<StructNode*> preds = cond.getPredecessors();
  cond.disconnect();
  thn.disconnect();
  succ.removePredecessor(cond);
  succ.removePredecessor(thn);

  IfThen& iff = newNode<IfThen>(cond, thn);
  for(StructNode* pred : preds)
    pred->addSuccessor(iff);
  succ.addPredecessor(iff);

  return true;
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

bool StructureAnalysis::reduceEndlessLoop(StructNode& header) {
  errs() << "Reducing endless loop: " << (uint64_t)&header << "\n";
  EndlessLoop& loop = newNode<EndlessLoop>(header);
  Vector<StructNode*> preds;
  for(StructNode* pred : loop.getHeader().getPredecessors())
    if(not loop.isInLoop(pred))
      preds.push_back(pred);

  for(StructNode* node : loop)
    node->disconnect();
  for(StructNode* pred : preds)
    pred->addSuccessor(loop);

  return true;
}

static bool isEndlessLoop1(StructNode* node) {
  return (node->getNumSuccessors() == 1) and (&node->getSuccessor(0) == node);
}

static bool isEndlessLoop2(StructNode* node) {
  return (node->getNumSuccessors() == 1)
         and (node->getSuccessor(0).getNumSuccessors() == 1)
         and (&node->getSuccessor(0).getSuccessor(0) == node);
}

bool StructureAnalysis::reduceAcyclicRegion(
    const List<StructNode*>& postorder) {
  // Endless loops
  //
  // node-->--
  //   |      |
  //    --<---
  //
  // node-->--node
  //   |       |
  //    ---<---
  for(StructNode* node : postorder) {
    if(isEndlessLoop1(node) or isEndlessLoop2(node))
      return reduceEndlessLoop(*node);
  }

  // Sequence
  //
  // node-->--node-->--node
  //
  // All nodes in the sequence must have exactly one predecessor and exactly one
  // successor
  errs() << "Test sequence\n";
  for(StructNode* node : postorder) {
    List<StructNode*> seq;
    StructNode* curr = node;
    errs() << curr->getKindName() << " " << curr->getNumPredecessors() << " "
           << curr->getNumSuccessors() << "\n";
    while(curr and (curr->getNumSuccessors() <= 1)
          and (curr->getNumPredecessors() == 1)) {
      seq.push_front(curr);
      curr = *curr->pred_begin();
      errs() << curr->getKindName() << " " << curr->getNumPredecessors()
             << " " << curr->getNumSuccessors() << "\n";
    }

    if(seq.size() > 1)
      return reduceSequence(seq);
  }
  errs() << "End sequence\n";

  // IfThen
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
          if((p0.getNumPredecessors() == 1)
             and (&p0.getPredecessor(0) == &p1)) {
            if(auto* cond = dyn_cast<Block>(&p1))
              return reduceIfThen(*cond, p0, *node);
            fatal(error() << "Condition of an IfThen must be a block");
          } else if((p1.getNumPredecessors() == 1)
                    and (&p1.getPredecessor(1) == &p0)) {
            if(auto* cond = dyn_cast<Block>(&p1))
              return reduceIfThen(*cond, p1, *node);
            fatal(error() << "Condition iof an IfThen must be a block");
          }
        }
      }
    }
  }

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
            if(auto* cond = dyn_cast<Block>(&thn.getPredecessor(0)))
              return reduceIfThenElse(*cond, thn, els, *node);
            fatal(error() << "Condition of an IfThenElse must be a block");
          }
        }
      }
    }
  }

  // Switch
  return false;
}

static void collectLoops(const Loop* loop, Set<const Loop*>& loops) {
  loops.insert(loop);
  for(Loop* subLoop : *loop)
    collectLoops(subLoop, loops);
}

bool StructureAnalysis::runOnFunction(const Function& f) {
  // Construct initial tree
  for(const BasicBlock& bb : f)
    nodeMap[&bb] = &newNode<Block>(bb);

  for(const BasicBlock& bb : f)
    for(const BasicBlock* succ : successors(&bb))
      nodeMap[&bb]->addSuccessor(*nodeMap[succ]);

  root.reset(new Entry(*nodeMap[&f.getEntryBlock()]));
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
    reduced = reduceAcyclicRegion(postorder);
    if(reduced)
      postorder = dfs(root->getEntry());
    {
      errs() << "#" << cfg << "\n";
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
