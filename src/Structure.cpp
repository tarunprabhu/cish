#include "Structure.h"
#include "Diagnostics.h"

#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

StructNode::StructNode(Kind kind) : kind(kind) {
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
  case S_LoopHeader:
    return "Header";
  case S_Label:
    return "Label";
  case S_IfThen:
    return "IfThen";
  case S_IfThenElse:
    return "IfThenElse";
  case S_IfThenBreak:
    return "IfThenBreak";
  case S_IfThenGoto:
    return "IfThenGoto";
  case S_EndlessLoop:
    return "EndlessLoop";
  case S_NaturalLoop:
    return "NaturalLoop";
  case S_Switch:
    return "Switch";
  default:
    return "Other";
  };
}

StructNode& StructNode::addPredecessor(long kase, StructNode& pred) {
  if(not hasPredecessor(pred)) {
    in.push_back(&pred);
    pred.addSuccessor(kase, *this);
  }
  return *this;
}

StructNode& StructNode::addSuccessor(long kase, StructNode& succ) {
  if(not hasSuccessor(succ)) {
    out.emplace(kase, &succ);
    succ.addPredecessor(kase, *this);
  }

  return *this;
}

StructNode& StructNode::removePredecessor(StructNode& pred) {
  if(hasPredecessor(pred)) {
    for(auto i = in.begin(); i != in.end(); i++) {
      if(*i == &pred) {
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
      if(*i->second == succ) {
        out.erase(i);
        break;
      }
    }
    succ.removePredecessor(*this);
  }
  return *this;
}

StructNode& StructNode::disconnect() {
  for(StructNode* pred : getPredecessors())
    removePredecessor(*pred);
  for(StructNode* succ : getSuccessors())
    removeSuccessor(*succ);
  return *this;
}

unsigned long StructNode::getId() const {
  return (unsigned long)this;
}

long StructNode::getSuccessorCase(const StructNode& succ) const {
  for(auto i = out.begin(); i != out.end(); i++)
    if(*i->second == succ)
      return i->first;
  fatal(error() << "No successor " << succ.getId() << " for " << getId());
}

bool StructNode::hasPredecessor(const StructNode& pred) const {
  for(const StructNode* node : in)
    if(*node == pred)
      return true;
  return false;
}

bool StructNode::hasSuccessor(const StructNode& succ) const {
  for(StructNode* node : out.values())
    if(*node == succ)
      return true;
  return false;
}

size_t StructNode::getNumPredecessors() const {
  return in.size();
}

size_t StructNode::getNumSuccessors() const {
  return out.size();
}

StructNode& StructNode::getPredecessor() const {
  if(in.size() != 1)
    fatal(error() << "No unique predecessor: " << in.size());
  return *in.front();
}

StructNode& StructNode::getPredecessorAt(size_t n) const {
  return *in[n];
}

StructNode& StructNode::getSuccessor() const {
  if(out.size() != 1)
    fatal(error() << "No unique successor: " << out.size());
  return *out.begin()->second;
}

StructNode& StructNode::getSuccessor(long kase) const {
  return *out.at(kase);
}

Vector<std::pair<long, StructNode*>> StructNode::getIncoming() const {
  Vector<std::pair<long, StructNode*>> ret;
  for(StructNode* pred : in)
    ret.emplace_back(pred->getSuccessorCase(*this), pred);
  return ret;
}

const Map<long, StructNode*>& StructNode::getOutgoing() const {
  return out;
}

Vector<StructNode*> StructNode::getPredecessors() const {
  return Vector<StructNode*>(in.begin(), in.end());
}

Vector<StructNode*> StructNode::getSuccessors() const {
  return Vector<StructNode*>(out.values().begin(), out.values().end());
}

iterator_range<StructNode::const_edge_iterator> StructNode::outgoing() const {
  return iterator_range<const_edge_iterator>(out.values().begin(),
                                             out.values().end());
}

iterator_range<StructNode::const_pred_iterator>
StructNode::predecessors() const {
  return iterator_range<const_pred_iterator>(in.begin(), in.end());
}

iterator_range<StructNode::const_succ_iterator> StructNode::successors() const {
  return iterator_range<const_succ_iterator>(out.values().begin(),
                                             out.values().end());
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
  for(StructNode* node : seq)
    add(*node);
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
  ;
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

IfThenBreak::IfThenBreak(StructNode& exit,
                         const BranchInst& br,
                         unsigned loopDepth)
    : StructNode(StructNode::S_IfThenBreak), exit(exit), br(br),
      loopDepth(loopDepth) {
  ;
}

StructNode& IfThenBreak::getExit() {
  return exit;
}

const StructNode& IfThenBreak::getExit() const {
  return exit;
}

StructNode& IfThenBreak::getContinue() {
  for(StructNode& succ : successors())
    if(succ != exit)
      return succ;
  fatal(error() << "Could not find continuing node in IfThenBreak");
}

const StructNode& IfThenBreak::getContinue() const {
  for(const StructNode& succ : successors())
    if(succ != exit)
      return succ;
  fatal(error() << "Could not find continuing node in IfThenBreak");
}

const BranchInst& IfThenBreak::getLLVMBranchInst() const {
  return br;
}

unsigned IfThenBreak::getLoopDepth() const {
  return loopDepth;
}

Label::Label(const std::string& name)
    : StructNode(StructNode::S_Label), name(name) {
  ;
}

const std::string& Label::getName() const {
  return name;
}

IfThenGoto::IfThenGoto(Label& target, Block& block, bool invert)
    : StructNode(S_IfThenGoto), target(target), block(block), invert(invert) {
  ;
}

bool IfThenGoto::isInverted() const {
  return invert;
}

const Block& IfThenGoto::getBlock() const {
  return block;
}

const Label& IfThenGoto::getTarget() const {
  return target;
}

const BranchInst& IfThenGoto::getLLVMBranchInst() const {
  return cast<BranchInst>(block.getLLVM().back());
}

LoopHeader::LoopHeader(unsigned loopDepth)
    : StructNode(StructNode::S_LoopHeader), loopDepth(loopDepth) {
  ;
}

unsigned LoopHeader::getLoopDepth() const {
  return loopDepth;
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

EndlessLoop::EndlessLoop(StructNode& header)
    : cish::LoopBase(StructNode::S_EndlessLoop, header) {
  for(StructNode& node : header.successors())
    nodes.push_back(&node);
}

NaturalLoop::NaturalLoop(LoopHeader& header, const List<IfThenBreak*>& exits)
    : LoopBase(StructNode::S_NaturalLoop, header), exits(exits) {
  StructNode* curr = &header;
  do {
    nodes.push_back(curr);
    if(auto* iftb = dyn_cast<IfThenBreak>(curr))
      curr = &iftb->getContinue();
    else
      curr = &curr->getSuccessor();
  } while(curr != &header);
}

const List<IfThenBreak*>& NaturalLoop::getExits() const {
  return exits;
}

} // namespace cish
