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

#include "StructureTree.h"
#include "Diagnostics.h"
#include "Map.h"

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
  case S_LoopLatch:
    return "Latch";
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
  case S_ForLoop:
    return "ForLoop";
  case S_SimplifiedLoop:
    return "SimplifiedLoop";
  case S_Switch:
    return "Switch";
  default:
    return "Other";
  };
}

StructNode& StructNode::addPredecessor(const ConstantInt* kase,
                                       StructNode& pred) {
  if(not hasPredecessor(pred)) {
    in.push_back(&pred);
    pred.addSuccessor(kase, *this);
  }
  return *this;
}

StructNode& StructNode::addSuccessor(const ConstantInt* kase,
                                     StructNode& succ) {
  bool hasSuccessor = false;
  if(not kase) {
    for(const Edge& e : out)
      if((not e.first) and (*e.second == succ))
        hasSuccessor = true;
  } else {
    for(const Edge& e : out)
      if(e.first and (e.first->getLimitedValue() == kase->getLimitedValue())
         and (*e.second == succ))
        hasSuccessor = true;
  }
  if(not hasSuccessor) {
    out.emplace_back(kase, &succ);
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

const ConstantInt* StructNode::getSuccessorCase(const StructNode& succ) const {
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
  for(StructNode& node : successors())
    if(node == succ)
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
    fatal(error() << "No unique successor for " << getId() << ": "
                  << out.size());
  return *out.begin()->second;
}

StructNode& StructNode::getSuccessor(const ConstantInt* kase) const {
  if(not kase) {
    for(const std::pair<const ConstantInt*, StructNode*>& p : out)
      if(not p.first)
        return *p.second;
  } else {
    for(const std::pair<const ConstantInt*, StructNode*>& p : out)
      if(p.first and (p.first->getLimitedValue() == kase->getLimitedValue()))
        return *p.second;
  }
  fatal(error() << "No successor for " << *kase << " in " << getId());
}

Vector<std::pair<const ConstantInt*, StructNode*>>
StructNode::getIncoming() const {
  Vector<std::pair<const ConstantInt*, StructNode*>> ret;
  for(StructNode* pred : in)
    ret.emplace_back(pred->getSuccessorCase(*this), pred);
  return ret;
}

const Vector<std::pair<const ConstantInt*, StructNode*>>&
StructNode::getOutgoing() const {
  return out;
}

Vector<StructNode*> StructNode::getPredecessors() const {
  return Vector<StructNode*>(in.begin(), in.end());
}

Vector<StructNode*> StructNode::getSuccessors() const {
  Vector<StructNode*> ret;
  for(const auto& i : out)
    ret.push_back(i.second);
  return ret;
}

iterator_range<StructNode::const_edge_iterator> StructNode::outgoing() const {
  return iterator_range<const_edge_iterator>(out.begin(), out.end());
}

iterator_range<StructNode::const_pred_iterator>
StructNode::predecessors() const {
  return iterator_range<const_pred_iterator>(in.begin(), in.end());
}

iterator_range<StructNode::const_succ_iterator> StructNode::successors() const {
  return iterator_range<const_succ_iterator>(const_succ_iterator(out.begin()),
                                             const_succ_iterator(out.end()));
}

bool StructNode::operator==(const StructNode& other) const {
  return this == &other;
}

bool StructNode::operator!=(const StructNode& other) const {
  return this != &other;
}

Entry::Entry(llvm::LLVMContext& llvmContext, StructNode& entry)
    : StructNode(StructNode::S_Entry) {
  addSuccessor(llvm::ConstantInt::getTrue(llvmContext), entry);
}

StructNode& Entry::getEntry() const {
  return getSuccessor();
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
                         bool invert,
                         bool unique)
    : StructNode(StructNode::S_IfThenBreak), exit(exit), br(br), invert(invert),
      unique(unique) {
  ;
}

bool IfThenBreak::isInverted() const {
  return invert;
}

bool IfThenBreak::isInLoopWithUniqueExit() const {
  return unique;
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

LoopLatch::LoopLatch(const BasicBlock& block)
    : StructNode(S_LoopLatch), block(block) {
  ;
}

const BasicBlock& LoopLatch::getBlock() const {
  return block;
}

Switch::Case::Case(const ConstantInt& value, StructNode* dest, bool fallthrough)
    : value(value), dest(dest), fallthrough(fallthrough) {
  ;
}

const ConstantInt& Switch::Case::getValue() const {
  return value;
}

const StructNode& Switch::Case::getBody() const {
  return *dest;
}

bool Switch::Case::isEmpty() const {
  return not dest;
}

bool Switch::Case::isFallthrough() const {
  return fallthrough;
}

Switch::Switch(const SwitchInst& sw,
               const Vector<Case>& cases,
               StructNode* deflt)
    : StructNode(StructNode::S_Switch), sw(sw), deflt(deflt) {
  // The order of the edges in the graph may have been changed during the
  // reduction. But keep to the order in the LLVM instruction
  Map<const ConstantInt*, const Case*> cmap;
  for(const Case& kase : cases)
    cmap[&kase.getValue()] = &kase;
  for(const auto& i : sw.cases()) {
    const ConstantInt* c = i.getCaseValue();
    if(cmap.contains(c))
      this->cases.push_back(*cmap[i.getCaseValue()]);
  }
}

const SwitchInst& Switch::getLLVM() const {
  return sw;
}

bool Switch::hasDefault() const {
  return deflt;
}

const StructNode& Switch::getDefault() const {
  return *deflt;
}

const Vector<Switch::Case>& Switch::getCases() const {
  return cases;
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

ForLoop::ForLoop(LoopHeader& header, IfThenBreak& brk, LoopLatch& latch)
    : LoopBase(StructNode::S_ForLoop, header), brk(brk), latch(latch) {
  ;
}

const BranchInst& ForLoop::getLLVMBranchInst() const {
  return brk.getLLVMBranchInst();
}

bool ForLoop::isInverted() const {
  return brk.isInverted();
}

const LoopLatch& ForLoop::getLatch() const {
  return latch;
}

SimplifiedLoop::SimplifiedLoop(LoopHeader& header,
                               const List<IfThenBreak*>& exits)
    : LoopBase(StructNode::S_SimplifiedLoop, header), exits(exits) {
  StructNode* curr = &header;
  do {
    nodes.push_back(curr);
    if(auto* iftb = dyn_cast<IfThenBreak>(curr))
      curr = &iftb->getContinue();
    else
      curr = &curr->getSuccessor();
  } while(curr != &header);
}

const List<IfThenBreak*>& SimplifiedLoop::getExits() const {
  return exits;
}

} // namespace cish
