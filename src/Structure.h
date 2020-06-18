#ifndef CISH_STRUCTURE_H
#define CISH_STRUCTURE_H

#include <llvm/IR/Instructions.h>

#include "DerefIter.h"
#include "List.h"
#include "Map.h"
#include "Set.h"
#include "Vector.h"

namespace cish {

class StructNode;

// This is a slightly modified version of the structural analysis described in
// Muchnick's book. Because we have a dedicated switch instruction in LLVM
// and Loop information, we use those when possible
class StructNode {
public:
  enum Kind {
    S_Entry,
    S_Exit,
    S_Block,
    S_Sequence,
    S_LoopHeader,
    S_EndlessLoop,
    S_NaturalLoop,
    S_IfThen,
    S_IfThenElse,
    S_IfThenBreak,
    S_IfThenGoto,
    S_Label,
    S_Switch,
  };

public:
  template <typename IteratorT>
  class pred_iterator_t : public IteratorT {
  public:
    pred_iterator_t() : IteratorT() {}
    pred_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator->() {
      return IteratorT::operator->()->getHead();
    }
    auto& operator*() {
      return IteratorT::operator*().getHead();
    }
  };

  template <typename IteratorT>
  class succ_iterator_t : public IteratorT {
  public:
    succ_iterator_t() : IteratorT() {}
    succ_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator->() {
      return IteratorT::operator->()->getTail();
    }
    auto& operator*() {
      return IteratorT::operator*().getTail();
    }
  };

private:
  Kind kind;
  List<StructNode*> in;
  Map<long, StructNode*> out;

public:
  using const_edge_iterator = decltype(out)::const_iterator;
  using const_pred_iterator = DerefIter<typename decltype(in)::const_iterator>;
  using const_succ_iterator
      = DerefIter<typename decltype(out)::const_mapped_iterator>;

protected:
  StructNode(Kind kind);

  StructNode& addPredecessor(long kase, StructNode& pred);

public:
  StructNode(const StructNode&) = delete;
  StructNode(StructNode&&) = delete;
  virtual ~StructNode() = default;

  llvm::StringRef getKindName() const;
  Kind getKind() const;
  unsigned long getId() const;

  StructNode& addSuccessor(long kase, StructNode& succ);
  StructNode& removeSuccessor(StructNode& succ);
  StructNode& removePredecessor(StructNode& pred);
  StructNode& disconnect();

  long getSuccessorCase(const StructNode& succ) const;
  bool hasPredecessor(const StructNode& node) const;
  bool hasSuccessor(const StructNode& node) const;
  size_t getNumPredecessors() const;
  size_t getNumSuccessors() const;
  StructNode& getSuccessor() const;
  StructNode& getSuccessor(long kase) const;
  StructNode& getPredecessor() const;
  StructNode& getPredecessorAt(size_t) const;
  Vector<std::pair<long, StructNode*>> getIncoming() const;
  const Map<long, StructNode*>& getOutgoing() const;
  Vector<StructNode*> getPredecessors() const;
  Vector<StructNode*> getSuccessors() const;

  llvm::iterator_range<const_edge_iterator> outgoing() const;
  llvm::iterator_range<const_succ_iterator> successors() const;
  llvm::iterator_range<const_pred_iterator> predecessors() const;

  // The nodes are intended to be uniqued and their copy constructors have
  // been deleted. So a comparison of address sufficient for uniqueness
  bool operator==(const StructNode& other) const;
  bool operator!=(const StructNode& other) const;
};

// Just a wrapper around a basic block so it can be added to the tree
// This also keeps track of whether or not the block has been reduced
class Block : public StructNode {
private:
  const llvm::BasicBlock& bb;

public:
  Block(const llvm::BasicBlock& bb);
  Block(const Block&) = delete;
  Block(Block&&) = delete;
  virtual ~Block() = default;

  const llvm::BasicBlock& getLLVM() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_Block;
  }
};

class Sequence : public StructNode {
private:
  List<StructNode*> seq;

public:
  using const_iterator = DerefIter<decltype(seq)::const_iterator>;

public:
  Sequence(const List<StructNode*>& seq = {});
  Sequence(const Sequence&) = delete;
  Sequence(Sequence&&) = delete;
  virtual ~Sequence() = default;

  void add(StructNode& elem);

  const_iterator begin() const;
  const_iterator end() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_Sequence;
  }
};

class IfThenBase : public StructNode {
protected:
  Block& cond;
  StructNode& thn;

protected:
  IfThenBase(StructNode::Kind kind, Block& cond, StructNode& thn);

public:
  IfThenBase(const IfThenBase&) = delete;
  IfThenBase(IfThenBase&&) = delete;
  virtual ~IfThenBase() = default;

  const llvm::BranchInst& getLLVMBranchInst() const;
  const Block& getCond() const;
  const StructNode& getThen() const;
};

class IfThen : public IfThenBase {
protected:
  bool inverted;

public:
  IfThen(Block& cond, StructNode& thn, bool inverted = false);
  IfThen(const IfThen&) = delete;
  IfThen(IfThen&&) = delete;
  virtual ~IfThen() = default;

  bool isInverted() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThen;
  }
};

class IfThenElse : public IfThenBase {
private:
  StructNode& els;

public:
  IfThenElse(Block& cond, StructNode& then, StructNode& els);
  IfThenElse(const IfThenElse&) = delete;
  IfThenElse(IfThenElse&&) = delete;
  virtual ~IfThenElse() = default;

  const StructNode& getElse() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThenElse;
  }
};

// Special case of an if statement that only occurs within a loop and
// if the branch if taken, has a (possibly empty) block and exits the loop
class IfThenBreak : public StructNode {
private:
  StructNode& exit;
  const llvm::BranchInst& br;
  unsigned loopDepth;

public:
  IfThenBreak(StructNode& exit, const llvm::BranchInst& br, unsigned loopDepth);
  IfThenBreak(const IfThenBreak&) = delete;
  IfThenBreak(IfThenBreak&&) = delete;
  virtual ~IfThenBreak() = default;

  StructNode& getExit();
  StructNode& getContinue();
  const StructNode& getExit() const;
  const StructNode& getContinue() const;
  const llvm::BranchInst& getLLVMBranchInst() const;
  unsigned getLoopDepth() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThenBreak;
  }
};

// Target of a IfThenGoto node
class Label : public StructNode {
private:
  std::string name;

public:
  Label(const std::string& name);
  Label(const Label&) = delete;
  Label(Label&&) = delete;
  virtual ~Label() = default;

  const std::string& getName() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_Label;
  }
};

class IfThenGoto : public StructNode {
private:
  Label& target;
  Block& block;
  bool invert;

public:
  IfThenGoto(Label& target, Block& block, bool invert);
  IfThenGoto(const IfThenGoto&) = delete;
  IfThenGoto(IfThenGoto&&) = delete;
  virtual ~IfThenGoto() = default;

  bool isInverted() const;
  const Label& getTarget() const;
  const Block& getBlock() const;
  const llvm::BranchInst& getLLVMBranchInst() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThenGoto;
  }
};

// Empty node to represent a loop header. There should be no instructions
// in the loop header. It's only purpose is to serve as the destination
// of the loop backedges
class LoopHeader : public StructNode {
private:
  unsigned loopDepth;

public:
  LoopHeader(unsigned loopDepth);
  LoopHeader(const LoopHeader&) = delete;
  LoopHeader(LoopHeader&&) = delete;
  virtual ~LoopHeader() = default;

  unsigned getLoopDepth() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_LoopHeader;
  }
};

class LoopBase : public StructNode {
protected:
  List<StructNode*> nodes;
  StructNode& header;

public:
  using const_iterator = DerefIter<decltype(nodes)::const_iterator>;

protected:
  LoopBase(StructNode::Kind kind, StructNode& header);

public:
  LoopBase(const LoopBase&) = delete;
  LoopBase(LoopBase&&) = delete;
  virtual ~LoopBase() = default;

  StructNode& getHeader() const;

  bool isInLoop(StructNode& node) const;
  const_iterator begin() const;
  const_iterator end() const;
};

// Infinite loop
class EndlessLoop : public LoopBase {
public:
  EndlessLoop(StructNode& header);
  EndlessLoop(const StructNode&) = delete;
  EndlessLoop(StructNode&&) = delete;
  virtual ~EndlessLoop() = default;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_EndlessLoop;
  }
};

// Loop with multiple exits
class NaturalLoop : public LoopBase {
private:
  List<IfThenBreak*> exits;

public:
  using const_iterator = decltype(nodes)::const_iterator;

public:
  NaturalLoop(LoopHeader& header, const List<IfThenBreak*>& exits);
  NaturalLoop(const StructNode&) = delete;
  NaturalLoop(StructNode&&) = delete;
  virtual ~NaturalLoop() = default;

  const List<IfThenBreak*>& getExits() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_NaturalLoop;
  }
};

class Switch : public StructNode {
private:
public:
  Switch();
  virtual ~Switch() = default;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_Switch;
  }
};

} // namespace cish

#endif // CISH_STRUCTURE_H
