#ifndef CISH_STRUCTURE_ANALYSIS_H
#define CISH_STRUCTURE_ANALYSIS_H

#include <llvm/ADT/GraphTraits.h>
#include <llvm/ADT/SmallSet.h>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Analysis/LoopInfo.h>
#include <llvm/Analysis/PostDominators.h>
#include <llvm/Analysis/ScalarEvolution.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Function.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

#include "DerefIter.h"
#include "List.h"
#include "Map.h"
#include "Set.h"
#include "Vector.h"

namespace cish {

class StructNode;

struct Edge {
  StructNode& head;
  StructNode& tail;

  // The instruction that associated with this edge.
  const llvm::Instruction* inst;

  // This is set if the edge is conditionally taken as a result
  // of a branch (ways = 2) or a switch (ways = n). If a 2-way branch, a value
  // of 0 is for the false branch and 1 for the true branch. For a switch,
  // this will be set from an llvm::SwitchInst and will be same as the
  // llvm::CaseHandle for a given case
  const llvm::ConstantInt* condition;

  // Either both inst and condition are null, or neither is.
  Edge(StructNode& head,
       StructNode& tail,
       const llvm::Instruction* inst = nullptr,
       const llvm::ConstantInt* condition = nullptr)
      : head(head), tail(tail), inst(inst), condition(condition) {
    ;
  }
};

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
    S_DoWhileLoop,
    S_EndlessLoop,
    S_NaturalLoop,
    S_IfThen,
    S_IfThenElse,
    S_IfThenBreak,
    S_IfThenContinue,
    S_Latch,
    S_Switch
  };

  enum Flags {
    F_Preheader = 0x1,
    F_Header = 0x2,
    F_Exiting = 0x4,
    F_Exit = 0x8,
  };

public:
  template <typename IteratorT>
  class pred_iterator_t : public IteratorT {
  public:
    pred_iterator_t() : IteratorT() {}
    pred_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator->() {
      return IteratorT::operator->()->head;
    }
    auto& operator*() {
      return IteratorT::operator*().head;
    }
  };

  template <typename IteratorT>
  class succ_iterator_t : public IteratorT {
  public:
    succ_iterator_t() : IteratorT() {}
    succ_iterator_t(IteratorT i) : IteratorT(i) {}
    auto& operator->() {
      return IteratorT::operator->()->tail;
    }
    auto& operator*() {
      return IteratorT::operator*().tail;
    }
  };

private:
  Kind kind;
  List<Edge> in;
  List<Edge> out;
  unsigned flags;

public:
  using const_edge_iterator = decltype(out)::const_iterator;
  using const_pred_iterator
      = pred_iterator_t<decltype(in)::const_iterator>;
  using const_succ_iterator
      = succ_iterator_t<decltype(out)::const_iterator>;

protected:
  StructNode(Kind kind);

  void setFlag(StructNode::Flags flag);
  bool hasFlag(StructNode::Flags flag) const;

public:
  StructNode(const StructNode&) = delete;
  StructNode(StructNode&&) = delete;
  virtual ~StructNode() = default;

  llvm::StringRef getKindName() const;
  std::string getFlagNames() const;
  Kind getKind() const;

  void setFlags(unsigned flags);
  void setPreheader();
  void setHeader();
  void setExiting();
  void setExit();
  StructNode& addSuccessor(StructNode& succ,
                           const llvm::Instruction* inst = nullptr,
                           const llvm::ConstantInt* cond = nullptr);
  StructNode& addPredecessor(StructNode& pred,
                             const llvm::Instruction* inst = nullptr,
                             const llvm::ConstantInt* cond = nullptr);
  StructNode& removeSuccessor(StructNode& succ);
  StructNode& removePredecessor(StructNode& pred);
  StructNode& replacePredecessor(StructNode& old, StructNode& neew);
  StructNode& replaceSuccessor(StructNode& old, StructNode& neew);
  StructNode& disconnect();

  unsigned getFlags() const;
  bool hasPreheader() const;
  bool hasHeader() const;
  bool hasExiting() const;
  bool hasExit() const;
  bool hasPredecessor(StructNode& node) const;
  bool hasSuccessor(StructNode& node) const;
  size_t getNumPredecessors() const;
  size_t getNumSuccessors() const;
  StructNode& getSuccessor(size_t at) const;
  StructNode& getPredecessor(size_t at) const;
  const Edge& getIncomingEdge(StructNode& pred) const;
  const Edge& getOutgoingEdge(StructNode& succ) const;
  Vector<StructNode*> getPredecessors() const;
  Vector<StructNode*> getSuccessors() const;

  llvm::iterator_range<const_edge_iterator> incoming() const;
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

class DoWhileLoop : public LoopBase {
private:
  const llvm::CmpInst* cond;

public:
  DoWhileLoop(StructNode& header, StructNode& exit);
  DoWhileLoop(const StructNode&) = delete;
  DoWhileLoop(StructNode&&) = delete;
  virtual ~DoWhileLoop() = default;

  const llvm::CmpInst& getCondition() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_DoWhileLoop;
  }
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
public:
  NaturalLoop(StructNode& header);
  NaturalLoop(const StructNode&) = delete;
  NaturalLoop(StructNode&&) = delete;
  virtual ~NaturalLoop() = default;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_NaturalLoop;
  }
};

class Latch : public StructNode {
protected:
  DoWhileLoop& loop;

public:
  Latch(DoWhileLoop& loop);
  virtual ~Latch() = default;

  const DoWhileLoop& getLoop() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_Latch;
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
class IfThenBreak : public IfThenBase {
private:
  bool inverted;

public:
  IfThenBreak(Block& cond, StructNode& thn, bool inverted);
  IfThenBreak(const IfThenBreak&) = delete;
  IfThenBreak(IfThenBreak&&) = delete;
  virtual ~IfThenBreak() = default;

  bool isInverted() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThenBreak;
  }
};

// Special case of an if statement that only occurs within a loop and if the
// branch if taken, has a (possible empty) block and continues to the loop
// header
class IfThenContinue : public IfThenBase {
private:
  DoWhileLoop& loop;

public:
  IfThenContinue(Block& cond, StructNode& then, DoWhileLoop& loop);
  virtual ~IfThenContinue() = default;

  const DoWhileLoop& getLoop() const;

public:
  static bool classof(const StructNode* ct) {
    return ct->getKind() == StructNode::S_IfThenContinue;
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

class StructureAnalysis;

} // namespace cish

class StructureAnalysisWrapperPass : public llvm::FunctionPass {
public:
  static char ID;

private:
  bool structured;
  std::unique_ptr<cish::StructureAnalysis> ctree;

public:
  StructureAnalysisWrapperPass();

  bool isStructured() const;
  const cish::StructNode& getStructured() const;

  virtual llvm::StringRef getPassName() const override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
  virtual bool runOnFunction(llvm::Function& f) override;
};

#endif // CISH_STRUCTURE_ANALYSIS_H
