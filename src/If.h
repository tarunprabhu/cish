#ifndef CISH_IF_H
#define CISH_IF_H

#include "ASTBase.h"

#include <llvm/IR/Instructions.h>

namespace cish {

class Context;

class If : public ASTBase {
protected:
  const llvm::BranchInst& br;

public:
  /// @param v The LLVM branch instruction that this if node represents
  /// @param then A node for block to jump to if the branch is taken
  /// @param els A node for the block to jump to if the branch is not taken
  If(Context& ctxt, const llvm::BranchInst& br);
  If(const If&) = delete;
  If(If&&) = delete;
  virtual ~If() = default;

  const llvm::BranchInst& getLLVM() const;

public:
  static bool classof(const ASTBase* base) {
    return base->getKind() == ASTKind::If;
  }
};

} // namespace cish

#endif // CISH_IF_H
