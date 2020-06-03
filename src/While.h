#ifndef CISH_WHILE_H
#define CISH_WHILE_H

#include "ASTBase.h"

#include <llvm/Analysis/LoopInfo.h>

namespace cish {

class While : public ASTBase {
protected:
  const llvm::Loop& loop;

public:
  While(Context& ctxt, const llvm::Loop& loop);
  While(const While&) = delete;
  While(While&&) = delete;
  virtual ~While() = default;

  const llvm::Loop& getLLVM() const;

public:
  static bool classof(const ASTBase* base) {
    return base->getKind() == ASTKind::While;
  }
};

} // namespace cish

#endif // CISH_WHILE_H
