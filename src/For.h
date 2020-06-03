#ifndef CISH_FOR_H
#define CISH_FOR_H

#include "ASTBase.h"

#include <llvm/Analysis/LoopInfo.h>

namespace cish {

class For : public ASTBase {
protected:
  const llvm::Loop& loop;

public:
  For(Context& ctxt, const llvm::Loop& loop);
  For(const For&) = delete;
  For(For&&) = delete;
  virtual ~For() = default;

  const llvm::Loop& getLLVM() const;

public:
  static bool classof(const ASTBase* base) {
    return base->getKind() == ASTKind::For;
  }
};

} // namespace cish

#endif // CISH_FOR_H
