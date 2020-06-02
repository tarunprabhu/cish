#ifndef CISH_STMT_H
#define CISH_STMT_H

#include "Expr.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

class Stmt : public ASTBase {
protected:
  const llvm::Value& val;

public:
  Stmt() = delete;
  Stmt(const Stmt&) = delete;
  Stmt(Stmt&&) = delete;

  template <typename... T>
  Stmt(const llvm::Value& val, T&&... exprs)
      : ASTBase(ASTKind::Stmt), val(val) {
    this->add(exprs..., std::string(";"));
  }

  const llvm::Value& getLLVM() const;

public:
  static bool classof(const ASTBase* b) {
    return b->getKind() == ASTKind::Stmt;
  }
};

} // namespace cish

#endif // CISH_STMT_H
