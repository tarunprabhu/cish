#ifndef CISH_EXPR_H
#define CISH_EXPR_H

#include "ASTBase.h"

#include <llvm/IR/Value.h>
#include <llvm/Support/raw_ostream.h>

namespace cish {

/// This class sort of represents a C expression. It usually wraps a single
/// llvm::Value or llvm::Type. It is a separate class because it helps with
/// parenthetization. Doing it the naive way means too many, and too few
/// parentheses may actually suggest incorrect compilation
class Expr : public ASTBase {
private:
  const llvm::Value& val;

  // Some expressions may need to be parenthetized in certain contexts but
  // not in others. The default expression is not parenthetized. This keeps
  // the parenthetized version around if we ever need it
  std::string parenthetized;

public:
  Expr() = delete;
  Expr(const Expr&) = delete;
  Expr(Expr&&) = delete;
  virtual ~Expr() = default;

  template <typename... T>
  Expr(const llvm::Value& val, T&&... exprs)
      : ASTBase(ASTKind::Expr), val(val) {
    this->add(exprs...);
    llvm::raw_string_ostream ss(parenthetized);
    ss << "(" << str() << ")";
    ss.flush();
  }

  /// returns The LLVM value that this Expr represents
  const llvm::Value& getLLVM() const;

  /// returns A reference to the parenthetized representation of the Expr
  const std::string& getParenthetized() const;

public:
  static bool classof(const ASTBase* b) {
    return b->getKind() == ASTKind::Expr;
  }
};

} // namespace cish

#endif // CISH_EXPR_H
