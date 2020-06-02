#ifndef CISH_DECL_H
#define CISH_DECL_H

#include "ASTBase.h"

#include <llvm/IR/Value.h>

namespace cish {

class Decl : public ASTBase {
protected:
  const llvm::Value& val;

public:
  Decl(const llvm::Value& val, const std::string& s);
  Decl(const Decl&) = delete;
  Decl(Decl&&) = delete;
  virtual ~Decl() = default;

  template <typename... T>
  Decl(const llvm::Value& val, T&&... exprs)
      : ASTBase(ASTKind::Decl), val(val) {
    this->add(exprs...);
  }

  const llvm::Value& getLLVM() const;

public:
  static bool classof(const ASTBase* b) {
    return b->getKind() == ASTKind::Decl;
  }
};

} // namespace cish

#endif // CISH_DECL_H
