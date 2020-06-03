#ifndef CISH_TYPE_H
#define CISH_TYPE_H

#include "ASTBase.h"

#include <llvm/IR/Type.h>

/// The C representation of a llvm::Type
namespace cish {

class Type : public ASTBase {
protected:
  llvm::Type* type;

public:
  Type() = delete;
  Type(const Type&) = delete;
  Type(Type&&) = delete;
  virtual ~Type() = default;

  /// @param ctxt The Context object containing all the AST nodes
  /// @param type The llvm::Type that this represents
  /// @param s The string representation of the type
  Type(Context& ctxt, llvm::Type* type, const std::string& s);

  /// @returns The llvm::Type that this represents
  llvm::Type* getLLVM() const;

public:
  static bool classof(const ASTBase* b) {
    return b->getKind() == ASTKind::Type;
  }
};

} // namespace cish

#endif // CISH_TYPE_H
