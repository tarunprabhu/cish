#include "Type.h"

namespace cish {

Type::Type(llvm::Type* type, const std::string& s)
    : ASTBase(ASTKind::Type, s), type(type) {
  ;
}

llvm::Type* Type::getLLVM() const {
  return type;
}

} // namespace cish
