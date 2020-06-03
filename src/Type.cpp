#include "Type.h"

namespace cish {

Type::Type(Context& ctxt, llvm::Type* type, const std::string& s)
    : ASTBase(ctxt, ASTKind::Type, s), type(type) {
  ;
}

llvm::Type* Type::getLLVM() const {
  return type;
}

} // namespace cish
