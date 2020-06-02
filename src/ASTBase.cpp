#include "ASTBase.h"

namespace cish {

ASTBase::ASTBase(ASTKind kind) : kind(kind) {
  ;
}

ASTBase::ASTBase(ASTKind kind, const std::string& s) : kind(kind), buf(s) {
  ;
}

ASTKind ASTBase::getKind() const {
  return kind;
}

ASTBase::operator std::string() const {
  return buf;
}

const std::string& ASTBase::str() const {
  return buf;
}

} // namespace cish
