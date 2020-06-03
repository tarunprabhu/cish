#include "ASTBase.h"
#include "Context.h"

namespace cish {

ASTBase::ASTBase(Context& ctxt, ASTKind kind) : ctxt(ctxt), kind(kind) {
  ;
}

ASTBase::ASTBase(Context& ctxt, ASTKind kind, const std::string& s)
    : ctxt(ctxt), kind(kind), buf(s) {
  ;
}

Context& ASTBase::getContext() {
  return ctxt;
}

const Context& ASTBase::getContext() const {
  return ctxt;
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
