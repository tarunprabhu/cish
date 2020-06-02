#include "Context.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

Context::Context() : nextVarSuffix(0) {
  ;
}

template <>
bool Context::has<Expr>(const llvm::Value* val) const {
  return exprs.find(val) != exprs.end();
}

template <>
bool Context::has<Stmt>(const llvm::Value* val) const {
  return stmts.find(val) != stmts.end();
}

template <>
bool Context::has<Decl>(const llvm::Value* val) const {
  return decls.find(val) != decls.end();
}

bool Context::has(llvm::Type* type) const {
  return types.find(type) != types.end();
}

template <>
const Expr& Context::get(const llvm::Value* val) const {
  return *exprs.at(val);
}

template <>
const Stmt& Context::get(const llvm::Value* val) const {
  return *stmts.at(val);
}

template <>
const Decl& Context::get(const llvm::Value* val) const {
  return *decls.at(val);
}

const Type& Context::get(llvm::Type* type) const {
  return *types.at(type);
}

const Type& Context::add(llvm::Type* type, const std::string& s) {
  types[type].reset(new Type(type, s));
  return get(type);
}

std::string Context::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << "c___";
  if(prefix.length())
    ss << prefix << "_";
  ss << nextVarSuffix;

  nextVarSuffix++;

  return ss.str();
}

} // namespace cish
