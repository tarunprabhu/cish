#include "Context.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

Context::Context() : nextVarSuffix(0) {
  ;
}

std::string Context::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << "c__";
  if(prefix.length())
    ss << prefix << "_";
  ss << nextVarSuffix;

  nextVarSuffix++;

  return ss.str();
}

const Type& Context::add(llvm::Type* type, const std::string& s) {
  types[type].reset(new Type(*this, type, s));
  return get(type);
}

Expr& Context::overwrite(const llvm::Value* val, const std::string& name) {
  ovrs[val] = std::move(exprs.at(val));
  return add<Expr>(val, name);
}

Expr& Context::overwrite(const llvm::Value& val, const std::string& name) {
  return overwrite(&val, name);
}

template <>
Decl& Context::get(const llvm::Value* val) {
  return *decls.at(val);
}

template <>
Expr& Context::get(const llvm::Value* val) {
  return *exprs.at(val);
}

template <>
For& Context::get(const llvm::Loop* loop) {
  return *fors.at(loop);
}

template <>
If& Context::get(const llvm::Value* val) {
  return *ifs.at(val);
}

template <>
Stmt& Context::get(const llvm::Value* val) {
  return *stmts.at(val);
}

template <>
While& Context::get(const llvm::Loop* loop) {
  return *whiles.at(loop);
}

template <>
bool Context::has<Decl>(const llvm::Value* val) const {
  return decls.find(val) != decls.end();
}

template <>
bool Context::has<Expr>(const llvm::Value* val) const {
  return exprs.find(val) != exprs.end();
}

template <>
bool Context::has<For>(const llvm::Loop* loop) const {
  return fors.find(loop) != fors.end();
}

template <>
bool Context::has<If>(const llvm::Value* val) const {
  return ifs.find(val) != ifs.end();
}

template <>
bool Context::has<Stmt>(const llvm::Value* val) const {
  return stmts.find(val) != stmts.end();
}

bool Context::has(llvm::Type* type) const {
  return types.find(type) != types.end();
}

template <>
bool Context::has<While>(const llvm::Loop* loop) const {
  return whiles.find(loop) != whiles.end();
}

bool Context::isOverwrite(const llvm::Value* val) const {
    return ovrs.find(val) != ovrs.end();
}

bool Context::isOverwrite(const llvm::Value& val) const {
  return isOverwrite(&val);
}

const Expr& Context::getOverwrite(const llvm::Value* val) const {
  return *ovrs.at(val);
}

const Expr& Context::getOverwrite(const llvm::Value& val) const {
  return getOverwrite(&val);
}

template <>
const Decl& Context::get(const llvm::Value* val) const {
  return *decls.at(val);
}

template <>
const Expr& Context::get(const llvm::Value* val) const {
  return *exprs.at(val);
}

template <>
const For& Context::get(const llvm::Loop* loop) const {
  return *fors.at(loop);
}

template <>
const If& Context::get(const llvm::Value* val) const {
  return *ifs.at(val);
}

template <>
const Stmt& Context::get(const llvm::Value* val) const {
  return *stmts.at(val);
}

const Type& Context::get(llvm::Type* type) const {
  return *types.at(type);
}

template <>
const While& Context::get(const llvm::Loop* loop) const {
  return *whiles.at(loop);
}

} // namespace cish
