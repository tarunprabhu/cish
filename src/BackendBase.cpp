#include "BackendBase.h"
#include "CishContext.h"
#include "LLVMUtils.h"

using namespace clang;

namespace cish {

BackendBase::BackendBase(CishContext& context)
    : varPrefix(context.getFormatOptions().prefix), varSuffix(0),
      context(context), astContext(context.getASTContext()) {
  ;
}

std::string BackendBase::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix << prefix << varSuffix;

  varSuffix++;

  return ss.str();
}

void BackendBase::beginFunction(FunctionDecl*) {
  varSuffix = 0;
  stmts.emplace();
}

void BackendBase::endFunction(FunctionDecl*) {
  varSuffix = 0;
  stmts.clear();
}

Stmt* BackendBase::add(Stmt* stmt) {
  stmts.top().push_back(stmt);
  return stmt;
}

Stmt& BackendBase::add(Stmt& stmt) {
  return *add(&stmt);
}

} // namespace cish
