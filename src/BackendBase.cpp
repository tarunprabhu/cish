#include "BackendBase.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"
#include "Options.h"

using namespace clang;

namespace cish {

BackendBase::BackendBase(CishContext& context)
    : varPrefix(opts().prefix), varSuffix(0),
      astContext(context.getASTContext()), ast(astContext) {
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

Expr* BackendBase::replace(Expr* src, Expr* old, Expr* repl) {
  if(src == old)
    return repl;
  else if(auto* castExpr = dyn_cast<CStyleCastExpr>(src))
    return ast.createCastExpr(replace(castExpr->getSubExpr(), old, repl),
                              castExpr->getType());
  else if(auto* unOp = dyn_cast<UnaryOperator>(src))
    return ast.createUnaryOperator(replace(unOp->getSubExpr(), old, repl),
                                   unOp->getOpcode(),
                                   unOp->getType());
  fatal(error() << "Unknown expression in which to replace: "
                << src->getStmtClassName());
}

} // namespace cish
