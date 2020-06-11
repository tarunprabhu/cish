#include "BackendBase.h"
#include "CishContext.h"

using namespace clang;

namespace cish {

BackendBase::BackendBase(CishContext& context)
    : context(context), astContext(context.getASTContext()),
      varPrefix(context.getFormatOptions().prefix), varSuffix(0) {
  ;
}

std::string BackendBase::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix;
  if(prefix.length())
    ss << prefix << "_";
  ss << varSuffix;

  varSuffix++;

  return ss.str();
}

void BackendBase::beginFunction(FunctionDecl*) {
  stmts.clear();
  varSuffix = 0;
}

void BackendBase::endFunction(FunctionDecl* f) {
  CompoundStmt* body = CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmts), invLoc, invLoc);
  f->setBody(body);
}

void BackendBase::beginBlock(LabelDecl* label) {
  stmts.push_back(new(astContext) LabelStmt(invLoc, label, nullptr));
}

void BackendBase::endBlock(LabelDecl*) {
  // Nothing to do here
}

} // namespace cish
