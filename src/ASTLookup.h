#ifndef CISH_AST_LOOKUP_H
#define CISH_AST_LOOKUP_H

#include "Map.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class DefUse;

// Parent child and structure-aware relationships of the clang::Stmt's for
// a function
class ASTLookup {
protected:
  const DefUse& du;
  Map<clang::Stmt*, clang::CompoundStmt*> parents;
  Map<clang::CompoundStmt*, clang::Stmt*> constructs;

protected:
  void associateStmts(clang::CompoundStmt* body, clang::Stmt* construct);

public:
  ASTLookup(const DefUse& du);
  ASTLookup(const ASTLookup&) = delete;
  ASTLookup(ASTLookup&&) = delete;

  clang::CompoundStmt* getCompoundStmtFor(clang::Stmt* stmt) const;
  clang::Stmt* getConstructFor(clang::Stmt* stmt) const;

  void runOnFunction(clang::FunctionDecl* f);
};

} // namespace cish

#endif // CISH_AST_LOOKUP_H
