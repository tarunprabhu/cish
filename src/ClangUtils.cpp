//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

#include "ClangUtils.h"
#include "ASTStreamer.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class FindVarsInStmt : public RecursiveASTVisitor<FindVarsInStmt> {
protected:
  Set<VarDecl*>& vars;

public:
  explicit FindVarsInStmt(Set<VarDecl*>& vars) : vars(vars) {
    ;
  }

  bool VisitDeclRefExpr(DeclRefExpr* ref) {
    if(auto* var = dyn_cast<VarDecl>(ref->getFoundDecl()))
      if(not isa<ParmVarDecl>(var))
        vars.insert(var);
    return true;
  }
};

const Type* getBaseType(const ArrayType* aty) {
  const Type* ety = aty->getElementType().getTypePtr();
  if(const auto* bty = dyn_cast<ArrayType>(ety))
    return getBaseType(bty);
  return ety;
}

Expr* stripCasts(Expr* expr) {
  if(auto* cst = dyn_cast<CastExpr>(expr))
    return stripCasts(cst->getSubExpr());
  return expr;
}

Vector<VarDecl*> getVarsInStmtAsVector(Stmt* stmt) {
  Set<VarDecl*> vars = getVarsInStmt(stmt);
  return Vector<VarDecl*>(vars.begin(), vars.end());
}

Set<VarDecl*> getVarsInStmt(Stmt* stmt) {
  Set<VarDecl*> vars;
  FindVarsInStmt(vars).TraverseStmt(stmt);
  return vars;
}

std::string toString(Stmt* stmt, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << stmt;

  return ss.str();
}

std::string toString(FunctionDecl* f, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << f;

  return ss.str();
}

} // namespace cish
