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

#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "ClangUtils.h"

using namespace clang;

namespace cish {

// This is the opposite of ASTExprPropagatePass. That pass looked at short
// expressions assigned to a temporary variable and propagated the expressions
// into the code. This looks at expressions in the code and if they match
// the RHS of a variable with a single definition, replaces the expression
// with the variable. This should be run after loop conversion and expression
// simplification passses because they are the ones that may introduce new
// expressions into the code
class ASTSubexprEliminationPass
    : public ASTFunctionPass<ASTSubexprEliminationPass> {
public:
  bool process(FunctionDecl* f) {
    bool changed = false;

    Map<Expr*, VarDecl*> repl;
    for(VarDecl* lhs : ast->vars()) {
      if(Expr* def = ast->getSingleDefRHS(lhs)) {
        if(ast->getEqvExprs(def).size() > 1) {
          bool replace = true;
          for(VarDecl* var : getVarsInStmt(def))
            if(not ast->hasSingleDef(var))
              replace = false;
          if(replace)
            repl[def] = lhs;
        }
      }
    }

    for(auto& i : repl) {
      Expr* expr = i.first;
      Expr* repl = builder.createDeclRefExpr(i.second);
      llvm::errs() << toString(expr, astContext) << " => "
                   << toString(repl, astContext) << "\n";
      changed |= ast->replaceEqvUsesWith(expr, repl);
    }

    return changed;
  }

public:
  ASTSubexprEliminationPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext) {
    ;
  }

  ASTSubexprEliminationPass(const ASTSubexprEliminationPass&) = delete;
  ASTSubexprEliminationPass(ASTSubexprEliminationPass&&) = delete;
  virtual ~ASTSubexprEliminationPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-cse";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish Subexpression Elimination";
  }
};

} // namespace cish

cish::ASTPass* createASTSubexprEliminationPass(cish::CishContext& cishContext) {
  return new cish::ASTSubexprEliminationPass(cishContext);
}
