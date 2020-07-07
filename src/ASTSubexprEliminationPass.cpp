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

#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "NameGenerator.h"

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
protected:
  bool canPropagateSubexpr(VarDecl* var, Stmt* def) {
    // This is a bit of an optimization and may not always be useful so it may
    // be a good idea to provide a knob to control it.
    // The idea is that eliminating subexpressions where the variable is a
    // generated variable is not terribly useful. So only do it for those
    // variables that may have come from the source code
    NameGenerator& names = cishContext.getNameGenerator();
    if(names.isGeneratedName(var->getName()))
      return false;

    for(VarDecl* var : Clang::getVarsInStmt(def))
      if(not ast->hasSingleDef(var))
        return false;
    return true;
  }

public:
  bool process(FunctionDecl* f) {
    bool changed = false;

    Map<Expr*, VarDecl*> repl;
    for(VarDecl* lhs : ast->getVars())
      if(Expr* def = ast->getSingleDefRHS(lhs))
        if(ast->getEqvExprs(def).size() > 1)
          if(canPropagateSubexpr(lhs, def))
            repl[def] = lhs;

    for(auto& i : repl) {
      Expr* expr = i.first;
      Expr* repl = ast->createDeclRefExpr(i.second);
      llvm::errs() << Clang::toString(expr, astContext) << " => "
                   << Clang::toString(repl, astContext) << " |"
                   << ast->getEqvExprs(expr).size() << "|\n";
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
