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

#include "AST.h"
#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "Set.h"

using namespace clang;

namespace cish {

class ASTDefUseCalculatorPass
    : public ASTFunctionPass<ASTDefUseCalculatorPass> {
protected:
  void findDefd(Expr* expr, Stmt* user) {
    if(VarDecl* var = getVar(expr))
      ast->addDef(var, user);
  }

  void findUsed(Expr* expr, Stmt* user) {
    if(VarDecl* var = getVar(expr))
      ast->addUse(var, user);
    else
      ast->addExprUse(expr, user);
  }

  // For statements may have multiple defs and statements in the init and
  // increment parts
  void process(BinaryOperator* binOp, ForStmt* user) {
    if(binOp->getOpcode() == BO_Assign)
      ast->addTopLevelDef(getVar(binOp->getLHS()), user);
    else
      ast->addTopLevelUse(binOp->getLHS(), user);
    ast->addTopLevelUse(binOp->getRHS(), user);
  }

public:
  bool process(ConditionalOperator* condOp) {
    findUsed(condOp->getCond(), condOp);
    findUsed(condOp->getTrueExpr(), condOp);
    findUsed(condOp->getFalseExpr(), condOp);

    return false;
  }

  bool process(BinaryOperator* binOp) {
    // If the parent of a BinaryOperator is a CompoundStatement, then it
    // must be a top-level statement. The only top-level BinaryOperator
    // expressions must be assignments
    if(isa<CompoundStmt>(ast->getParent(binOp))) {
      if(binOp->getOpcode() == BO_Assign) {
        ast->addTopLevelDef(getVar(binOp->getLHS()), binOp);
        ast->addTopLevelUse(binOp->getRHS(), binOp);
      } else {
        fatal(error() << "Unexpected top-level binary operator: "
                      << binOp->getOpcodeStr());
      }
    }

    if(binOp->getOpcode() == BO_Assign) {
      findDefd(binOp->getLHS(), binOp);
      findUsed(binOp->getRHS(), binOp);
    } else if(binOp->getOpcode() == BO_Comma) {
      // Comma operators won't be used anywhere except initializers and
      // increments for a ForStmt but each expression in the comma may
      // contain expressions
      for(BinaryOperator* binOp : getCommaExprs(binOp))
        process(binOp);
    } else {
      findUsed(binOp->getLHS(), binOp);
      findUsed(binOp->getRHS(), binOp);
    }

    return false;
  }

  bool process(UnaryOperator* unOp) {
    findUsed(unOp->getSubExpr(), unOp);

    return false;
  }

  bool process(CallExpr* callExpr) {
    // FIXME: Call expressions may have variables passed by pointer/reference
    // which might get mutated in the call. Not sure whether or not they should
    // be considered defs

    // Some calls may be top-level. They won't return a value. Any call that
    // returns a value will be assigned to some variable and will be treated
    // as the operand to a potentially top-level BinaryOperator
    if(isa<CompoundStmt>(ast->getParent(callExpr)))
      ast->addTopLevelUse(callExpr, callExpr);

    findUsed(callExpr->getCallee(), callExpr);
    for(Expr* arg : callExpr->arguments())
      findUsed(arg, callExpr);

    return false;
  }

  bool process(CStyleCastExpr* castExpr) {
    findUsed(castExpr->getSubExpr(), castExpr);

    return false;
  }

  bool process(ArraySubscriptExpr* arrExpr) {
    findUsed(arrExpr->getBase(), arrExpr);
    findUsed(arrExpr->getIdx(), arrExpr);

    return false;
  }

  bool process(MemberExpr* memberExpr) {
    findUsed(memberExpr->getBase(), memberExpr);

    return false;
  }

  bool process(InitListExpr* initList) {
    for(Expr* expr : initList->inits())
      findUsed(expr, initList);

    return false;
  }

  bool process(SwitchStmt* switchStmt) {
    findUsed(switchStmt->getCond(), switchStmt);

    return false;
  }

  bool process(ReturnStmt* retStmt) {
    if(Expr* expr = retStmt->getRetValue())
      ast->addTopLevelUse(expr, retStmt);

    return false;
  }

  bool process(IfStmt* ifStmt) {
    ast->addTopLevelUse(ifStmt->getCond(), ifStmt);

    return false;
  }

  bool process(DoStmt* doStmt) {
    ast->addTopLevelUse(doStmt->getCond(), doStmt);

    return false;
  }

  bool process(ForStmt* forStmt) {
    for(BinaryOperator* binOp : getForInits(forStmt))
      process(binOp, forStmt);

    ast->addTopLevelUse(forStmt->getCond(), forStmt);

    for(BinaryOperator* binOp : getForIncs(forStmt))
      process(binOp, forStmt);

    return false;
  }

  bool process(WhileStmt* whileStmt) {
    ast->addTopLevelUse(whileStmt->getCond(), whileStmt);

    return false;
  }

public:
  ASTDefUseCalculatorPass(CishContext& context)
      : ASTFunctionPass<ASTDefUseCalculatorPass>(context) {
    ;
  }

  ASTDefUseCalculatorPass(const ASTDefUseCalculatorPass&) = delete;
  ASTDefUseCalculatorPass(ASTDefUseCalculatorPass&&) = delete;
  virtual ~ASTDefUseCalculatorPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-defuse";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Def Use Pass";
  }
};

} // namespace cish

cish::ASTPass* createASTDefUseCalculatorPass(cish::CishContext& cishContext) {
  return new cish::ASTDefUseCalculatorPass(cishContext);
}
