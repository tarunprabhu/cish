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
#include "Set.h"

using namespace clang;

namespace cish {

class ASTDefUseCalculatorPass
    : public ASTFunctionPass<ASTDefUseCalculatorPass> {
protected:
  const Set<clang::UnaryOperator::Opcode> defOps1;
  const Set<clang::BinaryOperator::Opcode> defOps2;

protected:
  void findDefd(Expr* expr, Stmt* user) {
    if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
      if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
        ast->addDef(var, user);
  }

  void findUsed(Expr* expr, Stmt* user) {
    if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
      if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
        ast->addUse(var, user);
  }

public:
  bool process(ConditionalOperator* condOp) {
    findUsed(condOp->getCond(), condOp);
    findUsed(condOp->getTrueExpr(), condOp);
    findUsed(condOp->getFalseExpr(), condOp);

    return false;
  }

  bool process(BinaryOperator* binOp) {
    if(defOps2.contains(binOp->getOpcode())) {
      findDefd(binOp->getLHS(), binOp);
      // If this is an OperatorAssign (+=, -= etc.), the LHS will be definitely
      // also be read
      if(binOp->getOpcode() != BO_Assign)
        findUsed(binOp->getLHS(), binOp);
    } else {
      findUsed(binOp->getLHS(), binOp);
    }
    findUsed(binOp->getRHS(), binOp);

    return false;
  }

  bool process(UnaryOperator* unOp) {
    Expr* expr = unOp->getSubExpr();
    if(defOps1.contains(unOp->getOpcode()))
      findDefd(expr, unOp);
    findUsed(expr, unOp);

    return false;
  }

  bool process(CallExpr* callExpr) {
    // FIXME: Call expressions may have variables passed by pointer/reference
    // which might get mutated in the call. Not sure whether or not they should
    // be considered defs
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
      findUsed(expr, retStmt);

    return false;
  }

  bool process(IfStmt* ifStmt) {
    findUsed(ifStmt->getCond(), ifStmt);

    return false;
  }

  bool process(DoStmt* doStmt) {
    findUsed(doStmt->getCond(), doStmt);

    return false;
  }

  bool process(ForStmt* forStmt) {
    findUsed(forStmt->getCond(), forStmt);

    return false;
  }

  bool process(WhileStmt* whileStmt) {
    findUsed(whileStmt->getCond(), whileStmt);

    return false;
  }

public:
  ASTDefUseCalculatorPass(CishContext& context)
      : ASTFunctionPass<ASTDefUseCalculatorPass>(context),
        defOps1({UO_PreInc, UO_PreDec, UO_PostInc, UO_PostDec}),
        defOps2({BO_Assign,
                 BO_AddAssign,
                 BO_SubAssign,
                 BO_MulAssign,
                 BO_DivAssign,
                 BO_RemAssign,
                 BO_ShlAssign,
                 BO_ShrAssign,
                 BO_AndAssign,
                 BO_OrAssign,
                 BO_XorAssign}) {
    ;
  }

  ASTDefUseCalculatorPass(const ASTDefUseCalculatorPass&) = delete;
  ASTDefUseCalculatorPass(ASTDefUseCalculatorPass&&) = delete;
  virtual ~ASTDefUseCalculatorPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "Cish AST Def Use Pass";
  }
};

} // namespace cish

cish::ASTPass* createASTDefUseCalculatorPass(cish::CishContext& cishContext) {
  return new cish::ASTDefUseCalculatorPass(cishContext);
}
