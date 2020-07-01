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

  void process(BinaryOperator* binOp, ForStmt* user) {
    if(defOps2.contains(binOp->getOpcode())) {
      findDefd(binOp->getLHS(), user);
      if(binOp->getOpcode() != BO_Assign)
        // This is an OperatorAssign statement
        ast->addUse(binOp->getLHS(), user);
    } else {
      ast->addUse(binOp->getLHS(), user);
    }
    ast->addUse(binOp->getRHS(), user);
  }

public:
  bool process(ConditionalOperator* condOp) {
    ast->addUse(condOp->getCond(), condOp);
    ast->addUse(condOp->getTrueExpr(), condOp);
    ast->addUse(condOp->getFalseExpr(), condOp);

    return false;
  }

  bool process(BinaryOperator* binOp) {
    if(defOps2.contains(binOp->getOpcode())) {
      findDefd(binOp->getLHS(), binOp);
      // If this is an OperatorAssign (+=, -= etc.), the LHS will be definitely
      // also be read
      if(binOp->getOpcode() != BO_Assign)
        ast->addUse(binOp->getLHS(), binOp);
      ast->addUse(binOp->getRHS(), binOp);
    } else if(binOp->getOpcode() == BO_Comma) {
      // Comma operators won't be used anywhere except initializers and
      // increments for a ForStmt
      process(cast<BinaryOperator>(binOp->getLHS()));
      process(cast<BinaryOperator>(binOp->getRHS()));
    } else {
      ast->addUse(binOp->getLHS(), binOp);
      ast->addUse(binOp->getRHS(), binOp);
    }

    return false;
  }

  bool process(UnaryOperator* unOp) {
    Expr* expr = unOp->getSubExpr();
    if(defOps1.contains(unOp->getOpcode()))
      findDefd(expr, unOp);
    ast->addUse(expr, unOp);

    return false;
  }

  bool process(CallExpr* callExpr) {
    // FIXME: Call expressions may have variables passed by pointer/reference
    // which might get mutated in the call. Not sure whether or not they should
    // be considered defs
    for(Expr* arg : callExpr->arguments())
      ast->addUse(arg, callExpr);

    return false;
  }

  bool process(CStyleCastExpr* castExpr) {
    ast->addUse(castExpr->getSubExpr(), castExpr);

    return false;
  }

  bool process(ArraySubscriptExpr* arrExpr) {
    ast->addUse(arrExpr->getBase(), arrExpr);
    ast->addUse(arrExpr->getIdx(), arrExpr);

    return false;
  }

  bool process(MemberExpr* memberExpr) {
    ast->addUse(memberExpr->getBase(), memberExpr);

    return false;
  }

  bool process(InitListExpr* initList) {
    for(Expr* expr : initList->inits())
      ast->addUse(expr, initList);

    return false;
  }

  bool process(SwitchStmt* switchStmt) {
    ast->addUse(switchStmt->getCond(), switchStmt);

    return false;
  }

  bool process(ReturnStmt* retStmt) {
    if(Expr* expr = retStmt->getRetValue())
      ast->addUse(expr, retStmt);

    return false;
  }

  bool process(IfStmt* ifStmt) {
    ast->addUse(ifStmt->getCond(), ifStmt);

    return false;
  }

  bool process(DoStmt* doStmt) {
    ast->addUse(doStmt->getCond(), doStmt);

    return false;
  }

  void collectExprs(BinaryOperator* binOp, Vector<BinaryOperator*>& exprs) {
    if(binOp->getOpcode() == BO_Comma) {
      collectExprs(cast<BinaryOperator>(binOp->getLHS()), exprs);
      collectExprs(cast<BinaryOperator>(binOp->getRHS()), exprs);
    } else {
      exprs.push_back(binOp);
    }
  }

  Vector<BinaryOperator*> collectExprs(BinaryOperator* binOp) {
    Vector<BinaryOperator*> exprs;

    collectExprs(binOp, exprs);

    return exprs;
  }

  bool process(ForStmt* forStmt) {
    if(auto* init = forStmt->getInit()) {
      if(auto* binOp = dyn_cast<BinaryOperator>(init))
        for(BinaryOperator* expr : collectExprs(binOp))
          process(expr, forStmt);
      else if(auto* declStmt = dyn_cast<DeclStmt>(init))
        fatal(error() << "Not implemented: DeclStmt in for loop");
      else
        fatal(error() << "Unknown statement in for loop initializer: "
                      << init->getStmtClassName());
    }

    ast->addUse(forStmt->getCond(), forStmt);

    if(auto* inc = forStmt->getInc()) {
      if(auto* binOp = dyn_cast<BinaryOperator>(inc))
        for(BinaryOperator* expr : collectExprs(binOp))
          process(expr, forStmt);
      else
        fatal(error() << "Unkonwn statement in for loop increment: "
                      << inc->getStmtClassName());
    }

    return false;
  }

  bool process(WhileStmt* whileStmt) {
    ast->addUse(whileStmt->getCond(), whileStmt);

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
