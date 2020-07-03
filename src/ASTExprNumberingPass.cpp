//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu
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
#include "Map.h"

using namespace clang;

namespace cish {

class ASTExprNumberingPass : public ASTFunctionPass<ASTExprNumberingPass> {
protected:
  using ExprNum = AST::ExprNum;

protected:
  Map<UnaryOperator::Opcode, Map<ExprNum, ExprNum>> unOps;
  Map<BinaryOperator::Opcode, Map<std::pair<ExprNum, ExprNum>, ExprNum>> binOps;
  Map<Vector<ExprNum>, ExprNum> condOps;
  Map<std::pair<ExprNum, ExprNum>, ExprNum> arrExprs;
  Map<Vector<ExprNum>, ExprNum> callExprs;
  Map<ExprNum, ExprNum> castExprs;
  Map<std::pair<ExprNum, ValueDecl*>, ExprNum> memberExprs;

protected:
  ExprNum getOrInsertExpr(Expr* expr) {
    if(not ast->hasExprNum(expr)) {
      if(isa<CXXBoolLiteralExpr>(expr) or isa<CharacterLiteral>(expr)
         or isa<IntegerLiteral>(expr) or isa<FloatingLiteral>(expr)
         or isa<StringLiteral>(expr) or isa<CXXNullPtrLiteralExpr>(expr)
         or isa<DeclRefExpr>(expr)) {
        // These expressions are all uniqued, so if they have a different
        // pointer, they should have a different ID
        return ast->addExpr(expr);
      } else if(auto* unOp = dyn_cast<UnaryOperator>(expr)) {
        UnaryOperator::Opcode op = unOp->getOpcode();
        auto k = getOrInsertExpr(unOp->getSubExpr());
        if(not unOps[op].contains(k))
          return unOps[op][k] = ast->addExpr(expr);
        return unOps[op][k];
      } else if(auto* binOp = dyn_cast<BinaryOperator>(expr)) {
        BinaryOperator::Opcode op = binOp->getOpcode();
        auto p = std::make_pair(getOrInsertExpr(binOp->getLHS()),
                                getOrInsertExpr(binOp->getRHS()));
        if(not binOps[op].contains(p))
          return binOps[op][p] = ast->addExpr(expr);
        return binOps[op][p];
      } else if(auto* condOp = dyn_cast<ConditionalOperator>(expr)) {
        Vector<ExprNum> v = {getOrInsertExpr(condOp->getCond()),
                             getOrInsertExpr(condOp->getTrueExpr()),
                             getOrInsertExpr(condOp->getFalseExpr())};
        if(not condOps.contains(v))
          return condOps[v] = ast->addExpr(expr);
        return condOps[v];
      } else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(expr)) {
        auto p = std::make_pair(getOrInsertExpr(arrExpr->getBase()),
                                getOrInsertExpr(arrExpr->getIdx()));
        if(not arrExprs.contains(p))
          return arrExprs[p] = ast->addExpr(expr);
        return arrExprs[p];
      } else if(auto* callExpr = dyn_cast<CallExpr>(expr)) {
        Vector<ExprNum> v = {getOrInsertExpr(callExpr->getCallee())};
        for(Expr* arg : callExpr->arguments())
          v.push_back(getOrInsertExpr(arg));
        if(not callExprs.contains(v))
          return callExprs[v] = ast->addExpr(expr);
        return callExprs[v];
      } else if(auto* castExpr = dyn_cast<CStyleCastExpr>(expr)) {
        ExprNum k = getOrInsertExpr(castExpr->getSubExpr());
        if(not castExprs.contains(k))
          return castExprs[k] = ast->addExpr(expr);
        return castExprs[k];
      } else if(auto* memberExpr = dyn_cast<MemberExpr>(expr)) {
        auto p = std::make_pair(getOrInsertExpr(memberExpr->getBase()),
                                memberExpr->getMemberDecl());
        if(not memberExprs.contains(p))
          return memberExprs[p] = ast->addExpr(expr);
        return memberExprs[p];
      } else if(auto* initList = dyn_cast<InitListExpr>(expr)) {
        // Don't try to unique this because it doesn't have a name
        return ast->addExpr(expr);
      } else {
        fatal(error() << "Unknown expression when numbering: "
                      << expr->getStmtClassName());
      }
    }

    return ast->getExprNum(expr);
  }

public:
  bool process(UnaryOperator* unOp) {
    getOrInsertExpr(unOp);

    return false;
  }

  bool process(BinaryOperator* binOp) {
    getOrInsertExpr(binOp);

    return false;
  }

  bool process(ConditionalOperator* condOp) {
    getOrInsertExpr(condOp);

    return false;
  }

  bool process(ArraySubscriptExpr* arrExpr) {
    getOrInsertExpr(arrExpr);

    return false;
  }

  bool process(CallExpr* callExpr) {
    getOrInsertExpr(callExpr);

    return false;
  }

  bool process(CastExpr* castExpr) {
    getOrInsertExpr(castExpr);

    return false;
  }

  bool process(MemberExpr* memberExpr) {
    getOrInsertExpr(memberExpr);

    return false;
  }

  bool process(InitListExpr* initList) {
    getOrInsertExpr(initList);

    return false;
  }

  bool process(DeclRefExpr* declRefExpr) {
    getOrInsertExpr(declRefExpr);

    return false;
  }

  bool process(FunctionDecl* f) {
    unOps.clear();
    binOps.clear();
    condOps.clear();
    arrExprs.clear();
    callExprs.clear();
    castExprs.clear();
    memberExprs.clear();

    return false;
  }

public:
  ASTExprNumberingPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext) {
    ;
  }

  ASTExprNumberingPass(const ASTExprNumberingPass&) = delete;
  ASTExprNumberingPass(ASTExprNumberingPass&&) = delete;
  virtual ~ASTExprNumberingPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-gvn";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish Global Expr Numbering";
  }
};

} // namespace cish

cish::ASTPass* createASTExprNumberingPass(cish::CishContext& cishContext) {
  return new cish::ASTExprNumberingPass(cishContext);
}
