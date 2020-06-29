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

#ifndef CISH_AST_EXPR_PASS_H
#define CISH_AST_EXPR_PASS_H

#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "CishContext.h"
#include "Diagnostics.h"

using clang::cast;
using clang::dyn_cast;

namespace cish {

template <typename DerivedT>
class ASTExprPass : public ASTFunctionPass<ASTExprPass<DerivedT>> {
protected:
  bool changed;
  ASTBuilder builder;

protected:
  template <typename ClangExpr>
  struct has_process {
  private:
    template <typename T>
    static constexpr auto check(T*) -> typename std::is_same<
        decltype(std::declval<T>().process(std::declval<ClangExpr>())),
        clang::Expr*>::type;

    template <typename>
    static constexpr std::false_type check(...);

    typedef decltype(check<DerivedT>(0)) type;

  public:
    static constexpr bool value = type::value;
  };

  template <typename ClangExpr,
            std::enable_if_t<std::is_base_of<clang::Expr, ClangExpr>::value
                                 && has_process<ClangExpr*>::value,
                             int> = 0>
  clang::Expr* processDerived(ClangExpr* expr) {
    return static_cast<DerivedT*>(this)->process(expr);
  }

  template <typename ClangExpr,
            std::enable_if_t<std::is_base_of<clang::Expr, ClangExpr>::value
                                 && !has_process<ClangExpr*>::value,
                             int> = 0>
  clang::Expr* processDerived(ClangExpr* expr) {
    return expr;
  }

private:
  clang::Expr* maybeReplace(clang::Expr* expr) {
    if(expr) {
      if(clang::Expr* repl = process(expr)) {
        if(expr != repl) {
          changed |= true;
          return repl;
        }
      }
    }
    return nullptr;
  }

  template <typename ClangExpr>
  clang::Expr* maybeReplaceDerived(ClangExpr* expr) {
    clang::Expr* ret = processDerived(expr);
    changed |= (ret != expr);
    return ret;
  }

protected:
  void process(clang::CompoundStmt* compoundStmt) {
    for(clang::Stmt* stmt : compoundStmt->body())
      process(stmt);
  }

  void process(clang::IfStmt* ifStmt) {
    if(clang::Expr* repl = maybeReplace(ifStmt->getCond()))
      ifStmt->setCond(repl);
    if(clang::Stmt* thn = ifStmt->getThen())
      process(thn);
    if(clang::Stmt* els = ifStmt->getElse())
      process(els);
  }

  void process(clang::DoStmt* doStmt) {
    if(clang::Expr* repl = maybeReplace(doStmt->getCond()))
      doStmt->setCond(repl);
    process(doStmt->getBody());
  }

  void process(clang::ForStmt* forStmt) {
    process(forStmt->getInit());
    if(clang::Expr* repl = maybeReplace(forStmt->getCond()))
      forStmt->setCond(repl);
    if(clang::Expr* repl = maybeReplace(forStmt->getInc()))
      forStmt->setCond(repl);
  }

  void process(clang::WhileStmt* whileStmt) {
    if(clang::Expr* repl = maybeReplace(whileStmt->getCond()))
      whileStmt->setCond(repl);
    process(whileStmt->getBody());
  }

  void process(clang::SwitchStmt* switchStmt) {
    if(clang::Expr* repl = maybeReplace(switchStmt->getCond()))
      switchStmt->setCond(repl);
    for(clang::SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
        kase = kase->getNextSwitchCase())
      process(kase);
  }

  void process(clang::CaseStmt* castStmt) {
    process(castStmt->getSubStmt());
  }

  void process(clang::DefaultStmt* defaultStmt) {
    process(defaultStmt->getSubStmt());
  }

  void process(clang::ReturnStmt* retStmt) {
    if(clang::Expr* repl = maybeReplace(retStmt->getRetValue()))
      retStmt->setRetValue(repl);
  }

  void process(clang::LabelStmt*) {
    return;
  }

  void process(clang::GotoStmt*) {
    return;
  }

  void process(clang::BreakStmt*) {
    return;
  }

  void process(clang::ContinueStmt*) {
    return;
  }

  void process(clang::DeclStmt* declStmt) {
    for(clang::Decl* decl : declStmt->decls()) {
      if(auto* var = dyn_cast<clang::VarDecl>(decl)) {
        if(var->hasInit())
          if(clang::Expr* repl = maybeReplace(var->getInit()))
            var->setInit(repl);
      } else {
        fatal(error() << "Unknown decl in DeclStmt: "
                      << decl->getDeclKindName());
      }
    }
  }

  void process(clang::Stmt* stmt) {
    if(auto* compoundStmt = dyn_cast<clang::CompoundStmt>(stmt))
      return process(compoundStmt);
    else if(auto* ifStmt = dyn_cast<clang::IfStmt>(stmt))
      return process(ifStmt);
    else if(auto* doStmt = dyn_cast<clang::DoStmt>(stmt))
      return process(doStmt);
    else if(auto* whileStmt = dyn_cast<clang::WhileStmt>(stmt))
      return process(whileStmt);
    else if(auto* forStmt = dyn_cast<clang::ForStmt>(stmt))
      return process(forStmt);
    else if(auto* switchStmt = dyn_cast<clang::SwitchStmt>(stmt))
      return process(switchStmt);
    else if(auto* castStmt = dyn_cast<clang::CaseStmt>(stmt))
      return process(castStmt);
    else if(auto* defaultStmt = dyn_cast<clang::DefaultStmt>(stmt))
      return process(defaultStmt);
    else if(auto* retStmt = dyn_cast<clang::ReturnStmt>(stmt))
      return process(retStmt);
    else if(auto* breakStmt = dyn_cast<clang::BreakStmt>(stmt))
      return process(breakStmt);
    else if(auto* continueStmt = dyn_cast<clang::ContinueStmt>(stmt))
      return process(continueStmt);
    else if(auto* labelStmt = dyn_cast<clang::LabelStmt>(stmt))
      return process(labelStmt);
    else if(auto* gotoStmt = dyn_cast<clang::GotoStmt>(stmt))
      return process(gotoStmt);
    else if(auto* declStmt = dyn_cast<clang::DeclStmt>(stmt))
      return process(declStmt);
    else if(auto* binOp = dyn_cast<clang::BinaryOperator>(stmt))
      process(binOp);
    else if(auto* unOp = dyn_cast<clang::UnaryOperator>(stmt))
      process(unOp);
    else if(auto* callExpr = dyn_cast<clang::CallExpr>(stmt))
      process(callExpr);
    else
      fatal(error() << "Unknown statement: " << stmt->getStmtClassName());
  }

  clang::Expr* process(clang::CXXBoolLiteralExpr* boolLiteral) {
    return boolLiteral;
  }

  clang::Expr* process(clang::CharacterLiteral* charLiteral) {
    return charLiteral;
  }

  clang::Expr* process(clang::IntegerLiteral* intLiteral) {
    return intLiteral;
  }

  clang::Expr* process(clang::FloatingLiteral* floatLiteral) {
    return floatLiteral;
  }

  clang::Expr* process(clang::StringLiteral* stringLiteral) {
    return stringLiteral;
  }

  clang::Expr* process(clang::CXXNullPtrLiteralExpr* nullLiteral) {
    return nullLiteral;
  }

  clang::Expr* process(clang::DeclRefExpr* declRefExpr) {
    return declRefExpr;
  }

  clang::Expr* process(clang::InitListExpr* initList) {
    for(unsigned i = 0; i < initList->getNumInits(); i++)
      if(clang::Expr* repl = maybeReplace(initList->getInit(i)))
        initList->setInit(i, repl);
    return maybeReplaceDerived(initList);
  }

  clang::Expr* process(clang::BinaryOperator* binOp) {
    if(clang::Expr* repl = maybeReplace(binOp->getLHS()))
      binOp->setLHS(repl);
    if(clang::Expr* repl = maybeReplace(binOp->getRHS()))
      binOp->setRHS(repl);
    return maybeReplaceDerived(binOp);
  }

  clang::Expr* process(clang::UnaryOperator* unOp) {
    if(clang::Expr* repl = maybeReplace(unOp->getSubExpr()))
      unOp->setSubExpr(repl);
    return maybeReplaceDerived(unOp);
  }

  clang::Expr* process(clang::ConditionalOperator* condOp) {
    clang::Expr* cond = condOp->getCond();
    clang::Expr* trueExpr = condOp->getTrueExpr();
    clang::Expr* falseExpr = condOp->getFalseExpr();
    clang::Expr* newCond = process(cond);
    clang::Expr* newTrue = process(trueExpr);
    clang::Expr* newFalse = process(falseExpr);
    if((cond != newCond) or (trueExpr != newTrue) or (falseExpr != newFalse))
      condOp = builder.createConditionalOperator(
          newCond, newTrue, newFalse, newTrue->getType());
    return maybeReplaceDerived(condOp);
  }

  clang::Expr* process(clang::CallExpr* callExpr) {
    for(unsigned i = 0; i < callExpr->getNumArgs(); i++)
      if(clang::Expr* repl = maybeReplace(callExpr->getArg(i)))
        callExpr->setArg(i, repl);
    return maybeReplaceDerived(callExpr);
  }

  clang::Expr* process(clang::MemberExpr* memberExpr) {
    if(clang::Expr* repl = maybeReplace(memberExpr->getBase()))
      memberExpr->setBase(repl);
    return maybeReplaceDerived(memberExpr);
  }

  clang::Expr* process(clang::CStyleCastExpr* castExpr) {
    if(clang::Expr* repl = maybeReplace(castExpr->getSubExpr()))
      castExpr->setSubExpr(repl);
    return maybeReplaceDerived(castExpr);
  }

  clang::Expr* process(clang::ArraySubscriptExpr* arrExpr) {
    if(clang::Expr* repl = maybeReplace(arrExpr->getBase()))
      arrExpr->setLHS(repl);
    if(clang::Expr* repl = maybeReplace(arrExpr->getIdx()))
      arrExpr->setRHS(repl);
    return maybeReplaceDerived(arrExpr);
  }

  clang::Expr* process(clang::Expr* expr) {
    if(auto* boolLit = dyn_cast<clang::CXXBoolLiteralExpr>(expr))
      return process(boolLit);
    else if(auto* charLit = dyn_cast<clang::CharacterLiteral>(expr))
      return process(charLit);
    else if(auto* intLit = dyn_cast<clang::IntegerLiteral>(expr))
      return process(intLit);
    else if(auto* fpLit = dyn_cast<clang::FloatingLiteral>(expr))
      return process(fpLit);
    else if(auto* stringLit = dyn_cast<clang::StringLiteral>(expr))
      return process(stringLit);
    else if(auto* cnull = dyn_cast<clang::CXXNullPtrLiteralExpr>(expr))
      return process(cnull);
    else if(auto* initList = dyn_cast<clang::InitListExpr>(expr))
      return process(initList);
    else if(auto* binOp = dyn_cast<clang::BinaryOperator>(expr))
      return process(binOp);
    else if(auto* unOp = dyn_cast<clang::UnaryOperator>(expr))
      return process(unOp);
    else if(auto* condOp = dyn_cast<clang::ConditionalOperator>(expr))
      return process(condOp);
    else if(auto* callExpr = dyn_cast<clang::CallExpr>(expr))
      return process(callExpr);
    else if(auto* memberExpr = dyn_cast<clang::MemberExpr>(expr))
      return process(memberExpr);
    else if(auto* castExpr = dyn_cast<clang::CStyleCastExpr>(expr))
      return process(castExpr);
    else if(auto* arrExpr = dyn_cast<clang::ArraySubscriptExpr>(expr))
      return process(arrExpr);
    else if(auto* declRefExpr = dyn_cast<clang::DeclRefExpr>(expr))
      return process(declRefExpr);
    fatal(error() << "Unknown exprssion: " << expr->getStmtClassName());

    return expr;
  }

public:
  ASTExprPass(CishContext& cishContext)
      : ASTFunctionPass<ASTExprPass<DerivedT>>(cishContext), changed(false),
        builder(cishContext.getASTContext()) {
    ;
  }

  ASTExprPass(const ASTExprPass&) = delete;
  ASTExprPass(ASTExprPass&&) = delete;
  virtual ~ASTExprPass() = default;

  virtual llvm::StringRef getPassName() const override = 0;

  bool process(clang::FunctionDecl* f) {
    changed = false;
    process(f->getBody());

    return changed;
  }
};

} // namespace cish

#endif // CISH_AST_EXPR_PASS_H
