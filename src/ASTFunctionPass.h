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

#ifndef CISH_AST_FUNCTION_PASS_H
#define CISH_AST_FUNCTION_PASS_H

#include "AST.h"
#include "CishContext.h"
#include "Diagnostics.h"

#include <clang/AST/ExprCXX.h>

using clang::cast;
using clang::dyn_cast;
using clang::isa;

#define DECLARE_SPECIALIZABLE_(FUNC)                                 \
  template <typename ClangT>                                         \
  struct has_##FUNC {                                                \
  private:                                                           \
    template <typename T>                                            \
    static constexpr auto check(T*) -> typename std::is_same<        \
        decltype(std::declval<T>().FUNC(std::declval<ClangT>())),    \
        bool>::type;                                                 \
                                                                     \
    template <typename>                                              \
    static constexpr std::false_type check(...);                     \
                                                                     \
    typedef decltype(check<PassImpl>(0)) type;                       \
                                                                     \
  public:                                                            \
    static constexpr bool value = type::value;                       \
  };                                                                 \
                                                                     \
  template <class ClangT,                                            \
            std::enable_if_t<has_process<ClangT*>::value, int> = 0>  \
  bool derived_##FUNC(ClangT* cls) {                                 \
    return static_cast<PassImpl*>(this)->FUNC(cls);                  \
  }                                                                  \
                                                                     \
  template <class ClangT,                                            \
            std::enable_if_t<!has_process<ClangT*>::value, int> = 0> \
  bool derived_##FUNC(ClangT*) {                                     \
    return false;                                                    \
  }

namespace cish {

class ASTPass {
protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;
  AST* ast;

protected:
  ASTPass(CishContext& cishContext);
  ASTPass(const ASTPass&) = delete;
  ASTPass(ASTPass&&) = delete;
  virtual ~ASTPass() = default;

  virtual llvm::StringRef getPassName() const;

public:
  virtual bool runOnFunction(clang::FunctionDecl* f) = 0;
};

template <class PassImpl>
class ASTFunctionPass : public ASTPass {
private:
  DECLARE_SPECIALIZABLE_(process)
  DECLARE_SPECIALIZABLE_(beginFunction)
  DECLARE_SPECIALIZABLE_(endFunction)

protected:
  bool beginFunction(clang::FunctionDecl* f) {
    bool changed = false;

    changed |= derived_beginFunction(f);
    // Nothing to be done in the base class by default

    return changed;
  }

  bool endFunction(clang::FunctionDecl* f) {
    bool changed = false;

    changed |= derived_endFunction(f);
    // Nothing to be done in the base class by default

    return changed;
  }

  bool process(clang::CompoundStmt* compoundStmt) {
    bool changed = false;

    changed |= derived_process(compoundStmt);
    for(clang::Stmt* stmt : compoundStmt->body())
      changed |= process(stmt);

    return changed;
  }

  bool process(clang::IfStmt* ifStmt) {
    bool changed = false;

    changed |= derived_process(ifStmt);
    changed |= process(ifStmt->getCond());
    if(clang::Stmt* thn = ifStmt->getThen())
      changed |= process(thn);
    if(clang::Stmt* els = ifStmt->getElse())
      changed |= process(els);

    return changed;
  }

  bool process(clang::DoStmt* doStmt) {
    bool changed = false;

    changed |= derived_process(doStmt);
    changed |= process(doStmt->getCond());
    changed |= process(doStmt->getBody());

    return changed;
  }

  bool process(clang::ForStmt* forStmt) {
    bool changed = false;

    changed |= derived_process(forStmt);
    changed |= process(forStmt->getInit());
    changed |= process(forStmt->getCond());
    changed |= process(forStmt->getInc());

    return changed;
  }

  bool process(clang::WhileStmt* whileStmt) {
    bool changed = false;

    changed |= derived_process(whileStmt);
    changed |= process(whileStmt->getCond());
    changed |= process(whileStmt->getBody());

    return changed;
  }

  bool process(clang::SwitchStmt* switchStmt) {
    bool changed = false;

    changed |= derived_process(switchStmt);
    changed |= process(switchStmt->getCond());
    for(clang::SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
        kase = kase->getNextSwitchCase())
      changed |= process(kase);

    return changed;
  }

  bool process(clang::CaseStmt* caseStmt) {
    bool changed = false;

    changed |= derived_process(caseStmt);
    changed |= process(caseStmt->getSubStmt());

    return changed;
  }

  bool process(clang::DefaultStmt* defaultStmt) {
    bool changed = false;

    changed |= derived_process(defaultStmt);
    changed |= process(defaultStmt->getSubStmt());

    return changed;
  }

  bool process(clang::ReturnStmt* retStmt) {
    bool changed = false;

    changed |= derived_process(retStmt);
    if(clang::Expr* retValue = retStmt->getRetValue())
      changed |= process(retValue);

    return changed;
  }

  bool process(clang::LabelStmt* labelStmt) {
    bool changed = false;

    changed |= derived_process(labelStmt);

    return changed;
  }

  bool process(clang::GotoStmt* gotoStmt) {
    bool changed = false;

    changed |= derived_process(gotoStmt);

    return changed;
  }

  bool process(clang::BreakStmt* breakStmt) {
    bool changed = false;

    changed |= derived_process(breakStmt);

    return changed;
  }

  bool process(clang::ContinueStmt* continueStmt) {
    bool changed = false;

    changed |= derived_process(continueStmt);

    return changed;
  }

  bool process(clang::DeclStmt* declStmt) {
    bool changed = false;

    changed |= derived_process(declStmt);
    for(clang::Decl* decl : declStmt->decls()) {
      if(auto* var = dyn_cast<clang::VarDecl>(decl)) {
        if(var->hasInit())
          changed |= process(var->getInit());
      } else {
        fatal(error() << "Unknown decl in DeclStmt: "
                      << decl->getDeclKindName());
      }
    }

    return changed;
  }

  bool process(clang::CXXBoolLiteralExpr* boolLiteral) {
    bool changed = false;

    changed |= derived_process(boolLiteral);

    return changed;
  }

  bool process(clang::CharacterLiteral* charLiteral) {
    bool changed = false;

    changed |= derived_process(charLiteral);

    return changed;
  }

  bool process(clang::IntegerLiteral* intLiteral) {
    bool changed = false;

    changed |= derived_process(intLiteral);

    return changed;
  }

  bool process(clang::FloatingLiteral* floatLiteral) {
    bool changed = false;

    changed |= derived_process(floatLiteral);

    return changed;
  }

  bool process(clang::StringLiteral* stringLiteral) {
    bool changed = false;

    changed |= derived_process(stringLiteral);

    return changed;
  }

  bool process(clang::CXXNullPtrLiteralExpr* nullLiteral) {
    bool changed = false;

    changed |= derived_process(nullLiteral);

    return changed;
  }

  bool process(clang::DeclRefExpr* declRef) {
    bool changed = false;

    changed |= derived_process(declRef);

    return changed;
  }

  bool process(clang::InitListExpr* initList) {
    bool changed = false;

    changed |= derived_process(initList);
    for(unsigned i = 0; i < initList->getNumInits(); i++)
      changed |= process(initList->getInit(i));

    return changed;
  }

  bool process(clang::BinaryOperator* binOp) {
    bool changed = false;

    changed |= derived_process(binOp);
    changed |= process(binOp->getLHS());
    changed |= process(binOp->getRHS());

    return changed;
  }

  bool process(clang::UnaryOperator* unOp) {
    bool changed = false;

    changed |= derived_process(unOp);
    changed |= process(unOp->getSubExpr());

    return changed;
  }

  bool process(clang::ConditionalOperator* condOp) {
    bool changed = false;

    changed |= derived_process(condOp);
    changed |= process(condOp->getCond());
    changed |= process(condOp->getTrueExpr());
    changed |= process(condOp->getFalseExpr());

    return changed;
  }

  bool process(clang::CallExpr* callExpr) {
    bool changed = false;

    changed |= derived_process(callExpr);
    changed |= process(callExpr->getCallee());
    for(clang::Expr* arg : callExpr->arguments())
      changed |= process(arg);

    return changed;
  }

  bool process(clang::MemberExpr* memberExpr) {
    bool changed = false;

    changed |= derived_process(memberExpr);
    changed |= process(memberExpr->getBase());

    return changed;
  }

  bool process(clang::CStyleCastExpr* castExpr) {
    bool changed = false;

    changed |= derived_process(castExpr);
    changed |= process(castExpr->getSubExpr());

    return changed;
  }

  bool process(clang::ArraySubscriptExpr* arrExpr) {
    bool changed = false;

    changed |= derived_process(arrExpr);
    changed |= process(arrExpr->getBase());
    changed |= process(arrExpr->getIdx());

    return changed;
  }

  bool process(clang::Stmt* stmt) {
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
    else if(auto* initList = dyn_cast<clang::InitListExpr>(stmt))
      return process(initList);
    else if(auto* binOp = dyn_cast<clang::BinaryOperator>(stmt))
      return process(binOp);
    else if(auto* unOp = dyn_cast<clang::UnaryOperator>(stmt))
      return process(unOp);
    else if(auto* condOp = dyn_cast<clang::ConditionalOperator>(stmt))
      return process(condOp);
    else if(auto* callExpr = dyn_cast<clang::CallExpr>(stmt))
      return process(callExpr);
    else if(auto* memberExpr = dyn_cast<clang::MemberExpr>(stmt))
      return process(memberExpr);
    else if(auto* castExpr = dyn_cast<clang::CStyleCastExpr>(stmt))
      return process(castExpr);
    else if(auto* arrExpr = dyn_cast<clang::ArraySubscriptExpr>(stmt))
      return process(arrExpr);
    else if(auto* declRefExpr = dyn_cast<clang::DeclRefExpr>(stmt))
      return process(declRefExpr);
    else if(auto* boolLit = dyn_cast<clang::CXXBoolLiteralExpr>(stmt))
      return process(boolLit);
    else if(auto* charLit = dyn_cast<clang::CharacterLiteral>(stmt))
      return process(charLit);
    else if(auto* intLit = dyn_cast<clang::IntegerLiteral>(stmt))
      return process(intLit);
    else if(auto* fpLit = dyn_cast<clang::FloatingLiteral>(stmt))
      return process(fpLit);
    else if(auto* stringLit = dyn_cast<clang::StringLiteral>(stmt))
      return process(stringLit);
    else if(auto* cnull = dyn_cast<clang::CXXNullPtrLiteralExpr>(stmt))
      return process(cnull);
    else
      fatal(error() << "Unknown exprssion: " << stmt->getStmtClassName());

    return false;
  }

  bool process(clang::FunctionDecl* f) {
    bool changed = false;

    changed |= derived_process(f);
    changed |= process(f->getBody());

    return changed;
  }

public:
  ASTFunctionPass(CishContext& cishContext) : ASTPass(cishContext) {
    ;
  }
  ASTFunctionPass(const ASTFunctionPass&) = delete;
  ASTFunctionPass(ASTFunctionPass&&) = delete;
  virtual ~ASTFunctionPass() = default;

  virtual bool runOnFunction(clang::FunctionDecl* f) override {
    bool changed = false;

    ast = &cishContext.getAST(f);
    message() << getPassName() << " (" << f->getName() << ")\n";

    changed |= process(f);

    if(changed)
      ast->recalculate();

    return changed;
  }

public:
  friend class ASTPassManager;
};

} // namespace cish

#endif // CISH_AST_FUNCTION_PASS_H
