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
#include "ASTPass.h"
#include "CishContext.h"
#include "Diagnostics.h"

#include <clang/AST/ExprCXX.h>

using clang::cast;
using clang::dyn_cast;
using clang::isa;

#define DECLARE_SPECIALIZED(FUNC)                                 \
  template <class ClangT>                                         \
  struct has_##FUNC {                                             \
  private:                                                        \
    template <typename T>                                         \
    static constexpr auto check(T*) -> typename std::is_same<     \
        decltype(std::declval<T>().FUNC(std::declval<ClangT>())), \
        bool>::type;                                              \
                                                                  \
    template <typename>                                           \
    static constexpr std::false_type check(...);                  \
                                                                  \
    typedef decltype(check<PassImpl>(0)) type;                    \
                                                                  \
  public:                                                         \
    static constexpr bool value = type::value;                    \
  }

#define DEFINE_CALL_SPECIALIZED(FUNC)                               \
  template <class ClangT,                                           \
            std::enable_if_t<has_##FUNC<ClangT*>::value, int> = 0>  \
  bool derived_##FUNC(ClangT* stmt) {                               \
    return static_cast<PassImpl*>(this)->FUNC(stmt);                \
  }                                                                 \
                                                                    \
  template <class ClangT,                                           \
            std::enable_if_t<!has_##FUNC<ClangT*>::value, int> = 0> \
  bool derived_##FUNC(ClangT*) {                                    \
    return false;                                                   \
  }

#define CALL_SPECIALIZED_BEFORE(FUNC, stmt) \
  do {                                      \
    if(not hasPostorder())                  \
      changed |= derived_##FUNC(stmt);      \
  } while(0)

#define CALL_SPECIALIZED_AFTER(FUNC, stmt) \
  do {                                     \
    if(hasPostorder())                     \
      changed |= derived_##FUNC(stmt);     \
  } while(0)

namespace cish {

template <class PassImpl>
class ASTFunctionPass : public ASTPass {
private:
  DECLARE_SPECIALIZED(initialize);
  DECLARE_SPECIALIZED(process);
  DECLARE_SPECIALIZED(finalize);

private:
  DEFINE_CALL_SPECIALIZED(initialize)
  DEFINE_CALL_SPECIALIZED(process)
  DEFINE_CALL_SPECIALIZED(finalize)

  // ************************* PROBABLY SERIOUS BUG **************************
  //
  //                     FIXME: There is surely a bug here.
  //
  // For some reason, the template that checks for the existence of a derived
  // class member function process(T*) works correctly. But for initialize()
  // and finalize(), it always returns true even if those functions are not
  // defined in the derived class. For the moment, the workaround of renaming
  // the private initialize() and finalize() functions in this class works
  // but really should not rely on something like this
  // -------------------------------------------------------------------------
  bool doInitialize(clang::FunctionDecl* f) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(initialize, f);
    // Do nothing
    CALL_SPECIALIZED_AFTER(initialize, f);

    return changed;
  }

  bool doFinalize(clang::FunctionDecl* f) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(finalize, f);
    // Do nothing
    CALL_SPECIALIZED_AFTER(finalize, f);

    return changed;
  }

  bool process(clang::CXXBoolLiteralExpr* boolLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, boolLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, boolLiteral);

    return changed;
  }

  bool process(clang::CharacterLiteral* charLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, charLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, charLiteral);

    return changed;
  }

  bool process(clang::IntegerLiteral* intLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, intLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, intLiteral);

    return changed;
  }

  bool process(clang::FloatingLiteral* floatLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, floatLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, floatLiteral);

    return changed;
  }

  bool process(clang::StringLiteral* stringLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, stringLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, stringLiteral);

    return changed;
  }

  bool process(clang::CXXNullPtrLiteralExpr* nullLiteral) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, nullLiteral);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, nullLiteral);

    return changed;
  }

  bool process(clang::CompoundStmt* compoundStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, compoundStmt);
    for(clang::Stmt* stmt : compoundStmt->body())
      changed |= process(stmt);
    CALL_SPECIALIZED_AFTER(process, compoundStmt);

    return changed;
  }

  bool process(clang::NullStmt* nullStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, nullStmt);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, nullStmt);

    return changed;
  }

  bool process(clang::IfStmt* ifStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, ifStmt);
    changed |= process(ifStmt->getCond());
    if(clang::Stmt* thn = ifStmt->getThen())
      changed |= process(thn);
    if(clang::Stmt* els = ifStmt->getElse())
      changed |= process(els);
    CALL_SPECIALIZED_AFTER(process, ifStmt);

    return changed;
  }

  bool process(clang::SwitchStmt* switchStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, switchStmt);
    changed |= process(switchStmt->getCond());
    for(clang::SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
        kase = kase->getNextSwitchCase())
      changed |= process(kase);
    CALL_SPECIALIZED_AFTER(process, switchStmt);

    return changed;
  }

  bool process(clang::CaseStmt* caseStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, caseStmt);
    changed |= process(caseStmt->getSubStmt());
    CALL_SPECIALIZED_AFTER(process, caseStmt);

    return changed;
  }

  bool process(clang::DefaultStmt* defaultStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, defaultStmt);
    changed |= process(defaultStmt->getSubStmt());
    CALL_SPECIALIZED_AFTER(process, defaultStmt);

    return changed;
  }

  bool process(clang::DoStmt* doStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, doStmt);
    changed |= process(doStmt->getCond());
    changed |= process(doStmt->getBody());
    CALL_SPECIALIZED_AFTER(process, doStmt);

    return changed;
  }

  bool process(clang::ForStmt* forStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, forStmt);
    changed |= process(forStmt->getInit());
    changed |= process(forStmt->getCond());
    changed |= process(forStmt->getInc());
    changed |= process(forStmt->getBody());
    CALL_SPECIALIZED_AFTER(process, forStmt);

    return changed;
  }

  bool process(clang::WhileStmt* whileStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, whileStmt);
    changed |= process(whileStmt->getCond());
    changed |= process(whileStmt->getBody());
    CALL_SPECIALIZED_AFTER(process, whileStmt);

    return changed;
  }

  bool process(clang::ReturnStmt* retStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, retStmt);
    if(clang::Expr* retValue = retStmt->getRetValue())
      changed |= process(retValue);
    CALL_SPECIALIZED_AFTER(process, retStmt);

    return changed;
  }

  bool process(clang::LabelStmt* labelStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, labelStmt);
    changed |= process(labelStmt->getSubStmt());
    CALL_SPECIALIZED_AFTER(process, labelStmt);

    return changed;
  }

  bool process(clang::GotoStmt* gotoStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, gotoStmt);
    changed |= process(gotoStmt->getLabel()->getStmt());
    CALL_SPECIALIZED_AFTER(process, gotoStmt);

    return changed;
  }

  bool process(clang::BreakStmt* breakStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, breakStmt);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, breakStmt);

    return changed;
  }

  bool process(clang::ContinueStmt* continueStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, continueStmt);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, continueStmt);

    return changed;
  }

  bool process(clang::DeclStmt* declStmt) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, declStmt);
    for(clang::Decl* decl : declStmt->decls()) {
      if(auto* var = dyn_cast<clang::VarDecl>(decl)) {
        if(var->hasInit())
          changed |= process(var->getInit());
      } else {
        fatal(error() << "Unknown decl in DeclStmt: "
                      << decl->getDeclKindName());
      }
    }
    CALL_SPECIALIZED_AFTER(process, declStmt);

    return changed;
  }

  bool process(clang::DeclRefExpr* declRef) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, declRef);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, declRef);

    return changed;
  }

  bool process(clang::UnaryOperator* unOp) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, unOp);
    changed |= process(unOp->getSubExpr());
    CALL_SPECIALIZED_AFTER(process, unOp);

    return changed;
  }

  bool process(clang::BinaryOperator* binOp) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, binOp);
    changed |= process(binOp->getLHS());
    changed |= process(binOp->getRHS());
    CALL_SPECIALIZED_AFTER(process, binOp);

    return changed;
  }

  bool process(clang::ConditionalOperator* condOp) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, condOp);
    changed |= process(condOp->getCond());
    changed |= process(condOp->getTrueExpr());
    changed |= process(condOp->getFalseExpr());
    CALL_SPECIALIZED_AFTER(process, condOp);

    return changed;
  }

  bool process(clang::ArraySubscriptExpr* arrExpr) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, arrExpr);
    changed |= process(arrExpr->getBase());
    changed |= process(arrExpr->getIdx());
    CALL_SPECIALIZED_AFTER(process, arrExpr);

    return changed;
  }

  bool process(clang::CStyleCastExpr* castExpr) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, castExpr);
    changed |= process(castExpr->getSubExpr());
    CALL_SPECIALIZED_AFTER(process, castExpr);

    return changed;
  }

  bool process(clang::CallExpr* callExpr) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, callExpr);
    changed |= process(callExpr->getCallee());
    for(clang::Expr* arg : callExpr->arguments())
      changed |= process(arg);
    CALL_SPECIALIZED_AFTER(process, callExpr);

    return changed;
  }

  bool process(clang::MemberExpr* memberExpr) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, memberExpr);
    changed |= process(memberExpr->getBase());
    CALL_SPECIALIZED_AFTER(process, memberExpr);

    return changed;
  }

  bool process(clang::InitListExpr* initList) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, initList);
    for(unsigned i = 0; i < initList->getNumInits(); i++)
      changed |= process(initList->getInit(i));
    CALL_SPECIALIZED_AFTER(process, initList);

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
    else if(auto* nullStmt = dyn_cast<clang::NullStmt>(stmt))
      return process(nullStmt);
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
      fatal(error() << "Unknown expression: " << stmt->getStmtClassName());

    return false;
  }

  bool process(clang::FunctionDecl* f) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, f);
    changed |= process(f->getBody());
    CALL_SPECIALIZED_AFTER(process, f);

    return changed;
  }

public:
  ASTFunctionPass(CishContext& cishContext, unsigned flags = 0)
      : ASTPass(cishContext, flags) {
    ;
  }

  ASTFunctionPass(const ASTFunctionPass&) = delete;
  ASTFunctionPass(ASTFunctionPass&&) = delete;
  virtual ~ASTFunctionPass() = default;

  virtual bool runOnFunction(clang::FunctionDecl* f) override {
    bool changed = false;

    ast = &cishContext.getAST(f);
    message() << getPassLongName() << "\n";

    changed |= doInitialize(f);
    if(hasIterateUntilConvergence()) {
      bool iterChanged;
      do {
        iterChanged = process(f);
        changed |= iterChanged;
      } while(iterChanged);
    } else {
      changed |= process(f);
    }
    changed |= doFinalize(f);

    if(changed)
      ast->recalculate();

    return changed;
  }

public:
  friend class ASTPassManager;
};

} // namespace cish

#endif // CISH_AST_FUNCTION_PASS_H
