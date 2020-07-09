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
#include "ClangUtils.h"
#include "Diagnostics.h"

#include <clang/AST/ExprCXX.h>

using clang::cast;
using clang::dyn_cast;
using clang::isa;

#define DECLARE_SPECIALIZED(FUNC)                                       \
  template <class ClangT>                                               \
  struct has_##FUNC {                                                   \
  private:                                                              \
    template <typename T>                                               \
    static constexpr auto check(T*) -> typename std::is_same<           \
        decltype(std::declval<T>().FUNC(std::declval<ClangT>(),         \
                                        std::declval<clang::Stmt*>())), \
        bool>::type;                                                    \
                                                                        \
    template <typename>                                                 \
    static constexpr std::false_type check(...);                        \
                                                                        \
    typedef decltype(check<PassImpl>(0)) type;                          \
                                                                        \
  public:                                                               \
    static constexpr bool value = type::value;                          \
  }

#define DEFINE_CALL_SPECIALIZED(FUNC)                               \
  template <class ClangT,                                           \
            std::enable_if_t<has_##FUNC<ClangT*>::value, int> = 0>  \
  bool derived_##FUNC(ClangT* stmt, clang::Stmt* parent) {          \
    return static_cast<PassImpl*>(this)->FUNC(stmt, parent);        \
  }                                                                 \
                                                                    \
  template <class ClangT,                                           \
            std::enable_if_t<!has_##FUNC<ClangT*>::value, int> = 0> \
  bool derived_##FUNC(ClangT*, clang::Stmt*) {                      \
    return false;                                                   \
  }

#define CALL_SPECIALIZED_BEFORE(FUNC, stmt, parent) \
  do {                                              \
    if(hasPreOrder())                               \
      changed |= derived_##FUNC(stmt, parent);      \
  } while(0)

#define CALL_SPECIALIZED_AFTER(FUNC, stmt, parent) \
  do {                                             \
    if(not hasPreOrder())                          \
      changed |= derived_##FUNC(stmt, parent);     \
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

    CALL_SPECIALIZED_BEFORE(initialize, f, nullptr);
    // Do nothing
    CALL_SPECIALIZED_AFTER(initialize, f, nullptr);

    return changed;
  }

  bool doFinalize(clang::FunctionDecl* f) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(finalize, f, nullptr);
    // Do nothing
    CALL_SPECIALIZED_AFTER(finalize, f, nullptr);

    return changed;
  }

  bool process(clang::CXXBoolLiteralExpr* boolLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(boolLiteral);
    CALL_SPECIALIZED_BEFORE(process, boolLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, boolLiteral, parent);

    return changed;
  }

  bool process(clang::CharacterLiteral* charLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(charLiteral);
    CALL_SPECIALIZED_BEFORE(process, charLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, charLiteral, parent);

    return changed;
  }

  bool process(clang::IntegerLiteral* intLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(intLiteral);
    CALL_SPECIALIZED_BEFORE(process, intLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, intLiteral, parent);

    return changed;
  }

  bool process(clang::FloatingLiteral* floatLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(floatLiteral);
    CALL_SPECIALIZED_BEFORE(process, floatLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, floatLiteral, parent);

    return changed;
  }

  bool process(clang::StringLiteral* stringLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(stringLiteral);
    CALL_SPECIALIZED_BEFORE(process, stringLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, stringLiteral, parent);

    return changed;
  }

  bool process(clang::CXXNullPtrLiteralExpr* nullLiteral, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireExprNums())
      em.add(nullLiteral);
    CALL_SPECIALIZED_BEFORE(process, nullLiteral, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, nullLiteral, parent);

    return changed;
  }

  bool process(clang::CompoundStmt* compoundStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, compoundStmt, parent);
    for(clang::Stmt* stmt : compoundStmt->body())
      changed |= process(stmt, compoundStmt);
    CALL_SPECIALIZED_AFTER(process, compoundStmt, parent);

    return changed;
  }

  bool process(clang::NullStmt* nullStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, nullStmt, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, nullStmt, parent);

    return changed;
  }

  bool process(clang::IfStmt* ifStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, ifStmt, parent);
    changed |= process(ifStmt->getCond(), ifStmt);
    if(clang::Stmt* thn = ifStmt->getThen())
      changed |= process(thn, ifStmt);
    if(clang::Stmt* els = ifStmt->getElse())
      changed |= process(els, ifStmt);
    CALL_SPECIALIZED_AFTER(process, ifStmt, parent);

    return changed;
  }

  bool process(clang::SwitchStmt* switchStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, switchStmt, parent);
    changed |= process(switchStmt->getCond(), switchStmt);
    for(clang::SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
        kase = kase->getNextSwitchCase())
      changed |= process(kase, switchStmt);
    CALL_SPECIALIZED_AFTER(process, switchStmt, parent);

    return changed;
  }

  bool process(clang::CaseStmt* caseStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, caseStmt, parent);
    changed |= process(caseStmt->getSubStmt(), caseStmt);
    CALL_SPECIALIZED_AFTER(process, caseStmt, parent);

    return changed;
  }

  bool process(clang::DefaultStmt* defaultStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, defaultStmt, parent);
    changed |= process(defaultStmt->getSubStmt(), defaultStmt);
    CALL_SPECIALIZED_AFTER(process, defaultStmt, parent);

    return changed;
  }

  bool process(clang::DoStmt* doStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, doStmt, parent);
    changed |= process(doStmt->getCond(), doStmt);
    changed |= process(doStmt->getBody(), doStmt);
    CALL_SPECIALIZED_AFTER(process, doStmt, parent);

    return changed;
  }

  bool process(clang::ForStmt* forStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, forStmt, parent);
    changed |= process(forStmt->getInit(), forStmt);
    changed |= process(forStmt->getCond(), forStmt);
    changed |= process(forStmt->getInc(), forStmt);
    changed |= process(forStmt->getBody(), forStmt);
    CALL_SPECIALIZED_AFTER(process, forStmt, parent);

    return changed;
  }

  bool process(clang::WhileStmt* whileStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, whileStmt, parent);
    changed |= process(whileStmt->getCond(), whileStmt);
    changed |= process(whileStmt->getBody(), whileStmt);
    CALL_SPECIALIZED_AFTER(process, whileStmt, parent);

    return changed;
  }

  bool process(clang::ReturnStmt* retStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, retStmt, parent);
    if(clang::Expr* retValue = retStmt->getRetValue())
      changed |= process(retValue, retStmt);
    CALL_SPECIALIZED_AFTER(process, retStmt, parent);

    return changed;
  }

  bool process(clang::LabelStmt* labelStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, labelStmt, parent);
    changed |= process(labelStmt->getSubStmt(), labelStmt);
    CALL_SPECIALIZED_AFTER(process, labelStmt, parent);

    return changed;
  }

  bool process(clang::GotoStmt* gotoStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, gotoStmt, parent);
    changed |= process(gotoStmt->getLabel()->getStmt(), gotoStmt);
    CALL_SPECIALIZED_AFTER(process, gotoStmt, parent);

    return changed;
  }

  bool process(clang::BreakStmt* breakStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, breakStmt, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, breakStmt, parent);

    return changed;
  }

  bool process(clang::ContinueStmt* continueStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, continueStmt, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, continueStmt, parent);

    return changed;
  }

  bool process(clang::DeclStmt* declStmt, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, declStmt, parent);
    for(clang::Decl* decl : declStmt->decls()) {
      if(auto* var = dyn_cast<clang::VarDecl>(decl)) {
        if(hasRequireUses())
          um.addDecl(var);
        if(var->hasInit())
          changed |= process(var->getInit(), declStmt);
      } else {
        fatal(error() << "Unknown decl in DeclStmt: "
                      << decl->getDeclKindName());
      }
    }
    CALL_SPECIALIZED_AFTER(process, declStmt, parent);

    return changed;
  }

  bool process(clang::DeclRefExpr* declRefExpr, clang::Stmt* parent) {
    bool changed = false;

    if(hasRequireUses()) {
      if(clang::VarDecl* var = Clang::getVar(declRefExpr)) {
        if(auto* binOp = dyn_cast<clang::BinaryOperator>(parent)) {
          if((binOp->getOpcode() == clang::BO_Assign)
             and (binOp->getLHS() == declRefExpr))
            um.addDef(var, parent);
          else
            um.addUse(var, parent);
        } else if(auto* unOp = dyn_cast<clang::UnaryOperator>(parent)) {
          if(unOp->getOpcode() == clang::UO_AddrOf)
            addrTaken.insert(var);
          um.addUse(var, parent);
        } else {
          um.addUse(var, parent);
        }
      }
    }
    if(hasRequireExprNums())
      em.add(declRefExpr);

    CALL_SPECIALIZED_BEFORE(process, declRefExpr, parent);
    // Do nothing
    CALL_SPECIALIZED_AFTER(process, declRefExpr, parent);

    return changed;
  }

  bool process(clang::UnaryOperator* unOp, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, unOp, parent);
    changed |= process(unOp->getSubExpr(), unOp);
    if(hasRequireExprNums())
      em.add(unOp);
    CALL_SPECIALIZED_AFTER(process, unOp, parent);

    return changed;
  }

  bool process(clang::BinaryOperator* binOp, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, binOp, parent);
    changed |= process(binOp->getLHS(), binOp);
    changed |= process(binOp->getRHS(), binOp);
    if(hasRequireExprNums())
      em.add(binOp);
    CALL_SPECIALIZED_AFTER(process, binOp, parent);

    return changed;
  }

  bool process(clang::ConditionalOperator* condOp, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, condOp, parent);
    changed |= process(condOp->getCond(), condOp);
    changed |= process(condOp->getTrueExpr(), condOp);
    changed |= process(condOp->getFalseExpr(), condOp);
    if(hasRequireExprNums())
      em.add(condOp);
    CALL_SPECIALIZED_AFTER(process, condOp, parent);

    return changed;
  }

  bool process(clang::ArraySubscriptExpr* arrExpr, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, arrExpr, parent);
    changed |= process(arrExpr->getBase(), arrExpr);
    changed |= process(arrExpr->getIdx(), arrExpr);
    if(hasRequireExprNums())
      em.add(arrExpr);
    CALL_SPECIALIZED_AFTER(process, arrExpr, parent);

    return changed;
  }

  bool process(clang::CallExpr* callExpr, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, callExpr, parent);
    changed |= process(callExpr->getCallee(), callExpr);
    for(clang::Expr* arg : callExpr->arguments())
      changed |= process(arg, callExpr);
    if(hasRequireExprNums())
      em.add(callExpr);
    CALL_SPECIALIZED_AFTER(process, callExpr, parent);

    return changed;
  }

  bool process(clang::CStyleCastExpr* castExpr, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, castExpr, parent);
    changed |= process(castExpr->getSubExpr(), castExpr);
    if(hasRequireExprNums())
      em.add(castExpr);
    CALL_SPECIALIZED_AFTER(process, castExpr, parent);

    return changed;
  }

  bool process(clang::MemberExpr* memberExpr, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, memberExpr, parent);
    changed |= process(memberExpr->getBase(), memberExpr);
    if(hasRequireExprNums())
      em.add(memberExpr);
    CALL_SPECIALIZED_AFTER(process, memberExpr, parent);

    return changed;
  }

  bool process(clang::InitListExpr* initListExpr, clang::Stmt* parent) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, initListExpr, parent);
    for(unsigned i = 0; i < initListExpr->getNumInits(); i++)
      changed |= process(initListExpr->getInit(i), initListExpr);
    if(hasRequireExprNums())
      em.add(initListExpr);
    CALL_SPECIALIZED_AFTER(process, initListExpr, parent);

    return changed;
  }

  bool process(clang::Stmt* stmt, clang::Stmt* parent) {
    if(not isa<clang::NullStmt>(stmt))
      pm.add(stmt, parent);

    if(auto* compoundStmt = dyn_cast<clang::CompoundStmt>(stmt))
      return process(compoundStmt, parent);
    else if(auto* ifStmt = dyn_cast<clang::IfStmt>(stmt))
      return process(ifStmt, parent);
    else if(auto* doStmt = dyn_cast<clang::DoStmt>(stmt))
      return process(doStmt, parent);
    else if(auto* whileStmt = dyn_cast<clang::WhileStmt>(stmt))
      return process(whileStmt, parent);
    else if(auto* forStmt = dyn_cast<clang::ForStmt>(stmt))
      return process(forStmt, parent);
    else if(auto* switchStmt = dyn_cast<clang::SwitchStmt>(stmt))
      return process(switchStmt, parent);
    else if(auto* castStmt = dyn_cast<clang::CaseStmt>(stmt))
      return process(castStmt, parent);
    else if(auto* defaultStmt = dyn_cast<clang::DefaultStmt>(stmt))
      return process(defaultStmt, parent);
    else if(auto* retStmt = dyn_cast<clang::ReturnStmt>(stmt))
      return process(retStmt, parent);
    else if(auto* nullStmt = dyn_cast<clang::NullStmt>(stmt))
      return process(nullStmt, parent);
    else if(auto* breakStmt = dyn_cast<clang::BreakStmt>(stmt))
      return process(breakStmt, parent);
    else if(auto* continueStmt = dyn_cast<clang::ContinueStmt>(stmt))
      return process(continueStmt, parent);
    else if(auto* labelStmt = dyn_cast<clang::LabelStmt>(stmt))
      return process(labelStmt, parent);
    else if(auto* gotoStmt = dyn_cast<clang::GotoStmt>(stmt))
      return process(gotoStmt, parent);
    else if(auto* declStmt = dyn_cast<clang::DeclStmt>(stmt))
      return process(declStmt, parent);
    else if(auto* initList = dyn_cast<clang::InitListExpr>(stmt))
      return process(initList, parent);
    else if(auto* binOp = dyn_cast<clang::BinaryOperator>(stmt))
      return process(binOp, parent);
    else if(auto* unOp = dyn_cast<clang::UnaryOperator>(stmt))
      return process(unOp, parent);
    else if(auto* condOp = dyn_cast<clang::ConditionalOperator>(stmt))
      return process(condOp, parent);
    else if(auto* callExpr = dyn_cast<clang::CallExpr>(stmt))
      return process(callExpr, parent);
    else if(auto* memberExpr = dyn_cast<clang::MemberExpr>(stmt))
      return process(memberExpr, parent);
    else if(auto* castExpr = dyn_cast<clang::CStyleCastExpr>(stmt))
      return process(castExpr, parent);
    else if(auto* arrExpr = dyn_cast<clang::ArraySubscriptExpr>(stmt))
      return process(arrExpr, parent);
    else if(auto* declRefExpr = dyn_cast<clang::DeclRefExpr>(stmt))
      return process(declRefExpr, parent);
    else if(auto* boolLit = dyn_cast<clang::CXXBoolLiteralExpr>(stmt))
      return process(boolLit, parent);
    else if(auto* charLit = dyn_cast<clang::CharacterLiteral>(stmt))
      return process(charLit, parent);
    else if(auto* intLit = dyn_cast<clang::IntegerLiteral>(stmt))
      return process(intLit, parent);
    else if(auto* fpLit = dyn_cast<clang::FloatingLiteral>(stmt))
      return process(fpLit, parent);
    else if(auto* stringLit = dyn_cast<clang::StringLiteral>(stmt))
      return process(stringLit, parent);
    else if(auto* cnull = dyn_cast<clang::CXXNullPtrLiteralExpr>(stmt))
      return process(cnull, parent);
    else
      fatal(error() << "Unknown expression: " << stmt->getStmtClassName());

    return false;
  }

  bool process(clang::FunctionDecl* f) {
    bool changed = false;

    CALL_SPECIALIZED_BEFORE(process, f, nullptr);
    changed |= process(f->getBody(), nullptr);
    CALL_SPECIALIZED_AFTER(process, f, nullptr);

    return changed;
  }

  void reset(clang::FunctionDecl* f) {
    um.clear(f);
    addrTaken.clear();
    em.clear();
    pm.clear();
    if(hasRequireCFG())
      ast->updateCFG();
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
    if(not hasOnePass()) {
      bool iterChanged;
      do {
        reset(f);
        iterChanged = process(f);
        changed |= iterChanged;
      } while(iterChanged);
    } else {
      reset(f);
      changed |= process(f);
    }
    changed |= doFinalize(f);

    return changed;
  }

public:
  friend class ASTPassManager;
};

} // namespace cish

#endif // CISH_AST_FUNCTION_PASS_H
