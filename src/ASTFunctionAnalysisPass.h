#ifndef AST_FUNCTION_ANALYSIS_PASS_H
#define AST_FUNCTION_ANALYSIS_PASS_H

#include "ASTFunctionPass.h"
#include "Diagnostics.h"

using clang::dyn_cast;

namespace cish {

// specialization that does the checking

// Base class to walk over all statements and expressions in a function. This
// is intended for analysis passses only, so the AST nodes are all marked
// const

template <typename DerivedT>
class ASTFunctionAnalysisPass : public ASTFunctionPass {
protected:
  template <typename ClangStmt>
  struct has_process {
  private:
    template <typename T>
    static constexpr auto check(T*) -> typename std::is_same<
        decltype(std::declval<T>().process(std::declval<ClangStmt>())),
        void>::type;

    template <typename>
    static constexpr std::false_type check(...);

    typedef decltype(check<DerivedT>(0)) type;

  public:
    static constexpr bool value = type::value;
  };

  template <typename ClangStmt,
            std::enable_if_t<has_process<ClangStmt*>::value, int> = 0>
  void processDerived(ClangStmt* stmt) {
    static_cast<DerivedT*>(this)->process(stmt);
  }

  template <typename ClangStmt,
            std::enable_if_t<!has_process<ClangStmt*>::value, int> = 0>
  void processDerived(ClangStmt*) {
    ;
  }

protected:
  void process(clang::CompoundStmt* compoundStmt) {
    processDerived(compoundStmt);
    for(clang::Stmt* stmt : compoundStmt->body())
      process(stmt);
  }

  void process(clang::IfStmt* ifStmt) {
    processDerived(ifStmt);
    process(ifStmt->getCond());
    if(clang::Stmt* thn = ifStmt->getThen())
      process(thn);
    if(clang::Stmt* els = ifStmt->getElse())
      process(els);
  }

  void process(clang::DoStmt* doStmt) {
    processDerived(doStmt);
    process(doStmt->getCond());
    process(doStmt->getBody());
  }

  void process(clang::ForStmt* forStmt) {
    processDerived(forStmt);
    process(forStmt->getInit());
    process(forStmt->getCond());
    process(forStmt->getInc());
  }

  void process(clang::WhileStmt* whileStmt) {
    processDerived(whileStmt);
    process(whileStmt->getCond());
    process(whileStmt->getBody());
  }

  void process(clang::SwitchStmt* switchStmt) {
    processDerived(switchStmt);
    process(switchStmt->getCond());
    for(clang::SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
        kase = kase->getNextSwitchCase())
      process(kase);
  }

  void process(clang::CaseStmt* caseStmt) {
    processDerived(caseStmt);
    process(caseStmt->getSubStmt());
  }

  void process(clang::DefaultStmt* defaultStmt) {
    processDerived(defaultStmt);
    process(defaultStmt->getSubStmt());
  }

  void process(clang::ReturnStmt* retStmt) {
    processDerived(retStmt);
    if(clang::Expr* retValue = retStmt->getRetValue())
      process(retValue);
  }

  void process(clang::LabelStmt* labelStmt) {
    processDerived(labelStmt);
  }

  void process(clang::GotoStmt* gotoStmt) {
    processDerived(gotoStmt);
  }

  void process(clang::BreakStmt* breakStmt) {
    processDerived(breakStmt);
  }

  void process(clang::ContinueStmt* continueStmt) {
    processDerived(continueStmt);
  }

  void process(clang::DeclStmt* declStmt) {
    processDerived(declStmt);
    for(clang::Decl* decl : declStmt->decls()) {
      if(auto* var = dyn_cast<clang::VarDecl>(decl)) {
        if(var->hasInit())
          process(var->getInit());
      } else {
        fatal(error() << "Unknown decl in DeclStmt: "
                      << decl->getDeclKindName());
      }
    }
  }

  void process(clang::CXXBoolLiteralExpr* boolLiteral) {
    processDerived(boolLiteral);
    return;
  }

  void process(clang::CharacterLiteral* charLiteral) {
    processDerived(charLiteral);
    return;
  }

  void process(clang::IntegerLiteral* intLiteral) {
    processDerived(intLiteral);
    return;
  }

  void process(clang::FloatingLiteral* floatLiteral) {
    processDerived(floatLiteral);
    return;
  }

  void process(clang::StringLiteral* stringLiteral) {
    processDerived(stringLiteral);
    return;
  }

  void process(clang::CXXNullPtrLiteralExpr* nullLiteral) {
    processDerived(nullLiteral);
    return;
  }

  void process(clang::DeclRefExpr* declRef) {
    processDerived(declRef);
    return;
  }

  void process(clang::InitListExpr* initList) {
    processDerived(initList);
    for(unsigned i = 0; i < initList->getNumInits(); i++)
      process(initList->getInit(i));
  }

  void process(clang::BinaryOperator* binOp) {
    processDerived(binOp);
    process(binOp->getLHS());
    process(binOp->getRHS());
  }

  void process(clang::UnaryOperator* unOp) {
    processDerived(unOp);
    process(unOp->getSubExpr());
  }

  void process(clang::ConditionalOperator* condOp) {
    processDerived(condOp);
    process(condOp->getCond());
    process(condOp->getTrueExpr());
    process(condOp->getFalseExpr());
  }

  void process(clang::CallExpr* callExpr) {
    processDerived(callExpr);
    process(callExpr->getCallee());
    for(clang::Expr* arg : callExpr->arguments())
      process(arg);
  }

  void process(clang::MemberExpr* memberExpr) {
    processDerived(memberExpr);
    process(memberExpr->getBase());
  }

  void process(clang::CStyleCastExpr* castExpr) {
    processDerived(castExpr);
    process(castExpr->getSubExpr());
  }

  void process(clang::ArraySubscriptExpr* arrExpr) {
    processDerived(arrExpr);
    process(arrExpr->getBase());
    process(arrExpr->getIdx());
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
  }

  void process(clang::FunctionDecl* f) {
    processDerived(f);
    process(f->getBody());
  }

public:
  ASTFunctionAnalysisPass(CishContext& context) : ASTFunctionPass(context) {
    ;
  }

  ASTFunctionAnalysisPass(ASTFunctionAnalysisPass&) = delete;
  ASTFunctionAnalysisPass(ASTFunctionAnalysisPass&&) = delete;
  virtual ~ASTFunctionAnalysisPass() = default;

  virtual llvm::StringRef getPassName() const override = 0;
  virtual bool runOnFunction(clang::FunctionDecl* f) override {
    process(f);

    return false;
  }
};

} // namespace cish

#endif // AST_FUNCTION_ANALYSIS_PASS_H
