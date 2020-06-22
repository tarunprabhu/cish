#ifndef CISH_AST_EXPR_PASS_H
#define CISH_AST_EXPR_PASS_H

#include "ASTFunctionPass.h"

namespace cish {

class ASTExprPass : public ASTFunctionPass {
protected:
  virtual void process(clang::IfStmt* ifStmt);
  virtual void process(clang::DoStmt* doStmt);
  virtual void process(clang::WhileStmt* whileStmt);
  virtual void process(clang::ForStmt* forStmt);
  virtual void process(clang::SwitchStmt* swtchStmt);
  virtual void process(clang::CaseStmt* caseStmt);
  virtual void process(clang::DefaultStmt* defaultStmt);
  virtual void process(clang::ReturnStmt* retStmt);
  virtual void process(clang::BreakStmt* brkStmt);
  virtual void process(clang::ContinueStmt* continueStmt);
  virtual void process(clang::LabelStmt* labelStmt);
  virtual void process(clang::GotoStmt* gotoStmt);
  virtual void process(clang::DeclStmt* declStmt);
  virtual void process(clang::CompoundStmt* cmpndStmt);
  virtual void process(clang::Stmt* stmt);

  virtual clang::Expr* process(clang::CXXBoolLiteralExpr* boolExpr);
  virtual clang::Expr* process(clang::CharacterLiteral* charLiteral);
  virtual clang::Expr* process(clang::IntegerLiteral* intLiteral);
  virtual clang::Expr* process(clang::FloatingLiteral* floatLiteral);
  virtual clang::Expr* process(clang::StringLiteral* stringLiteral);
  virtual clang::Expr* process(clang::CXXNullPtrLiteralExpr *nullLiteral);
  virtual clang::Expr* process(clang::DeclRefExpr* declRefExpr);
  virtual clang::Expr* process(clang::InitListExpr* initList);
  virtual clang::Expr* process(clang::BinaryOperator* binOp);
  virtual clang::Expr* process(clang::UnaryOperator* unOp);
  virtual clang::Expr* process(clang::ConditionalOperator* cond);
  virtual clang::Expr* process(clang::CallExpr* call);
  virtual clang::Expr* process(clang::CStyleCastExpr* cexpr);
  virtual clang::Expr* process(clang::ArraySubscriptExpr* arrayExpr);
  virtual clang::Expr* process(clang::Expr* expr);

public:
  ASTExprPass(clang::ASTContext& astContext);
  ASTExprPass(const clang::ASTContext&) = delete;
  ASTExprPass(clang::ASTContext&&) = delete;
  virtual ~ASTExprPass() = default;

  virtual llvm::StringRef getPassName() const override = 0;
  virtual void runOnFunction(clang::FunctionDecl* f) override;
};

} // namespace cish

#endif // CISH_AST_EXPR_PASS_H
