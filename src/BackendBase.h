#ifndef CISH_BACKEND_BASE_H
#define CISH_BACKEND_BASE_H

#include "Map.h"
#include "Stack.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class CishContext;

class BackendBase {
private:
  std::string varPrefix;
  uint64_t varSuffix;

protected:
  CishContext& context;

  // All of these are needed to set up the clang::ASTContext object
  // std::unique_ptr<clang::ASTContext> astContext;
  clang::ASTContext& astContext;

  // We obviously don't have any SourceLocations, so just use this wherever
  // we need a clang::SourceLocation
  const clang::FullSourceLoc invLoc;

  // Need valid TypeSourceInfo otherwise the AST cannot be walked
  Map<const clang::Type*, clang::TypeSourceInfo*> typeSrcInfo;

  // The statements comprising the body of the current function being
  // converted
  Stack<Vector<clang::Stmt*>> stmts;

  Map<std::string, clang::LabelDecl*> labels;

protected:
  BackendBase(CishContext& context);
  BackendBase() = delete;
  BackendBase(const BackendBase&) = delete;
  BackendBase(BackendBase&&) = delete;

  void beginFunction(clang::FunctionDecl* f);
  void endFunction(clang::FunctionDecl* f);

  clang::Stmt* add(clang::Stmt* stmt);
  clang::Stmt& add(clang::Stmt& stmt);

  clang::RecordDecl* createStruct(const std::string& name);

  clang::FieldDecl* createField(const std::string& name,
                                clang::QualType type,
                                clang::RecordDecl* strct);

  clang::FunctionDecl* createFunction(const std::string& name,
                                      clang::QualType type);

  clang::ParmVarDecl* createParam(const std::string& name,
                                  clang::QualType type,
                                  clang::FunctionDecl* func);

  clang::DeclRefExpr* createLocalVariable(const std::string& name,
                                          clang::QualType type,
                                          clang::FunctionDecl* func);

  clang::DeclRefExpr* createGlobalVariable(const std::string& name,
                                           clang::QualType type);

  // FIXME: This should probably go away. Right now it is used for "temporary"
  // variables which themselves should probably go away
  clang::DeclRefExpr* createVariable(const std::string& name,
                                     clang::QualType type,
                                     clang::DeclContext* parent);

  clang::ConditionalOperator* createConditionalOperator(clang::Expr* cond,
                                                        clang::Expr* t,
                                                        clang::Expr* f,
                                                        clang::QualType type);

  clang::BinaryOperator* createBinaryOperator(clang::Expr* lhs,
                                              clang::Expr* rhs,
                                              clang::BinaryOperator::Opcode op,
                                              clang::QualType type);

  clang::UnaryOperator* createUnaryOperator(clang::Expr* lhs,
                                            clang::UnaryOperator::Opcode op,
                                            clang::QualType type);

  clang::ArraySubscriptExpr* createArraySubscriptExpr(clang::Expr* arr,
                                                      clang::Expr* idx,
                                                      clang::QualType type);

  clang::CStyleCastExpr* createCastExpr(clang::Expr* expr,
                                        clang::QualType type);

  clang::IfStmt*
  createIfStmt(clang::Expr* cond, clang::Stmt* thn, clang::Stmt* els = nullptr);

  clang::GotoStmt* createGotoStmt(clang::LabelDecl* label);

  clang::CompoundStmt*
  createCompoundStmt(const cish::Vector<clang::Stmt*>& stmts);

  clang::CompoundStmt* createCompoundStmt(clang::Stmt* stmt);

  clang::BreakStmt* createBreakStmt();

  clang::ContinueStmt* createContinueStmt();

  clang::ReturnStmt* createReturnStmt(clang::Expr* retExpr);

  clang::DeclRefExpr* createDeclRefExpr(clang::ValueDecl* decl);

  clang::CallExpr* createCallExpr(clang::Expr* callee,
                                  const Vector<clang::Expr*>& args,
                                  clang::QualType type);

  clang::DoStmt* createDoStmt(clang::Stmt* body, clang::Expr* cond);
  clang::WhileStmt* createWhileStmt(clang::Expr* cond, clang::Stmt* body);

  clang::CXXBoolLiteralExpr* createBoolLiteral(bool b, clang::QualType type);
  clang::IntegerLiteral* createIntLiteral(const llvm::APInt& i,
                                          clang::QualType type);
  clang::IntegerLiteral* createIntLiteral(short i);
  clang::IntegerLiteral* createIntLiteral(int i);
  clang::IntegerLiteral* createIntLiteral(long i);
  clang::FloatingLiteral* createFloatLiteral(const llvm::APFloat& f,
                                             clang::QualType type);
  clang::FloatingLiteral* createFloatLiteral(float f);
  clang::FloatingLiteral* createFloatLiteral(double g);
  clang::FloatingLiteral* createFloatLiteral(long double f);
  clang::StringLiteral* createStringLiteral(llvm::StringRef str,
                                            clang::QualType type);
  clang::StringLiteral* createStringLiteral(const std::string& str,
                                            clang::QualType type);
  clang::StringLiteral* createStringLiteral(const char* cstr,
                                            clang::QualType type);
  clang::CXXNullPtrLiteralExpr* createNullptr(clang::QualType type);
  clang::InitListExpr* createInitListExpr(const Vector<clang::Expr*>& exprs);

  clang::DeclStmt* createDeclStmt(clang::Decl* decl);
  clang::LabelStmt* createLabelStmt(clang::LabelDecl* label);
  clang::LabelDecl* createLabelDecl(clang::FunctionDecl* f,
                                    const std::string& name);

  clang::SwitchStmt* createSwitchStmt(clang::Expr* cond);
  clang::CaseStmt* createCaseStmt(clang::Expr* value);
  clang::DefaultStmt* createDefaultStmt(clang::Stmt* body);

public:
  virtual ~BackendBase() = default;

  /// Returns a new entity name that (hopefully) will not collide with
  /// any other entity name that already exists in the code being Cish'ed
  ///
  /// @param[optional] prefix An additional prefix to add to the variable
  ///                         name. Useful to disambiguate between local
  ///                         variables, args, blocks, labels etc.
  /// @returns The new variable name
  std::string getNewVar(const std::string& prefix = "v");
};

} // namespace cish

#endif // CISH_BACKEND_BASE_H
