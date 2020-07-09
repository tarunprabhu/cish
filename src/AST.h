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

#ifndef CISH_AST_H
#define CISH_AST_H

#include "Map.h"
#include "Set.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class CishContext;
class ParentMap;

// AST construction and manipulation class. This is entirely stateless and
class AST {
protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;

  // ParentMap will be non-null when the object is used in a transformation
  // pass. It is only used when performing replacements
  ParentMap* pm;

private:
  clang::FullSourceLoc invLoc;

protected:
  bool replace(clang::UnaryOperator* unOp, clang::Expr* repl);
  bool
  replace(clang::BinaryOperator* binOp, clang::Expr* repl, bool replaceLHS);
  bool replace(clang::ArraySubscriptExpr* arrExpr,
               clang::Expr* repl,
               bool replaceBase);
  bool replace(clang::CallExpr* callExpr, clang::Expr* repl, long i);
  bool replace(clang::CStyleCastExpr* castExpr, clang::Expr* repl);
  bool replace(clang::MemberExpr* memberExpr, clang::Expr* repl);
  bool replace(clang::ReturnStmt* retStmt, clang::Expr* repl);
  bool replace(clang::IfStmt* ifStmt, clang::Expr* repl);
  bool replace(clang::SwitchStmt* switchStmt, clang::Expr* repl);
  bool replace(clang::DoStmt* doStmt, clang::Expr* repl);
  bool replace(clang::ForStmt* forStmt, clang::Expr* repl);
  bool replace(clang::WhileStmt* whileStmt, clang::Expr* repl);
  bool replace(clang::Stmt* parent, clang::Expr* expr, clang::Expr* repl);

  clang::Stmt* clone(clang::CXXBoolLiteralExpr*);
  clang::Stmt* clone(clang::CharacterLiteral*);
  clang::Stmt* clone(clang::IntegerLiteral*);
  clang::Stmt* clone(clang::FloatingLiteral*);
  clang::Stmt* clone(clang::StringLiteral*);
  clang::Stmt* clone(clang::CXXNullPtrLiteralExpr*);
  clang::Stmt* clone(clang::UnaryOperator*);
  clang::Stmt* clone(clang::BinaryOperator*);
  clang::Stmt* clone(clang::ConditionalOperator*);
  clang::Stmt* clone(clang::ArraySubscriptExpr*);
  clang::Stmt* clone(clang::CallExpr*);
  clang::Stmt* clone(clang::CStyleCastExpr*);
  clang::Stmt* clone(clang::MemberExpr*);
  clang::Stmt* clone(clang::InitListExpr*);
  clang::Stmt* clone(clang::DeclRefExpr*);
  clang::Stmt* clone(clang::CompoundStmt*);
  clang::Stmt* clone(clang::IfStmt*);
  clang::Stmt* clone(clang::SwitchStmt*);
  clang::Stmt* clone(clang::CaseStmt*);
  clang::Stmt* clone(clang::DefaultStmt*);
  clang::Stmt* clone(clang::DoStmt*);
  clang::Stmt* clone(clang::ForStmt*);
  clang::Stmt* clone(clang::WhileStmt*);
  clang::Stmt* clone(clang::GotoStmt*);
  clang::Stmt* clone(clang::BreakStmt*);
  clang::Stmt* clone(clang::ContinueStmt*);
  clang::Stmt* clone(clang::ReturnStmt*);
  clang::Stmt* clone(clang::NullStmt*);
  clang::Stmt* clone(clang::LabelStmt*);

  clang::LabelStmt* createLabelStmt(clang::LabelDecl* label);

public:
  AST(CishContext& cishContext);
  AST(CishContext& cishContext, ParentMap& pm);
  AST(const AST&) = delete;
  AST(AST&&) = delete;

  bool replaceEqvUsesWith(clang::Expr* expr, clang::Expr* repl);
  bool
  replaceExprWith(clang::Expr* expr, clang::Expr* repl, clang::Stmt* parent);
  bool
  replaceStmtWith(clang::Stmt* stmt, clang::Stmt* repl, clang::Stmt* parent);
  bool erase(clang::Stmt* stmt, clang::Stmt* parent);

  // AST construction functions
  clang::Stmt* clone(clang::Stmt* stmt);
  clang::Expr* cloneExpr(clang::Expr* expr);

  clang::DeclarationNameInfo getDeclarationNameInfo(const std::string& name);
  clang::IdentifierInfo& getIdentifierInfo(const std::string& name);
  clang::DeclarationName createDeclName(const std::string& name);

  // Decls
  clang::RecordDecl* createStruct(const std::string& name);
  clang::FieldDecl* createField(const std::string& name,
                                clang::QualType type,
                                clang::RecordDecl* strct);
  clang::FunctionDecl* createFunction(const std::string& name,
                                      clang::QualType type);
  clang::ParmVarDecl* createParam(const std::string& name,
                                  clang::QualType type,
                                  clang::FunctionDecl* func);
  clang::VarDecl* createLocalVariable(const std::string& name,
                                          clang::QualType type,
                                          clang::FunctionDecl* func);
  clang::VarDecl* createGlobalVariable(const std::string& name,
                                           clang::QualType type);

  clang::LabelDecl* createLabelDecl(clang::FunctionDecl* f,
                                    const std::string& name);

  // Literals
  clang::CXXBoolLiteralExpr* createBoolLiteral(bool b);
  clang::CXXBoolLiteralExpr* createBoolLiteral(bool b, clang::QualType type);
  clang::CharacterLiteral* createCharacterLiteral(unsigned c);
  clang::IntegerLiteral* createIntLiteral(const llvm::APInt& i,
                                          clang::QualType type);
  clang::IntegerLiteral* createIntLiteral(short i);
  clang::IntegerLiteral* createIntLiteral(unsigned short i);
  clang::IntegerLiteral* createIntLiteral(int i);
  clang::IntegerLiteral* createIntLiteral(unsigned int i);
  clang::IntegerLiteral* createIntLiteral(long i);
  clang::IntegerLiteral* createIntLiteral(unsigned long i);
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

  // Exprs
  clang::DeclRefExpr* createDeclRefExpr(clang::ValueDecl* decl);
  clang::UnaryOperator* createUnaryOperator(clang::Expr* lhs,
                                            clang::UnaryOperator::Opcode op,
                                            clang::QualType type);
  clang::BinaryOperator* createBinaryOperator(clang::Expr* lhs,
                                              clang::Expr* rhs,
                                              clang::BinaryOperator::Opcode op,
                                              clang::QualType type);
  clang::ConditionalOperator* createConditionalOperator(clang::Expr* cond,
                                                        clang::Expr* t,
                                                        clang::Expr* f,
                                                        clang::QualType type);
  clang::ArraySubscriptExpr* createArraySubscriptExpr(clang::Expr* arr,
                                                      clang::Expr* idx,
                                                      clang::QualType type);
  clang::CallExpr* createCallExpr(clang::Expr* callee,
                                  const Vector<clang::Expr*>& args,
                                  clang::QualType type);
  clang::CStyleCastExpr* createCastExpr(clang::Expr* expr,
                                        clang::QualType type);
  clang::MemberExpr* createMemberExpr(clang::Expr* base,
                                      clang::ValueDecl* member,
                                      clang::QualType type);
  clang::InitListExpr* createInitListExpr(const Vector<clang::Expr*>& exprs);

  // Statements. These can only be top-level statements. Exprs can also be
  // top level statements at times, but these can never appear as an operand
  // of an expression
  clang::NullStmt* createNullStmt();
  clang::CompoundStmt* createCompoundStmt(const Vector<clang::Stmt*>& stmts);
  clang::CompoundStmt* createCompoundStmt(clang::Stmt* stmt);
  clang::IfStmt*
  createIfStmt(clang::Expr* cond, clang::Stmt* thn, clang::Stmt* els = nullptr);
  clang::SwitchStmt* createSwitchStmt(clang::Expr* cond, clang::Stmt* body);
  clang::CaseStmt* createCaseStmt(clang::Expr* value, clang::Stmt* body);
  clang::DefaultStmt* createDefaultStmt(clang::Stmt* body);
  clang::DoStmt* createDoStmt(clang::Stmt* body, clang::Expr* cond);
  clang::WhileStmt* createWhileStmt(clang::Expr* cond, clang::Stmt* body);
  clang::ForStmt* createForStmt(clang::Stmt* init,
                                clang::Expr* cond,
                                clang::Expr* inc,
                                clang::Stmt* body);
  clang::GotoStmt* createGotoStmt(clang::LabelDecl* label);
  clang::BreakStmt* createBreakStmt();
  clang::ContinueStmt* createContinueStmt();
  clang::ReturnStmt* createReturnStmt(clang::Expr* retExpr);

  clang::DeclStmt* createDeclStmt(clang::Decl* decl);
  clang::DeclStmt* createDeclStmt(Vector<clang::Decl*>& decls);

  // This is used to create a variable but it is not added to the decl list of
  // the function. This is useful when declaring variables for private use
  // inside an inner scope of the function
  clang::VarDecl* createVariable(const std::string& name,
                                 clang::QualType type,
                                 clang::DeclContext* declContext);
};

} // namespace cish

#endif // CISH_AST_H
