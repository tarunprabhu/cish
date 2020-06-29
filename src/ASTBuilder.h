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

#ifndef CISH_AST_BUILDER_H
#define CISH_AST_BUILDER_H

#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

// Convenience functions to create AST nodes
class ASTBuilder {
protected:
  clang::ASTContext& astContext;
  clang::FullSourceLoc invLoc;

public:
  ASTBuilder(clang::ASTContext& astContext);
  ASTBuilder(const ASTBuilder&) = delete;
  ASTBuilder(ASTBuilder&&) = delete;

  clang::DeclarationName createDeclName(const std::string& name);

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
  clang::NullStmt* createNullStmt();
  clang::IfStmt*
  createIfStmt(clang::Expr* cond, clang::Stmt* thn, clang::Stmt* els = nullptr);
  clang::GotoStmt* createGotoStmt(clang::LabelDecl* label);
  clang::CompoundStmt* createCompoundStmt(const Vector<clang::Stmt*>& stmts);
  clang::CompoundStmt* createCompoundStmt(clang::Stmt* stmt);
  clang::BreakStmt* createBreakStmt();
  clang::ContinueStmt* createContinueStmt();
  clang::ReturnStmt* createReturnStmt(clang::Expr* retExpr);
  clang::DeclRefExpr* createDeclRefExpr(clang::ValueDecl* decl);
  clang::CallExpr* createCallExpr(clang::Expr* callee,
                                  const Vector<clang::Expr*>& args,
                                  clang::QualType type);
  clang::MemberExpr* createMemberExpr(clang::Expr* base,
                                      clang::ValueDecl* member,
                                      clang::QualType type);
  clang::DoStmt* createDoStmt(clang::Stmt* body, clang::Expr* cond);
  clang::WhileStmt* createWhileStmt(clang::Expr* cond, clang::Stmt* body);
  clang::ForStmt* createForStmt(clang::Stmt* init, clang::Expr* cond,
                                clang::Expr* inc, clang::Stmt* body = nullptr);
  clang::CXXBoolLiteralExpr* createBoolLiteral(bool b);
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

  // FIXME: This should probably go away. Right now it is used for "temporary"
  // variables which themselves should probably go away
  clang::DeclRefExpr* createVariable(const std::string& name,
                                     clang::QualType type,
                                     clang::DeclContext* parent);
};

} // namespace cish

#endif // CISH_AST_BUILDER_H
