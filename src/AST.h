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

#include "ExprNumberMap.h"
#include "Map.h"
#include "ParentMap.h"
#include "Set.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>
#include <clang/AST/ParentMap.h>
#include <clang/Analysis/Analyses/Dominators.h>
#include <clang/Analysis/CFG.h>
#include <clang/Analysis/CFGStmtMap.h>

#include <llvm/ADT/iterator_range.h>

namespace cish {

class ASTPass;
class CishContext;

// Single class that keeps track of the AST. Any modifications to the AST
// must be done through this graph to keep the def-use information, flow
// graph and parent-child relationships between statements consistent
class AST {
protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;
  clang::FunctionDecl* decl;
  ParentMap pm;
  ExprNumberMap en;
  clang::CFG::BuildOptions cfgBuildOpts;
  std::unique_ptr<clang::ParentMap> stmtParents;
  std::unique_ptr<clang::CFG> cfg;
  std::unique_ptr<clang::CFGStmtMap> cfgStmtMap;
  std::unique_ptr<clang::DominatorTree> dt;

  // For now, every def of a variable is an assignment statement.
  // In clang-speak, this would be a BinaryOperator where the operand is
  // BO_Assign. When returning the def, it will return the entire statement
  Map<clang::VarDecl*, Set<clang::Stmt*>> defMap;

  // The uses are the nearest Expr containing the variable directly.
  // The Expr in the use will be a DeclRefExpr but since those are not uniqued,
  // they can't be used to keep the map
  Map<clang::VarDecl*, Set<clang::Stmt*>> useMap;

  // Variables that have their address taken
  Set<clang::VarDecl*> addrTaken;

  Set<clang::VarDecl*> locals;
  Set<clang::VarDecl*> globals;

private:
  clang::FullSourceLoc invLoc;

protected:
  void add(clang::Expr* expr, clang::Stmt* user);
  void remove(clang::Expr* expr, clang::Stmt* user);
  void addDef(clang::VarDecl* var, clang::Stmt* user);
  void addUse(clang::VarDecl* var, clang::Stmt* user);

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

  void recalculateCFG();
  void recalculateDefUse();

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

  void remove(clang::CXXBoolLiteralExpr*);
  void remove(clang::CharacterLiteral*);
  void remove(clang::IntegerLiteral*);
  void remove(clang::FloatingLiteral*);
  void remove(clang::StringLiteral*);
  void remove(clang::CXXNullPtrLiteralExpr*);
  void remove(clang::UnaryOperator*);
  void remove(clang::BinaryOperator*);
  void remove(clang::ConditionalOperator*);
  void remove(clang::ArraySubscriptExpr*);
  void remove(clang::CallExpr*);
  void remove(clang::CStyleCastExpr*);
  void remove(clang::MemberExpr*);
  void remove(clang::InitListExpr*);
  void remove(clang::DeclRefExpr*);
  void remove(clang::CompoundStmt*);
  void remove(clang::IfStmt*);
  void remove(clang::SwitchStmt*);
  void remove(clang::CaseStmt*);
  void remove(clang::DefaultStmt*);
  void remove(clang::DoStmt*);
  void remove(clang::ForStmt*);
  void remove(clang::WhileStmt*);
  void remove(clang::GotoStmt*);
  void remove(clang::BreakStmt*);
  void remove(clang::ContinueStmt*);
  void remove(clang::ReturnStmt*);
  void remove(clang::NullStmt*);
  void remove(clang::LabelStmt*);
  void remove(clang::Stmt*);

  clang::LabelStmt* createLabelStmt(clang::LabelDecl*);

public:
  AST(CishContext& cishContext);
  AST(CishContext& cishContext, clang::FunctionDecl* decl, const AST&);
  AST(const AST&) = delete;
  AST(AST&&) = delete;

  clang::FunctionDecl* getFunction() const;

  void removeUse(clang::VarDecl* var, clang::Stmt* stmt);
  void removeDef(clang::VarDecl* var, clang::Stmt* stmt);
  bool replaceAllUsesWith(clang::VarDecl* var, clang::Expr* expr);
  bool replaceEqvUsesWith(clang::Expr* expr, clang::Expr* repl);
  bool replaceExprWith(clang::Expr* expr, clang::Expr* repl);
  bool replaceStmtWith(clang::Stmt* old, clang::Stmt* repl);
  bool erase(clang::Stmt* stmt);
  bool erase(clang::VarDecl* var);

  const ParentMap& getParentMap() const;
  const ExprNumberMap& getExprNumberMap() const;
  const clang::DominatorTree& getDominatorTree() const;
  clang::CFG* getCFG() const;
  clang::CFGBlock* getCFGBlock(clang::Stmt* stmt) const;
  const clang::CFG::BuildOptions& getCFGBuildOpts() const;

  bool hasAddressTaken(clang::VarDecl* var) const;
  void resetAddressTaken(clang::VarDecl* var);

  Set<clang::Stmt*> getTopLevelDefs(clang::VarDecl* var) const;
  const Set<clang::Stmt*>& getDefs(clang::VarDecl* var) const;
  unsigned getNumDefs(clang::VarDecl* var) const;
  bool isDefined(clang::VarDecl* var) const;
  bool hasZeroDefs(clang::VarDecl* var) const;
  bool hasSingleDef(clang::VarDecl* var) const;
  clang::Stmt* getSingleDef(clang::VarDecl* var) const;
  clang::Expr* getSingleDefRHS(clang::VarDecl* var) const;

  Set<clang::Stmt*> getTopLevelUses(clang::VarDecl* var) const;
  const Set<clang::Stmt*>& getUses(clang::VarDecl* var) const;
  unsigned getNumUses(clang::VarDecl* var) const;
  bool isUsed(clang::VarDecl* var) const;
  bool hasZeroUses(clang::VarDecl* var) const;
  bool hasSingleUse(clang::VarDecl* var) const;
  clang::Stmt* getSingleUse(clang::VarDecl* var) const;

  const Set<clang::VarDecl*>& getVars() const;

  bool isContainedIn(clang::Stmt* needle, clang::Stmt* haystack) const;
  bool isDirectlyContainedIn(clang::Stmt* needle, clang::Stmt* haystack) const;
  bool isTopLevel(clang::Stmt* stmt) const;
  ExprNum getExprNum(clang::Expr* expr) const;
  const Set<clang::Expr*>& getEqvExprs(clang::Expr* expr) const;

  // AST construction functions
  clang::Stmt* clone(clang::Stmt* stmt);
  clang::Expr* cloneExpr(clang::Expr* expr);

  void setFunctionBody(clang::Stmt* body);

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

  // FIXME: This should probably go away. Right now it is used for "temporary"
  // variables which themselves should probably go away
  clang::DeclRefExpr* createVariable(const std::string& name,
                                     clang::QualType type,
                                     clang::DeclContext* parent);

  void recalculate();
};

} // namespace cish

#endif // CISH_AST_H
