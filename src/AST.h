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

#include "ASTBuilder.h"
#include "List.h"
#include "Map.h"
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
class ASTDefUseCalculatorPass;
class ASTExprNumberingPass;
class CishContext;

// Single class that keeps track of the AST. Any modifications to the AST
// must be done through this graph to keep the def-use information, flow
// graph and parent-child relationships between statements consistent
class AST {
protected:
  struct StmtInfo {
    clang::CompoundStmt* body;
    clang::Stmt* ctrl;
    unsigned depth;

    StmtInfo(clang::CompoundStmt* body, clang::Stmt* ctrl, unsigned depth)
        : body(body), ctrl(ctrl), depth(depth) {
      ;
    }
  };

  using ExprNum = int64_t;

protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;
  ASTBuilder& builder;
  clang::FunctionDecl* decl;

private:
  ASTPass* defUseCalculator;
  ASTPass* exprNumberingCalculator;

protected:
  clang::CFG::BuildOptions cfgBuildOpts;
  std::unique_ptr<clang::ParentMap> stmtParents;
  std::unique_ptr<clang::CFG> cfg;
  std::unique_ptr<clang::CFGStmtMap> cfgStmtMap;
  std::unique_ptr<clang::DominatorTree> dt;

  // Clang does not unique anything because that is impossible to do in order
  // to correctly associate souce information. But that makes a lot of the
  // AST transformations a little more difficult. But it may not be safe to do
  // uniquing ourselves because it may break something in Clang. Instead, just
  // maintain a map of equivalent expressions instead
  Map<ExprNum, Set<clang::Expr*>> eqvExprs;
  Map<clang::Expr*, ExprNum> exprNums;

  // The loops in the function because those are usually "interesting" and
  // therefore worthy of keeping separately
  Vector<clang::Stmt*> loopStmts;

  // Each of the statements here are only the "top-level" statements
  // These are either control structures (if, while, for, switch) or
  // statements terminated by a semicolon. It is easy to think of these as
  // occupying a "line of code"
  Map<clang::Stmt*, StmtInfo> stmtInfo;

  // Map from a compound statement to its corresponding control structure
  // If statements may have up to two
  Map<clang::CompoundStmt*, clang::Stmt*> ctrls;

  // These are the statements contained within each control structure or
  // function body. The function body is a special "statement" and the key
  // is nullptr in this map
  Map<clang::Stmt*, Set<clang::Stmt*>> subStmts;

  // This map is the closure of the subStmts map
  Map<clang::Stmt*, Set<clang::Stmt*>> descendants;

  // For now, every def of a variable is an assignment statement.
  // In clang-speak, this would be a BinaryOperator where the operand is
  // BO_*Assign. When returning the def, it will return the entire statement
  Map<clang::VarDecl*, Set<clang::Stmt*>> defMap;

  // The uses are the nearest Expr containing the variable directly.
  Map<clang::VarDecl*, Set<clang::Stmt*>> useMap;

  // These are uses at the expression level. Comes in handy when replacing
  Map<clang::Expr*, Set<clang::Stmt*>> exprUseMap;

  // These are the top-level uses. The top-level statements are those that
  // are either a control structure or a statement terminated by a semicolon
  // The statements can be though of as those that would normally be considered
  // a "line of code" (so the semi-colon terminated statements in a for loop
  // declaration don't count). If the statement is a control structure, the
  // variable is used in the following ways:
  //
  //   IfStmt, WhileStmt, DoStmt, SwitchStmt: In the condition expression
  //   ForStmt: In either the initialzation statement, the condition expression
  //            or the increment expression
  //
  Map<clang::VarDecl*, Set<clang::Stmt*>> tlUseMap;
  Map<clang::VarDecl*, Set<clang::Stmt*>> tlDefMap;

  // Variables that have their address taken
  Set<clang::VarDecl*> addrTaken;

  // Local variables in the function
  // Keep this separate because function parameters are also VarDecl's, so
  // it's a bit messy keeping track of everything
  List<clang::VarDecl*> locals;

public:
  using loop_iterator = decltype(loopStmts)::const_iterator;
  using loop_range = llvm::iterator_range<loop_iterator>;
  using child_iterator = decltype(subStmts)::mapped_type::const_iterator;
  using child_range = llvm::iterator_range<child_iterator>;
  using tl_iterator = decltype(stmtInfo)::const_key_iterator;
  using tl_range = llvm::iterator_range<tl_iterator>;
  using var_iterator = decltype(locals)::const_iterator;
  using var_range = llvm::iterator_range<var_iterator>;
  using def_iterator = decltype(defMap)::mapped_type::const_iterator;
  using def_range = llvm::iterator_range<def_iterator>;
  using use_iterator = decltype(useMap)::mapped_type::const_iterator;
  using use_range = llvm::iterator_range<use_iterator>;
  using tluse_iterator = decltype(tlUseMap)::mapped_type::const_iterator;
  using tluse_range = llvm::iterator_range<tluse_iterator>;
  using tldef_iterator = decltype(tlDefMap)::mapped_type::const_iterator;
  using tldef_range = llvm::iterator_range<tldef_iterator>;

protected:
  ExprNum addExpr(clang::Expr* expr);
  ExprNum getExprNum(clang::Expr* expr) const;
  bool hasExprNum(clang::Expr* expr) const;

  void addChild(clang::Stmt* stmt,
                clang::CompoundStmt* body,
                clang::Stmt* construct,
                unsigned depth);

  void associateStmts(clang::CompoundStmt* body,
                      clang::Stmt* construct,
                      unsigned stmtDepth);

  void addDef(clang::VarDecl* var, clang::Stmt* user);
  void addUse(clang::VarDecl* var, clang::Stmt* user);
  void addExprUse(clang::Expr* expr, clang::Stmt* user);
  void addTopLevelDef(clang::VarDecl* var, clang::Stmt* user);
  void addTopLevelUse(clang::Stmt* stmt, clang::Stmt* user);

  Set<clang::Stmt*> getUses(clang::VarDecl* var);
  Set<clang::Stmt*> getDefs(clang::VarDecl* var);

  bool replace(clang::UnaryOperator* unOp, clang::Expr* repl);
  bool replace(clang::BinaryOperator* binOp, clang::Expr* repl, bool lhs);
  bool
  replace(clang::ArraySubscriptExpr* arrExpr, clang::Expr* repl, bool base);
  bool replace(clang::CallExpr* callExpr, clang::Expr* repl, long arg);
  bool replace(clang::CStyleCastExpr* castExpr, clang::Expr* repl);
  bool replace(clang::MemberExpr* memberExpr, clang::Expr* repl);
  bool replace(clang::ReturnStmt* retStmt, clang::Expr* repl);
  bool replace(clang::IfStmt* ifStmt, clang::Expr* repl);
  bool replace(clang::SwitchStmt* switchStmt, clang::Expr* repl);
  bool replace(clang::DoStmt* doStmt, clang::Expr* repl);
  bool replace(clang::ForStmt* forStmt, clang::Expr* repl, bool cond);
  bool replace(clang::WhileStmt* whileStmt, clang::Expr* repl);
  bool replace(clang::Stmt* parent, clang::Expr* expr, clang::Expr* repl);

  void recalculateCFG();
  void recalculateStructure();
  void recalculateDefUse();

public:
  AST(CishContext& cishContext, clang::FunctionDecl* decl);
  AST(const AST&) = delete;
  AST(AST&&) = delete;
  ~AST();

  clang::FunctionDecl* getFunction() const;

  void removeUse(clang::VarDecl* var, clang::Stmt* stmt);
  void removeDef(clang::VarDecl* var, clang::Stmt* stmt);
  bool replaceAllUsesWith(clang::VarDecl* var, clang::Expr* expr);
  bool replaceEqvUsesWith(clang::Expr* expr, clang::Expr* repl);
  bool replaceExprWith(clang::Expr* expr, clang::Expr* repl);
  bool replaceStmtWith(clang::Stmt* old, clang::Stmt* repl);
  bool erase(clang::Stmt* stmt);
  bool erase(clang::VarDecl* var);

  const clang::DominatorTree& getDominatorTree() const;
  clang::Stmt* getParent(clang::Stmt*) const;
  clang::CFG* getCFG() const;
  clang::CFGBlock* getCFGBlock(clang::Stmt* stmt) const;
  const clang::CFG::BuildOptions& getCFGBuildOpts() const;

  bool hasAddressTaken(clang::VarDecl* var) const;

  const Set<clang::Expr*>& getEqvExprs(clang::Expr* expr) const;

  unsigned getNumUses(clang::VarDecl* var) const;
  unsigned getNumDefs(clang::VarDecl* var) const;
  Vector<clang::Stmt*> getTopLevelUses(clang::VarDecl* var) const;
  Set<clang::Stmt*> getTopLevelUsesSet(clang::VarDecl* var) const;
  Vector<clang::Stmt*> getTopLevelDefs(clang::VarDecl* var) const;
  Set<clang::Stmt*> getTopLevelDefsSet(clang::VarDecl* var) const;
  unsigned getNumTopLevelDefs(clang::VarDecl* var) const;
  unsigned getNumTopLevelUses(clang::VarDecl* var) const;
  bool isDefined(clang::VarDecl* var) const;
  bool isUsed(clang::VarDecl* var) const;
  bool hasSingleDef(clang::VarDecl* var) const;
  bool hasSingleUse(clang::VarDecl* var) const;
  bool hasZeroDefs(clang::VarDecl* var) const;
  bool hasZeroUses(clang::VarDecl* var) const;
  bool hasSingleTopLevelDef(clang::VarDecl* var) const;
  bool hasSingleTopLevelUse(clang::VarDecl* var) const;
  bool hasZeroTopLevelDefs(clang::VarDecl* var) const;
  bool hasZeroTopLevelUses(clang::VarDecl* var) const;
  clang::Stmt* getSingleDef(clang::VarDecl* var) const;
  clang::Expr* getSingleDefRHS(clang::VarDecl* var) const;
  clang::Stmt* getSingleUse(clang::VarDecl* var) const;

  bool isStructureFor(const clang::FunctionDecl* f) const;
  bool isContainedIn(clang::Stmt* needle,
                     clang::Stmt* haystack = nullptr) const;
  bool isDirectlyContainedIn(clang::Stmt* needle,
                             clang::Stmt* haystack = nullptr) const;
  bool isTopLevel(clang::Stmt* stmt) const;
  unsigned getDepth(clang::Stmt* stmt) const;
  clang::CompoundStmt* getBodyFor(clang::Stmt* stmt) const;
  clang::Stmt* getConstructFor(clang::Stmt* stmt) const;

  Vector<clang::Stmt*> getLoops() const;
  Vector<clang::VarDecl*> getVars() const;

  loop_range loops() const;
  tl_range tlstmts() const;
  var_range vars() const;
  tluse_range tluses(clang::VarDecl* var) const;
  tldef_range tldefs(clang::VarDecl* var) const;
  child_range children(clang::Stmt* stmt) const;

  void recalculate(bool defUseOnly);

public:
  friend class ASTDefUseCalculatorPass;
  friend class ASTExprNumberingPass;
};

} // namespace cish

#endif // CISH_AST_H