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

#ifndef CISH_EXPR_NUMBER_MAP_H
#define CISH_EXPR_NUMBER_MAP_H

#include "Map.h"
#include "Set.h"

#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

namespace cish {

class CishContext;

using ExprNum = int64_t;

// Clang does not unique anything because that is impossible to do in order
// to correctly associate souce information. But that makes the AST
// transformations more difficult. It is certainly not safe to unique anything
// because it breaks many thigns in Clang, especially the CFG construction
// that is needed to do the propagation passes correctly. This is an
// expression numbering map that maintains sets of equivalent expressions

class ExprNumberMap {
protected:
  CishContext& cishContext;
  ExprNum nextExprNum;

  Map<bool, ExprNum> blits;
  Map<unsigned, ExprNum> clits;
  Map<int16_t, ExprNum> ilits16;
  Map<int32_t, ExprNum> ilits32;
  Map<int64_t, ExprNum> ilits64;
  Map<uint16_t, ExprNum> ulits16;
  Map<uint32_t, ExprNum> ulits32;
  Map<uint64_t, ExprNum> ulits64;
  Map<float, ExprNum> flits;
  Map<double, ExprNum> dlits;
  Map<long double, ExprNum> glits;
  Map<std::string, ExprNum> slits;
  Map<clang::UnaryOperator::Opcode, Map<ExprNum, ExprNum>> unOps;
  Map<clang::BinaryOperator::Opcode, Map<std::pair<ExprNum, ExprNum>, ExprNum>>
      binOps;
  Map<Vector<ExprNum>, ExprNum> condOps;
  Map<std::pair<ExprNum, ExprNum>, ExprNum> arrExprs;
  Map<Vector<ExprNum>, ExprNum> callExprs;
  Map<ExprNum, ExprNum> castExprs;
  Map<std::pair<ExprNum, clang::ValueDecl*>, ExprNum> memberExprs;
  Map<clang::ValueDecl*, ExprNum> declRefs;

  Map<clang::VarDecl*, ExprNum> varNums;
  Map<clang::Expr*, ExprNum> exprNums;
  Map<ExprNum, Set<clang::Expr*>> eqvExprs;

protected:
  ExprNum add(clang::Expr* expr, ExprNum exprNum);
  ExprNum add(clang::VarDecl* var);

  ExprNum getNewExprNum();

  template <typename IntKind>
  ExprNum add(clang::IntegerLiteral* ilit, Map<IntKind, ExprNum>& m) {
    IntKind i = (IntKind)ilit->getValue().getLimitedValue();
    if(not m.contains(i))
      return m[i] = add(ilit, getNewExprNum());
    return add(ilit, m[i]);
  }

  template <typename FloatKind>
  ExprNum
  add(clang::FloatingLiteral* flit, FloatKind val, Map<FloatKind, ExprNum>& m) {
    if(not m.contains(val))
      return m[val] = add(flit, getNewExprNum());
    return add(flit, m[val]);
  }

public:
  ExprNumberMap(CishContext& cishContext);
  ExprNumberMap(const ExprNumberMap&) = delete;
  ExprNumberMap(ExprNumberMap&&) = delete;

  ExprNumberMap& reset(clang::FunctionDecl* f = nullptr);
  ExprNum add(clang::CXXBoolLiteralExpr* blit);
  ExprNum add(clang::CharacterLiteral* clit);
  ExprNum add(clang::IntegerLiteral* ilit);
  ExprNum add(clang::FloatingLiteral* flit);
  ExprNum add(clang::StringLiteral* slit);
  ExprNum add(clang::CXXNullPtrLiteralExpr* nlit);
  ExprNum add(clang::UnaryOperator* unOp);
  ExprNum add(clang::BinaryOperator* binOp);
  ExprNum add(clang::ConditionalOperator* condOp);
  ExprNum add(clang::ArraySubscriptExpr* arrExpr);
  ExprNum add(clang::CallExpr* callExpr);
  ExprNum add(clang::CStyleCastExpr* castExpr);
  ExprNum add(clang::MemberExpr* memberExpr);
  ExprNum add(clang::InitListExpr* initList);
  ExprNum add(clang::DeclRefExpr* declRefExpr);
  ExprNum add(clang::Expr* expr);
  void remove(clang::Expr* expr);
  void refresh(clang::Expr* expr);

  ExprNum get(clang::VarDecl* var) const;
  ExprNum get(clang::Expr* expr) const;
  bool has(clang::VarDecl* var) const;
  bool has(clang::Expr* expr) const;

  const Set<clang::Expr*>& getEqv(clang::Expr* expr) const;
  const Set<clang::Expr*>& getEqv(ExprNum exprNum) const;

  void dump(llvm::raw_ostream& os = llvm::errs()) const;
};

} // namespace cish

#endif // CISH_EXPR_NUMBER_MAP_H
