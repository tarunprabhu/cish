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

#ifndef CISH_CLANG_UTILS_H
#define CISH_CLANG_UTILS_H

#include "Set.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

namespace Clang {

/// @param aty The clang::ArrayType
/// @returns The innermost non-array type within @aty
const clang::Type* getBaseType(const clang::ArrayType* aty);

clang::Expr* stripCasts(clang::Expr* expr);

/// @returns true if both lhs and rhs are constants of equal value
bool isEqual(clang::Expr* lhs, clang::Expr* rhs);
bool isConstant(clang::Expr*, uint64_t val);
bool isZero(clang::Expr*);
bool isOne(clang::Expr*);
bool isLiteral(const clang::Expr*);

clang::VarDecl* getVar(clang::Expr* expr);
Vector<clang::VarDecl*> getVarsInStmtAsVector(clang::Stmt* stmt);
Set<clang::VarDecl*> getVarsInStmt(clang::Stmt* stmt);

Vector<clang::BinaryOperator*> getCommaExprs(clang::BinaryOperator* binOp);
Vector<clang::BinaryOperator*> getForInits(clang::ForStmt* forStmt);
Vector<clang::BinaryOperator*> getForIncs(clang::ForStmt* forStmt);

Vector<clang::VarDecl*> getLocalVars(clang::FunctionDecl* f);

std::string toString(clang::QualType type, clang::ASTContext& astContext);
std::string toString(const clang::Stmt* stmt, clang::ASTContext& astContext);
std::string toString(const clang::FunctionDecl* f,
                     clang::ASTContext& astContext);
std::string toString(const clang::VarDecl* var, clang::ASTContext& astContext);
std::string toString(const clang::RecordDecl* record,
                     clang::ASTContext& astContext);

} // namespace Clang

} // namespace cish

#endif // CISH_CLANG_UTILS_H
