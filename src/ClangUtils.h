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

/// @param aty The clang::ArrayType
/// @returns The innermost non-array type within @aty
const clang::Type* getBaseType(const clang::ArrayType* aty);

clang::Expr* stripCasts(clang::Expr* expr);

Vector<clang::VarDecl*> getVarsInStmtAsVector(clang::Stmt* stmt);
Set<clang::VarDecl*> getVarsInStmt(clang::Stmt* stmt);

std::string toString(clang::Stmt* stmt, clang::ASTContext& astContext);
std::string toString(clang::FunctionDecl* f, clang::ASTContext& astContext);

} // namespace cish

#endif // CISH_CLANG_UTILS_H
