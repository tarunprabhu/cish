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

#ifndef CISH_BACKEND_BASE_H
#define CISH_BACKEND_BASE_H

#include "Map.h"
#include "Stack.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class ASTBuilder;
class CishContext;
class NameGenerator;

class BackendBase {
protected:
  CishContext& cishContext;
  NameGenerator& names;
  clang::ASTContext& astContext;
  ASTBuilder& builder;

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

  clang::Expr* replace(clang::Expr* src, clang::Expr* old, clang::Expr* repl);

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
