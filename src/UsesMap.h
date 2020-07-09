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

#ifndef CISH_USES_MAP_H
#define CISH_USES_MAP_H

#include "Map.h"
#include "Set.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class CishContext;
class ParentMap;

class UsesMap {
protected:
  clang::ASTContext& astContext;
  ParentMap& pm;

  // For now, every def of a variable is an assignment statement.
  // In clang-speak, this would be a BinaryOperator where the operand is
  // BO_Assign. When returning the def, it will return the entire statement
  Map<clang::VarDecl*, Set<clang::Stmt*>> defMap;

  // The uses are the nearest Expr containing the variable directly.
  // The Expr in the use will be a DeclRefExpr but since those are not uniqued,
  // they can't be used to keep the map
  Map<clang::VarDecl*, Set<clang::Stmt*>> useMap;

public:
  UsesMap(CishContext& cishContext, ParentMap& pm);
  UsesMap(const UsesMap&) = delete;
  UsesMap(UsesMap&&) = delete;

  UsesMap& reset(clang::FunctionDecl* f = nullptr);
  void addDecl(clang::VarDecl* var);
  void addDef(clang::VarDecl* var, clang::Stmt* user);
  void addUse(clang::VarDecl* var, clang::Stmt* user);

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
};

} // namespace cish

#endif // CISH_USES_MAP_H
