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

#ifndef CISH_PARENT_MAP_H
#define CISH_PARENT_MAP_H

#include "Map.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class CishContext;

// Clang has a parent map too, but this one has error checking to ensure
// that clang::Stmt*'s never accidentally get more than one parent and also
// allows the map to be cleared
class ParentMap {
private:
  CishContext& cishContext;
  clang::FunctionDecl* decl;
  Map<clang::Stmt*, clang::Stmt*> parents;

protected:
  unsigned getDepth(clang::Stmt* stmt) const;
  bool isContainedInImpl(clang::Stmt* needle, clang::Stmt* haystack) const;

public:
  ParentMap(CishContext& cishContext, clang::FunctionDecl* decl);
  ParentMap(const ParentMap&) = delete;
  ParentMap(ParentMap&&) = delete;

  void clear();
  void add(clang::Stmt* stmt, clang::Stmt* parent);
  void addOrReplace(clang::Stmt* stmt, clang::Stmt* parent);
  void replace(clang::Stmt* stmt, clang::Stmt* parent);
  void remove(clang::Stmt* stmt);

  bool isOrphan(clang::Stmt* stmt) const;
  bool isTopLevel(clang::Stmt* stmt) const;
  bool isDirectlyContainedIn(clang::Stmt* needle, clang::Stmt* haystack) const;
  bool isContainedIn(clang::Stmt* needle, clang::Stmt* haystack) const;

  bool hasParent(clang::Stmt* stmt) const;
  clang::Stmt* getParent(clang::Stmt* stmt) const;
  clang::Stmt* getTopLevelAncestor(clang::Stmt* stmt) const;
};

} // namespace cish

#endif // CISH_PARENT_MAP_H
