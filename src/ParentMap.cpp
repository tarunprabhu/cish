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

#include "ParentMap.h"
#include "CishContext.h"
#include "ClangUtils.h"
#include "Diagnostics.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

ParentMap::ParentMap(CishContext& cishContext) : cishContext(cishContext) {
  ;
}

void ParentMap::add(Stmt* stmt, Stmt* parent) {
  if(parents.contains(stmt))
    fatal(error() << "Statement already in map with parent: "
                  << stmt->getStmtClassName());
  if(not stmt)
    fatal(error() << "Cannot add nullptr in parent or child");

  addOrReplace(stmt, parent);
}

void ParentMap::addOrReplace(Stmt* stmt, Stmt* parent) {
  parents[stmt] = parent;
}

void ParentMap::replace(Stmt* stmt, Stmt* parent) {
  if(not parents.contains(stmt))
    fatal(error() << "Cannot replace unknown parent of statement");
  if(not stmt)
    fatal(error() << "Cannot replace nullptr in parent or child");

  addOrReplace(stmt, parent);
}

void ParentMap::remove(Stmt* stmt) {
  parents.erase(stmt);
}

void ParentMap::clear() {
  parents.clear();
}

bool ParentMap::hasParent(Stmt* stmt) const {
  return parents.contains(stmt);
}

Stmt* ParentMap::getParent(Stmt* stmt) const {
  return parents.at(stmt);
}

unsigned ParentMap::getDepth(Stmt* stmt) const {
  if(not hasParent(stmt))
    return 0;
  return 1 + getDepth(getParent(stmt));
}

bool ParentMap::isContainedInImpl(Stmt* needle, Stmt* haystack) const {
  Stmt* parent = getParent(needle);
  if(parent == haystack)
    return true;
  else if(isContainedInImpl(needle, parent))
    return true;
  return false;
}

bool ParentMap::isContainedIn(Stmt* needle, Stmt* haystack) const {
  // If the needle depth of the needle is less than that of the haystack then
  // it occurs in a scope higher than the stack
  if(getDepth(needle) <= getDepth(haystack))
    return false;
  return isContainedInImpl(needle, haystack);
}

bool ParentMap::isDirectlyContainedIn(Stmt* needle, Stmt* haystack) const {
  if(getDepth(needle) <= getDepth(haystack))
    return false;

  Stmt* parent = getParent(needle);
  if(parent == haystack) {
    return true;
  } else if(auto* ifStmt = dyn_cast<IfStmt>(haystack)) {
    if(parent == ifStmt->getThen())
      return true;
    else if(parent == ifStmt->getElse())
      return true;
    else if(parent == ifStmt->getCond())
      return true;
  } else if(auto* doStmt = dyn_cast<DoStmt>(haystack)) {
    if(parent == doStmt->getCond())
      return true;
    else if(parent == doStmt->getBody())
      return true;
  } else if(auto* forStmt = dyn_cast<ForStmt>(haystack)) {
    if(parent == forStmt->getBody())
      return true;
    else if(parent == forStmt->getCond())
      return true;
    else if(parent == forStmt->getInit())
      return true;
    else if(parent == forStmt->getInc())
      return true;
    for(BinaryOperator* binOp : Clang::getForInits(forStmt))
      if(parent == binOp)
        return true;
    for(BinaryOperator* binOp : Clang::getForIncs(forStmt))
      if(parent == binOp)
        return true;
  } else if(auto* whileStmt = dyn_cast<WhileStmt>(haystack)) {
    if(parent == whileStmt->getBody())
      return true;
    else if(parent == whileStmt->getCond())
      return true;
  }
  return false;
}

bool ParentMap::isTopLevel(Stmt* stmt) const {
  if(hasParent(stmt) and isa<CompoundStmt>(getParent(stmt)))
    return true;
  return false;
}

Stmt* ParentMap::getTopLevelAncestor(Stmt* stmt) const {
  if(not hasParent(stmt))
    return nullptr;
  else if(isa<CompoundStmt>(stmt))
    fatal(error() << "Unexpected compound statement when looking for ancestor");
  else if(isTopLevel(stmt))
    fatal(error() << "Statement is already top level: "
                  << Clang::toString(stmt, cishContext.getASTContext()));

  Stmt* parent = getParent(stmt);
  if(isTopLevel(parent))
    return parent;
  return getTopLevelAncestor(parent);
}

void ParentMap::dump(llvm::raw_ostream& os) const {
  os << "pm:\n";
  for(auto& i : parents) {
    Stmt* stmt = i.first;
    Stmt* parent = i.second;
    os << stmt << ": " << Clang::toString(stmt, cishContext.getASTContext())
       << " => " << parent << "\n";
  }
}

} // namespace cish
