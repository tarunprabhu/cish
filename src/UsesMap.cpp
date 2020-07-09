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

#include "UsesMap.h"
#include "CishContext.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "ParentMap.h"

using namespace clang;

namespace cish {

UsesMap::UsesMap(CishContext& cishContext, ParentMap& pm)
    : astContext(cishContext.getASTContext()), pm(pm) {
  ;
}

UsesMap& UsesMap::reset(FunctionDecl* f) {
  useMap.clear();
  defMap.clear();

  if(f) {
    // Globals
    for(Decl* decl : astContext.getTranslationUnitDecl()->decls())
      if(auto* g = dyn_cast<VarDecl>(decl))
        addDecl(g);

    // Parameters
    for(ParmVarDecl* param : f->parameters())
      addDecl(param);

    // Locals
    for(Decl* decl : f->decls())
      if(VarDecl* var = dyn_cast<VarDecl>(decl))
        addDecl(var);
  }

  return *this;
}

void UsesMap::addDecl(VarDecl* decl) {
  useMap[decl].clear();
  defMap[decl].clear();
}

void UsesMap::addDef(VarDecl* var, Stmt* user) {
  if(not var)
    fatal(error() << "Cannot add nullptr");
  defMap.at(var).insert(user);
}

void UsesMap::addUse(VarDecl* var, Stmt* user) {
  useMap.at(var).insert(user);
}

Set<Stmt*> UsesMap::getTopLevelDefs(VarDecl* var) const {
  Set<Stmt*> defs;
  for(Stmt* def : getDefs(var))
    if(pm.isTopLevel(def))
      defs.insert(def);
    else
      defs.insert(pm.getTopLevelAncestor(def));
  return defs;
}

const Set<Stmt*>& UsesMap::getDefs(VarDecl* var) const {
  return defMap.at(var);
}

unsigned UsesMap::getNumDefs(VarDecl* var) const {
  return getDefs(var).size();
}

bool UsesMap::isDefined(VarDecl* var) const {
  return getNumDefs(var);
}

bool UsesMap::hasZeroDefs(VarDecl* var) const {
  return getNumDefs(var) == 0;
}

bool UsesMap::hasSingleDef(VarDecl* var) const {
  return getNumDefs(var) == 1;
}

Stmt* UsesMap::getSingleDef(VarDecl* var) const {
  if(hasSingleDef(var))
    return *getDefs(var).begin();
  return nullptr;
}

Expr* UsesMap::getSingleDefRHS(VarDecl* var) const {
  if(hasSingleDef(var))
    return cast<BinaryOperator>(getSingleDef(var))->getRHS();
  return nullptr;
}

Set<Stmt*> UsesMap::getTopLevelUses(VarDecl* var) const {
  Set<Stmt*> uses;
  for(Stmt* use : getUses(var))
    if(pm.isTopLevel(use))
      uses.insert(use);
    else
      uses.insert(pm.getTopLevelAncestor(use));
  return uses;
}

const Set<Stmt*>& UsesMap::getUses(VarDecl* var) const {
  return useMap.at(var);
}

unsigned UsesMap::getNumUses(VarDecl* var) const {
  return useMap.at(var).size();
}

bool UsesMap::isUsed(VarDecl* var) const {
  return getNumUses(var);
}

bool UsesMap::hasZeroUses(VarDecl* var) const {
  return getNumUses(var) == 0;
}

bool UsesMap::hasSingleUse(VarDecl* var) const {
  return getNumUses(var) == 1;
}

Stmt* UsesMap::getSingleUse(VarDecl* var) const {
  if(hasSingleUse(var))
    return *useMap.at(var).begin();
  return nullptr;
}

} // namespace cish
