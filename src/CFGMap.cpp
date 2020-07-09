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

#include "CishContext.h"
#include "CFGMap.h"

using namespace clang;

namespace cish {

CFGMap::CFGMap(CishContext& cishContext)
    : cishContext(cishContext) {
  ;
}

CFGMap& CFGMap::reset(FunctionDecl* f) {
  stmtParents.reset(nullptr);
  cfg.reset(nullptr);
  cfgStmtMap.reset(nullptr);
  dt.reset(nullptr);

  if(f) {
    Stmt* body = f->getBody();
    stmtParents.reset(new clang::ParentMap(body));
    cfg = CFG::buildCFG(f, body, &cishContext.getASTContext(), cfgBuildOpts);
    cfgStmtMap.reset(CFGStmtMap::Build(cfg.get(), stmtParents.get()));
    dt.reset(new DominatorTree);

    // FIXME: The dominator tree calculation fails because there is some
    // inconsistency when constructing the CFG. This is possible because the AST
    // may not be in the form that it expects because I am screwing up somewhere
    // along the way. At some point this should be fixed
    //
    // dt->getBase().recalculate(*cfg);
  }

  return *this;
}

CFG* CFGMap::getCFG() const {
  return cfg.get();
}

CFGBlock* CFGMap::getCFGBlock(Stmt* stmt) const {
  return cfgStmtMap->getBlock(stmt);
}

const DominatorTree* CFGMap::getDominatorTree() const {
  return dt.get();
}

} // namespace cish
