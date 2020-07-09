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

#ifndef CISH_CFG_MAP_H
#define CISH_CFG_MAP_H

#include <clang/AST/ParentMap.h>
#include <clang/Analysis/Analyses/Dominators.h>
#include <clang/Analysis/CFG.h>
#include <clang/Analysis/CFGStmtMap.h>

namespace cish {

class CishContext;

// Wrapper around Clang's CFG - mostly to encapsulate any construction
// options and other objects that are needed to create the CFG and DominatorTree
// but not needed to actually use them
//
class CFGMap {
protected:
  CishContext& cishContext;
  clang::CFG::BuildOptions cfgBuildOpts;
  std::unique_ptr<clang::ParentMap> stmtParents;
  std::unique_ptr<clang::CFG> cfg;
  std::unique_ptr<clang::CFGStmtMap> cfgStmtMap;
  std::unique_ptr<clang::DominatorTree> dt;

public:
  CFGMap(CishContext& cishContext);
  CFGMap(const CFGMap&) = delete;
  CFGMap(CFGMap&&) = delete;

  CFGMap& reset(clang::FunctionDecl* f);
  clang::CFG* getCFG() const;
  clang::CFGBlock* getCFGBlock(clang::Stmt* stmt) const;
  const clang::DominatorTree* getDominatorTree() const;
};

} // namespace cish

#endif // CISH_CFG_MAP_H
