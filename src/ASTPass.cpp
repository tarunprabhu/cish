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

#include "ASTPass.h"
#include "CishContext.h"

namespace cish {

ASTPass::ASTPass(CishContext& cishContext, unsigned flags)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      flags(flags), cm(cishContext), em(cishContext), pm(cishContext),
      um(cishContext, pm), ast(cishContext, pm) {
  ;
}

llvm::StringRef ASTPass::getPassLongName() const {
  return "Cish AST Pass (Unnamed)";
}

bool ASTPass::hasFlag(unsigned flag) const {
  return (flags & flag) == flag;
}

bool ASTPass::hasPreOrder() const {
  return hasFlag(PreOrder);
}

bool ASTPass::hasRequireExprNums() const {
  return hasFlag(RequireExprNums);
}

bool ASTPass::hasRequireUses() const {
  return hasFlag(RequireUses);
}

bool ASTPass::hasRequireCFG() const {
  return hasFlag(RequireCFG);
}

bool ASTPass::hasOnePass() const {
  return hasFlag(OnePass);
}

const clang::CFG* ASTPass::getCFG() const {
  return cm.getCFG();
}

const clang::DominatorTree* ASTPass::getDominatorTree() const {
  return cm.getDominatorTree();
}

const ExprNumberMap& ASTPass::getExprNumberMap() const {
  return em;
}

const ParentMap& ASTPass::getParentMap() const {
  return pm;
}

const UsesMap& ASTPass::getUsesMap() const {
  return um;
}

} // namespace cish
