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
      flags(flags), ast(nullptr) {
  ;
}

llvm::StringRef ASTPass::getPassLongName() const {
  return "Cish AST Pass (Unnamed)";
}

bool ASTPass::hasFlag(unsigned flag) const {
  return (flags & flag) == flag;
}

bool ASTPass::hasModifiesAST() const {
  return hasFlag(ModifiesAST);
}

bool ASTPass::hasPostorder() const {
  return hasFlag(PostOrder);
}

bool ASTPass::hasIterateUntilConvergence() const {
  return hasFlag(IterateUntilConvergence);
}

} // namespace cish
