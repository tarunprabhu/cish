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

#ifndef CISH_AST_PASSES_H
#define CISH_AST_PASSES_H

namespace cish {

class ASTPass;
class CishContext;

} // namespace cish

cish::ASTPass* createASTDefUseCalculatorPass(cish::CishContext&);
cish::ASTPass* createASTStripCastsPass(cish::CishContext&);
cish::ASTPass* createASTSimplifyOperatorsPass(cish::CishContext&);
cish::ASTPass* createASTDeadCodeEliminationPass(cish::CishContext&);
cish::ASTPass* createASTPropagateExprsPass(cish::CishContext&);
cish::ASTPass* createASTRenameVarsPass(cish::CishContext&);
cish::ASTPass* createASTConvertLoopsPass(cish::CishContext&);
cish::ASTPass* createASTConstantFoldingPass(cish::CishContext&);
cish::ASTPass* createASTSubexprEliminationPass(cish::CishContext&);
cish::ASTPass* createASTExprNumberingPass(cish::CishContext&);
cish::ASTPass* createASTPrivatizeVarsPass(cish::CishContext&);

#endif // CISH_AST_PASSES_H
