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

#ifndef CISH_AST_PASS_MANAGER_H
#define CISH_AST_PASS_MANAGER_H

#include "Map.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class AST;
class ASTFunctionPassBase;
class CishContext;

class ASTPassManager {
private:
  Map<clang::FunctionDecl*, std::unique_ptr<AST>> asts;
  std::unique_ptr<ASTFunctionPassBase> defUsePass;
  Vector<ASTFunctionPassBase*> passes;

public:
  ASTPassManager(CishContext& astContext);
  ASTPassManager(const ASTPassManager&) = delete;
  ASTPassManager(ASTPassManager&&) = delete;

  void addPass(ASTFunctionPassBase* pass);
  bool runOnFunction(clang::FunctionDecl* f);
};

} // namespace cish

#endif // CISH_AST_PASS_MANAGER_H
