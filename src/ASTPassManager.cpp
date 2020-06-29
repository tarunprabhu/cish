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

#include "ASTPassManager.h"
#include "AST.h"
#include "ASTFunctionPass.h"
#include "ASTPasses.h"
#include "CishContext.h"
#include "Diagnostics.h"

#include <clang/AST/ASTContext.h>

using namespace clang;

namespace cish {

ASTPassManager::ASTPassManager(CishContext& cishContext) {
  ASTContext& astContext = cishContext.getASTContext();
  for(clang::Decl* decl : astContext.getTranslationUnitDecl()->decls())
    if(auto* f = dyn_cast<clang::FunctionDecl>(decl))
      if(f->hasBody())
        asts.emplace(f, new AST(f));

  defUsePass.reset(createASTDefUsePass(cishContext));
}

void ASTPassManager::addPass(ASTFunctionPassBase* pass) {
  passes.push_back(pass);
}

bool ASTPassManager::runOnFunction(FunctionDecl* f) {
  bool changed = false;

  AST* ast = asts.at(f).get();
  defUsePass->setAST(ast);
  for(ASTFunctionPassBase* pass : passes)
    pass->setAST(ast);

  defUsePass->runOnFunction(f);
  for(ASTFunctionPassBase* pass : passes) {
    message() << "Running " << pass->getPassName() << " on " << f->getName()
              << "\n";
    bool passChanged = pass->runOnFunction(f);
    if(passChanged) {
      message() << "Updating AST structure information\n";
      defUsePass->runOnFunction(f);
    }
    changed |= passChanged;
  }

  defUsePass->setAST(nullptr);
  for(ASTFunctionPassBase* pass : passes)
    pass->setAST(nullptr);

  return changed;
}

} // namespace cish
