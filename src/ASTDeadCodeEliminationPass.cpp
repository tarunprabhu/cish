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

#include "AST.h"
#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "Diagnostics.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTDCEPass : public ASTFunctionPass<ASTDCEPass> {
protected:
  ASTBuilder builder;

public:
  bool process(FunctionDecl* f) {
    Set<Stmt*> removeStmts;
    Set<VarDecl*> removeVars;
    bool changed = false;
    bool iterChanged = false;
    do {
      iterChanged = false;

      for(VarDecl* var : ast->vars())
        if(not ast->isUsed(var))
          for(Stmt* def : ast->getTopLevelDefs(var))
            iterChanged |= ast->erase(def);

      for(VarDecl* var : ast->getVars())
        if(ast->hasZeroTopLevelDefs(var) and ast->hasZeroTopLevelUses(var))
          iterChanged |= ast->erase(var);

      changed |= iterChanged;
    } while(iterChanged);

    return changed;
  }

public:
  ASTDCEPass(CishContext& context)
      : ASTFunctionPass(context, true), builder(astContext) {
    ;
  }

  ASTDCEPass(const ASTDCEPass&) = delete;
  ASTDCEPass(ASTDCEPass&&) = delete;
  virtual ~ASTDCEPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "Cish AST Dead Code Elimination Pass";
  }
};

} // namespace cish

cish::ASTPass* createASTDeadCodeEliminationPass(cish::CishContext& context) {
  return new cish::ASTDCEPass(context);
}
