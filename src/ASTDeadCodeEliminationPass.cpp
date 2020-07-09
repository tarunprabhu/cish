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
#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "Diagnostics.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTDCEPass : public ASTFunctionPass<ASTDCEPass> {
public:
  bool process(FunctionDecl* f, Stmt*) {
    bool changed = false;

    for(VarDecl* var : Clang::getLocalVars(f))
      if(not um.isUsed(var))
        for(Stmt* def : um.getDefs(var).clone())
          if(pm.isTopLevel(def))
            changed |= ast->erase(def, pm.getParent(def));

    for(VarDecl* var : Clang::getLocalVars(f)) {
      if(um.hasZeroDefs(var) and um.hasZeroUses(var)) {
        f->removeDecl(var);
        changed |= true;
      }
    }

    return changed;
  }

public:
  ASTDCEPass(CishContext& context) : ASTFunctionPass(context, RequireUses) {
    ;
  }

  ASTDCEPass(const ASTDCEPass&) = delete;
  ASTDCEPass(ASTDCEPass&&) = delete;
  virtual ~ASTDCEPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-dce";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Dead Code Elimination Pass";
  }
};

} // namespace cish

cish::ASTPass* createASTDeadCodeEliminationPass(cish::CishContext& context) {
  return new cish::ASTDCEPass(context);
}
