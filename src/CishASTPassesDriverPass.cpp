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

#include <llvm/Pass.h>

#include "ASTFunctionPass.h"
#include "ASTPasses.h"
#include "CishContext.h"
#include "ClangUtils.h"

using namespace llvm;

// This is not really a pass
class CishASTPassesDriverPass : public ModulePass {
public:
  static char ID;

public:
  CishASTPassesDriverPass() : ModulePass(ID) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish AST Simplify Pass";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.addRequired<CishContextWrapperPass>();
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module& m) override {
    cish::CishContext& cishContext
        = getAnalysis<CishContextWrapperPass>().getCishContext();

    cish::Vector<cish::ASTPass*> passes = {
        createASTStripCastsPass(cishContext),
        createASTSimplifyOperatorsPass(cishContext),
        createASTPropagateExprsPass(cishContext),
        createASTSimplifyLoopsPass(cishContext),
        createASTConstantFoldingPass(cishContext),
        createASTPropagateExprsPass(cishContext),
        createASTDeadCodeEliminationPass(cishContext),
        createASTRenameVarsPass(cishContext),
    };

    for(clang::FunctionDecl* f : cishContext.funcs()) {
      // llvm::outs() << cish::toString(f, cishContext.getASTContext()) << "\n";
      for(cish::ASTPass* pass : passes) {
        pass->runOnFunction(f);
        // llvm::outs() << cish::toString(f, cishContext.getASTContext()) <<
        // "\n"; llvm::outs().flush();
      }
    }

    return false;
  }
};

char CishASTPassesDriverPass::ID = 0;

static RegisterPass<CishASTPassesDriverPass>
    X("cish-ast-simplify",
      "Simplify the Cish AST to make it more readable",
      true,
      true);

Pass* createCishASTPassesDriverPass() {
  return new CishASTPassesDriverPass();
}
