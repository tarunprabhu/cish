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
#include "Logging.h"

using namespace llvm;

// This is not really a pass
class CishASTPassesDriverPass : public ModulePass {
public:
  static char ID;

private:
  cish::CishContext& cishContext;
  unsigned passIdx;

protected:
  void log(clang::FunctionDecl* f, cish::ASTPass* pass = nullptr) {
    if(cish::Logger log
       = cish::Logger::openFile(f->getName(), passIdx++, "cish")) {
      if(pass)
        log() << "// After " << pass->getPassName() << "\n\n";
      else
        log() << "// Initial AST\n\n";
      log() << cish::toString(f, f->getASTContext()) << "\n";
    }
  }

public:
  explicit CishASTPassesDriverPass(cish::CishContext& cishContext)
      : ModulePass(ID), cishContext(cishContext), passIdx(0) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish AST Simplify Pass";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module& m) override {
    cish::Vector<cish::ASTPass*> passes = {
        createASTStripCastsPass(cishContext),
        createASTSimplifyOperatorsPass(cishContext),
        createASTPropagateExprsPass(cishContext),
        createASTConvertLoopsPass(cishContext),
        createASTSimplifyOperatorsPass(cishContext),
        createASTConstantFoldingPass(cishContext),
        createASTSubexprEliminationPass(cishContext),
        createASTDeadCodeEliminationPass(cishContext),
        createASTRenameVarsPass(cishContext),
    };

    for(clang::FunctionDecl* f : cishContext.funcs()) {
      cish::message() << "Processing function " << f->getName() << "\n";
      passIdx = 0;
      log(f);
      for(cish::ASTPass* pass : passes) {
        pass->runOnFunction(f);
        log(f, pass);
      }
    }

    for(cish::ASTPass* pass : passes)
      delete pass;

    return false;
  }
};

char CishASTPassesDriverPass::ID = 0;

Pass* createCishASTPassesDriverPass(cish::CishContext& cishContext) {
  return new CishASTPassesDriverPass(cishContext);
}
