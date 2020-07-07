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
#include "Options.h"
#include "Logging.h"

using namespace llvm;
namespace Clang = cish::Clang;

// This is not really a pass
class CishASTPassesDriverPass : public ModulePass {
public:
  static char ID;

private:
  cish::CishContext& cishContext;
  unsigned passIdx;

protected:
  std::string getTag(const std::string& base) {
    std::string buf;
    raw_string_ostream ss(buf);
    ss << base;
    if(passIdx)
      ss << "." << passIdx;
    return ss.str();
  }

  void
  logAST(clang::FunctionDecl* f, const std::string& tag) {

    if(cish::opts().has(cish::LogCategory::AST))
      if(cish::Logger log = cish::Logger::openFile(f->getName(), tag, "cish"))
        log() << Clang::toString(f, f->getASTContext()) << "\n";
  }

  void logCFG(clang::FunctionDecl* f, const std::string& tag) {
    if(cish::opts().has(cish::LogCategory::CFG))
      if(cish::Logger log = cish::Logger::openFile(f->getName(), tag, "cfg"))
        cishContext.getAST(f).getCFG()->print(
            log(), cishContext.getLangOptions(), true);
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
      logAST(f, "init");
      logCFG(f, "init");
      for(cish::ASTPass* pass : passes) {
        pass->runOnFunction(f);
        passIdx += 1;
        // The Cish pass names will always be prefixed with "cish-"
        logAST(f, getTag(pass->getPassName().substr(5)));
        logCFG(f, getTag(pass->getPassName().substr(5)));
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
