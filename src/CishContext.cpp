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

#include "CishContext.h"
#include "AST.h"
#include "ASTPassManager.h"
#include "Diagnostics.h"
#include "IRSourceInfo.h"
#include "LLVMBackend.h"
#include "LLVMFrontend.h"

using namespace llvm;

namespace cish {

CishContext::CishContext(const Module& m, const SourceInfo& si)
    : llvmContext(m.getContext()), fileMgr(fileOpts),
      diagIDs(new clang::DiagnosticIDs()),
      diagOpts(new clang::DiagnosticOptions()), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new clang::TargetOptions()),
      targetInfo() {
  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new clang::IdentifierTable(langOpts));

  targetOpts->Triple = m.getTargetTriple();
  targetInfo.reset(clang::TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(
      new clang::ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);
  be.reset(new LLVMBackend(*this));
  fe.reset(new LLVMFrontend(*this, si));
}

AST& CishContext::addAST(clang::FunctionDecl* f) {
  asts.emplace(f, new AST(*this, f));

  return getAST(f);
}

AST& CishContext::getAST(clang::FunctionDecl* f) {
  return *asts.at(f);
}

const AST& CishContext::getAST(clang::FunctionDecl* f) const {
  return *asts.at(f);
}

LLVMContext& CishContext::getLLVMContext() const {
  return llvmContext;
}

clang::ASTContext& CishContext::getASTContext() const {
  return *astContext;
}

const clang::LangOptions& CishContext::getLangOptions() const {
  return langOpts;
}

LLVMFrontend& CishContext::getLLVMFrontend() const {
  return *fe;
}

LLVMBackend& CishContext::getLLVMBackend() const {
  return *be;
}

CishContext::func_range CishContext::funcs() {
  return asts.keys();
}

} // namespace cish

CishContextWrapperPass::CishContextWrapperPass()
    : ModulePass(ID), context(nullptr) {
  ;
}

StringRef CishContextWrapperPass::getPassName() const {
  return "Cish Context Wrapper Pass";
}

void CishContextWrapperPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<IRSourceInfoWrapperPass>();
  AU.setPreservesAll();
}

cish::CishContext& CishContextWrapperPass::getCishContext() const {
  return *context;
}

bool CishContextWrapperPass::runOnModule(Module& m) {
  cish::message() << "Running " << getPassName() << "\n";

  const cish::SourceInfo& si
      = getAnalysis<IRSourceInfoWrapperPass>().getSourceInfo();

  context.reset(new cish::CishContext(m, si));

  return false;
}

char CishContextWrapperPass::ID = 0;

static RegisterPass<CishContextWrapperPass>
    X("cish-context-wrapper", "Cish Context Wrapper Pass", true, true);

Pass* createCishContextWrapperPass() {
  return new CishContextWrapperPass();
}
