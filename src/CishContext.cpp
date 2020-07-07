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
#include "Diagnostics.h"
#include "IRSourceInfo.h"
#include "LLVMBackend.h"
#include "NameGenerator.h"

using namespace llvm;

namespace cish {

CishContext::CishContext(const Module& m)
    : llvmContext(m.getContext()), fileMgr(fileOpts),
      diagIDs(new clang::DiagnosticIDs()),
      diagOpts(new clang::DiagnosticOptions()), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new clang::TargetOptions()),
      targetInfo(), nameGen(new NameGenerator(*this)), si(new SourceInfo(m)) {
  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new clang::IdentifierTable(langOpts));

  targetOpts->Triple = m.getTargetTriple();
  targetInfo.reset(clang::TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(
      new clang::ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);

  // This must be done before the backends are created but after the ASTContext
  // has been initialized
  topLevelAST.reset(new AST(*this));

  be.reset(new LLVMBackend(*this));
}

AST& CishContext::addAST(clang::FunctionDecl* f) {
  asts.emplace(f, new AST(*this, f, *topLevelAST));

  return getAST(f);
}

AST& CishContext::getAST(clang::FunctionDecl* f) {
  if(not f)
    return *topLevelAST;
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

LLVMBackend& CishContext::getLLVMBackend() const {
  return *be;
}

NameGenerator& CishContext::getNameGenerator() const {
  return *nameGen;
}

const SourceInfo& CishContext::getSourceInfo() const {
  return *si;
}

CishContext::func_range CishContext::funcs() {
  return asts.keys();
}

} // namespace cish
