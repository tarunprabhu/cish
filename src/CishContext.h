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

#ifndef CISH_CISH_CONTEXT_H
#define CISH_CISH_CONTEXT_H

#include "Map.h"

#include <llvm/ADT/iterator_range.h>
#include <llvm/IR/Dominators.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>

namespace cish {

class AST;
class LLVMBackend;
class LLVMFrontend;
class SourceInfo;

class CishContext {
private:
  llvm::LLVMContext& llvmContext;

  clang::FileSystemOptions fileOpts;
  clang::FileManager fileMgr;
  clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diagIDs;
  clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagOpts;
  clang::DiagnosticsEngine diagEngine;
  clang::SourceManager srcMgr;
  clang::LangOptions langOpts;
  std::shared_ptr<clang::IdentifierTable> idents;
  clang::Builtin::Context builtins;
  clang::SelectorTable sels;
  std::shared_ptr<clang::TargetOptions> targetOpts;
  std::shared_ptr<clang::TargetInfo> targetInfo;

protected:
  std::unique_ptr<clang::ASTContext> astContext;
  std::unique_ptr<LLVMFrontend> fe;
  std::unique_ptr<LLVMBackend> be;
  Map<clang::FunctionDecl*, std::unique_ptr<AST>> asts;

public:
  using func_iterator = decltype(asts)::const_key_iterator;
  using func_range = llvm::iterator_range<func_iterator>;

public:
  CishContext(const llvm::Module& m, const SourceInfo& si);
  CishContext(const CishContext&) = delete;
  CishContext(CishContext&&) = delete;

  AST& addAST(clang::FunctionDecl* f);
  AST& getAST(clang::FunctionDecl* f);
  const AST& getAST(clang::FunctionDecl* f) const;
  llvm::LLVMContext& getLLVMContext() const;
  clang::ASTContext& getASTContext() const;
  const clang::LangOptions& getLangOptions() const;
  LLVMFrontend& getLLVMFrontend() const;
  LLVMBackend& getLLVMBackend() const;

  func_range funcs();
};

} // namespace cish

class CishContextWrapperPass : public llvm::ModulePass {
public:
  static char ID;

private:
  std::unique_ptr<cish::CishContext> context;

public:
  CishContextWrapperPass();

  cish::CishContext& getCishContext() const;

  virtual llvm::StringRef getPassName() const override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
  virtual bool runOnModule(llvm::Module& m) override;
};

#endif // CISH_CISH_CONTEXT_H
