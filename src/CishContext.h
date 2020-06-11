#ifndef CISH_CISH_CONTEXT_H
#define CISH_CISH_CONTEXT_H

#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>

#include "FormatOptions.h"
#include "LLVMBackend.h"
#include "LLVMFrontend.h"
#include "SourceInfo.h"

namespace cish {

class SourceInfo;

class CishContext {
private:
  SourceInfo si;
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
  FormatOptions fmtOpts;
  std::unique_ptr<clang::ASTContext> astContext;
  std::unique_ptr<LLVMFrontend> fe;
  std::unique_ptr<LLVMBackend> be;

public:
  CishContext(const llvm::Module& m);
  CishContext(const CishContext&) = delete;
  CishContext(CishContext&&) = delete;

  clang::ASTContext& getASTContext() const;
  cish::LLVMFrontend& getLLVMFrontend() const;
  cish::LLVMBackend& getLLVMBackend() const;
  const cish::SourceInfo& getSourceInfo() const;
  const cish::FormatOptions& getFormatOptions() const;
};

} // namespace cish

class CishContextWrapperPass : public llvm::ModulePass {
public:
  static char ID;

private:
  std::unique_ptr<cish::CishContext> context;

public:
  CishContextWrapperPass();

  const cish::CishContext& getCishContext() const;

  virtual llvm::StringRef getPassName() const override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
  virtual bool runOnModule(llvm::Module& m) override;
};

#endif // CISH_CISH_CONTEXT_H
