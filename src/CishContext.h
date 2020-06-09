#ifndef CISH_CISH_CONTEXT_H
#define CISH_CISH_CONTEXT_H

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>

#include <llvm/IR/LLVMContext.h>

#include "FormatOptions.h"

namespace cish {

class CishContext {
private:
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
  llvm::LLVMContext llvmContext;
  std::unique_ptr<clang::ASTContext> astContext;
  FormatOptions fmtOpts;

public:
  CishContext();
  CishContext(const std::string& triple);
  CishContext(const CishContext&) = delete;
  CishContext(CishContext&&) = delete;
  ~CishContext();

  clang::ASTContext& createASTContext(const std::string& triple);

  llvm::LLVMContext& getLLVMContext();
  clang::ASTContext& getASTContext();
  FormatOptions& getFormatOptions();
  const llvm::LLVMContext& getLLVMContext() const;
  const clang::ASTContext& getASTContext() const;
  const FormatOptions& getFormatOptions() const;
};

} // namespace cish

#endif // CISH_CISH_CONTEXT_H
