#include "CishContext.h"

namespace cish {

CishContext::CishContext()
    : fileMgr(fileOpts), diagIDs(new clang::DiagnosticIDs()),
      diagOpts(new clang::DiagnosticOptions()), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new clang::TargetOptions()),
      targetInfo() {
  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new clang::IdentifierTable(langOpts));
}

CishContext::CishContext(const std::string& triple) : CishContext() {
  createASTContext(triple);
}

CishContext::~CishContext() {
  llvm::llvm_shutdown();
}

clang::ASTContext& CishContext::createASTContext(const std::string& triple) {
  targetOpts->Triple = triple;
  targetInfo.reset(clang::TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(
      new clang::ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);

  return *astContext;
}

llvm::LLVMContext& CishContext::getLLVMContext() {
  return llvmContext;
}

const llvm::LLVMContext& CishContext::getLLVMContext() const {
  return llvmContext;
}

clang::ASTContext& CishContext::getASTContext() {
  return *astContext;
}

const clang::ASTContext& CishContext::getASTContext() const {
  return *astContext;
}

FormatOptions& CishContext::getFormatOptions() {
  return fmtOpts;
}

const FormatOptions& CishContext::getFormatOptions() const {
  return fmtOpts;
}

} // namespace cish
