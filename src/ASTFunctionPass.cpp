#include "ASTFunctionPass.h"
#include "ASTPassManager.h"
#include "CishContext.h"

namespace cish {

ASTFunctionPass::ASTFunctionPass(CishContext& context)
    : astContext(context.getASTContext()),
      passMgr(context.getASTPassManager()) {
  ;
}

DefUse& ASTFunctionPass::getDefUse() {
  return passMgr.getDefUse();
}

DefUse& ASTFunctionPass::getDefUse() const {
  return passMgr.getDefUse();
}

llvm::StringRef ASTFunctionPass::getPassName() const {
  return "Cish AST Pass (Unnamed)";
}

} // namespace cish
