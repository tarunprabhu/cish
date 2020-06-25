#include "ASTFunctionPass.h"
#include "ASTPassManager.h"
#include "CishContext.h"

namespace cish {

ASTFunctionPass::ASTFunctionPass(CishContext& context)
    : astContext(context.getASTContext()),
      passMgr(context.getASTPassManager()) {
  ;
}

bool ASTFunctionPass::modifiesAST() const {
  return false;
}

ASTLookup& ASTFunctionPass::getASTLookup() {
  return passMgr.getASTLookup();
}

DefUse& ASTFunctionPass::getDefUse() {
  return passMgr.getDefUse();
}

llvm::StringRef ASTFunctionPass::getPassName() const {
  return "Cish AST Pass (Unnamed)";
}

} // namespace cish
