#include "ASTExprPass.h"
#include "Diagnostics.h"

using namespace clang;

namespace cish {

ASTExprPass::ASTExprPass(ASTContext& astContext) : ASTFunctionPass(astContext) {
  ;
}


} // namespace cish
