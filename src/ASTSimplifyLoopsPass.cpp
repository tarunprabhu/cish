#include "ASTFunctionPass.h"
#include "DefUse.h"

using namespace clang;

namespace cish {

// Convert do-while loops into for-loops and while loops whenever possible
class ASTSimplifyLoopsPass : public ASTFunctionPass {
public:
  ASTSimplifyLoopsPass(CishContext& context);
  ASTSimplifyLoopsPass(const ASTSimplifyLoopsPass&) = delete;
  ASTSimplifyLoopsPass(ASTSimplifyLoopsPass&&) = delete;
  virtual ~ASTSimplifyLoopsPass() = default;

  virtual llvm::StringRef getPassName() const override;
  virtual bool runOnFunction(FunctionDecl* f) override;
};

ASTSimplifyLoopsPass::ASTSimplifyLoopsPass(CishContext& context)
    : ASTFunctionPass(context) {
  ;
}

llvm::StringRef ASTSimplifyLoopsPass::getPassName() const {
  return "Cish AST Simplify Loops";
}

bool ASTSimplifyLoopsPass::runOnFunction(FunctionDecl* f) {
  bool changed = false;

  return changed;
}

} // namespace cish

cish::ASTFunctionPass* createASTSimplifyLoopsPass(cish::CishContext& context) {
  return new cish::ASTSimplifyLoopsPass(context);
}
