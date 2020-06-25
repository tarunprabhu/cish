#ifndef CISH_AST_FUNCTION_PASS_H
#define CISH_AST_FUNCTION_PASS_H

#include <clang/AST/ExprCXX.h>

namespace cish {

class ASTPassManager;
class CishContext;
class DefUse;

class ASTFunctionPass {
protected:
  clang::ASTContext& astContext;
  clang::FullSourceLoc invLoc;

private:
  ASTPassManager& passMgr;

protected:
  DefUse& getDefUse();
  DefUse& getDefUse() const;

public:
  ASTFunctionPass(CishContext& context);
  ASTFunctionPass(const ASTFunctionPass&) = delete;
  ASTFunctionPass(ASTFunctionPass&&) = delete;
  virtual ~ASTFunctionPass() = default;

  virtual llvm::StringRef getPassName() const;
  virtual bool runOnFunction(clang::FunctionDecl* f) = 0;
};

} // namespace cish

#endif // CISH_AST_FUNCTION_PASS_H
