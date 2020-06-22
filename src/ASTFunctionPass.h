#ifndef CISH_AST_FUNCTION_PASS_H
#define CISH_AST_FUNCTION_PASS_H

#include <clang/AST/ExprCXX.h>

namespace cish {

class ASTFunctionPass {
protected:
  clang::ASTContext& astContext;
  clang::FullSourceLoc invLoc;

public:
  ASTFunctionPass(clang::ASTContext& astContext) : astContext(astContext) {
    ;
  }
  ASTFunctionPass(const ASTFunctionPass&) = delete;
  ASTFunctionPass(ASTFunctionPass&&) = delete;
  virtual ~ASTFunctionPass() = default;

  virtual llvm::StringRef getPassName() const = 0;
  virtual void runOnFunction(clang::FunctionDecl* f) = 0;
};

} // namespace cish

#endif // CISH_AST_FUNCTION_PASS_H
