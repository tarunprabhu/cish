#ifndef CISH_AST_PASS_MANAGER_H
#define CISH_AST_PASS_MANAGER_H

#include "DefUse.h"
#include "Vector.h"

namespace cish {

class ASTFunctionPass;
class CishContext;

class ASTPassManager {
private:
  DefUse du;
  Vector<ASTFunctionPass*> passes;

public:
  ASTPassManager(CishContext& astContext);
  ASTPassManager(const ASTPassManager&) = delete;
  ASTPassManager(ASTPassManager&&) = delete;

  DefUse& getDefUse();

  void addPass(ASTFunctionPass* pass);
  bool runOnFunction(clang::FunctionDecl* f);
};

} // namespace cish

#endif // CISH_AST_PASS_MANAGER_H
