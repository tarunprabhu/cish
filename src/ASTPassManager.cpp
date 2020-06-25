#include "ASTPassManager.h"

using namespace clang;

namespace cish {

ASTPassManager::ASTPassManager(CishContext& context) : du(context) {
  ;
}

DefUse& ASTPassManager::getDefUse() {
  return du;
}

void ASTPassManager::addPass(ASTFunctionPass* pass) {
  passes.push_back(pass);
}

bool ASTPassManager::runOnFunction(FunctionDecl* f) {
  bool changed = false;

  for(ASTFunctionPass* pass : passes) {
    message() << "Running " << pass->getPassName() << " on " << f->getName()
              << "\n";
    bool passChanged = pass->runOnFunction(f);
    if(passChanged) {
      message() << "Updating Cish Def Use information\n";
      du.runOnFunction(f);
    }
    changed |= passChanged;
  }

  return changed;
}

} // namespace cish
