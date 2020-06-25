#include <llvm/Pass.h>

#include "ASTPassManager.h"
#include "ASTPasses.h"
#include "CishContext.h"

using namespace llvm;

// This is not really a pass
class CishASTSimplifyPass : public ModulePass {
public:
  static char ID;

public:
  CishASTSimplifyPass();

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnModule(Module& m) override;
};

CishASTSimplifyPass::CishASTSimplifyPass() : ModulePass(ID) {
  ;
}

StringRef CishASTSimplifyPass::getPassName() const {
  return "Cish AST Simplify Pass";
}

void CishASTSimplifyPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<CishContextWrapperPass>();
  AU.setPreservesAll();
}

bool CishASTSimplifyPass::runOnModule(Module& m) {
  cish::CishContext& cishContext
      = getAnalysis<CishContextWrapperPass>().getCishContext();

  clang::ASTContext& astContext = cishContext.getASTContext();

  cish::Vector<clang::FunctionDecl*> funcs;
  for(clang::Decl* decl : astContext.getTranslationUnitDecl()->decls())
    if(auto* fdecl = dyn_cast<clang::FunctionDecl>(decl))
      if(fdecl->hasBody())
        funcs.push_back(fdecl);

  cish::ASTPassManager& passMgr = cishContext.getASTPassManager();
  passMgr.addPass(createASTStripCastsPass(cishContext));
  passMgr.addPass(createASTSimplifyOperatorsPass(cishContext));
  passMgr.addPass(createASTPropagateExprsPass(cishContext));
  passMgr.addPass(createASTDeadCodeEliminationPass(cishContext));

  for(clang::FunctionDecl* f : funcs)
    passMgr.runOnFunction(f);

  return false;
}

char CishASTSimplifyPass::ID = 0;

static RegisterPass<CishASTSimplifyPass>
    X("cish-ast-simplify",
      "Simplify the Cish AST to make it more readable",
      true,
      true);

Pass* createCishASTSimplifyPass() {
  return new CishASTSimplifyPass();
}
