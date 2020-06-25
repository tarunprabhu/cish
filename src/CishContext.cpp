#include "CishContext.h"
#include "Diagnostics.h"

using namespace llvm;

namespace cish {

CishContext::CishContext(const Module& m, const SourceInfo& si)
    : llvmContext(m.getContext()), fileMgr(fileOpts),
      diagIDs(new clang::DiagnosticIDs()),
      diagOpts(new clang::DiagnosticOptions()), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new clang::TargetOptions()),
      targetInfo() {
  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new clang::IdentifierTable(langOpts));

  targetOpts->Triple = m.getTargetTriple();
  targetInfo.reset(clang::TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(
      new clang::ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);
  be.reset(new LLVMBackend(*this));
  fe.reset(new LLVMFrontend(*this, si));
  passMgr.reset(new ASTPassManager(*this));
}

LLVMContext& CishContext::getLLVMContext() const {
  return llvmContext;
}

clang::ASTContext& CishContext::getASTContext() const {
  return *astContext;
}

LLVMFrontend& CishContext::getLLVMFrontend() const {
  return *fe;
}

LLVMBackend& CishContext::getLLVMBackend() const {
  return *be;
}

ASTPassManager& CishContext::getASTPassManager() const {
  return *passMgr;
}

} // namespace cish

CishContextWrapperPass::CishContextWrapperPass()
    : ModulePass(ID), context(nullptr) {
  ;
}

StringRef CishContextWrapperPass::getPassName() const {
  return "Cish Context Wrapper Pass";
}

void CishContextWrapperPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<SourceInfoWrapperPass>();
  AU.setPreservesAll();
}

cish::CishContext& CishContextWrapperPass::getCishContext() const {
  return *context;
}

bool CishContextWrapperPass::runOnModule(Module& m) {
  cish::message() << "Running " << getPassName() << "\n";

  const cish::SourceInfo& si
      = getAnalysis<SourceInfoWrapperPass>().getSourceInfo();

  context.reset(new cish::CishContext(m, si));

  return false;
}

char CishContextWrapperPass::ID = 0;

static RegisterPass<CishContextWrapperPass>
    X("cish-context-wrapper", "Cish Context Wrapper Pass", true, true);

Pass* createCishContextWrapperPass() {
  return new CishContextWrapperPass();
}
