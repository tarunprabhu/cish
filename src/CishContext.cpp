#include "CishContext.h"
#include "CishCmdLineFlags.h"

using namespace llvm;

namespace cish {

CishContext::CishContext(const Module& m)
    : llvmContext(m.getContext()), si(m), fileMgr(fileOpts),
      diagIDs(new clang::DiagnosticIDs()),
      diagOpts(new clang::DiagnosticOptions()), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new clang::TargetOptions()),
      targetInfo() {
  fmtOpts.prefix = optPrefix;
  fmtOpts.indentation = optIndentStyle;
  fmtOpts.parens = optParens;
  fmtOpts.offset = optOffset;
  fmtOpts.quiet = optQuiet;
  for(StripCasts cst : optStripCasts)
    fmtOpts.set(cst);
  for(Annotations ann : optAnnotations)
    fmtOpts.set(ann);

  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new clang::IdentifierTable(langOpts));

  targetOpts->Triple = m.getTargetTriple();
  targetInfo.reset(clang::TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(
      new clang::ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);
  be.reset(new LLVMBackend(*this));
  fe.reset(new LLVMFrontend(*this));
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

const FormatOptions& CishContext::getFormatOptions() const {
  return fmtOpts;
}

const SourceInfo& CishContext::getSourceInfo() const {
  return si;
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
  AU.setPreservesAll();
}

const cish::CishContext& CishContextWrapperPass::getCishContext() const {
  return *context;
}

bool CishContextWrapperPass::runOnModule(Module& m) {
  context.reset(new cish::CishContext(m));

  return false;
}

char CishContextWrapperPass::ID = 0;

static RegisterPass<CishContextWrapperPass>
    X("cish-context-wrapper", "Cish Context Wrapper Pass", true, true);

Pass* createCishContextWrapperPass() {
  return new CishContextWrapperPass();
}
