#ifndef CISH_PASSES_H
#define CISH_PASSES_H

#include <llvm/Pass.h>

#include <string>

llvm::Pass* createCishPreparePass();
llvm::Pass* createCishModulePass();
llvm::Pass* createCishFunctionPass();
llvm::Pass* createCishContextWrapperPass();
llvm::Pass* createCishOutputPass(const std::string& outFile);
llvm::Pass* createStructureAnalysisWrapperPass();
llvm::Pass* createSourceInfoWrapperPass();

#endif // CISH_PASSES_H
