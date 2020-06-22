#ifndef CISH_IR_PASSES_H
#define CISH_IR_PASSES_H

#include <llvm/Pass.h>

#include <string>

llvm::Pass* createCishASTSimplifyPass();
llvm::Pass* createCishContextWrapperPass();
llvm::Pass* createCishFunctionPass();
llvm::Pass* createCishModulePass();
llvm::Pass* createCishOutputPass(const std::string& outFile);
llvm::Pass* createCishPreparePass();
llvm::Pass* createStructureAnalysisWrapperPass();
llvm::Pass* createSourceInfoWrapperPass();

#endif // CISH_IR_PASSES_H
