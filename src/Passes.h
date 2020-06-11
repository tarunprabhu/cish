#ifndef CISH_PASSES_H
#define CISH_PASSES_H

#include <llvm/Pass.h>

#include <string>

namespace cish {

class FormatOptions;

} // namespace cish

llvm::Pass* createCishModulePass();
llvm::Pass* createCishFunctionPass();
llvm::Pass* createCishContextWrapperPass();
llvm::Pass* createCishOutputPass(const std::string& outFile);

#endif // CISH_PASSES_H
