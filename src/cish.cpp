#include "CishContext.h"
#include "Diagnostics.h"
#include "IRPasses.h"
#include "Options.h"
#include "Set.h"

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/LegacyPassManager.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/InitializePasses.h>
#include <llvm/Support/TargetSelect.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/Transforms/InstCombine/InstCombine.h>
#include <llvm/Transforms/Scalar.h>

#include <unistd.h>

namespace cl = llvm::cl;

void initLLVM(int argc, char* argv[]) {
  llvm::InitializeAllAsmParsers();

  llvm::PassRegistry& registry = *llvm::PassRegistry::getPassRegistry();
  llvm::initializeCore(registry);
  llvm::initializeAnalysis(registry);
  llvm::initializeInstCombine(registry);

  // This is a lousy hack but using LLVM's command line library ends up
  // populating it with a lot of options that are obviously not relevant
  // to this. Calling this with -help ends up displaying all of them which is
  // rather unpleasant. So just get rid of whatever we don't need
  cish::Set<std::string> optsKeep = {"help", "o"};
  for(auto& i : cl::getRegisteredOptions()) {
    llvm::StringRef key = i.first();
    cl::Option* opt = i.second;
    if(optsKeep.contains(key))
      opt->setDescription("Display available options");
    else if(not(cish::isOpt(*opt) or opt->isPositional()))
      i.second->removeArgument();
  }

  cl::ParseCommandLineOptions(argc, argv, "LLVM to C-ish converter\n");
  cish::parseOpts();
}

int main(int argc, char* argv[]) {
  llvm::LLVMContext llvmContext;
  initLLVM(argc, argv);

  llvm::SMDiagnostic err;
  std::unique_ptr<llvm::Module> pModule
      = parseIRFile(cish::opts().fileIn, err, llvmContext);
  if(!pModule) {
    err.print(argv[0], llvm::errs());
    return 1;
  }

  // It doesn't seem worth it to put this into its own pass where it arguably
  // belongs. Compiling with -O0 results in clang adding an optnone attribute
  // to the IR. But we really need to do loop simplify so remove an optnone
  // attributes from functions if there are any
  for(llvm::Function& f : pModule->functions())
    if(f.hasFnAttribute(llvm::Attribute::AttrKind::OptimizeNone))
      f.removeFnAttr(llvm::Attribute::AttrKind::OptimizeNone);

  // Now that the clang AST Context has been set up, get down to business
  llvm::legacy::PassManager pm;
  pm.add(llvm::createDemoteRegisterToMemoryPass());
  // pm.add(llvm::createLoopSimplifyCFGPass());
  pm.add(createCishPreparePass());
  pm.add(createCishModulePass());
  pm.add(createCishFunctionPass());
  pm.add(createCishASTSimplifyPass());
  pm.add(createCishOutputPass(cish::opts().fileOut));
  pm.run(*pModule);

  llvm::llvm_shutdown();

  return 0;
}
