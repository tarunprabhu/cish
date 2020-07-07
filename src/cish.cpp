//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

#include "CishLLVMContext.h"
#include "CishPasses.h"
#include "Diagnostics.h"
#include "LLVMPasses.h"
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
  std::unique_ptr<llvm::Module> m
      = parseIRFile(cish::opts().fileIn, err, llvmContext);
  if(!m) {
    err.print(argv[0], llvm::errs());
    return 1;
  }

  // It doesn't seem worth it to put this into its own pass where it arguably
  // belongs. Compiling with -O0 results in clang adding an optnone attribute
  // to the IR. But we really need to do loop simplify so remove an optnone
  // attributes from functions if there are any
  for(llvm::Function& f : m->functions())
    if(f.hasFnAttribute(llvm::Attribute::AttrKind::OptimizeNone))
      f.removeFnAttr(llvm::Attribute::AttrKind::OptimizeNone);

  cish::CishLLVMContext cishContext(*m);

  // Now that the clang AST Context has been set up, get down to business
  llvm::legacy::PassManager pm;
  pm.add(llvm::createDemoteRegisterToMemoryPass());
  pm.add(createLLVMPrepareModulePass(cishContext));
  pm.add(createLLVMPrepareFunctionPass(cishContext));
  pm.add(createCishLLVMModuleConvertPass(cishContext));
  pm.add(createCishLLVMFunctionConvertPass(cishContext));
  pm.add(createCishASTPassesDriverPass(cishContext));
  pm.add(createCishASTWriterPass(cishContext));
  pm.run(*m);

  llvm::llvm_shutdown();

  return 0;
}
