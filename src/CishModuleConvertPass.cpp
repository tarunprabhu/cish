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

#include "CishContext.h"
#include "Diagnostics.h"
#include "IRSourceInfo.h"
#include "LLVMBackend.h"
#include "LLVMFrontend.h"
#include "Options.h"

using namespace llvm;

class CishModuleConvertPass : public ModulePass {
public:
  static char ID;

public:
  CishModuleConvertPass();

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnModule(Module& m) override;
};

CishModuleConvertPass::CishModuleConvertPass() : ModulePass(ID) {
  ;
}

StringRef CishModuleConvertPass::getPassName() const {
  return "Cish Module Pass";
}

void CishModuleConvertPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<IRSourceInfoWrapperPass>();
  AU.addRequired<CishContextWrapperPass>();
  AU.setPreservesAll();
}

static bool isMetadataFunction(const Function& f) {
  FunctionType* fty = f.getFunctionType();
  if(fty->getReturnType()->isMetadataTy())
    return true;
  for(Type* param : fty->params())
    if(param->isMetadataTy())
      return true;
  return false;
}

bool CishModuleConvertPass::runOnModule(Module& m) {
  cish::message() << "Running " << getPassName() << "\n";

  const cish::SourceInfo& si
      = getAnalysis<IRSourceInfoWrapperPass>().getSourceInfo();
  const cish::CishContext& context
      = getAnalysis<CishContextWrapperPass>().getCishContext();
  cish::LLVMFrontend& fe = context.getLLVMFrontend();
  cish::LLVMBackend& be = context.getLLVMBackend();

  if(cish::opts().log) {
    std::string buf;
    raw_string_ostream filename(buf);
    if(cish::opts().logDir.size())
      filename << cish::opts().logDir << "/";
    std::string file = m.getSourceFileName();
    size_t start = file.rfind('/');
    size_t end = file.rfind('.');
    if(start == std::string::npos)
      start = 0;
    filename << file.substr(start, end - start) << ".prepared.ll";
    std::error_code ec;
    raw_fd_ostream fs(filename.str(), ec);
    if(not ec) {
      fs << m << "\n";
      fs.close();
      cish::message() << "Wrote prepare LLVM IR to " << filename.str() << "\n";
    } else {
      cish::warning() << "Could not write to log file " << filename.str()
                      << "\n";
    }
  }

  // First find anything that we know are never going to be
  // converted. These would be any LLVM debug and lifetime intrinsics but
  // could be other things as well
  for(const Function& f : m.functions()) {
    if(isMetadataFunction(f) or f.getName().startswith("llvm.lifetime")
       or f.getName().startswith("llvm.dbg.")) {
      fe.addIgnoreValue(&f);
      for(const Use& u : f.uses())
        fe.addIgnoreValue(u.getUser());
    }
  }

  // Add all the structs to the context first so a decl exists for each of
  // them first because there may be recursive structs
  for(StructType* sty : m.getIdentifiedStructTypes())
    be.add(sty, fe.getName(sty));

  // Then add bodies for them
  for(StructType* sty : m.getIdentifiedStructTypes()) {
    cish::Vector<std::string> fields;
    for(unsigned i = 0; i < sty->getNumElements(); i++) {
      Type* ety = sty->getElementType(i);
      if(not isa<StructType>(ety))
        fe.handle(sty->getElementType(i));
      if(si.hasElementName(sty, i))
        fields.push_back(si.getElementName(sty, i));
      else
        fields.push_back("elem_" + std::to_string(i));
    }
    be.add(sty, fields);
  }

  for(const GlobalVariable& g : m.globals())
    fe.handle(&g);

  for(const Function& f : m.functions())
    if(not isMetadataFunction(f))
      fe.handle(&f);

  for(const GlobalAlias& alias : m.aliases()) {
    cish::fatal(cish::error() << "NOT IMPLEMENTED: " << alias);
  }

  return false;
}

char CishModuleConvertPass::ID = 0;

static RegisterPass<CishModuleConvertPass>
    X("cish-module", "Cish Module Pass", true, true);

Pass* createCishModuleConvertPass() {
  return new CishModuleConvertPass();
}
