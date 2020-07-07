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
#include "LLVMCishMetadata.h"
#include "LLVMUtils.h"
#include "Options.h"

using namespace llvm;
namespace LLVM = cish::LLVM;

class CishModuleConvertPass : public ModulePass {
public:
  static char ID;

private:
  cish::CishContext& cishContext;
  const cish::SourceInfo& si;
  cish::LLVMBackend& be;

protected:
  std::string getName(llvm::StructType* sty) {
    if(si.hasName(sty))
      return si.getName(sty);
    else if(sty->hasName())
      if(sty->getName().find("struct.") == 0)
        return LLVM::formatName(sty->getName().substr(7));
      else if(sty->getName().find("class.") == 0)
        return LLVM::formatName(sty->getName().substr(6));
      else if(sty->getName().find("union.") == 0)
        return LLVM::formatName(sty->getName().substr(6));
      else
        return LLVM::formatName(sty->getName());
    else
      return be.getNewVar("struct");
  }

public:
  explicit CishModuleConvertPass(cish::CishContext& cishContext)
      : ModulePass(ID), cishContext(cishContext),
        si(cishContext.getSourceInfo()), be(cishContext.getLLVMBackend()) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish Module Pass";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.addRequired<LoopInfoWrapperPass>();
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module& m) override {
    cish::message() << "Running " << getPassName() << "\n";

    // Add all the structs to the context first so a decl exists for each of
    // them first because there may be recursive structs
    for(StructType* sty : m.getIdentifiedStructTypes())
      be.add(sty, getName(sty));

    // Then add bodies for them
    for(StructType* sty : m.getIdentifiedStructTypes()) {
      cish::Vector<std::string> fields;
      for(unsigned i = 0; i < sty->getNumElements(); i++) {
        if(si.hasElementName(sty, i))
          fields.push_back(si.getElementName(sty, i));
        else
          fields.push_back("elem_" + std::to_string(i));
      }
      be.add(sty, fields);
    }

    for(const GlobalVariable& g : m.globals()) {
      if(not si.isStringLiteral(g))
        be.add(g, be.getName(g, "g"));
    }

    for(const Function& f : m.functions()) {
      if(not LLVM::isMetadataFunction(f)) {
        std::string fname = be.getName(f);
        cish::Vector<std::string> argNames(f.getFunctionType()->getNumParams());
        if(f.size()) {
          for(const Argument& arg : f.args()) {
            unsigned i = arg.getArgNo();
            if(si.hasName(arg))
              argNames[i] = si.getName(arg);
            else if(arg.hasName())
              argNames[i] = LLVM::formatName(arg.getName());
            else
              argNames[i] = "arg_" + std::to_string(i);
          }
          be.add(f, fname, argNames);

          for(const BasicBlock& bb : f) {
            if(bb.hasNPredecessors(0))
              be.add(bb);
            else
              be.add(bb, be.getNewVar("bb"));
          }
        } else if(not LLVM::hasCishMetadataIgnore(f)) {
          be.add(f, fname, argNames);
        }
      }
    }

    for(const GlobalAlias& alias : m.aliases()) {
      cish::fatal(cish::error() << "Unimplemented GlobalAlias");
    }

    return false;
  }
};

char CishModuleConvertPass::ID = 0;

Pass* createCishModuleConvertPass(cish::CishContext& cishContext) {
  return new CishModuleConvertPass(cishContext);
}
