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
#include "Diagnostics.h"
#include "LLVMCishMetadata.h"
#include "LLVMUtils.h"
#include "Set.h"

#include <llvm/Pass.h>

using namespace llvm;
namespace LLVM = cish::LLVM;

class LLVMPrepareModulePass : public ModulePass {
public:
  static char ID;

protected:
  cish::CishLLVMContext& cishContext;

protected:
  bool allUsesIn(const Value* v, const cish::Set<Value*> container) const {
    if(v->getNumUses() == 0)
      return false;
    for(const Use& u : v->uses())
      if(not container.contains(u.getUser()))
        return false;
    return true;
  }

  void expandIgnoredValues(cish::Set<Value*>& ignoreValues) {
    // Grow the ignore list to include anything that is only used by values in
    // the ignore list
    cish::Set<Value*> wl = ignoreValues;
    while(wl.size()) {
      cish::Set<Value*> next;
      for(Value* v : wl)
        if(auto* user = dyn_cast<User>(v))
          for(Use& op : user->operands())
            if(allUsesIn(op.get(), ignoreValues))
              next.insert(op.get());
      for(Value* v : next)
        ignoreValues.insert(v);
      wl = std::move(next);
    }
  }

  cish::Set<Value*> getIgnoreValues(Module& m) {
    cish::Set<Value*> ignoreValues;

    // First find anything that we know are never going to be
    // converted. These would be any LLVM debug and lifetime intrinsics but
    // could be other things as well
    for(Function& f : m.functions()) {
      StringRef fname = f.getName();
      if(LLVM::isMetadataFunction(f) or fname.startswith("llvm.lifetime")
         or fname.startswith("llvm.dbg.")) {
        ignoreValues.insert(&f);
        for(const Use& u : f.uses())
          ignoreValues.insert(u.getUser());
      }
    }
    expandIgnoredValues(ignoreValues);

    return ignoreValues;
  }

public:
  explicit LLVMPrepareModulePass(cish::CishLLVMContext& cishContext)
      : ModulePass(ID), cishContext(cishContext) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish IR Prepare Module";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module& m) override {
    cish::message() << "Running " << getPassName() << "\n";

    bool changed = false;

    for(Value* v : getIgnoreValues(m)) {
      if(auto* f = dyn_cast<Function>(v))
        changed |= LLVM::addCishMetadataIgnore(*f);
      else if(auto* inst = dyn_cast<Instruction>(v))
        changed |= LLVM::addCishMetadataIgnore(*inst);
      else if(auto* g = dyn_cast<GlobalVariable>(v))
        changed |= LLVM::addCishMetadataIgnore(*g);
      else if(isa<MetadataAsValue>(v))
        ;
      else
        cish::fatal(cish::error() << "Unknown LLVM value to ignore: " << *v);
    }

    return changed;
  }
};

char LLVMPrepareModulePass::ID = 0;

Pass* createLLVMPrepareModulePass(cish::CishLLVMContext& cishContext) {
  return new LLVMPrepareModulePass(cishContext);
}
