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

#include "LLVMUtils.h"
#include "Diagnostics.h"
#include "Set.h"

#include <llvm/IR/Instructions.h>

using namespace llvm;

namespace cish {

namespace LLVM {

static void collectLoops(Loop* loop, List<Loop*>& loops) {
  loops.push_back(loop);
  for(Loop* subLoop : *loop)
    collectLoops(subLoop, loops);
}

List<Loop*> collectLoops(const LoopInfo& li) {
  List<Loop*> loops;
  for(Loop* loop : li)
    collectLoops(loop, loops);
  return loops;
}

bool isMetadataFunction(const Function& f) {
  FunctionType* fty = f.getFunctionType();
  if(fty->getReturnType()->isMetadataTy())
    return true;
  for(Type* param : fty->params())
    if(param->isMetadataTy())
      return true;
  return false;
}

const Argument& getArg(const Function& f, unsigned i) {
  for(const Argument& a : f.args())
    if(a.getArgNo() == i)
      return a;
  cish::fatal(cish::error() << "Argument " << i << " out of range for function "
                            << f.getName());
}

const Function& getFunction(const Instruction& inst) {
  return *inst.getParent()->getParent();
}

const Function& getFunction(const Instruction* inst) {
  return getFunction(*inst);
}

llvm::Type* getBaseType(ArrayType* aty) {
  Type* ety = aty->getElementType();
  if(auto* bty = dyn_cast<ArrayType>(ety))
    return getBaseType(bty);
  return ety;
}

const Value* stripCasts(const Value* v) {
  if(const auto* cst = dyn_cast<CastInst>(v))
    return stripCasts(cst->getOperand(0));
  return v;
}

std::string formatName(const std::string& s) {
  static const Set<char> repl = {'.', '"', '-'};
  std::string name = s;
  for(size_t i = 0; i < name.length(); i++)
    if(repl.contains(name[i]))
      name[i] = '_';
  return name;
}

} // namespace LLVM

} // namespace cish
