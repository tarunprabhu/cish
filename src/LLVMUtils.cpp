#include "LLVMUtils.h"
#include "Diagnostics.h"

#include <llvm/IR/Instructions.h>

using namespace llvm;

namespace cish {

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

} // namespace cish
