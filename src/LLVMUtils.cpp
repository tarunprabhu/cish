#include "LLVMUtils.h"

#include <llvm/Support/raw_ostream.h>
#include <llvm/Support/WithColor.h>

using namespace llvm;

const Argument& getArg(const Function& f, unsigned i) {
  for(const Argument& a : f.args())
    if(a.getArgNo() == i)
      return a;
  WithColor::error(errs()) << "Argument " << i << " out of range for function "
                           << f.getName() << "\n";
  exit(1);
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
