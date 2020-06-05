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
