#include "Decl.h"

namespace cish {

const llvm::Value& Decl::getLLVM() const {
  return val;
}

} // namespace cish
