#include "Stmt.h"

namespace cish {

const llvm::Value& Stmt::getLLVM() const {
  return val;
}

} // namespace cish
