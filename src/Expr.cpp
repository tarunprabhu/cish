#include "Expr.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

const llvm::Value& Expr::getLLVM() const {
  return val;
}

const std::string& Expr::getParenthetized() const {
  return parenthetized;
}

} // namespace cish
