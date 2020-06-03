#include "While.h"

namespace cish {

While::While(Context& ctxt, const llvm::Loop& loop)
    : ASTBase(ctxt, ASTKind::While), loop(loop) {
  ;
}

const llvm::Loop& While::getLLVM() const {
  return loop;
}

} // namespace cish
