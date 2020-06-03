#include "For.h"

namespace cish {

For::For(Context& ctxt, const llvm::Loop& loop)
    : ASTBase(ctxt, ASTKind::For), loop(loop) {
  ;
}

const llvm::Loop& For::getLLVM() const {
  return loop;
}

} // namespace cish
