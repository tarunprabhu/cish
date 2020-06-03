#include "If.h"

namespace cish {

If::If(Context& ctxt, const llvm::BranchInst& br)
    : ASTBase(ctxt, ASTKind::If), br(br) {
  ;
}

const llvm::BranchInst& If::getLLVM() const {
  return br;
}

} // namespace cish
