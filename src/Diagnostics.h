#ifndef CISH_DIAGNOSTICS_H
#define CISH_DIAGNOSTICS_H

#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

namespace cish {

llvm::WithColor warning(llvm::raw_ostream& = llvm::errs());
llvm::WithColor error(llvm::raw_ostream& = llvm::errs());
[[noreturn]] void fatal(llvm::raw_ostream& = llvm::errs());

} // namespace cish

#endif // CISH_DIAGNOSTICS_H
