#ifndef CISH_LLVM_UTILS_H
#define CISH_LLVM_UTILS_H

#include <llvm/IR/Function.h>

// A collection of utilities to because LLVM's API is inconsistent across
// versions and missing some useful functions in some versions

/// @param f The llvm::Function
/// @param i The argument requested
/// @returns Returns the @i'th argument of function @f
const llvm::Argument& getArg(const llvm::Function& f, unsigned i);

template <typename T>
static bool isPointerToType(llvm::Type* ty) {
  if(auto* pty = llvm::dyn_cast<llvm::PointerType>(ty))
    return llvm::isa<T>(pty->getElementType());
  return false;
}

#endif // CISH_LLVM_UTILS_H
