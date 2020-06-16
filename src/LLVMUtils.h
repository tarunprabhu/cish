#ifndef CISH_LLVM_UTILS_H
#define CISH_LLVM_UTILS_H

#include "Vector.h"

#include <llvm/IR/Function.h>

// A collection of utilities to because LLVM's API is inconsistent across
// versions and missing some useful functions in some versions

/// @param f The llvm::Function
/// @param i The argument requested
/// @returns Returns the @i'th argument of function @f
const llvm::Argument& getArg(const llvm::Function& f, unsigned i);

/// @param f The llvm::Instruction
/// @returns The function to which this instruction belongs
const llvm::Function& getFunction(const llvm::Instruction& inst);

/// @param f The llvm::Instruction
/// @returns The function to which this instruction belongs
const llvm::Function& getFunction(const llvm::Instruction* inst);

/// @param ty The llvm::Type
/// @returns true if @ty is a pointer to the template parameter type T
template <typename T>
bool isPointerToType(llvm::Type* ty) {
  if(auto* pty = llvm::dyn_cast<llvm::PointerType>(ty))
    return llvm::isa<T>(pty->getElementType());
  return false;
}

/// @param aty The llvm::ArrayType
/// @returns The innermost non-array type within @aty
llvm::Type* getBaseType(llvm::ArrayType* aty);

const llvm::Value* stripCasts(const llvm::Value* v);

template<typename T>
llvm::ArrayRef<T> makeArrayRef(const cish::Vector<T>& vec) {
  return llvm::ArrayRef<T>(vec.data(), vec.size());
}

#endif // CISH_LLVM_UTILS_H
