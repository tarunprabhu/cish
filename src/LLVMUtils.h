//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

#ifndef CISH_LLVM_UTILS_H
#define CISH_LLVM_UTILS_H

#include "List.h"
#include "Vector.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Function.h>

// A collection of utilities to because LLVM's API is inconsistent across
// versions and missing some useful functions in some versions

namespace cish {

namespace LLVM {

List<llvm::Loop*> collectLoops(const llvm::LoopInfo& li);

bool isMetadataFunction(const llvm::Function& f);

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

std::string formatName(const std::string& name);

template <typename T>
llvm::ArrayRef<T> makeArrayRef(const cish::Vector<T>& vec) {
  return llvm::ArrayRef<T>(vec.data(), vec.size());
}

} // namespace LLVM

} // namespace cish

#endif // CISH_LLVM_UTILS_H
