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

#ifndef CISH_LLVM_CISH_METADATA_H
#define CISH_LLVM_CISH_METADATA_H

#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>
#include <llvm/IR/Instruction.h>

#define CISH_INST_METADATA(NAME)                             \
  bool addCishMetadata##NAME(llvm::Instruction& inst);       \
  bool addCishMetadata##NAME(llvm::Instruction* inst);       \
  bool hasCishMetadata##NAME(const llvm::Instruction& inst); \
  bool hasCishMetadata##NAME(const llvm::Instruction& inst);

#define CISH_FUNC_METADATA(NAME)                       \
  bool addCishMetadata##NAME(llvm::Function& f);       \
  bool addCishMetadata##NAME(llvm::Function* f);       \
  bool hasCishMetadata##NAME(const llvm::Function& f); \
  bool hasCishMetadata##NAME(const llvm::Function* f);

#define CISH_GLOBAL_METADATA(NAME)                           \
  bool addCishMetadata##NAME(llvm::GlobalVariable& g);       \
  bool addCishMetadata##NAME(llvm::GlobalVariable* g);       \
  bool hasCishMetadata##NAME(const llvm::GlobalVariable& g); \
  bool hasCishMetadata##NAME(const llvm::GlobalVariable* g);

namespace cish {

namespace LLVM {

#include "LLVMCishMetadata.inc"

} // namespace LLVM

} // namespace cish

#undef CISH_INST_METADATA
#undef CISH_FUNC_METADATA
#undef CISH_GLOBAL_METADATA

#endif // CISH_LLVM_CISH_METADATA_H
