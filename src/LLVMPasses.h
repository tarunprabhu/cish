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

#ifndef CISH_LLVM_PASSES_H
#define CISH_LLVM_PASSES_H

#include <llvm/Pass.h>

namespace cish {

class CishLLVMContext;

} // namespace cish

llvm::Pass* createLLVMPrepareFunctionPass(cish::CishLLVMContext& cishContext);
llvm::Pass* createLLVMPrepareModulePass(cish::CishLLVMContext& cishContext);

#endif // CISH_LLVM_PASSES_H
