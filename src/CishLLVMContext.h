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

#ifndef CISH_CISH_LLVM_CONTEXT_H
#define CISH_CISH_LLVM_CONTEXT_H

#include "CishContext.h"
#include "LLVMBackend.h"
#include "LLVMClangMap.h"
#include "LLVMSourceInfo.h"

#include <llvm/IR/Module.h>

namespace cish {

class CishLLVMContext : public CishContext {
protected:
  llvm::LLVMContext& llvmContext;
  std::unique_ptr<LLVMSourceInfo> si;
  std::unique_ptr<LLVMBackend> be;

public:
  CishLLVMContext(const llvm::Module& m);
  CishLLVMContext(const CishLLVMContext&) = delete;
  CishLLVMContext(CishLLVMContext&&) = delete;
  virtual ~CishLLVMContext() = default;

  NameGenerator& addNameGenerator(const llvm::Function& f);

  llvm::LLVMContext& getLLVMContext() const;
  LLVMBackend& getLLVMBackend() const;
  LLVMClangMap& getLLVMClangMap() const;
  const LLVMSourceInfo& getLLVMSourceInfo() const;
  NameGenerator& getNameGenerator(const llvm::Function& f) const;
};

} // namespace cish

#endif // CISH_CISH_LLVM_CONTEXT_H
