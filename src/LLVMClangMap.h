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

#ifndef CISH_LLVM_CLANG_MAP_H
#define CISH_LLVM_CLANG_MAP_H

#include "IRClangMap.h"
#include "Map2.h"

#include <llvm/IR/Function.h>
#include <llvm/IR/GlobalVariable.h>

#include <clang/AST/ExprCXX.h>

namespace cish {

class LLVMClangMap : public IRClangMap {
protected:
  Map2<const llvm::Function*, clang::FunctionDecl*> fmap;
  Map2<const llvm::GlobalVariable*, clang::VarDecl*> gmap;

public:
  LLVMClangMap();
  LLVMClangMap(LLVMClangMap&) = delete;
  LLVMClangMap(LLVMClangMap&&) = delete;
  virtual ~LLVMClangMap() = default;

  void add(const llvm::Function& llvmFunc, clang::FunctionDecl* decl);
  void add(const llvm::GlobalVariable& llvmGlobal, clang::VarDecl* decl);

  clang::FunctionDecl* get(const llvm::Function& f) const;
  clang::VarDecl* get(const llvm::GlobalVariable& g) const;
  const llvm::Function& get(clang::FunctionDecl* decl) const;
  const llvm::GlobalVariable& get(clang::VarDecl* decl) const;
};

} // namespace cish

#endif // CISH_LLVM_CLANG_MAP_H
