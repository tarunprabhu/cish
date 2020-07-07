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

#include "LLVMClangMap.h"

namespace cish {

LLVMClangMap::LLVMClangMap() : IRClangMap() {
  ;
}

void LLVMClangMap::add(const llvm::Function& llvmFunc,
                       clang::FunctionDecl* decl) {
  fmap.insert(&llvmFunc, decl);
  addFunction(llvmFunc.getName(), decl);
}

void LLVMClangMap::add(const llvm::GlobalVariable& llvmGlobal,
                       clang::VarDecl* decl) {
  gmap.insert(&llvmGlobal, decl);
  addGlobal(llvmGlobal.getName(), decl);
}

const llvm::Function& LLVMClangMap::get(clang::FunctionDecl* decl) const {
  return *fmap.at(decl);
}

clang::FunctionDecl* LLVMClangMap::get(const llvm::Function& f) const {
  return fmap.at(&f);
}

const llvm::GlobalVariable& LLVMClangMap::get(clang::VarDecl* decl) const {
  return *gmap.at(decl);
}

clang::VarDecl* LLVMClangMap::get(const llvm::GlobalVariable& g) const {
  return gmap.at(&g);
}

} // namespace cish
