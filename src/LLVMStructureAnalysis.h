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

#ifndef CISH_LLVM_STRUCTURE_ANALYSIS_H
#define CISH_LLVM_STRUCTURE_ANALYSIS_H

#include "StructureAnalysis.h"

#include <llvm/Analysis/LoopInfo.h>

namespace cish {

class LLVMStructureAnalysis : public StructureAnalysis {
protected:
  const llvm::Function& func;
  const llvm::LoopInfo& li;
  Map<const llvm::BasicBlock*, StructNode*> nodeMap;

protected:
  template <typename T = StructNode>
  T& getNodeFor(const llvm::BasicBlock& bb) const {
    return *llvm::dyn_cast<T>(nodeMap.at(&bb));
  }

  template <typename T = StructNode>
  T& getNodeFor(const llvm::BasicBlock* bb) const {
    return getNodeFor<T>(*bb);
  }

  virtual void buildTree() override;

public:
  LLVMStructureAnalysis(const llvm::Function& f, const llvm::LoopInfo& li);
  LLVMStructureAnalysis(const LLVMStructureAnalysis&) = delete;
  LLVMStructureAnalysis(LLVMStructureAnalysis&&) = delete;
  virtual ~LLVMStructureAnalysis() = default;

  virtual Result calculate() override;
};

} // namespace cish

#endif // CISH_LLVM_STRUCTURE_ANALYSIS_H
