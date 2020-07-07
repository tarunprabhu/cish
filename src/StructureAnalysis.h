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

#ifndef CISH_STRUCTURE_ANALYSIS_H
#define CISH_STRUCTURE_ANALYSIS_H

#include "Map.h"
#include "StructureTree.h"

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

#include <memory>

namespace cish {

class StructureAnalysis {
public:
  enum Result {
    Unstructured,
    SemiStructured,
    Structured,
    PerfectlyStructured,
  };

protected:
  llvm::LLVMContext& llvmContext;
  std::string func;

  bool hasGoto;
  unsigned reductions;
  unsigned labelId;
  std::unique_ptr<Entry> root;
  List<std::unique_ptr<StructNode>> nodes;
  llvm::raw_null_ostream nullStream;

  Set<StructNode*> loopExitingBlocks;
  Map<StructNode*, unsigned> loopHeaders;

protected:
  template <
      typename ClassT,
      typename... ArgsT,
      std::enable_if_t<std::is_base_of<StructNode, ClassT>::value, int> = 0>
  ClassT& newNode(ArgsT&&... args) {
    return llvm::cast<ClassT>(*nodes.emplace_back(new ClassT(args...)));
  }

  void dfs(StructNode& node,
           Set<StructNode*>& seen,
           List<StructNode*>& postorder) const;
  List<StructNode*> dfs() const;

  bool reduceEndlessLoop(StructNode& header);
  bool reduceSimplifiedLoop(LoopHeader& header,
                            const List<IfThenBreak*>& exits);
  bool reduceSequence(const List<StructNode*>& nodes);
  bool reduceIfThen(Block& cond, StructNode& thn, StructNode& succ);
  bool reduceIfThenElse(Block& cond,
                        StructNode& pi,
                        StructNode& pj,
                        StructNode& succ);
  bool reduceIfThenGoto(StructNode& dest);
  bool reduceSwitch(StructNode::Edges cases, Block& block, StructNode& succ);

  bool tryReduce(const List<StructNode*>& postorder);
  bool tryReduceSequence(const List<StructNode*>& postorder);
  bool tryReduceIfThen(const List<StructNode*>& postorder);
  bool tryReduceIfThenElse(const List<StructNode*>& postorder);
  bool tryReduceIfThenGoto(const List<StructNode*>& postorder);
  bool tryReduceEndlessLoop(const List<StructNode*>& postorder);
  bool tryReduceSimplifiedLoop(const List<StructNode*>& postorder);
  bool tryReduceSwitch(const List<StructNode*>& postorder);

  void log(const std::string& = "");

protected:
  StructureAnalysis(llvm::LLVMContext& llvmContext, const std::string& func);
  StructureAnalysis(const StructureAnalysis&) = delete;
  StructureAnalysis(const StructureAnalysis&&) = delete;

  void stripEmptyBlocks();
  void specializeLoopHeaders();

  virtual void buildTree() = 0;
  virtual Result calculate();

public:
  const StructNode& getStructured() const;
};

} // namespace cish

#endif // CISH_STRUCTURE_ANALYSIS_H
