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

#include <llvm/IR/Function.h>
#include <llvm/Pass.h>

#include "Structure.h"

namespace cish {

class StructureAnalysis;

enum class StructureKind {
  Unstructured,
  SemiStructured,
  Structured,
  PerfectlyStructured,
};

} // namespace cish

class StructureAnalysisWrapperPass : public llvm::FunctionPass {
public:
  static char ID;

private:
  cish::StructureKind structureKind;
  std::unique_ptr<cish::StructureAnalysis> ctree;

public:
  StructureAnalysisWrapperPass();

  cish::StructureKind getStructureKind() const;
  const cish::StructNode& getStructured() const;

  virtual llvm::StringRef getPassName() const override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
  virtual bool runOnFunction(llvm::Function& f) override;
};

#endif // CISH_STRUCTURE_ANALYSIS_H
