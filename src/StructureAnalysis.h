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
