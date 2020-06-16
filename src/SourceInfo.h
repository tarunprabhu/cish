#ifndef CISH_SOURCE_INFO_H
#define CISH_SOURCE_INFO_H

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>

#include "Map.h"
#include "Set.h"
#include "Vector.h"

namespace cish {

/// Parses the debug information contained within in the module if any
/// and associates names found there with the corresponding LLVM values
/// and types
class SourceInfo {
protected:
  Map<const llvm::Value*, std::string> valueNames;
  Map<llvm::StructType*, std::string> structNames;
  Map<llvm::StructType*, Vector<std::string>> elemNames;
  Set<const llvm::Value*> cstrings;
  Set<const llvm::Value*> stringLiterals;

protected:
  void runOnStruct(llvm::StructType* sty,
                   const llvm::DICompositeType* di,
                   const llvm::StructLayout& sl);
  void runOnClass(llvm::StructType* sty,
                  const llvm::DICompositeType* di,
                  const llvm::StructLayout& sl);
  void runOnUnion(llvm::StructType* sty,
                  const llvm::DICompositeType* di,
                  const llvm::StructLayout& sl);
  void runOnGlobal(const llvm::GlobalVariable& g);
  void runOnFunction(const llvm::Function& f);
  void runOnModule(const llvm::Module& m);

  SourceInfo(const SourceInfo&) = delete;
  SourceInfo(SourceInfo&&) = delete;

public:
  SourceInfo(const llvm::Module& m);

  bool isCString(const llvm::Value* g) const;
  bool isCString(const llvm::Value& g) const;
  bool isStringLiteral(const llvm::Value& g) const;
  bool isStringLiteral(const llvm::Value* g) const;

  bool hasName(const llvm::Value* v) const;
  bool hasName(const llvm::Value& v) const;
  bool hasName(llvm::StructType* sty) const;
  bool hasElementName(llvm::StructType* sty, unsigned i) const;

  const std::string& getName(const llvm::Value* v) const;
  const std::string& getName(const llvm::Value& v) const;
  const std::string& getName(llvm::StructType* sty) const;
  const std::string& getElementName(llvm::StructType* sty, unsigned i) const;
};

} // namespace cish

class SourceInfoWrapperPass : public llvm::ModulePass {
public:
  static char ID;

protected:
  std::unique_ptr<cish::SourceInfo> si;

public:
  SourceInfoWrapperPass();

  const cish::SourceInfo& getSourceInfo() const;

  virtual llvm::StringRef getPassName() const override;
  virtual void getAnalysisUsage(llvm::AnalysisUsage& AU) const override;
  virtual bool runOnModule(llvm::Module& m) override;
};

#endif // CISH_SOURCE_INFO_H
