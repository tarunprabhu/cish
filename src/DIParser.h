#ifndef CISH_DI_PARSER_H
#define CISH_DI_PARSER_H

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Module.h>

#include "Map.h"
#include "Vector.h"

namespace cish {

/// Parses the debug information contained within in the module if any
/// and associates names found there with the corresponding LLVM values
/// and types
class DIParser {
protected:
  Map<const llvm::Value*, std::string> valueNames;
  Map<llvm::StructType*, std::string> structNames;
  Map<llvm::StructType*, Vector<std::string>> elemNames;

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

public:
  bool hasSourceName(const llvm::Value* v) const;
  bool hasSourceName(const llvm::Value& v) const;
  bool hasSourceName(llvm::StructType* sty) const;
  bool hasSourceName(llvm::StructType* sty, unsigned i) const;

  const std::string& getSourceName(const llvm::Value* v) const;
  const std::string& getSourceName(const llvm::Value& v) const;
  const std::string& getSourceName(llvm::StructType* sty) const;
  const std::string& getSourceName(llvm::StructType* sty, unsigned i) const;

  void runOnModule(const llvm::Module& m);
};

} // namespace cish

#endif // CISH_DI_PARSER_H
