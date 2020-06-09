#ifndef CISH_DI_PARSER_H
#define CISH_DI_PARSER_H

#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Module.h>

#include "Map.h"
#include "Set.h"
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

public:
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

  void runOnModule(const llvm::Module& m);
};

} // namespace cish

#endif // CISH_DI_PARSER_H
