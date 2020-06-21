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
public:
  struct DbgValElements {
    llvm::Value* value;
    llvm::DINode* di;
    llvm::DIExpression* expr;

    DbgValElements(llvm::Value* value,
                   llvm::DINode* di,
                   llvm::DIExpression* expr)
        : value(value), di(di), expr(expr) {
      ;
    }

    operator bool() const {
      return value;
    }
  };

private:
  Vector<const llvm::Function*> dbgFns;
  Map<llvm::StructType*, const llvm::DICompositeType*> structs;

protected:
  const llvm::DataLayout& dl;
  Map<const llvm::Value*, std::string> valueNames;
  Map<llvm::StructType*, std::string> structNames;
  Map<llvm::StructType*, std::string> classNames;
  Map<llvm::StructType*, std::string> unionNames;
  Map<llvm::StructType*, Vector<std::string>> elemNames;
  Set<const llvm::Value*> cstrings;
  Set<const llvm::Value*> stringLiterals;

protected:
  void collectStructs(llvm::Type* type, const llvm::Metadata* md);
  void collectStructs(llvm::PointerType* pty, const llvm::DIType* derived);
  void collectStructs(llvm::ArrayType* aty, const llvm::DIType* comp);
  void collectStructs(llvm::FunctionType* fty, const llvm::DIType* md);
  void collectStructs(llvm::StructType* sty, const llvm::DIType* md);

  void collectStructsFromStruct(llvm::StructType* sty,
                                const llvm::DICompositeType* comp);
  void collectStructsFromUnion(llvm::StructType* sty,
                               const llvm::DICompositeType* md);
  void collectStructsFromClass(llvm::StructType* sty,
                               const llvm::DICompositeType* md);

  void runOnGlobal(const llvm::GlobalVariable& g);
  void runOnFunction(const llvm::Function& f);
  void runOnModule(const llvm::Module& m);

  SourceInfo(const SourceInfo&) = delete;
  SourceInfo(SourceInfo&&) = delete;

public:
  SourceInfo(const llvm::Module& m);

  DbgValElements parseDbgValCall(const llvm::CallInst& call) const;

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
