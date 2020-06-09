#ifndef CISH_LLVM_PARSER_H
#define CISH_LLVM_PARSER_H

#include <llvm/IR/Instructions.h>
#include <llvm/IR/Module.h>

#include <clang/AST/ExprCXX.h>

#include "Map.h"
#include "Set.h"

namespace cish {

class CishContext;
class DIParser;

class LLVMParser {
protected:
  CishContext& ctxt;
  const DIParser& di;

  // Options from the command line
  // Set<IgnoreCasts> ignoreCasts;
  // Set<Annotations> annotations;

  // The values to tbe ignored when converting
  Set<const llvm::Value*> ignoreValues;

  // When processing llvm::ConstantExpr, we may end up creating an instruction
  // that implements the same operation. Those are kept here for easy cleanup.
  // Right now, we only make one pass over the function, but if that ever
  // changes, better to have it around so it's not converted every time
  Map<const llvm::ConstantExpr*,
      std::unique_ptr<llvm::Instruction,
                      std::function<void(llvm::Instruction*)>>>
      cexprs;

protected:
  bool allUsesIgnored(const llvm::Value* v) const;
  bool shouldUseTemporary(const llvm::Instruction& inst);
  const llvm::Instruction&
  getInstructionForConstantExpr(const llvm::ConstantExpr& cexpr);
  const llvm::ConstantDataArray*
  getStringLiteral(const llvm::Instruction& inst);

  void handle(const llvm::AllocaInst& alloca);
  void handle(const llvm::LoadInst& load);
  void handle(const llvm::StoreInst& store);
  void handle(const llvm::GetElementPtrInst& gep);
  void handle(const llvm::PHINode& phi);
  void handle(const llvm::CallInst& call);
  void handle(const llvm::InvokeInst& invoke);
  void handle(const llvm::CmpInst& cmp);
  void handle(const llvm::BinaryOperator& op);
  void handle(const llvm::UnaryOperator& op);
  void handle(const llvm::CastInst& cst);
  void handle(const llvm::BranchInst& br);
  void handle(const llvm::SwitchInst& sw);
  void handle(const llvm::SelectInst& select);
  void handle(const llvm::ReturnInst& ret);
  void handle(const llvm::ShuffleVectorInst& shuffle);

  void handle(const llvm::ConstantInt& cint);
  void handle(const llvm::ConstantFP& cfp);
  void handle(const llvm::ConstantPointerNull& cnull);
  void handle(const llvm::ConstantAggregateZero& czero);
  void handle(const llvm::ConstantExpr& cexpr);
  void handle(const llvm::UndefValue& undef);
  void handle(const llvm::ConstantDataArray& cda);
  void handle(const llvm::ConstantArray& carray);
  void handle(const llvm::ConstantStruct& cstruct);
  void handle(const llvm::ConstantVector& cvec);

  void handle(const llvm::BasicBlock& bb);

  void handle(const llvm::Instruction* inst);
  void handle(const llvm::GlobalValue* g);
  void handle(const llvm::Constant* c);
  void handle(const llvm::Value* v);

  void handle(llvm::PointerType* pty);
  void handle(llvm::ArrayType* aty);
  void handle(llvm::FunctionType* fty);
  void handle(llvm::Type* type);

  void handleIndices(llvm::Type* ty,
                     unsigned idx,
                     const Vector<const llvm::Value*>& indices,
                     const llvm::Instruction& inst,
                     llvm::raw_string_ostream& ss);

  std::string getName(llvm::StructType* sty);
  std::string getName(const llvm::Value* v, const std::string& prefix = "");
  std::string getName(const llvm::Value& v, const std::string& prefix = "");

  void runOnDeclaration(const llvm::Function& f);
  void runOnFunction(const llvm::Function& f);
  void runOnGlobal(const llvm::GlobalVariable& g);
  void runOnStruct(llvm::StructType* sty);

public:
  LLVMParser(CishContext& ctxt, const DIParser& di);

  void runOnModule(const llvm::Module& m);
};

} // namespace cish

#endif // CISH_LLVM_PARSER_H