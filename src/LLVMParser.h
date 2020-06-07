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
  void initialize(const llvm::Function& f);
  void initialize(const llvm::Module& m);
  void finalize(const llvm::Module& m);
  bool allUsesIgnored(const llvm::Value* v) const;
  bool shouldUseTemporary(const llvm::Instruction& inst);
  const llvm::Instruction&
  getInstructionForConstantExpr(const llvm::ConstantExpr& cexpr);

  clang::DeclRefExpr& handle(const llvm::AllocaInst& alloca);
  clang::Expr& handle(const llvm::LoadInst& load);
  clang::BinaryOperator& handle(const llvm::StoreInst& store);
  clang::Expr& handle(const llvm::GetElementPtrInst& gep);
  clang::DeclRefExpr& handle(const llvm::PHINode& phi);
  clang::CallExpr& handle(const llvm::CallInst& call);
  clang::CallExpr& handle(const llvm::InvokeInst& invoke);
  clang::BinaryOperator& handle(const llvm::CmpInst& cmp);
  clang::BinaryOperator& handle(const llvm::BinaryOperator& op);
  clang::UnaryOperator& handle(const llvm::UnaryOperator& op);
  clang::CastExpr& handle(const llvm::CastInst& cst);
  clang::Stmt& handle(const llvm::BranchInst& br);
  clang::SwitchStmt& handle(const llvm::SwitchInst& sw);
  clang::ConditionalOperator& handle(const llvm::SelectInst& select);
  clang::ReturnStmt& handle(const llvm::ReturnInst& ret);
  clang::Stmt& handle(const llvm::ShuffleVectorInst& shuffle);
  clang::Stmt& handle(const llvm::Instruction* inst);

  clang::DeclRefExpr& handle(const llvm::GlobalValue* g);

  clang::IntegerLiteral& handle(const llvm::ConstantInt& cint);
  clang::FloatingLiteral& handle(const llvm::ConstantFP& cfp);
  clang::CXXNullPtrLiteralExpr& handle(const llvm::ConstantPointerNull& cnull);
  clang::Expr& handle(const llvm::ConstantExpr& cexpr);
  clang::GNUNullExpr& handle(const llvm::UndefValue& undef);
  clang::CXXStdInitializerListExpr& handle(const llvm::ConstantDataArray& cda);
  clang::Expr& handle(const llvm::ConstantArray& carray);
  clang::CXXStdInitializerListExpr& handle(const llvm::ConstantStruct& cstruct);
  clang::CXXStdInitializerListExpr& handle(const llvm::ConstantVector& cvec);
  clang::Expr& handle(const llvm::Constant* c);

  clang::DeclRefExpr& handle(const llvm::BasicBlock& bb);

  clang::Stmt& handle(const llvm::Value* v);

  clang::QualType handle(llvm::Type* type);

  void handleIndices(llvm::Type* ty,
                     unsigned idx,
                     const Vector<const llvm::Value*>& indices,
                     const llvm::Instruction& inst,
                     llvm::raw_string_ostream& ss);

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
