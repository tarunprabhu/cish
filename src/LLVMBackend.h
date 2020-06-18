#ifndef CISH_LLVM_BACKEND_H
#define CISH_LLVM_BACKEND_H

#include "BackendBase.h"
#include "Map.h"
#include "Set.h"
#include "Stack.h"
#include "Vector.h"

#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <clang/AST/ExprCXX.h>

#include <memory>

namespace cish {

/// This is a single object that contains the maps from LLVM entities
/// (types and values) to their corresponding Cish AST nodes and keeps track
/// of names of entities that were determined from DebugInfo or elsewehere
/// and any that are generated
class LLVMBackend : public BackendBase {
private:
  Map<const llvm::Value*, clang::Stmt*> exprs;
  Map<llvm::Type*, clang::QualType> types;

  // In SSA form, all values are "assigned" to some variable and that
  // variable may get used multiple times. But we don't want to do exactly
  // the same thing in the resulting C code because that would look really
  // messy. But in certain cases, we probably do want to do that. Some
  // situations would be:
  //
  //  - "Many" uses of the RHS. If the RHS were a load for instance, it might
  //    suggest that the compiler is performing many more loads than expected
  //    by potentially reading values many times.
  //
  //  - Function calls can definitely not be duplicated - not just because
  //    of the cost but because the function could have side effects.
  //    Duplicating the call would suggest that the compiler hasn't figured
  //    this out which would again probably be wrong
  //
  Map<const llvm::Value*, clang::DeclStmt*> temps;

  // Decls
  Map<llvm::StructType*, clang::RecordDecl*> udts;
  Map<llvm::StructType*, Vector<clang::DeclRefExpr*>> fields;
  Map<const llvm::Function*, clang::FunctionDecl*> funcs;
  Map<const llvm::BasicBlock*, clang::LabelDecl*> blocks;
  Map<std::string, clang::LabelDecl*> labels;

protected:
  template <
      typename ClassT,
      typename... Args,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClassT>::value, int> = 0>
  ClassT& add(const llvm::Value& val, ClassT* ast) {
    return *llvm::dyn_cast<ClassT>(exprs[&val] = ast);
  }

  clang::Expr* handleIndexOperand(llvm::PointerType* ty,
                                  clang::Expr* curr,
                                  unsigned idx,
                                  const Vector<const llvm::Value*>& op,
                                  const llvm::Instruction& inst);

  clang::Expr* handleIndexOperand(llvm::ArrayType* ty,
                                  clang::Expr* curr,
                                  unsigned idx,
                                  const Vector<const llvm::Value*>& op,
                                  const llvm::Instruction& inst);

  clang::Expr* handleIndexOperand(llvm::StructType* ty,
                                  clang::Expr* curr,
                                  unsigned field,
                                  const llvm::Instruction& inst);

  clang::DeclRefExpr* createVariable(const std::string& name,
                                     clang::QualType type,
                                     clang::DeclContext* parent);

  clang::BinaryOperator* createBinaryOperator(clang::Expr& lhs,
                                              clang::Expr& rhs,
                                              clang::BinaryOperator::Opcode op,
                                              clang::QualType type);

  clang::UnaryOperator* createUnaryOperator(clang::Expr& lhs,
                                            clang::UnaryOperator::Opcode op,
                                            clang::QualType type);

  clang::ArraySubscriptExpr* createArraySubscriptExpr(clang::Expr& arr,
                                                      clang::Expr& idx,
                                                      clang::QualType type);

  clang::CStyleCastExpr* createCastExpr(clang::Expr& expr,
                                        clang::QualType type);

  clang::IfStmt*
  createIfStmt(clang::Expr& cond, clang::Stmt* thn, clang::Stmt* els = nullptr);

  clang::GotoStmt* createGoto(clang::LabelDecl* label);

  clang::CompoundStmt*
  createCompoundStmt(const cish::Vector<clang::Stmt*>& stmts);

  clang::CompoundStmt* createCompoundStmt(clang::Stmt* stmt);

  clang::BreakStmt* createBreakStmt();

  clang::ContinueStmt* createContinueStmt();

  clang::ReturnStmt* createReturnStmt(clang::Expr* retExpr);

  clang::DeclRefExpr* createDeclRefExpr(clang::ValueDecl* decl);

  clang::CallExpr* createCallExpr(clang::Expr& callee,
                                  const Vector<clang::Expr*>& args,
                                  clang::QualType type);

  clang::DoStmt* createDoStmt(clang::Stmt* body, clang::Expr& cond);
  clang::WhileStmt* createWhileStmt(clang::Expr& cond, clang::Stmt* body);

  clang::CXXBoolLiteralExpr* createBoolLiteral(bool b, clang::QualType type);
  clang::IntegerLiteral* createIntLiteral(const llvm::APInt& i,
                                          clang::QualType type);
  clang::FloatingLiteral* createFloatLiteral(const llvm::APFloat& f,
                                             clang::QualType type);
  clang::CXXNullPtrLiteralExpr* createNullptr(clang::QualType type);
  clang::InitListExpr* createInitListExpr(const Vector<clang::Expr*>& exprs);

  clang::LabelStmt* createLabelStmt(clang::LabelDecl* label);
  clang::LabelDecl* createLabelDecl(clang::FunctionDecl* f,
                                    const std::string& name);

public:
  /// @param prefix The prefix to use when generating names
  LLVMBackend(CishContext& context);
  LLVMBackend() = delete;
  LLVMBackend(const LLVMBackend&) = delete;
  LLVMBackend(LLVMBackend&&) = delete;
  ~LLVMBackend() = default;

  void beginFunction(const llvm::Function& f);
  void endFunction(const llvm::Function& f);

  /// Begin an unstructured block. In this case, a label will be created
  /// and there may or may not be a new scope (compound statement)
  /// created for the block.
  /// @params bb A llvm::BasicBlock
  void beginBlock(const llvm::BasicBlock& bb);

  /// End the given basic block. This must match a call to beginBlock() with
  /// the same llvm:BasicBlock
  /// @params bb A llvm::BasicBlock
  void endBlock(const llvm::BasicBlock& bb);

  /// Begin a structured block. A label will not be created, a new compound
  /// statement will definitely be created and all statements will be added
  /// to that compound statement
  void beginBlock();

  /// End the compound statement
  void endBlock();

  void add(const llvm::AllocaInst& alloca, const std::string& name);
  void add(const llvm::AtomicCmpXchgInst& axchg);
  void add(const llvm::AtomicRMWInst& rmw);
  void add(const llvm::BinaryOperator& op);
  void add(const llvm::BranchInst& br);
  void add(const llvm::CastInst& cst);
  void add(const llvm::InvokeInst& invoke);
  void add(const llvm::CallInst& call);
  void add(const llvm::CatchReturnInst& catchRet);
  void add(const llvm::CatchSwitchInst& catchSwitch);
  void add(const llvm::CleanupReturnInst& cleanup);
  void add(const llvm::CmpInst& cmp);
  void add(const llvm::ExtractElementInst& extract);
  void add(const llvm::ExtractValueInst& extract);
  void add(const llvm::FenceInst& fence);
  void add(const llvm::CatchPadInst& pad);
  void add(const llvm::GetElementPtrInst& gep);
  void add(const llvm::IndirectBrInst& br);
  void add(const llvm::InsertElementInst& insert);
  void add(const llvm::InsertValueInst& insert);
  void add(const llvm::LandingPadInst& pad);
  void add(const llvm::LoadInst& load);
  void add(const llvm::PHINode& phi, const std::string& name);
  void add(const llvm::ResumeInst& resume);
  void add(const llvm::ReturnInst& returnInst);
  void add(const llvm::SelectInst& select);
  void add(const llvm::ShuffleVectorInst& shuffle);
  void add(const llvm::StoreInst& store);
  void add(const llvm::SwitchInst& sw);
  void add(const llvm::UnaryOperator& op);
  void add(const llvm::UnreachableInst& unreachable);

  void add(const llvm::ConstantInt& cint);
  void add(const llvm::ConstantFP& cfp);
  void add(const llvm::ConstantPointerNull& cnull);
  void add(const llvm::ConstantAggregateZero& czero);
  void add(const llvm::UndefValue& cundef);
  void add(const llvm::ConstantDataSequential& cseq);
  void add(const llvm::ConstantStruct& cstruct);
  void add(const llvm::ConstantExpr& cexpr, const llvm::Value& val);
  void add(const llvm::ConstantArray& carray);

  void add(const llvm::Function& f,
           const std::string& name,
           const Vector<std::string>& argNames = {});
  void add(const llvm::GlobalVariable& g, const std::string& name);
  void add(const llvm::GlobalAlias& g, const std::string& name);
  void add(const llvm::Argument& arg, const std::string& name);

  void add(const llvm::BasicBlock& bb, const std::string& name = "");

  void add(llvm::IntegerType* ity);
  void add(llvm::PointerType* pty);
  void add(llvm::ArrayType* aty);
  void add(llvm::FunctionType* fty);
  void add(llvm::VectorType* vty);
  void add(llvm::Type* type);

  // Struct types need to be added in two phases because they may be
  // recursive. In the first phase, all the structs are added and they are all
  // empty. In the second phase, bodies are added to all of them.
  void add(llvm::StructType* sty, const std::string& name);
  void add(llvm::StructType* sty, const Vector<std::string>& elems);

  void addTemp(const llvm::Instruction& val, const std::string& name);
  void addIfThen(const llvm::BranchInst& br, bool invert);
  void addIfThenElse(const llvm::BranchInst& br);
  void addIfThenBreak(const llvm::BranchInst& br);
  void addIfThenGoto(const std::string& label,
                     const llvm::BranchInst& br,
                     bool invert);
  void addLabel(const std::string& name, const llvm::Function& f);
  void addEndlessLoop();
  void addBreak();
  void addContinue();

  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  ClangT& get(const llvm::Value* val) {
    return *llvm::dyn_cast<ClangT>(exprs.at(val));
  }

  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  ClangT& get(const llvm::Value& val) {
    return get<ClangT>(&val);
  }

  template <
      typename ClangT,
      std::enable_if_t<std::is_base_of<clang::Decl, ClangT>::value, int> = 0>
  ClangT& get(const llvm::Value* val) {
    return *llvm::dyn_cast<ClangT>(get<clang::DeclRefExpr>(val).getFoundDecl());
  }

  template <
      typename ClangT,
      std::enable_if_t<std::is_base_of<clang::Decl, ClangT>::value, int> = 0>
  ClangT& get(const llvm::Value& val) {
    return get<ClangT>(&val);
  }

  /// @param val A llvm::Value
  /// @returns true if the @val has a corresponding AST node
  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  bool has(const llvm::Value* val) const {
    return exprs.contains(val);
  }

  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  bool has(const llvm::Value& val) const {
    return has<ClangT>(&val);
  }

  bool has(llvm::Type* type) const;
  bool hasTemp(const llvm::Value& val) const;
  bool hasTemp(const llvm::Value* val) const;

  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  const ClangT& get(const llvm::Value* val) const {
    return *llvm::dyn_cast<ClangT>(exprs.at(val));
  }

  template <
      typename ClangT = clang::Stmt,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClangT>::value, int> = 0>
  const ClangT& get(const llvm::Value& val) const {
    return get<ClangT>(&val);
  }

  clang::QualType get(llvm::Type* type) const;
  clang::DeclRefExpr& getTemp(const llvm::Value& val) const;
  clang::DeclRefExpr& getTemp(const llvm::Value* val) const;
};

} // namespace cish

#endif // CISH_LLVM_BACKEND_H
