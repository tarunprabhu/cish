#ifndef CISH_CONTEXT_H
#define CISH_CONTEXT_H

#include "Map.h"
#include "Set.h"
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
class CishContext {
private:
  std::string varPrefix;
  uint64_t varSuffix;

  // The statements comprising the body of the function
  std::vector<clang::Stmt*> stmts;

  // The same LLVM value could have more than one C representation
  // For instance, a function parameter could have a
  // declaration (with a type) and a use (which would be just the name).
  // Similarly, local variables would be allocas which would be a cish::Stmt
  // when declared but a cish::Expr when used
  Map<const llvm::Value*, clang::Stmt*> exprs;

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
  Map<llvm::Type*, clang::QualType> types;
  Map<llvm::StructType*, clang::RecordDecl*> udts;
  Map<const llvm::Function*, clang::FunctionDecl*> funcs;
  Map<const llvm::BasicBlock*, clang::LabelDecl*> blocks;

  // All of these are needed to set up the clang::ASTContext object
  // std::unique_ptr<clang::ASTContext> astContext;
  clang::ASTContext& astContext;

protected:
  template <
      typename ClassT,
      typename... Args,
      std::enable_if_t<std::is_base_of<clang::Stmt, ClassT>::value, int> = 0>
  ClassT& add(const llvm::Value& val, ClassT* ast) {
    return *llvm::dyn_cast<ClassT>(exprs[&val] = ast);
  }

public:
  /// @param prefix The prefix to use when generating names
  CishContext(const std::string& prefix, clang::ASTContext& clangContext);
  CishContext(const CishContext&) = delete;
  CishContext(CishContext&&) = delete;
  ~CishContext() = default;

  /// Returns a new entity name that (hopefully) will not collide with
  /// any other entity name that already exists in the code being Cish'ed
  ///
  /// @param[optional] prefix An additional prefix to add to the variable
  ///                         name. Useful to disambiguate between local
  ///                         variables, args, blocks, labels etc.
  /// @returns The new variable name
  std::string getNewVar(const std::string& prefix = "v");

  void beginFunction(const llvm::Function& f);
  void endFunction(const llvm::Function& f);
  void beginBlock(const llvm::BasicBlock& bb);
  void endBlock(const llvm::BasicBlock& bb);

  clang::Stmt& add(const llvm::AllocaInst& alloca,
                          const std::string& name);
  clang::Stmt& add(const llvm::BranchInst& br);
  clang::Stmt& add(const llvm::CastInst& cst);
  clang::Stmt& add(const llvm::CallInst& call);
  clang::Stmt& add(const llvm::InvokeInst& call);
  clang::Stmt& add(const llvm::LoadInst& load);
  clang::Stmt& add(const llvm::StoreInst& store);
  clang::Stmt& add(const llvm::BinaryOperator& inst,
                             clang::BinaryOperator::Opcode opc);
  clang::Stmt& add(const llvm::CmpInst& cmp,
                             clang::BinaryOperator::Opcode opc);
  clang::Stmt& add(const llvm::UnaryOperator& inst,
                            clang::UnaryOperator::Opcode opc);
  clang::Stmt& add(const llvm::PHINode& phi, const std::string& name);
  clang::Stmt& add(const llvm::SelectInst& select);
  clang::Stmt& add(const llvm::ReturnInst& ret);

  clang::Stmt& add(const llvm::ConstantInt& cint);
  clang::Stmt& add(const llvm::ConstantFP& cfp);
  clang::Stmt& add(const llvm::ConstantPointerNull& cnull);
  clang::Stmt& add(const llvm::ConstantAggregateZero& czero);
  clang::Stmt& add(const llvm::UndefValue& cundef);
  clang::Stmt& add(const llvm::ConstantDataArray& cda);
  clang::Stmt& add(const llvm::ConstantStruct& cstruct);
  clang::Stmt& add(const llvm::ConstantExpr& cexpr,
                   const llvm::Value& val);
  clang::Stmt& add(const llvm::ConstantArray& carray);

  clang::Stmt& add(const llvm::Function& f, const std::string& name,
                   const Vector<std::string>& argNames = {});
  clang::Stmt& add(const llvm::GlobalVariable& g,
                          const std::string& name);
  clang::Stmt& add(const llvm::Argument& arg, const std::string& name);

  void add(const llvm::BasicBlock& bb, const std::string& name = "");


  clang::QualType add(llvm::IntegerType* ity);
  clang::QualType add(llvm::PointerType* pty);
  clang::QualType add(llvm::ArrayType* aty);
  clang::QualType add(llvm::FunctionType* fty);
  clang::QualType add(llvm::VectorType* vty);
  clang::QualType add(llvm::Type* type);

  // Struct types need to be added in two phases because they may be recursive.
  // In the first phase, all the structs are added and they are all empty.
  // In the second phase, bodies are added to all of them.
  clang::QualType add(llvm::StructType* sty,
                      const std::string& name);
  clang::QualType add(llvm::StructType* sty,
                      const Vector<std::string>& elems);

  void addTemp(const llvm::Instruction& val, const std::string& name);

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
    return *llvm::dyn_cast<ClangT>(
        get<clang::DeclRefExpr>(val).getFoundDecl());
  }

  template <
      typename ClangT,
      std::enable_if_t<std::is_base_of<clang::Decl, ClangT>::value, int> = 0>
  ClangT& get(const llvm::Value& val) {
    return get<ClangT>(&val);
  }

  // /// @param loop A llvm::Loop
  // /// @returns The AST node for the llvm::Loop @loop
  // template <
  //     typename ClangT,
  //     std::enable_if_t<std::is_same<clang::ForStmt, ClangT>::value
  //                          || std::is_same<clang::WhileStmt, ClangT>::value,
  //                      int> = 0>
  // ClangT& get(const llvm::Loop* loop) {
  //   return *llvm::dyn_cast<ClangT>(loops.at(loop));
  // }

  // /// @param val A llvm::Loop
  // /// @returns The AST node for the llvm::Loop @val
  // template <
  //     typename ClangT,
  //     std::enable_if_t<std::is_same<clang::ForStmt, ClangT>::value
  //                          || std::is_same<clang::WhileStmt, ClangT>::value,
  //                      int> = 0>
  // ClangT& get(const llvm::Loop& loop) {
  //   return get<ClangT>(&loop);
  // }

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

#endif // CISH_CONTEXT_H
