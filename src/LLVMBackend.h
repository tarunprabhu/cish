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

#ifndef CISH_LLVM_BACKEND_H
#define CISH_LLVM_BACKEND_H

#include "AST.h"
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

class SourceInfo;

/// This is a single object that contains the maps from LLVM entities
/// (types and values) to their corresponding Cish AST nodes and keeps track
/// of names of entities that were determined from DebugInfo or elsewehere
/// and any that are generated
class LLVMBackend : public BackendBase {
private:
  const SourceInfo& si;

  // LLVM values may have multiple uses. But Clang's statements/expressions are
  // never uniqued. This is a map of a "template" for each LLVM value. The
  // Clang statement for the LLVM value should be cloned when being used.
  // Every statement in this map should be an orphan
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
  Map<llvm::StructType*, Vector<clang::FieldDecl*>> fields;
  Map<const llvm::Function*, clang::FunctionDecl*> funcs;
  Map<const llvm::Argument*, clang::ParmVarDecl*> args;
  Map<const llvm::BasicBlock*, clang::LabelDecl*> blocks;
  Map<const llvm::AllocaInst*, clang::VarDecl*> locals;
  Map<const llvm::GlobalVariable*, clang::VarDecl*> globals;
  Map<const llvm::GlobalAlias*, clang::VarDecl*> aliases;

  // When processing llvm::ConstantExpr, we may end up creating an instruction
  // that implements the same operation. Those are kept here for easy cleanup.
  // Right now, we only make one pass over the function, but if that ever
  // changes, better to have it around so it's not converted every time
  Map<const llvm::ConstantExpr*,
      std::unique_ptr<llvm::Instruction,
                      std::function<void(llvm::Instruction*)>>>
      cexprs;

protected:
  const llvm::ConstantDataArray*
  getStringLiteral(const llvm::Instruction& inst);
  const llvm::Instruction&
  getInstructionForConstantExpr(const llvm::ConstantExpr& cexpr);

  clang::QualType get(llvm::IntegerType* ity);
  clang::QualType get(llvm::PointerType* pty);
  clang::QualType get(llvm::ArrayType* aty);
  clang::QualType get(llvm::FunctionType* fty);
  clang::QualType get(llvm::VectorType* vty);
  clang::QualType get(llvm::Type* type);

  clang::Expr* get(const llvm::AllocaInst& alloca);
  // clang::Expr* get(const llvm::AtomicCmpXchgInst& axchg);
  // clang::Expr* get(const llvm::AtomicRMWInst& rmw);
  clang::Expr* get(const llvm::BinaryOperator& op);
  clang::Expr* get(const llvm::CastInst& cst);
  clang::Expr* get(const llvm::InvokeInst& invoke);
  clang::Expr* get(const llvm::CallInst& call);
  clang::Expr* get(const llvm::CmpInst& cmp);
  clang::Expr* get(const llvm::ExtractElementInst& extract);
  clang::Expr* get(const llvm::ExtractValueInst& extract);
  clang::Expr* get(const llvm::FenceInst& fence);
  clang::Expr* get(const llvm::GetElementPtrInst& gep);
  clang::Expr* get(const llvm::IndirectBrInst& br);
  clang::Expr* get(const llvm::InsertElementInst& insert);
  clang::Expr* get(const llvm::InsertValueInst& insert);
  clang::Expr* get(const llvm::LandingPadInst& pad);
  clang::Expr* get(const llvm::LoadInst& load);
  clang::Expr* get(const llvm::PHINode& phi);
  clang::Expr* get(const llvm::SelectInst& select);
  clang::Expr* get(const llvm::ShuffleVectorInst& shuffle);
  clang::Expr* get(const llvm::UnaryOperator& op);

  clang::Expr* get(const llvm::ConstantInt& cint);
  clang::Expr* get(const llvm::ConstantFP& cfp);
  clang::Expr* get(const llvm::ConstantPointerNull& cnull);
  clang::Expr* get(const llvm::ConstantAggregateZero& czero);
  clang::Expr* get(const llvm::ConstantExpr& cexpr);
  clang::Expr* get(const llvm::UndefValue& undef);
  clang::Expr* get(const llvm::ConstantDataSequential& cseq);
  clang::Expr* get(const llvm::ConstantArray& carray);
  clang::Expr* get(const llvm::ConstantStruct& cstruct);
  clang::Expr* get(const llvm::ConstantVector& cvec);

  clang::Expr* get(const llvm::Argument& arg);
  clang::Expr* get(const llvm::Function& f);
  clang::Expr* get(const llvm::GlobalAlias& alias);
  clang::Expr* get(const llvm::GlobalVariable& g);

  clang::Expr* get(const llvm::Value* v);

  clang::QualType get(llvm::Type* type) const;
  clang::DeclRefExpr& getTemp(const llvm::Value& val) const;
  clang::DeclRefExpr& getTemp(const llvm::Value* val) const;

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

  void addSwitchCase(const llvm::ConstantInt* value);

public:
  LLVMBackend(CishContext& cishContext);
  LLVMBackend(const LLVMBackend&) = delete;
  LLVMBackend(LLVMBackend&&) = delete;
  ~LLVMBackend() = default;

  std::string getName(const llvm::Value* v, const std::string& prefix = "");
  std::string getName(const llvm::Value& v, const std::string& prefix = "");

  bool has(const llvm::Value* v) const;
  bool has(const llvm::Value& v) const;
  bool has(llvm::Type* type) const;
  bool hasTemp(const llvm::Value& val) const;
  bool hasTemp(const llvm::Value* val) const;

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

  void add(const llvm::BranchInst& br);
  void add(const llvm::CallInst& call);
  void add(const llvm::CatchReturnInst& catchRet);
  void add(const llvm::CatchSwitchInst& catchSwitch);
  void add(const llvm::CleanupReturnInst& cleanup);
  void add(const llvm::FenceInst& fence);
  void add(const llvm::CatchPadInst& pad);
  void add(const llvm::IndirectBrInst& br);
  void add(const llvm::InvokeInst& invoke);
  void add(const llvm::InsertElementInst& insert);
  void add(const llvm::InsertValueInst& insert);
  void add(const llvm::LandingPadInst& pad);
  void add(const llvm::ResumeInst& resume);
  void add(const llvm::ReturnInst& returnInst);
  void add(const llvm::ShuffleVectorInst& shuffle);
  void add(const llvm::StoreInst& store);
  void add(const llvm::SwitchInst& sw);

  void add(const llvm::Function& f,
           const std::string& name,
           const Vector<std::string>& argNames);
  void add(const llvm::GlobalVariable& g, const std::string& name);
  void add(const llvm::GlobalAlias& g, const std::string& name);
  void add(const llvm::Argument& arg, const std::string& name);
  void add(const llvm::AllocaInst& alloca, const std::string& name);
  void add(const llvm::BasicBlock& bb, const std::string& name = "");

  // Struct types need to be added in two phases because they may be
  // recursive. In the first phase, all the structs are added and they are all
  // empty. In the second phase, bodies are added to all of them.
  void add(llvm::StructType* sty, const std::string& name);
  void add(llvm::StructType* sty, const Vector<std::string>& elems);

  void addTemp(const llvm::Instruction& val, const std::string& name);
  void addGoto(const llvm::BasicBlock& bb);
  void addGoto(const std::string& dest, const llvm::Function& f);
  void addIfThen(const llvm::BranchInst& br, bool invert);
  void addIfThenElse(const llvm::BranchInst& br);
  void addIfThenBreak(const llvm::BranchInst& br, bool invert);
  void addIfThenGoto(const std::string& label,
                     const llvm::BranchInst& br,
                     bool invert);
  void addLabel(const std::string& name, const llvm::Function& f);
  void addEndlessLoop();
  void addBreak();
  void addContinue();
  void addSwitchCase(const llvm::ConstantInt& value);
  void addSwitchDefault();
  void addSwitchStmt(const llvm::SwitchInst& sw);
};

} // namespace cish

#endif // CISH_LLVM_BACKEND_H
