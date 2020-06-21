#ifndef CISH_STREAM_H
#define CISH_STREAM_H

#include <clang/AST/ASTContext.h>
#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

namespace cish {

class Stream {
protected:
  const clang::ASTContext& astContext;
  llvm::raw_ostream& os;

  std::string tabStr;
  unsigned ilevel;

protected:
  bool shouldPrintCast(const clang::Type*) const;
  bool shouldPrintCast(clang::QualType type) const;

  Stream& parenthetize(const clang::Stmt*);

  bool isVoidTy(const clang::Type*) const;
  bool isBoolTy(const clang::Type*) const;
  bool isCharTy(const clang::Type*) const;
  bool isShortTy(const clang::Type*) const;
  bool isIntTy(const clang::Type*) const;
  bool isLongTy(const clang::Type*) const;
  bool isFloatTy(const clang::Type*) const;
  bool isDoubleTy(const clang::Type*) const;
  bool isLongDoubleTy(const clang::Type*) const;

  Stream& doCompoundStmt(const clang::CompoundStmt* stmt, bool block);

public:
  Stream(const clang::ASTContext& astContext, llvm::raw_ostream& os);

  Stream& tab();
  Stream& endl();
  Stream& space(unsigned spaces = 1);
  Stream& endst(bool newline = true);

  // The optional label is used if the block represents a basic block and a
  // label is added before the block
  Stream& beginBlock(const std::string& label = "");

  // endst will be true when ending the body of a struct because a semicolon
  // must be added immediately after the block in that case
  Stream& endBlock(bool endst = false);

  Stream& operator<<(char);
  Stream& operator<<(int16_t);
  Stream& operator<<(uint16_t);
  Stream& operator<<(int32_t);
  Stream& operator<<(uint32_t);
  Stream& operator<<(int64_t);
  Stream& operator<<(uint64_t);
  Stream& operator<<(float);
  Stream& operator<<(double);
  Stream& operator<<(long double);
  Stream& operator<<(const char*);
  Stream& operator<<(llvm::StringRef);
  Stream& operator<<(const std::string&);
  Stream& operator<<(Stream&);

  Stream& operator<<(clang::BinaryOperator::Opcode);
  Stream& operator<<(clang::UnaryOperator::Opcode);

  Stream& operator<<(const clang::Type*);
  Stream& operator<<(const clang::PointerType*);
  Stream& operator<<(const clang::ConstantArrayType*);
  Stream& operator<<(const clang::FunctionProtoType*);
  Stream& operator<<(const clang::RecordType*);
  Stream& operator<<(const clang::VectorType*);
  Stream& operator<<(clang::QualType);

  Stream& operator<<(const clang::Stmt*);
  Stream& operator<<(const clang::DeclStmt*);
  Stream& operator<<(const clang::CompoundStmt*);
  Stream& operator<<(const clang::LabelStmt*);
  Stream& operator<<(const clang::GotoStmt*);
  Stream& operator<<(const clang::ReturnStmt*);
  Stream& operator<<(const clang::BinaryOperator*);
  Stream& operator<<(const clang::UnaryOperator*);
  Stream& operator<<(const clang::ConditionalOperator*);
  Stream& operator<<(const clang::CallExpr*);
  Stream& operator<<(const clang::CStyleCastExpr*);
  Stream& operator<<(const clang::ArraySubscriptExpr*);
  Stream& operator<<(const clang::ForStmt*);
  Stream& operator<<(const clang::WhileStmt*);
  Stream& operator<<(const clang::DoStmt*);
  Stream& operator<<(const clang::BreakStmt*);
  Stream& operator<<(const clang::ContinueStmt*);
  Stream& operator<<(const clang::IfStmt*);
  Stream& operator<<(const clang::SwitchStmt*);
  Stream& operator<<(const clang::CaseStmt*);
  Stream& operator<<(const clang::DefaultStmt*);

  Stream& operator<<(const clang::CXXBoolLiteralExpr*);
  Stream& operator<<(const clang::CharacterLiteral*);
  Stream& operator<<(const clang::IntegerLiteral*);
  Stream& operator<<(const clang::FloatingLiteral*);
  Stream& operator<<(const clang::StringLiteral*);
  Stream& operator<<(const clang::CXXNullPtrLiteralExpr*);
  Stream& operator<<(const clang::InitListExpr*);
  Stream& operator<<(const clang::DeclRefExpr*);

  Stream& operator<<(const clang::DeclaratorDecl*);
  Stream& operator<<(const clang::VarDecl*);
  Stream& operator<<(const clang::FunctionDecl*);
  Stream& operator<<(const clang::FieldDecl*);
  Stream& operator<<(const clang::RecordDecl*);
  Stream& operator<<(const clang::ParmVarDecl*);
};

} // namespace cish

#endif // CISH_STREAM_H
