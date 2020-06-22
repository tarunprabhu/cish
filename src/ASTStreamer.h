#ifndef CISH_AST_STREAMER_H
#define CISH_AST_STREAMER_H

#include <clang/AST/ASTContext.h>
#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

namespace cish {

class ASTStreamer {
protected:
  const clang::ASTContext& astContext;
  llvm::raw_ostream& os;

  std::string tabStr;
  unsigned ilevel;

protected:
  ASTStreamer& parenthetize(const clang::Stmt*);

  bool isVoidTy(const clang::Type*) const;
  bool isBoolTy(const clang::Type*) const;
  bool isCharTy(const clang::Type*) const;
  bool isShortTy(const clang::Type*) const;
  bool isIntTy(const clang::Type*) const;
  bool isLongTy(const clang::Type*) const;
  bool isFloatTy(const clang::Type*) const;
  bool isDoubleTy(const clang::Type*) const;
  bool isLongDoubleTy(const clang::Type*) const;

  ASTStreamer& process(const clang::CompoundStmt* stmt, bool block);

  ASTStreamer& tab();
  ASTStreamer& space(unsigned spaces = 1);
  ASTStreamer& endst(bool newline = true);

  // The optional label is used if the block represents a basic block and a
  // label is added before the block
  ASTStreamer& beginBlock(const std::string& label = "");

  // endst will be true when ending the body of a struct because a semicolon
  // must be added immediately after the block in that case
  ASTStreamer& endBlock(bool endst = false);

  ASTStreamer& operator<<(char);
  ASTStreamer& operator<<(int16_t);
  ASTStreamer& operator<<(uint16_t);
  ASTStreamer& operator<<(int32_t);
  ASTStreamer& operator<<(uint32_t);
  ASTStreamer& operator<<(int64_t);
  ASTStreamer& operator<<(uint64_t);
  ASTStreamer& operator<<(float);
  ASTStreamer& operator<<(double);
  ASTStreamer& operator<<(long double);
  ASTStreamer& operator<<(const char*);
  ASTStreamer& operator<<(llvm::StringRef);
  ASTStreamer& operator<<(const std::string&);

  ASTStreamer& operator<<(clang::BinaryOperator::Opcode);
  ASTStreamer& operator<<(clang::UnaryOperator::Opcode);

  ASTStreamer& operator<<(const clang::Type*);
  ASTStreamer& operator<<(const clang::PointerType*);
  ASTStreamer& operator<<(const clang::ConstantArrayType*);
  ASTStreamer& operator<<(const clang::FunctionProtoType*);
  ASTStreamer& operator<<(const clang::RecordType*);
  ASTStreamer& operator<<(const clang::VectorType*);
  ASTStreamer& operator<<(clang::QualType);

  ASTStreamer& operator<<(const clang::Stmt*);
  ASTStreamer& operator<<(const clang::DeclStmt*);
  ASTStreamer& operator<<(const clang::CompoundStmt*);
  ASTStreamer& operator<<(const clang::LabelStmt*);
  ASTStreamer& operator<<(const clang::GotoStmt*);
  ASTStreamer& operator<<(const clang::ReturnStmt*);
  ASTStreamer& operator<<(const clang::BinaryOperator*);
  ASTStreamer& operator<<(const clang::UnaryOperator*);
  ASTStreamer& operator<<(const clang::ConditionalOperator*);
  ASTStreamer& operator<<(const clang::CallExpr*);
  ASTStreamer& operator<<(const clang::CStyleCastExpr*);
  ASTStreamer& operator<<(const clang::ArraySubscriptExpr*);
  ASTStreamer& operator<<(const clang::ForStmt*);
  ASTStreamer& operator<<(const clang::WhileStmt*);
  ASTStreamer& operator<<(const clang::DoStmt*);
  ASTStreamer& operator<<(const clang::BreakStmt*);
  ASTStreamer& operator<<(const clang::ContinueStmt*);
  ASTStreamer& operator<<(const clang::IfStmt*);
  ASTStreamer& operator<<(const clang::SwitchStmt*);
  ASTStreamer& operator<<(const clang::CaseStmt*);
  ASTStreamer& operator<<(const clang::DefaultStmt*);

  ASTStreamer& operator<<(const clang::CXXBoolLiteralExpr*);
  ASTStreamer& operator<<(const clang::CharacterLiteral*);
  ASTStreamer& operator<<(const clang::IntegerLiteral*);
  ASTStreamer& operator<<(const clang::FloatingLiteral*);
  ASTStreamer& operator<<(const clang::StringLiteral*);
  ASTStreamer& operator<<(const clang::CXXNullPtrLiteralExpr*);
  ASTStreamer& operator<<(const clang::InitListExpr*);
  ASTStreamer& operator<<(const clang::DeclRefExpr*);

  ASTStreamer& operator<<(const clang::DeclaratorDecl*);
  ASTStreamer& operator<<(const clang::FieldDecl*);
  ASTStreamer& operator<<(const clang::ParmVarDecl*);

public:
  ASTStreamer(const clang::ASTContext& astContext, llvm::raw_ostream& os);
  ASTStreamer(const ASTStreamer&) = delete;
  ASTStreamer(ASTStreamer&&) = delete;

  ASTStreamer& operator<<(ASTStreamer&);

  ASTStreamer& endl();

  ASTStreamer& operator<<(const clang::VarDecl*);
  ASTStreamer& operator<<(const clang::FunctionDecl*);
  ASTStreamer& operator<<(const clang::RecordDecl*);
};

} // namespace cish

#endif // CISH_ASTSTREAMER_H
