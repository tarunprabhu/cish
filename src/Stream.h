#ifndef CISH_STREAM_H
#define CISH_STREAM_H

#include "FormatOptions.h"

#include <clang/AST/ASTContext.h>
#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

namespace cish {

class Stream {
protected:
  clang::ASTContext& astContext;
  llvm::raw_ostream& os;
  const FormatOptions& fmtOpts;

  std::string tabStr;
  unsigned ilevel;

public:
  Stream(llvm::raw_ostream& os,
         clang::ASTContext& astContext,
         const FormatOptions& fmtOpts);

  Stream& tab();
  Stream& endl();

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

  Stream& operator<<(const clang::Type*);
  Stream& operator<<(const clang::PointerType*);
  Stream& operator<<(const clang::ConstantArrayType*);
  Stream& operator<<(const clang::RecordType*);
  Stream& operator<<(clang::QualType);

  Stream& operator<<(const clang::CharacterLiteral*);
  Stream& operator<<(const clang::IntegerLiteral*);
  Stream& operator<<(const clang::FloatingLiteral*);
  Stream& operator<<(const clang::Expr*);
  Stream& operator<<(const clang::VarDecl*);
};

} // namespace cish

#endif // CISH_STREAM_H
