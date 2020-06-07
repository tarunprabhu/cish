#include "Stream.h"

#include <llvm/Support/WithColor.h>

using namespace clang;

namespace cish {

Stream::Stream(llvm::raw_ostream& os,
               clang::ASTContext& astContext,
               const FormatOptions& fmtOpts)
    : astContext(astContext), os(os), fmtOpts(fmtOpts), ilevel(0) {
  if(fmtOpts.offset == 0)
    tabStr = "\t";
  else
    for(unsigned i = 0; i < fmtOpts.offset; i++)
      tabStr.append(" ");
}

Stream& Stream::tab() {
  for(unsigned i = 0; i < ilevel; i++)
    os << tabStr;
  return *this;
}

Stream& Stream::endl() {
  os << "\n";
  return *this;
}

Stream& Stream::operator<<(char c) {
  os << c;
  return *this;
}

Stream& Stream::operator<<(int16_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(uint16_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(int32_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(uint32_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(int64_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(uint64_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(float i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(double i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(const char* c) {
  os << c;
  return *this;
}

Stream& Stream::operator<<(llvm::StringRef s) {
  os << s;
  return *this;
}

Stream& Stream::operator<<(const std::string& s) {
  os << s;
  return *this;
}

Stream& Stream::operator<<(Stream& ss) {
  return *this;
}

Stream& Stream::operator<<(const PointerType* pty) {
  *this << pty->getPointeeType() << "*";
  return *this;
}

Stream& Stream::operator<<(const ConstantArrayType* aty) {
  *this << aty->getElementType().getTypePtr() << "["
        << aty->getSize().getLimitedValue() << "]";
  return *this;
}

Stream& Stream::operator<<(const RecordType* sty) {
  *this << "struct " << sty->getDecl()->getName();
  return *this;
}

Stream& Stream::operator<<(const Type* type) {
  if(type == astContext.VoidTy.getTypePtr())
    os << "void";
  else if(type == astContext.BoolTy.getTypePtr())
    os << "bool";
  else if(type == astContext.CharTy.getTypePtr())
    os << "char";
  else if(type == astContext.ShortTy.getTypePtr())
    os << "short";
  else if(type == astContext.IntTy.getTypePtr())
    os << "int";
  else if(type == astContext.LongTy.getTypePtr())
    os << "long";
  else if(type == astContext.Int128Ty.getTypePtr())
    os << "__int128";
  else if(type == astContext.FloatTy.getTypePtr())
    os << "float";
  else if(type == astContext.DoubleTy.getTypePtr())
    os << "double";
  else if(type == astContext.LongDoubleTy.getTypePtr())
    os << "long double";
  else if(auto* pty = dyn_cast<PointerType>(type))
    *this << pty;
  else if(auto* aty = dyn_cast<ConstantArrayType>(type))
    *this << aty;
  else if(auto* sty = dyn_cast<RecordType>(type))
    *this << sty;
  else {
    llvm::WithColor::error(llvm::errs()) << "UNEXPECTED TYPE\n";
    exit(1);
  }

  return *this;
}

Stream& Stream::operator<<(QualType qual) {
  return *this << qual.getTypePtr();
}

Stream& Stream::operator<<(const CharacterLiteral* c) {
  llvm::WithColor::warning(llvm::errs()) << "UNIMPLEMENTED: CharacterLiteral\n";
  return *this;
}

Stream& Stream::operator<<(const IntegerLiteral* i) {
  llvm::APInt ival = i->getValue();
  if(ival.isNegative())
    os << "-";
  os << ival.abs().getLimitedValue();
  return *this;
}

Stream& Stream::operator<<(const FloatingLiteral* f) {
  // Printing from an APFloat adds a newline at the end. Obviously, we don't
  // want that
  llvm::APFloat fval = f->getValue();
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  fval.print(ss);
  ss.flush();
  if(buf.back() == '\n')
    os << buf.substr(0, buf.length() - 1);
  else
    os << buf;
  return *this;
}

Stream& Stream::operator<<(const Expr* expr) {
  if(const auto* charLit = dyn_cast<CharacterLiteral>(expr))
    *this << charLit;
  else if(auto* intLit = dyn_cast<IntegerLiteral>(expr))
    *this << intLit;
  else if(auto* fpLit = dyn_cast<FloatingLiteral>(expr))
    *this << fpLit;
  else
    llvm::WithColor::error(llvm::errs())
        << "UNKNOWN EXPR: " << expr->getStmtClassName() << "\n";
  return *this;
}

Stream& Stream::operator<<(const VarDecl* var) {
  tab() << var->getType() << " " << var->getName();
  if(const Expr* init = var->getInit())
    *this << " = " << init;
  *this << ";" << endl();

  return *this;
}

} // namespace cish
