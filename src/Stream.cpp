#include "Stream.h"
#include "ClangUtils.h"

#include <llvm/Support/WithColor.h>

#include <sstream>

using namespace clang;

namespace cish {

static void formatArrayDims(const ConstantArrayType* aty,
                            llvm::raw_ostream& ss) {
  ss << "[" << aty->getSize().getLimitedValue() << "]";
  if(const auto* bty = dyn_cast<ConstantArrayType>(aty->getElementType()))
    formatArrayDims(bty, ss);
}

static std::string formatArrayDims(const ConstantArrayType* aty) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  formatArrayDims(aty, ss);

  return ss.str();
}

Stream::Stream(const clang::ASTContext& astContext,
               const FormatOptions& fmtOpts,
               llvm::raw_ostream& os)
    : astContext(astContext), fmtOpts(fmtOpts), os(os), ilevel(0) {
  if(fmtOpts.offset == 0)
    tabStr = "\t";
  else
    for(unsigned i = 0; i < fmtOpts.offset; i++)
      tabStr.append(" ");
}

bool Stream::shouldPrintCast(const Type* type) const {
  const Set<IgnoreCasts>& ignore = fmtOpts.ignoreCasts;
  if(ignore.contains(IgnoreCasts::All)) {
    return false;
  } else if(ignore.contains(IgnoreCasts::None)) {
    return true;
  } else if(const auto* pty = dyn_cast<PointerType>(type)) {
    if(isa<FunctionProtoType>(pty->getPointeeType()))
      return not ignore.contains(IgnoreCasts::Function);
  } else if(isa<VectorType>(type)) {
    return not ignore.contains(IgnoreCasts::Vector);
  }
  return true;
}

Stream& Stream::parenthetize(const Stmt* stmt) {
  if(fmtOpts.parens == Parens::Smart) {
    if(isa<BinaryOperator>(stmt) or isa<ConditionalOperator>(stmt))
      *this << "(" << stmt << ")";
    else
      *this << stmt;
  } else {
    *this << "(" << stmt << ")";
  }
  return *this;
}

Stream& Stream::tab() {
  for(unsigned i = 0; i < ilevel; i++)
    *this << tabStr;
  return *this;
}

Stream& Stream::space(unsigned count) {
  for(unsigned i = 0; i < count; i++)
    *this << " ";
  return *this;
}

Stream& Stream::endl() {
  *this << "\n";
  return *this;
}

Stream& Stream::endst(bool newl) {
  *this << ";";
  if(newl)
    endl();
  return *this;
}

Stream& Stream::beginBlock(const std::string& label) {
  switch(fmtOpts.indentation) {
  case Indentation::KR:
  case Indentation::Stroustrup:
    space();
    break;
  case Indentation::Allman:
    endl() << tab();
    break;
  }
  *this << "{" << endl();
  ilevel += 1;

  return *this;
}

Stream& Stream::endBlock(bool semicolon) {
  *this << "}";
  if(semicolon)
    endst(false);
  ilevel -= 1;

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
  os << i << "L";
  return *this;
}

Stream& Stream::operator<<(uint64_t i) {
  os << i;
  return *this;
}

Stream& Stream::operator<<(float f) {
  os << f;
  return *this;
}

Stream& Stream::operator<<(double g) {
  os << g << "L";
  return *this;
}

Stream& Stream::operator<<(long double g) {
  std::stringstream ss;
  ss << g;
  os << ss.str() << "L";
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
  if(&ss != this) {
    llvm::WithColor::error(llvm::errs());
    llvm::WithColor(llvm::errs())
        << "INTERNAL ERROR: Cannot stream a different stream instance\n";
    exit(1);
  }

  return *this;
}

Stream& Stream::operator<<(const PointerType* pty) {
  *this << pty->getPointeeType();
  if(not isa<FunctionProtoType>(pty->getPointeeType()))
    *this << "*";
  return *this;
}

Stream& Stream::operator<<(const ConstantArrayType* aty) {
  *this << aty->getElementType().getTypePtr() << formatArrayDims(aty);
  return *this;
}

Stream& Stream::operator<<(const RecordType* sty) {
  *this << "struct " << sty->getDecl()->getName();
  return *this;
}

Stream& Stream::operator<<(const FunctionProtoType* fty) {
  ArrayRef<QualType> params = fty->getParamTypes();
  *this << fty->getReturnType() << "(*)(";
  if(params.size()) {
    *this << params[0];
    for(unsigned i = 1; i < params.size(); i++)
      *this << ", " << params[i];
  }
  *this << ")";

  return *this;
}

Stream& Stream::operator<<(const VectorType* vty) {
  *this << vty->getElementType().getTypePtr() << "<" << vty->getNumElements()
        << ">";
  return *this;
}

Stream& Stream::operator<<(const Type* type) {
  if(type == astContext.VoidTy.getTypePtr())
    return *this << "void";
  else if(type == astContext.BoolTy.getTypePtr())
    return *this << "bool";
  else if(type == astContext.CharTy.getTypePtr())
    return *this << "char";
  else if(type == astContext.ShortTy.getTypePtr())
    return *this << "short";
  else if(type == astContext.IntTy.getTypePtr())
    return *this << "int";
  else if(type == astContext.LongTy.getTypePtr())
    return *this << "long";
  else if(type == astContext.Int128Ty.getTypePtr())
    return *this << "__int128";
  else if(type == astContext.FloatTy.getTypePtr())
    return *this << "float";
  else if(type == astContext.DoubleTy.getTypePtr())
    return *this << "double";
  else if(type == astContext.LongDoubleTy.getTypePtr())
    return *this << "long double";
  else if(auto* pty = dyn_cast<PointerType>(type))
    *this << pty;
  else if(auto* aty = dyn_cast<ConstantArrayType>(type))
    *this << aty;
  else if(auto* sty = dyn_cast<RecordType>(type))
    *this << sty;
  else if(auto* fty = dyn_cast<FunctionProtoType>(type))
    *this << fty;
  else if(auto* vty = dyn_cast<VectorType>(type))
    *this << vty;
  else {
    llvm::WithColor::error(llvm::errs()) << "UNEXPECTED TYPE: ";
    type->dump(llvm::errs());
    llvm::errs() << "\n";
    exit(1);
  }

  return *this;
}

Stream& Stream::operator<<(QualType qual) {
  const Type* type = qual.getTypePtr();
  const ConstantArrayType* aty = dyn_cast<ConstantArrayType>(type);
  if(aty)
    *this << getBaseType(aty);
  else
    *this << type;
  if(qual.isConstQualified())
    *this << " const";
  if(qual.isRestrictQualified())
    *this << " __restrict__";
  return *this;
}

Stream& Stream::operator<<(const CXXBoolLiteralExpr* b) {
  if(b->getValue())
    return *this << "true";
  else
    return *this << "false";
}

Stream& Stream::operator<<(const CharacterLiteral* c) {
  llvm::WithColor::warning(llvm::errs()) << "UNIMPLEMENTED: CharacterLiteral\n";
  return *this;
}

Stream& Stream::operator<<(const IntegerLiteral* i) {
  llvm::APInt ival = i->getValue();
  if(ival.isNegative())
    *this << "-";
  *this << ival.abs().getLimitedValue();
  if(ival.getBitWidth() >= 64)
    *this << "L";
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
    *this << buf.substr(0, buf.length() - 1);
  else
    *this << buf;
  if(f->getType().getTypePtr() == astContext.LongDoubleTy.getTypePtr())
    *this << "L";
  return *this;
}

Stream& Stream::operator<<(const StringLiteral* slit) {
  return *this << "\"" << slit->getString() << "\"";
}

Stream& Stream::operator<<(const CXXNullPtrLiteralExpr* cnull) {
  return *this << "nullptr";
}

Stream& Stream::operator<<(const InitListExpr* list) {
  *this << "{";
  bool comma = false;
  for(const Expr* expr : list->inits()) {
    if(comma)
      *this << ", ";
    *this << expr;
    comma = true;
  }
  *this << "}";

  return *this;
}

Stream& Stream::operator<<(const Stmt* stmt) {
  if(const auto* boolLit = dyn_cast<CXXBoolLiteralExpr>(stmt))
    *this << boolLit;
  else if(const auto* charLit = dyn_cast<CharacterLiteral>(stmt))
    *this << charLit;
  else if(const auto* intLit = dyn_cast<IntegerLiteral>(stmt))
    *this << intLit;
  else if(const auto* fpLit = dyn_cast<FloatingLiteral>(stmt))
    *this << fpLit;
  else if(const auto* sLit = dyn_cast<StringLiteral>(stmt))
    *this << sLit;
  else if(const auto* cnull = dyn_cast<CXXNullPtrLiteralExpr>(stmt))
    *this << cnull;
  else if(const auto* initList = dyn_cast<InitListExpr>(stmt))
    *this << initList;
  else if(const auto* declRefExpr = dyn_cast<DeclRefExpr>(stmt))
    *this << declRefExpr;
  else if(const auto* compoundStmt = dyn_cast<CompoundStmt>(stmt))
    *this << compoundStmt;
  else if(const auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    *this << retStmt;
  else if(const auto* ifStmt = dyn_cast<IfStmt>(stmt))
    *this << ifStmt;
  else if(const auto* labelStmt = dyn_cast<LabelStmt>(stmt))
    *this << labelStmt;
  else if(const auto* gotoStmt = dyn_cast<GotoStmt>(stmt))
    *this << gotoStmt;
  else if(const auto* binOp = dyn_cast<BinaryOperator>(stmt))
    *this << binOp;
  else if(const auto* unOp = dyn_cast<UnaryOperator>(stmt))
    *this << unOp;
  else if(const auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    *this << condOp;
  else if(const auto* callExpr = dyn_cast<CallExpr>(stmt))
    *this << callExpr;
  else if(const auto* castExpr = dyn_cast<CStyleCastExpr>(stmt))
    *this << castExpr;
  else
    llvm::WithColor::error(llvm::errs())
        << "UNKNOWN STMT: " << stmt->getStmtClassName() << "\n";
  return *this;
}

Stream& Stream::operator<<(const VarDecl* var) {
  *this << cast<DeclaratorDecl>(var);
  if(const Expr* init = var->getInit())
    *this << " = " << init;
  *this << endst();

  return *this;
}

Stream& Stream::operator<<(const DeclaratorDecl* decl) {
  if(isa<FieldDecl>(decl) or isa<VarDecl>(decl)) {
    QualType qual = decl->getType();
    *this << qual;
    if(decl->getName().size())
      *this << " " << decl->getName();
    if(const auto* aty = dyn_cast<ConstantArrayType>(qual.getTypePtr()))
      *this << formatArrayDims(aty);

    return *this;
  }

  llvm::WithColor::error(llvm::errs())
      << "Unsupported declarator: " << decl->getDeclKindName() << "\n";
  exit(1);
}

Stream& Stream::operator<<(const ParmVarDecl* param) {
  return *this << cast<DeclaratorDecl>(param);
}

Stream& Stream::operator<<(const FieldDecl* field) {
  return *this << cast<DeclaratorDecl>(field) << endst();
}

Stream& Stream::operator<<(const FunctionDecl* f) {
  ArrayRef<const ParmVarDecl*> params = f->parameters();

  *this << f->getReturnType() << " " << f->getName() << "(";
  if(params.size()) {
    *this << params[0];
    for(unsigned i = 1; i < params.size(); i++)
      *this << ", " << params[i];
  }
  if(f->isVariadic()) {
    if(params.size())
      *this << ", ";
    *this << "...";
  }
  *this << ")";

  if(f->hasBody()) {
    *this << f->getBody();
  } else {
    endst(false);
  }

  return *this;
}

Stream& Stream::operator<<(const RecordDecl* record) {
  tab() << "struct " << record->getName() << beginBlock();
  for(const FieldDecl* decl : record->fields())
    tab() << decl;
  endBlock(false) << endst();

  return *this;
}

Stream& Stream::operator<<(const DeclRefExpr* declRefExpr) {
  const Decl* decl = declRefExpr->getFoundDecl();
  if(const auto* namedDecl = dyn_cast<NamedDecl>(decl))
    return *this << namedDecl->getName();

  llvm::WithColor::error(llvm::errs())
      << "Unknown decl in declRef: " << decl->getDeclKindName() << "\n";
  exit(1);
}

Stream& Stream::operator<<(UnaryOperator::Opcode opc) {
  switch(opc) {
  case UO_Minus:
    return *this << "-";
  case UO_Not:
    return *this << "~";
  case UO_AddrOf:
    return *this << "&";
  case UO_Deref:
    return *this << "*";
  default:
    return *this << "<<UNKNOWN UNARY OPERATOR>>";
  }
}

Stream& Stream::operator<<(BinaryOperator::Opcode opc) {
  switch(opc) {
  case BO_Add:
    return *this << "+";
  case BO_Sub:
    return *this << "-";
  case BO_Mul:
    return *this << "*";
  case BO_Div:
    return *this << "/";
  case BO_Rem:
    return *this << "%";
  case BO_Shl:
    return *this << "<<";
  case BO_Shr:
    return *this << ">>";
  case BO_And:
    return *this << "&";
  case BO_Or:
    return *this << "|";
  case BO_Xor:
    return *this << "^";
  case BO_EQ:
    return *this << "==";
  case BO_NE:
    return *this << "!=";
  case BO_GT:
    return *this << ">";
  case BO_GE:
    return *this << ">=";
  case BO_LT:
    return *this << "<";
  case BO_LE:
    return *this << "<=";
  case BO_Assign:
    return *this << "=";
  case BO_PtrMemD:
    return *this << ".";
  case BO_PtrMemI:
    return *this << "->";
  default:
    return *this << "<<UNKNOWN BINARY OPERATOR>>";
  }
}

Stream& Stream::operator<<(const BinaryOperator* binOp) {
  return *this << parenthetize(binOp->getLHS()) << " " << binOp->getOpcode()
               << " " << parenthetize(binOp->getRHS());
}

Stream& Stream::operator<<(const UnaryOperator* unOp) {
  return *this << unOp->getOpcode() << parenthetize(unOp->getSubExpr());
}

Stream& Stream::operator<<(const ConditionalOperator* condOp) {
  return *this << parenthetize(condOp->getCond()) << " ? "
               << parenthetize(condOp->getTrueExpr()) << " : "
               << parenthetize(condOp->getFalseExpr());
}

Stream& Stream::operator<<(const CallExpr* callExpr) {
  *this << callExpr->getCallee() << "(";
  if(unsigned numArgs = callExpr->getNumArgs()) {
    *this << callExpr->getArg(0);
    for(unsigned i = 1; i < numArgs; i++)
      *this << ", " << callExpr->getArg(i);
  }
  *this << ")";

  return *this;
}

Stream& Stream::operator<<(const CStyleCastExpr* castExpr) {
  const Type* type = castExpr->getType().getTypePtr();
  const Expr* expr = castExpr->getSubExpr();
  if(shouldPrintCast(type))
    *this << "((" << type << ")" << expr << ")";
  else
    *this << expr;
  return *this;
}

Stream& Stream::operator<<(const LabelStmt* labelStmt) {
  llvm::StringRef name = labelStmt->getName();
  if(name.size())
    *this << labelStmt->getName() << ":" << endl();

  return *this;
}

Stream& Stream::operator<<(const GotoStmt* gotoStmt) {
  tab() << "goto " << gotoStmt->getLabel()->getName() << endst();

  return *this;
}

Stream& Stream::operator<<(const IfStmt* ifStmt) {
  *this << "if (" << ifStmt->getCond() << ")" << beginBlock()
        << ifStmt->getThen() << endBlock();
  if(const Stmt* then = ifStmt->getElse()) {
    switch(fmtOpts.indentation) {
    case Indentation::KR:
      *this << " else";
      break;
    case Indentation::Allman:
    case Indentation::Stroustrup:
      *this << endl() << tab() << "else";
      break;
    }
    *this << beginBlock() << then << endBlock();
  }

  return *this;
}

Stream& Stream::operator<<(const ReturnStmt* retStmt) {
  *this << "return";
  if(const Expr* val = retStmt->getRetValue())
    space() << val;

  return *this;
}

Stream& Stream::operator<<(const CompoundStmt* compoundStmt) {
  beginBlock();
  for(Stmt* stmt : compoundStmt->body()) {
    if(not isa<LabelStmt>(stmt))
      tab();
    *this << stmt;
    if(not(isa<IfStmt>(stmt) or isa<ForStmt>(stmt) or isa<WhileStmt>(stmt)
           or isa<LabelStmt>(stmt)))
      *this << endst();
  }
  endBlock();

  return *this;
}

Stream& Stream::operator<<(const ForStmt* forStmt) {
  llvm::WithColor::error(llvm::errs())
      << "NOT IMPLEMENTED: " << forStmt->getStmtClassName() << "\n";
  return *this;
}

Stream& Stream::operator<<(const WhileStmt* whileStmt) {
  *this << "while (" << whileStmt->getCond() << ")" << beginBlock()
        << whileStmt->getBody() << endBlock();

  return *this;
}

} // namespace cish
