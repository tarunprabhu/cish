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

#include "ASTStreamer.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "Operators.h"
#include "Options.h"

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

static bool highestPrecedence(const Expr* expr) {
  return isa<DeclRefExpr>(expr) or isa<InitListExpr>(expr)
         or isa<ArraySubscriptExpr>(expr) or isa<CallExpr>(expr)
         or isa<CStyleCastExpr>(expr) or isa<MemberExpr>(expr);
}

ASTStreamer::ASTStreamer(const clang::ASTContext& astContext)
    : astContext(astContext), os(buf), ilevel(0) {
  if(opts().indentOffset == 0)
    tabStr = "\t";
  else
    for(unsigned i = 0; i < opts().indentOffset; i++)
      tabStr.append(" ");
}

bool ASTStreamer::isVoidTy(const Type* type) const {
  return type == astContext.VoidTy.getTypePtr();
}

bool ASTStreamer::isBoolTy(const Type* type) const {
  return type == astContext.BoolTy.getTypePtr();
}

bool ASTStreamer::isCharTy(const Type* type) const {
  return type == astContext.CharTy.getTypePtr();
}

bool ASTStreamer::isShortTy(const Type* type) const {
  return type == astContext.ShortTy.getTypePtr();
}

bool ASTStreamer::isIntTy(const Type* type) const {
  return type == astContext.IntTy.getTypePtr();
}

bool ASTStreamer::isLongTy(const Type* type) const {
  return type == astContext.LongTy.getTypePtr();
}

bool ASTStreamer::isFloatTy(const Type* type) const {
  return type == astContext.FloatTy.getTypePtr();
}

bool ASTStreamer::isDoubleTy(const Type* type) const {
  return type == astContext.DoubleTy.getTypePtr();
}

bool ASTStreamer::isLongDoubleTy(const Type* type) const {
  return type == astContext.LongDoubleTy.getTypePtr();
}

ASTStreamer& ASTStreamer::parenthetize(const Expr* expr,
                                       const BinaryOperator::Opcode opc) {
  if(Clang::isLiteral(expr) or highestPrecedence(expr)) {
    *this << expr;
  } else if(opts().parens == Parens::Always) {
    *this << "(" << expr << ")";
  } else if(const auto* subOp = dyn_cast<BinaryOperator>(expr)) {
    BinaryOperator::Opcode op = subOp->getOpcode();
    if(opts().parens == Parens::Fuzzy) {
      if((op < opc) or Operator::isBitwise(op) or Operator::isBitwise(opc))
        *this << "(" << expr << ")";
      else
        *this << expr;
    } else if(opts().parens == Parens::Strict) {
      if(op < opc)
        *this << "(" << expr << ")";
      else
        *this << expr;
    } else {
      fatal(error() << "Unknown parenthetization mode");
    }
  } else if(isa<ConditionalOperator>(expr)) {
    *this << "(" << expr << ")";
  } else if(isa<UnaryOperator>(expr)) {
    *this << expr;
  } else {
    fatal(error() << "Unexpected expr to parenthetize: "
                  << expr->getStmtClassName());
  }
  return *this;
}

ASTStreamer& ASTStreamer::parenthetize(const Expr* expr,
                                       const UnaryOperator::Opcode) {
  if(Clang::isLiteral(expr) or highestPrecedence(expr)) {
    *this << expr;
  } else if(opts().parens == Parens::Always) {
    *this << "(" << expr << ")";
  } else if(isa<UnaryOperator>(expr) or isa<BinaryOperator>(expr)
            or isa<ConditionalOperator>(expr)) {
    *this << "(" << expr << ")";
  } else {
    fatal(error() << "Unexpected expr to parenthetize: "
                  << expr->getStmtClassName());
  }

  return *this;
}

ASTStreamer& ASTStreamer::parenthetize(const Expr* expr) {
  if(Clang::isLiteral(expr) or highestPrecedence(expr))
    *this << expr;
  else
    *this << "(" << expr << ")";
  return *this;
}

ASTStreamer& ASTStreamer::tab() {
  for(unsigned i = 0; i < ilevel; i++)
    *this << tabStr;
  return *this;
}

ASTStreamer& ASTStreamer::space(unsigned count) {
  for(unsigned i = 0; i < count; i++)
    *this << " ";
  return *this;
}

ASTStreamer& ASTStreamer::endl() {
  *this << "\n";
  return *this;
}

ASTStreamer& ASTStreamer::endst(bool newl) {
  *this << ";";
  if(newl)
    endl();
  return *this;
}

char ASTStreamer::back(unsigned skip) {
  auto isFlagSet
      = [](unsigned flags, unsigned flag) { return (flags & flag) == flag; };

  os.flush();
  bool skipIndent = isFlagSet(skip, ASTStreamer::SkipIndent);
  bool skipNewline = isFlagSet(skip, ASTStreamer::SkipNewline);
  if(not buf.size()) {
    return '\0';
  } else if(skip) {
    for(size_t i = buf.size(); i > 0; i--)
      if(not [&]() {
           switch(buf[i - 1]) {
           case ' ':
           case '\t':
             if(skipIndent)
               return true;
             break;
           case '\n':
             if(skipNewline)
               return true;
             break;
           }
           return false;
         }())
        return buf[i - 1];
    return '\0';
  } else {
    return buf.back();
  }
}

ASTStreamer& ASTStreamer::beginBlock() {
  switch(opts().indentStyle) {
  case IndentStyle::KR:
  case IndentStyle::Stroustrup:
    space();
    break;
  case IndentStyle::Allman:
    endl() << tab();
    break;
  }
  *this << "{" << endl();
  ilevel += 1;

  return *this;
}

ASTStreamer& ASTStreamer::endBlock(bool semicolon) {
  ilevel -= 1;
  tab() << "}";
  if(semicolon)
    endst(false);

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(char c) {
  os << c;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(int16_t i) {
  os << i;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(uint16_t i) {
  os << i;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(int32_t i) {
  os << i;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(uint32_t i) {
  os << i;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(int64_t i) {
  os << i;
  if(((i > 0) and (i >= std::numeric_limits<int32_t>::max()))
     or ((i < 0) and (i <= std::numeric_limits<int32_t>::min())))
    os << "L";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(uint64_t i) {
  os << i;
  if(i >= std::numeric_limits<int32_t>::max())
    os << "L";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(float f) {
  os << f;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(double g) {
  os << g;
  if(((g > 0) and (g >= std::numeric_limits<float>::max()))
     or ((g < 0) and (g <= std::numeric_limits<float>::min())))
    os << "L";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(long double g) {
  std::stringstream ss;
  ss << g;
  os << ss.str() << "L";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const char* c) {
  os << c;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(llvm::StringRef s) {
  os << s;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const std::string& s) {
  os << s;
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(ASTStreamer& ss) {
  if(&ss != this)
    fatal(
        error() << "INTERNAL ERROR: Cannot stream a different stream instance");

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const PointerType* pty) {
  *this << pty->getPointeeType();
  if(not isa<FunctionProtoType>(pty->getPointeeType()))
    *this << "*";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ConstantArrayType* aty) {
  *this << aty->getElementType().getTypePtr() << formatArrayDims(aty);
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const RecordType* sty) {
  *this << "struct " << sty->getDecl()->getName();
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const FunctionProtoType* fty) {
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

ASTStreamer& ASTStreamer::operator<<(const VectorType* vty) {
  *this << vty->getElementType().getTypePtr() << "<" << vty->getNumElements()
        << ">";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const Type* type) {
  if(isVoidTy(type))
    return *this << "void";
  else if(isBoolTy(type))
    return *this << "bool";
  else if(isCharTy(type))
    return *this << "char";
  else if(isShortTy(type))
    return *this << "short";
  else if(isIntTy(type))
    return *this << "int";
  else if(isLongTy(type))
    return *this << "long";
  else if(type == astContext.Int128Ty.getTypePtr())
    return *this << "__int128";
  else if(isFloatTy(type))
    return *this << "float";
  else if(isDoubleTy(type))
    return *this << "double";
  else if(isLongDoubleTy(type))
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
  else
    fatal(error() << "UNEXPECTED TYPE: " << type->getTypeClassName());

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(QualType qual) {
  const Type* type = qual.getTypePtr();
  const ConstantArrayType* aty = dyn_cast<ConstantArrayType>(type);
  if(aty)
    *this << Clang::getBaseType(aty);
  else
    *this << type;
  if(qual.isConstQualified())
    *this << " const";
  if(qual.isRestrictQualified())
    *this << " __restrict__";
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const DeclaratorDecl* decl) {
  if(isa<FieldDecl>(decl) or isa<VarDecl>(decl)) {
    QualType qual = decl->getType();
    *this << qual;
    if(decl->getName().size())
      *this << " " << decl->getName();
    if(const auto* aty = dyn_cast<ConstantArrayType>(qual.getTypePtr()))
      *this << formatArrayDims(aty);

    return *this;
  }

  fatal(error() << "Unsupported declarator: " << decl->getDeclKindName());
}

ASTStreamer& ASTStreamer::operator<<(const VarDecl* var) {
  *this << cast<DeclaratorDecl>(var);
  if(const Expr* init = var->getInit())
    *this << " = " << init;

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const FieldDecl* field) {
  return *this << cast<DeclaratorDecl>(field) << endst();
}

ASTStreamer& ASTStreamer::operator<<(const RecordDecl* record) {
  tab() << "struct " << record->getName() << beginBlock();
  for(const FieldDecl* decl : record->fields())
    tab() << decl;
  endBlock(false) << endst();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ParmVarDecl* param) {
  return *this << cast<DeclaratorDecl>(param);
}

ASTStreamer& ASTStreamer::operator<<(const FunctionDecl* f) {
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
    beginBlock();
    for(const Decl* d : f->decls())
      if(const auto* var = dyn_cast<VarDecl>(d))
        *this << tab() << var << endst();
    *this << endl();
    process(cast<CompoundStmt>(f->getBody()), false);
    endBlock(false);
  } else {
    endst(false);
  }

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const Decl* decl) {
  if(const auto* func = dyn_cast<FunctionDecl>(decl))
    return *this << func;
  else if(const auto* param = dyn_cast<ParmVarDecl>(decl))
    return *this << param;
  else if(const auto* record = dyn_cast<RecordDecl>(decl))
    return *this << record;
  else if(const auto* field = dyn_cast<FieldDecl>(decl))
    return *this << field;
  else if(const auto* var = dyn_cast<VarDecl>(decl))
    return *this << var;
  else
    fatal(error() << "Unsupported Decl: " << decl->getDeclKindName());

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const CXXBoolLiteralExpr* b) {
  if(b->getValue())
    return *this << "true";
  else
    return *this << "false";
}

ASTStreamer& ASTStreamer::operator<<(const CharacterLiteral* c) {
  *this << "'" << (char)c->getValue() << "'";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const IntegerLiteral* i) {
  llvm::APInt ival = i->getValue();
  if(ival.isNegative())
    *this << "-" << (int64_t)ival.abs().getLimitedValue();
  else
    *this << ival.getLimitedValue();
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const FloatingLiteral* f) {
  llvm::APFloat fval = f->getValue();
  const Type* type = f->getType().getTypePtr();
  if(isFloatTy(type)) {
    *this << fval.convertToFloat();
  } else if(isDoubleTy(type)) {
    *this << fval.convertToDouble();
  } else if(isLongDoubleTy(type)) {
    std::string buf;
    llvm::raw_string_ostream ss(buf);
    fval.print(ss);
    ss.flush();
    if(buf.back() == '\n')
      *this << buf.substr(0, buf.length() - 1);
    else
      *this << buf;
    *this << "L";
  } else {
    fatal(error() << "Unknown floating literal type: "
                  << type->getTypeClassName());
  }
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const StringLiteral* slit) {
  return *this << "\"" << slit->getString() << "\"";
}

ASTStreamer& ASTStreamer::operator<<(const CXXNullPtrLiteralExpr*) {
  return *this << "nullptr";
}

ASTStreamer& ASTStreamer::operator<<(const InitListExpr* list) {
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

ASTStreamer& ASTStreamer::operator<<(const DeclRefExpr* declRefExpr) {
  const Decl* decl = declRefExpr->getDecl();
  if(const auto* namedDecl = dyn_cast<NamedDecl>(decl))
    return *this << namedDecl->getName();

  fatal(error() << "Unknown decl in declRef: " << decl->getDeclKindName());
}

ASTStreamer& ASTStreamer::operator<<(const NullStmt*) {
  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const BinaryOperator* binOp) {
  const Expr* lhs = binOp->getLHS();
  const Expr* rhs = binOp->getRHS();
  switch(binOp->getOpcode()) {
  case BO_Assign:
  case BO_AddAssign:
  case BO_SubAssign:
  case BO_MulAssign:
  case BO_DivAssign:
  case BO_RemAssign:
  case BO_ShlAssign:
  case BO_ShrAssign:
  case BO_AndAssign:
  case BO_OrAssign:
  case BO_XorAssign:
    *this << lhs << " " << binOp->getOpcodeStr() << " " << rhs;
    break;
  case BO_PtrMemI:
  case BO_PtrMemD:
    *this << lhs << binOp->getOpcodeStr() << rhs;
    break;
  default:
    *this << parenthetize(lhs, binOp->getOpcode()) << " ";
    if(const auto* vty = dyn_cast<VectorType>(binOp->getType().getTypePtr())) {
      unsigned num = vty->getNumElements();
      for(unsigned i = 0; i < num / 2; i++)
        *this << "<";
      *this << binOp->getOpcodeStr();
      for(unsigned i = 0; i < num / 2; i++)
        *this << ">";
    } else {
      *this << binOp->getOpcodeStr();
    }
    *this << " " << parenthetize(rhs, binOp->getOpcode());
  }

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const UnaryOperator* unOp) {
  UnaryOperator::Opcode opc = unOp->getOpcode();
  *this << UnaryOperator::getOpcodeStr(opc)
        << parenthetize(unOp->getSubExpr(), opc);

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ConditionalOperator* condOp) {
  *this << parenthetize(condOp->getCond()) << " ? "
        << parenthetize(condOp->getTrueExpr()) << " : "
        << parenthetize(condOp->getFalseExpr());

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const CallExpr* callExpr) {
  *this << callExpr->getCallee() << "(";
  if(unsigned numArgs = callExpr->getNumArgs()) {
    *this << callExpr->getArg(0);
    for(unsigned i = 1; i < numArgs; i++)
      *this << ", " << callExpr->getArg(i);
  }
  *this << ")";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const MemberExpr* memberExpr) {
  *this << memberExpr->getBase()
        << (memberExpr->isArrow() ? "->" : ".")
        << memberExpr->getMemberDecl()->getName();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const CStyleCastExpr* castExpr) {
  const Type* type = castExpr->getType().getTypePtr();
  const Expr* expr = castExpr->getSubExpr();
  *this << "((" << type << ")" << expr << ")";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ArraySubscriptExpr* arrExpr) {
  *this << arrExpr->getBase() << "[" << arrExpr->getIdx() << "]";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const LabelStmt* labelStmt) {
  llvm::StringRef name = labelStmt->getName();
  if(name.size())
    endl() << labelStmt->getName() << ":";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const GotoStmt* gotoStmt) {
  *this << "goto " << gotoStmt->getLabel()->getName();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const BreakStmt*) {
  return *this << "break";
}

ASTStreamer& ASTStreamer::operator<<(const ContinueStmt*) {
  return *this << "continue";
}

ASTStreamer& ASTStreamer::operator<<(const CaseStmt* kase) {
  // The case and default statements was created with a compound statement.
  // Although it is correct to use it, it's cleaner not to.
  // The only reason to use it at all would be to support declaring variables
  // "near" their actual definition, but that's not done now
  *this << tab() << "case " << kase->getLHS() << ":" << endl();
  ilevel += 1;
  if(const auto* stmt = dyn_cast<CompoundStmt>(kase->getSubStmt()))
    process(stmt, false);
  else
    fatal(error() << "Expected compound statement in switch case: Got "
                  << kase->getSubStmt()->getStmtClassName());
  ilevel -= 1;

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const DefaultStmt* deflt) {
  // The case and default statements was created with a compound statement.
  // Although it is correct to use it, it's cleaner not to.
  // The only reason to use it at all would be to support declaring variables
  // "near" their actual definition, but that's not done now
  *this << tab() << "default:" << endl();
  ilevel += 1;
  if(const auto* stmt = dyn_cast<CompoundStmt>(deflt->getSubStmt()))
    process(stmt, false);
  else
    fatal(error() << "Expected compound statement in switch default: Got "
                  << deflt->getSubStmt()->getStmtClassName());
  ilevel -= 1;

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const SwitchStmt* sw) {
  static const Vector<char> sentinels = {';'};
  if(sentinels.contains(back(SkipIndent | SkipNewline)))
    *this << endl() << tab();

  *this << "switch(" << sw->getCond() << ")" << beginBlock();

  for(const clang::SwitchCase* i = sw->getSwitchCaseList(); i;
      i = i->getNextSwitchCase())
    if(const auto* kase = dyn_cast<CaseStmt>(i))
      *this << kase;

  for(const clang::SwitchCase* i = sw->getSwitchCaseList(); i;
      i = i->getNextSwitchCase())
    if(const auto* deflt = dyn_cast<DefaultStmt>(i))
      *this << deflt;

  *this << endBlock();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const IfStmt* ifStmt) {
  if(back(SkipIndent | SkipNewline) == ';')
    *this << endl() << tab();

  *this << "if (" << ifStmt->getCond() << ")" << ifStmt->getThen();
  if(const Stmt* els = ifStmt->getElse()) {
    switch(opts().indentStyle) {
    case IndentStyle::KR:
      *this << " else";
      break;
    case IndentStyle::Allman:
    case IndentStyle::Stroustrup:
      endl() << tab() << "else";
      break;
    }
    *this << els;
  }

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ReturnStmt* retStmt) {
  if(back(SkipIndent | SkipNewline) == ';')
    *this << endl() << tab();

  *this << "return";
  if(const Expr* val = retStmt->getRetValue())
    space() << val;

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const DeclStmt* declStmt) {
  bool comma = false;
  for(Decl* decl : declStmt->getDeclGroup()) {
    if(comma)
      *this << ", ";
    *this << decl;
    comma = true;
  }

  return *this;
}

ASTStreamer& ASTStreamer::process(const CompoundStmt* stmt, bool block) {
  if(block)
    beginBlock();

  for(Stmt* stmt : stmt->body()) {
    if(not(isa<LabelStmt>(stmt) or isa<NullStmt>(stmt))) {
      if(back(SkipIndent | SkipNewline) == '}')
        *this << endl();
      *this << tab();
    }
    *this << stmt;
    if(isa<IfStmt>(stmt) or isa<ForStmt>(stmt) or isa<WhileStmt>(stmt)
       or isa<SwitchStmt>(stmt) or isa<DeclStmt>(stmt)) {
      // Statements that should not have a semicolon at the end
      *this << endl();
    } else if(isa<NullStmt>(stmt)) {
      if(back(SkipIndent) != '\n')
        *this << endl();
    } else if(const auto* label = dyn_cast<LabelStmt>(stmt)) {
      // Labels definitely don't end with a semicolon
      if(label->getDecl()->getName().size())
        *this << endl();
    } else {
      *this << endst();
    }
  }

  if(block)
    endBlock();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const CompoundStmt* compoundStmt) {
  return process(compoundStmt, true);
}

ASTStreamer& ASTStreamer::operator<<(const DoStmt* doStmt) {
  if(back(SkipIndent | SkipNewline) == ';')
    *this << endl() << tab();

  *this << "do" << doStmt->getBody();
  switch(opts().indentStyle) {
  case IndentStyle::KR:
    *this << " while";
    break;
  case IndentStyle::Allman:
  case IndentStyle::Stroustrup:
    endl() << tab() << "while";
    break;
  }
  *this << space() << "(" << doStmt->getCond() << ")";

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const ForStmt* forStmt) {
  if(back(SkipIndent | SkipNewline) == ';')
    *this << endl() << tab();

  *this << "for(";
  if(const Stmt* init = forStmt->getInit())
    *this << init;
  *this << "; ";
  if(const Expr* cond = forStmt->getCond())
    *this << cond;
  *this << "; ";
  if(const Expr* inc = forStmt->getInc())
    *this << inc;
  *this << ")";
  *this << forStmt->getBody();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const WhileStmt* whileStmt) {
  if(back(SkipIndent | SkipNewline) == ';')
    *this << endl() << tab();

  *this << "while (" << whileStmt->getCond() << ")" << whileStmt->getBody();

  return *this;
}

ASTStreamer& ASTStreamer::operator<<(const Stmt* stmt) {
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
  else if(const auto* nullStmt = dyn_cast<NullStmt>(stmt))
    *this << nullStmt;
  else if(const auto* declStmt = dyn_cast<DeclStmt>(stmt))
    *this << declStmt;
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
  else if(const auto* doStmt = dyn_cast<DoStmt>(stmt))
    *this << doStmt;
  else if(const auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    *this << whileStmt;
  else if(const auto* forStmt = dyn_cast<ForStmt>(stmt))
    *this << forStmt;
  else if(const auto* swStmt = dyn_cast<SwitchStmt>(stmt))
    *this << swStmt;
  else if(const auto* caseStmt = dyn_cast<CaseStmt>(stmt))
    *this << caseStmt;
  else if(const auto* defltStmt = dyn_cast<DefaultStmt>(stmt))
    *this << defltStmt;
  else if(const auto* breakStmt = dyn_cast<BreakStmt>(stmt))
    *this << breakStmt;
  else if(const auto* continueStmt = dyn_cast<ContinueStmt>(stmt))
    *this << continueStmt;
  else if(const auto* binOp = dyn_cast<BinaryOperator>(stmt))
    *this << binOp;
  else if(const auto* unOp = dyn_cast<UnaryOperator>(stmt))
    *this << unOp;
  else if(const auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    *this << condOp;
  else if(const auto* callExpr = dyn_cast<CallExpr>(stmt))
    *this << callExpr;
  else if(const auto* memberExpr = dyn_cast<MemberExpr>(stmt))
    *this << memberExpr;
  else if(const auto* castExpr = dyn_cast<CStyleCastExpr>(stmt))
    *this << castExpr;
  else if(const auto* arrExpr = dyn_cast<ArraySubscriptExpr>(stmt))
    *this << arrExpr;
  else
    fatal(error() << "UNKNOWN STMT: " << stmt->getStmtClassName());
  return *this;
}

const std::string& ASTStreamer::str() {
  os.flush();
  return buf;
}

} // namespace cish
