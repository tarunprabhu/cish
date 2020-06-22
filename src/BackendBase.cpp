#include "BackendBase.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"
#include "Options.h"

using namespace clang;

namespace cish {

BackendBase::BackendBase(CishContext& context)
    : varPrefix(opts().prefix), varSuffix(0), context(context),
      astContext(context.getASTContext()) {
  ;
}

std::string BackendBase::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix << prefix << varSuffix;

  varSuffix++;

  return ss.str();
}

void BackendBase::beginFunction(FunctionDecl*) {
  varSuffix = 0;
  stmts.emplace();
}

void BackendBase::endFunction(FunctionDecl*) {
  varSuffix = 0;
  stmts.clear();
}

Stmt* BackendBase::add(Stmt* stmt) {
  stmts.top().push_back(stmt);
  return stmt;
}

Stmt& BackendBase::add(Stmt& stmt) {
  return *add(&stmt);
}

DeclRefExpr* BackendBase::createDeclRefExpr(ValueDecl* decl) {
  return DeclRefExpr::Create(astContext,
                             NestedNameSpecifierLoc(),
                             invLoc,
                             decl,
                             false,
                             invLoc,
                             decl->getType(),
                             VK_LValue,
                             decl);
}

DeclStmt* BackendBase::createDeclStmt(Decl* decl) {
  auto* group = new(astContext) DeclGroupRef(decl);

  return new(astContext) DeclStmt(*group, invLoc, invLoc);
}

LabelStmt* BackendBase::createLabelStmt(LabelDecl* label) {
  return new(astContext) LabelStmt(invLoc, label, nullptr);
}

LabelDecl* BackendBase::createLabelDecl(FunctionDecl* f,
                                        const std::string& name) {
  return LabelDecl::Create(astContext, f, invLoc, &astContext.Idents.get(name));
}

ParmVarDecl* BackendBase::createParam(const std::string& name,
                                      QualType type,
                                      FunctionDecl* func) {
  return ParmVarDecl::Create(astContext,
                             func,
                             invLoc,
                             invLoc,
                             &astContext.Idents.get(name),
                             type,
                             nullptr,
                             clang::SC_None,
                             nullptr);
}

FunctionDecl* BackendBase::createFunction(const std::string& name,
                                          QualType type) {
  FunctionDecl* f
      = FunctionDecl::Create(astContext,
                             astContext.getTranslationUnitDecl(),
                             invLoc,
                             invLoc,
                             DeclarationName(&astContext.Idents.get(name)),
                             type,
                             nullptr,
                             clang::SC_None);
  astContext.getTranslationUnitDecl()->addDecl(f);

  return f;
}

RecordDecl* BackendBase::createStruct(const std::string& name) {
  RecordDecl* decl = RecordDecl::Create(astContext,
                                        TTK_Struct,
                                        astContext.getTranslationUnitDecl(),
                                        invLoc,
                                        invLoc,
                                        &astContext.Idents.get(name));
  astContext.getTranslationUnitDecl()->addDecl(decl);
  return decl;
}

FieldDecl* BackendBase::createField(const std::string& name,
                                    QualType type,
                                    RecordDecl* strct) {
  FieldDecl* field = FieldDecl::Create(astContext,
                                       strct,
                                       invLoc,
                                       invLoc,
                                       &astContext.Idents.get(name),
                                       type,
                                       nullptr,
                                       nullptr,
                                       true,
                                       ICIS_NoInit);
  strct->addDecl(field);
  return field;
}

DeclRefExpr* BackendBase::createGlobalVariable(const std::string& name,
                                               QualType type) {
  TranslationUnitDecl* tu = astContext.getTranslationUnitDecl();
  VarDecl* var = VarDecl::Create(astContext,
                                 tu,
                                 invLoc,
                                 invLoc,
                                 &astContext.Idents.get(name),
                                 type,
                                 nullptr,
                                 SC_None);
  tu->addDecl(var);

  return createDeclRefExpr(var);
}

DeclRefExpr* BackendBase::createLocalVariable(const std::string& name,
                                              QualType type,
                                              FunctionDecl* func) {
  VarDecl* var = VarDecl::Create(astContext,
                                 func,
                                 invLoc,
                                 invLoc,
                                 &astContext.Idents.get(name),
                                 type,
                                 nullptr,
                                 SC_None);
  func->addDecl(var);

  return createDeclRefExpr(var);
}

DeclRefExpr* BackendBase::createVariable(const std::string& name,
                                         QualType type,
                                         DeclContext* parent) {
  VarDecl* var = VarDecl::Create(astContext,
                                 parent,
                                 invLoc,
                                 invLoc,
                                 &astContext.Idents.get(name),
                                 type,
                                 nullptr,
                                 SC_None);

  return createDeclRefExpr(var);
}

ConditionalOperator* BackendBase::createConditionalOperator(Expr* cond,
                                                            Expr* t,
                                                            Expr* f,
                                                            QualType type) {
  return new(astContext) ConditionalOperator(
      cond, invLoc, t, invLoc, f, type, VK_LValue, OK_Ordinary);
}

BinaryOperator* BackendBase::createBinaryOperator(Expr* lhs,
                                                  Expr* rhs,
                                                  BinaryOperator::Opcode opc,
                                                  QualType type) {
  return new(astContext) BinaryOperator(
      lhs, rhs, opc, type, VK_RValue, OK_Ordinary, invLoc, FPOptions());
}

UnaryOperator* BackendBase::createUnaryOperator(Expr* lhs,
                                                UnaryOperator::Opcode opc,
                                                QualType type) {
  return new(astContext)
      UnaryOperator(lhs, opc, type, VK_RValue, OK_Ordinary, invLoc, false);
}

ArraySubscriptExpr*
BackendBase::createArraySubscriptExpr(Expr* base, Expr* idx, QualType type) {
  return new(astContext)
      ArraySubscriptExpr(base, idx, type, VK_RValue, OK_Ordinary, invLoc);
}

CStyleCastExpr* BackendBase::createCastExpr(Expr* expr, QualType qty) {
  const Type* type = qty.getTypePtr();
  if(not typeSrcInfo.contains(type))
    typeSrcInfo[type] = astContext.CreateTypeSourceInfo(qty);
  return CStyleCastExpr::Create(astContext,
                                qty,
                                VK_RValue,
                                CastKind::CK_BitCast,
                                expr,
                                nullptr,
                                typeSrcInfo.at(type),
                                invLoc,
                                invLoc);
}

IfStmt* BackendBase::createIfStmt(Expr* cond, Stmt* thn, Stmt* els) {
  return IfStmt::Create(
      astContext, invLoc, false, nullptr, nullptr, cond, thn, invLoc, els);
}

GotoStmt* BackendBase::createGotoStmt(LabelDecl* label) {
  return new(astContext) GotoStmt(label, invLoc, invLoc);
}

CompoundStmt* BackendBase::createCompoundStmt(const Vector<Stmt*>& stmts) {
  return CompoundStmt::Create(astContext, makeArrayRef(stmts), invLoc, invLoc);
}

CompoundStmt* BackendBase::createCompoundStmt(Stmt* stmt) {
  return CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmt), invLoc, invLoc);
}

SwitchStmt* BackendBase::createSwitchStmt(Expr* cond) {
  return SwitchStmt::Create(astContext, nullptr, nullptr, cond);
}

CaseStmt* BackendBase::createCaseStmt(Expr* value) {
  return CaseStmt::Create(astContext, value, nullptr, invLoc, invLoc, invLoc);
}

DefaultStmt* BackendBase::createDefaultStmt(Stmt* body) {
  return new(astContext) DefaultStmt(invLoc, invLoc, body);
}

BreakStmt* BackendBase::createBreakStmt() {
  return new(astContext) BreakStmt(invLoc);
}

ContinueStmt* BackendBase::createContinueStmt() {
  return new(astContext) ContinueStmt(invLoc);
}

ReturnStmt* BackendBase::createReturnStmt(Expr* retExpr) {
  return ReturnStmt::Create(astContext, invLoc, retExpr, nullptr);
}

CallExpr* BackendBase::createCallExpr(Expr* callee,
                                      const Vector<Expr*>& args,
                                      QualType type) {
  return CallExpr::Create(
      astContext, callee, makeArrayRef(args), type, VK_RValue, invLoc);
}

DoStmt* BackendBase::createDoStmt(Stmt* body, Expr* cond) {
  return new(astContext) DoStmt(body, cond, invLoc, invLoc, invLoc);
}

WhileStmt* BackendBase::createWhileStmt(Expr* cond, Stmt* body) {
  return WhileStmt::Create(astContext, nullptr, cond, body, invLoc);
}

CXXBoolLiteralExpr* BackendBase::createBoolLiteral(bool b, QualType type) {
  return new(astContext) CXXBoolLiteralExpr(b, type, invLoc);
}

IntegerLiteral* BackendBase::createIntLiteral(const llvm::APInt& i,
                                              QualType type) {
  return IntegerLiteral::Create(astContext, i, type, invLoc);
}

IntegerLiteral* BackendBase::createIntLiteral(short i) {
  return createIntLiteral(llvm::APInt(16, i, true), astContext.ShortTy);
}

IntegerLiteral* BackendBase::createIntLiteral(int i) {
  return createIntLiteral(llvm::APInt(32, i, true), astContext.IntTy);
}

IntegerLiteral* BackendBase::createIntLiteral(long i) {
  return createIntLiteral(llvm::APInt(64, i, true), astContext.LongTy);
}

FloatingLiteral* BackendBase::createFloatLiteral(const llvm::APFloat& f,
                                                 QualType type) {
  return FloatingLiteral::Create(astContext, f, false, type, invLoc);
}

FloatingLiteral* BackendBase::createFloatLiteral(float f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.FloatTy);
}

FloatingLiteral* BackendBase::createFloatLiteral(double f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.DoubleTy);
}

FloatingLiteral* BackendBase::createFloatLiteral(long double f) {
  fatal(error() << "UNIMPLEMENTED: Float literal for long double\n");
}

CXXNullPtrLiteralExpr* BackendBase::createNullptr(QualType type) {
  return new(astContext) CXXNullPtrLiteralExpr(type, invLoc);
}

StringLiteral* BackendBase::createStringLiteral(llvm::StringRef str,
                                                QualType type) {
  return StringLiteral::Create(
      astContext, str, StringLiteral::Ascii, false, type, invLoc);
}

StringLiteral* BackendBase::createStringLiteral(const std::string& str,
                                                QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

StringLiteral* BackendBase::createStringLiteral(const char* str,
                                                QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

InitListExpr* BackendBase::createInitListExpr(const Vector<Expr*>& exprs) {
  return new(astContext)
      InitListExpr(astContext, invLoc, makeArrayRef(exprs), invLoc);
}

} // namespace cish
