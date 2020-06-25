#include "ASTBuilder.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"

using namespace clang;

namespace cish {

ASTBuilder::ASTBuilder(ASTContext& astContext) : astContext(astContext) {
  ;
}

DeclRefExpr* ASTBuilder::createDeclRefExpr(ValueDecl* decl) {
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

DeclStmt* ASTBuilder::createDeclStmt(Decl* decl) {
  auto* group = new(astContext) DeclGroupRef(decl);

  return new(astContext) DeclStmt(*group, invLoc, invLoc);
}

LabelStmt* ASTBuilder::createLabelStmt(LabelDecl* label) {
  return new(astContext) LabelStmt(invLoc, label, nullptr);
}

LabelDecl* ASTBuilder::createLabelDecl(FunctionDecl* f,
                                       const std::string& name) {
  return LabelDecl::Create(astContext, f, invLoc, &astContext.Idents.get(name));
}

ParmVarDecl* ASTBuilder::createParam(const std::string& name,
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

FunctionDecl* ASTBuilder::createFunction(const std::string& name,
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

RecordDecl* ASTBuilder::createStruct(const std::string& name) {
  RecordDecl* decl = RecordDecl::Create(astContext,
                                        TTK_Struct,
                                        astContext.getTranslationUnitDecl(),
                                        invLoc,
                                        invLoc,
                                        &astContext.Idents.get(name));
  astContext.getTranslationUnitDecl()->addDecl(decl);
  return decl;
}

FieldDecl* ASTBuilder::createField(const std::string& name,
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

DeclRefExpr* ASTBuilder::createGlobalVariable(const std::string& name,
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

DeclRefExpr* ASTBuilder::createLocalVariable(const std::string& name,
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

DeclRefExpr* ASTBuilder::createVariable(const std::string& name,
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

ConditionalOperator* ASTBuilder::createConditionalOperator(Expr* cond,
                                                           Expr* t,
                                                           Expr* f,
                                                           QualType type) {
  return new(astContext) ConditionalOperator(
      cond, invLoc, t, invLoc, f, type, VK_LValue, OK_Ordinary);
}

BinaryOperator* ASTBuilder::createBinaryOperator(Expr* lhs,
                                                 Expr* rhs,
                                                 BinaryOperator::Opcode opc,
                                                 QualType type) {
  return new(astContext) BinaryOperator(
      lhs, rhs, opc, type, VK_RValue, OK_Ordinary, invLoc, FPOptions());
}

UnaryOperator* ASTBuilder::createUnaryOperator(Expr* lhs,
                                               UnaryOperator::Opcode opc,
                                               QualType type) {
  return new(astContext)
      UnaryOperator(lhs, opc, type, VK_RValue, OK_Ordinary, invLoc, false);
}

ArraySubscriptExpr*
ASTBuilder::createArraySubscriptExpr(Expr* base, Expr* idx, QualType type) {
  return new(astContext)
      ArraySubscriptExpr(base, idx, type, VK_RValue, OK_Ordinary, invLoc);
}

CStyleCastExpr* ASTBuilder::createCastExpr(Expr* expr, QualType qty) {
  return CStyleCastExpr::Create(astContext,
                                qty,
                                VK_RValue,
                                CastKind::CK_BitCast,
                                expr,
                                nullptr,
                                astContext.CreateTypeSourceInfo(qty),
                                invLoc,
                                invLoc);
}

IfStmt* ASTBuilder::createIfStmt(Expr* cond, Stmt* thn, Stmt* els) {
  return IfStmt::Create(
      astContext, invLoc, false, nullptr, nullptr, cond, thn, invLoc, els);
}

GotoStmt* ASTBuilder::createGotoStmt(LabelDecl* label) {
  return new(astContext) GotoStmt(label, invLoc, invLoc);
}

CompoundStmt* ASTBuilder::createCompoundStmt(const Vector<Stmt*>& stmts) {
  return CompoundStmt::Create(
      astContext, cish::makeArrayRef(stmts), invLoc, invLoc);
}

CompoundStmt* ASTBuilder::createCompoundStmt(Stmt* stmt) {
  return CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmt), invLoc, invLoc);
}

SwitchStmt* ASTBuilder::createSwitchStmt(Expr* cond) {
  return SwitchStmt::Create(astContext, nullptr, nullptr, cond);
}

CaseStmt* ASTBuilder::createCaseStmt(Expr* value) {
  return CaseStmt::Create(astContext, value, nullptr, invLoc, invLoc, invLoc);
}

DefaultStmt* ASTBuilder::createDefaultStmt(Stmt* body) {
  return new(astContext) DefaultStmt(invLoc, invLoc, body);
}

BreakStmt* ASTBuilder::createBreakStmt() {
  return new(astContext) BreakStmt(invLoc);
}

ContinueStmt* ASTBuilder::createContinueStmt() {
  return new(astContext) ContinueStmt(invLoc);
}

ReturnStmt* ASTBuilder::createReturnStmt(Expr* retExpr) {
  return ReturnStmt::Create(astContext, invLoc, retExpr, nullptr);
}

CallExpr* ASTBuilder::createCallExpr(Expr* callee,
                                     const Vector<Expr*>& args,
                                     QualType type) {
  return CallExpr::Create(
      astContext, callee, makeArrayRef(args), type, VK_RValue, invLoc);
}

MemberExpr*
ASTBuilder::createMemberExpr(Expr* base, ValueDecl* member, QualType type) {
  return MemberExpr::Create(
      astContext,
      base,
      false,
      invLoc,
      NestedNameSpecifierLoc(),
      invLoc,
      member,
      DeclAccessPair::make(member, AS_public),
      DeclarationNameInfo(&astContext.Idents.get(member->getName()), invLoc),
      nullptr,
      type,
      VK_RValue,
      OK_Ordinary);
}

DoStmt* ASTBuilder::createDoStmt(Stmt* body, Expr* cond) {
  return new(astContext) DoStmt(body, cond, invLoc, invLoc, invLoc);
}

WhileStmt* ASTBuilder::createWhileStmt(Expr* cond, Stmt* body) {
  return WhileStmt::Create(astContext, nullptr, cond, body, invLoc);
}

CXXBoolLiteralExpr* ASTBuilder::createBoolLiteral(bool b, QualType type) {
  return new(astContext) CXXBoolLiteralExpr(b, type, invLoc);
}

IntegerLiteral* ASTBuilder::createIntLiteral(const llvm::APInt& i,
                                             QualType type) {
  return IntegerLiteral::Create(astContext, i, type, invLoc);
}

IntegerLiteral* ASTBuilder::createIntLiteral(short i) {
  return createIntLiteral(llvm::APInt(16, i, true), astContext.ShortTy);
}

IntegerLiteral* ASTBuilder::createIntLiteral(int i) {
  return createIntLiteral(llvm::APInt(32, i, true), astContext.IntTy);
}

IntegerLiteral* ASTBuilder::createIntLiteral(long i) {
  return createIntLiteral(llvm::APInt(64, i, true), astContext.LongTy);
}

FloatingLiteral* ASTBuilder::createFloatLiteral(const llvm::APFloat& f,
                                                QualType type) {
  return FloatingLiteral::Create(astContext, f, false, type, invLoc);
}

FloatingLiteral* ASTBuilder::createFloatLiteral(float f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.FloatTy);
}

FloatingLiteral* ASTBuilder::createFloatLiteral(double f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.DoubleTy);
}

FloatingLiteral* ASTBuilder::createFloatLiteral(long double f) {
  fatal(error() << "UNIMPLEMENTED: Float literal for long double\n");
}

CXXNullPtrLiteralExpr* ASTBuilder::createNullptr(QualType type) {
  return new(astContext) CXXNullPtrLiteralExpr(type, invLoc);
}

StringLiteral* ASTBuilder::createStringLiteral(llvm::StringRef str,
                                               QualType type) {
  return StringLiteral::Create(
      astContext, str, StringLiteral::Ascii, false, type, invLoc);
}

StringLiteral* ASTBuilder::createStringLiteral(const std::string& str,
                                               QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

StringLiteral* ASTBuilder::createStringLiteral(const char* str, QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

InitListExpr* ASTBuilder::createInitListExpr(const Vector<Expr*>& exprs) {
  return new(astContext)
      InitListExpr(astContext, invLoc, makeArrayRef(exprs), invLoc);
}

} // namespace cish