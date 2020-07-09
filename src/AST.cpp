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

#include "AST.h"
#include "ASTFunctionPass.h"
#include "ASTPasses.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"
#include "ParentMap.h"

using namespace clang;

namespace cish {

AST::AST(CishContext& cishContext)
    : cishContext(cishContext), astContext(cishContext.getASTContext()) {
  ;
}

AST::AST(CishContext& cishContext, ParentMap& pm)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      pm(&pm) {
  ;
}

bool AST::replace(UnaryOperator* unOp, Expr* repl) {
  unOp->setSubExpr(repl);

  return true;
}

bool AST::replace(BinaryOperator* binOp, Expr* repl, bool replaceLHS) {
  if(replaceLHS)
    binOp->setLHS(repl);
  else
    binOp->setRHS(repl);

  return true;
}

bool AST::replace(ArraySubscriptExpr* arrExpr, Expr* repl, bool replaceBase) {
  if(replaceBase)
    arrExpr->setLHS(repl);
  else
    arrExpr->setRHS(repl);

  return true;
}

bool AST::replace(CallExpr* callExpr, Expr* repl, long i) {
  if(i < 0)
    callExpr->setCallee(repl);
  else
    callExpr->setArg(i, repl);

  return true;
}

bool AST::replace(CStyleCastExpr* castExpr, Expr* repl) {
  castExpr->setSubExpr(repl);

  return true;
}

bool AST::replace(MemberExpr* memberExpr, Expr* repl) {
  memberExpr->setBase(repl);

  return true;
}

bool AST::replace(ReturnStmt* retStmt, Expr* repl) {
  retStmt->setRetValue(repl);

  return true;
}

bool AST::replace(IfStmt* ifStmt, Expr* repl) {
  ifStmt->setCond(repl);

  return true;
}

bool AST::replace(SwitchStmt* switchStmt, Expr* repl) {
  switchStmt->setCond(repl);

  return true;
}

bool AST::replace(DoStmt* doStmt, Expr* repl) {
  doStmt->setCond(repl);

  return true;
}

bool AST::replace(ForStmt* forStmt, Expr* repl) {
  forStmt->setCond(repl);

  return true;
}

bool AST::replace(WhileStmt* whileStmt, Expr* repl) {
  whileStmt->setCond(repl);

  return true;
}

bool AST::replace(Stmt* parent, Expr* expr, Expr* repl) {
  bool changed = false;

  if(auto* unOp = dyn_cast<UnaryOperator>(parent)) {
    changed |= replace(unOp, cloneExpr(repl));
  } else if(auto* binOp = dyn_cast<BinaryOperator>(parent)) {
    if(binOp->getLHS() == expr)
      changed |= replace(binOp, cloneExpr(repl), true);
    if(binOp->getRHS() == expr)
      changed |= replace(binOp, cloneExpr(repl), false);
  } else if(auto* condOp = dyn_cast<ConditionalOperator>(parent)) {
    Expr* newCond = condOp->getCond();
    if(newCond == expr)
      newCond = cloneExpr(repl);
    Expr* newTrue = condOp->getTrueExpr();
    if(newTrue == expr)
      newTrue = cloneExpr(repl);
    Expr* newFalse = condOp->getFalseExpr();
    if(newFalse == expr)
      newFalse = cloneExpr(repl);
    if((newCond != expr) or (newTrue != expr) or (newFalse != expr))
      changed |= replace(pm->getParent(condOp),
                         condOp,
                         createConditionalOperator(
                             newCond, newTrue, newFalse, newTrue->getType()));
  } else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(parent)) {
    if(arrExpr->getBase() == expr)
      changed |= replace(arrExpr, cloneExpr(repl), true);
    if(arrExpr->getIdx() == expr)
      changed |= replace(arrExpr, cloneExpr(repl), false);
  } else if(auto* callExpr = dyn_cast<CallExpr>(parent)) {
    if(callExpr->getCallee() == expr)
      changed |= replace(callExpr, cloneExpr(repl), -1);
    for(unsigned i = 0; i < callExpr->getNumArgs(); i++)
      if(callExpr->getArg(i) == expr)
        changed |= replace(callExpr, cloneExpr(repl), i);
  } else if(auto* castExpr = dyn_cast<CStyleCastExpr>(parent)) {
    changed |= replace(castExpr, cloneExpr(repl));
  } else if(auto* memberExpr = dyn_cast<MemberExpr>(parent)) {
    changed |= replace(memberExpr, cloneExpr(repl));
  } else if(auto* retStmt = dyn_cast<ReturnStmt>(parent)) {
    changed |= replace(retStmt, cloneExpr(repl));
  } else if(auto* ifStmt = dyn_cast<IfStmt>(parent)) {
    changed |= replace(ifStmt, cloneExpr(repl));
  } else if(auto* switchStmt = dyn_cast<SwitchStmt>(parent)) {
    changed |= replace(switchStmt, cloneExpr(repl));
  } else if(auto* doStmt = dyn_cast<DoStmt>(parent)) {
    changed |= replace(doStmt, cloneExpr(repl));
  } else if(auto* forStmt = dyn_cast<ForStmt>(parent)) {
    for(BinaryOperator* binOp : Clang::getForInits(forStmt))
      changed |= replace(cast<Stmt>(binOp), expr, repl);
    if(forStmt->getCond() == expr)
      changed |= replace(forStmt, cloneExpr(repl));
    for(BinaryOperator* binOp : Clang::getForIncs(forStmt))
      changed |= replace(cast<Stmt>(binOp), expr, repl);
  } else if(auto* whileStmt = dyn_cast<WhileStmt>(parent)) {
    changed |= replace(whileStmt, cloneExpr(repl));
  } else if(auto* compoundStmt = dyn_cast<CompoundStmt>(parent)) {
    fatal(error() << "Not implemented. Replacing top-level statement");
  } else {
    fatal(error() << "Unexpected use for expression being replaced: "
                  << parent->getStmtClassName());
  }

  return changed;
}

// bool AST::replaceEqvUsesWith(Expr* expr, Expr* repl) {
//   bool changed = false;

//   if(isa<DeclRefExpr>(expr))
//     fatal(error() << "Cannot replace a DeclRefExpr. Replace the underlying "
//                      "ValueDecl instead");

//   Set<VarDecl*> vars = Clang::getVarsInStmt(expr);
//   for(Expr* e : en.getEqv(expr).clone())
//     if(e != expr)
//       if(pm.hasParent(e))
//         changed |= replace(pm.getParent(e), e, repl);

//   return changed;
// }

bool AST::replaceExprWith(Expr* expr, Expr* repl, Stmt* parent) {
  bool changed = false;

  changed |= replace(parent, expr, repl);

  return changed;
}

bool AST::replaceStmtWith(Stmt* stmt, Stmt* repl, Stmt* parent) {
  bool changed = false;

  if(auto* body = dyn_cast<CompoundStmt>(parent)) {
    Stmt** stmts = body->body_begin();
    for(unsigned i = 0; i < body->size(); i++) {
      if(stmts[i] == stmt) {
        if(repl)
          stmts[i] = repl;
        else
          stmts[i] = createNullStmt();
        changed |= true;
      }
    }
  } else if(auto* forStmt = dyn_cast<ForStmt>(parent)) {
    if(forStmt->getInit() == stmt)
      if(repl)
        forStmt->setInit(repl);
      else
        forStmt->setInit(nullptr);
    else if(forStmt->getCond() == stmt)
      if(repl)
        forStmt->setCond(cast<Expr>(repl));
      else
        forStmt->setCond(nullptr);
    else if(forStmt->getInc() == stmt)
      if(repl)
        forStmt->setInc(cast<Expr>(repl));
      else
        forStmt->setInc(nullptr);
    else
      fatal(error() << "Could not match repl with part of forStmt");
    changed |= true;
  } else {
    fatal(error() << "Unsupported parent to replace statement: "
                  << parent->getStmtClassName());
  }

  return changed;
}

bool AST::erase(Stmt* key, Stmt* parent) {
  bool changed = false;

  changed |= replaceStmtWith(key, nullptr, parent);

  return changed;
}

DeclarationNameInfo AST::getDeclarationNameInfo(const std::string& name) {
  return DeclarationNameInfo(&astContext.Idents.get(name), invLoc);
}

IdentifierInfo& AST::getIdentifierInfo(const std::string& name) {
  return astContext.Idents.get(name);
}

DeclarationName AST::createDeclName(const std::string& name) {
  return DeclarationName(&astContext.Idents.get(name));
}

LabelDecl* AST::createLabelDecl(FunctionDecl* f, const std::string& name) {
  LabelDecl* label
      = LabelDecl::Create(astContext, f, invLoc, &getIdentifierInfo(name));

  label->setStmt(createLabelStmt(label));

  return label;
}

ParmVarDecl*
AST::createParam(const std::string& name, QualType type, FunctionDecl* func) {
  ParmVarDecl* param = ParmVarDecl::Create(astContext,
                                           func,
                                           invLoc,
                                           invLoc,
                                           &getIdentifierInfo(name),
                                           type,
                                           nullptr,
                                           SC_None,
                                           nullptr);
  return param;
}

FunctionDecl* AST::createFunction(const std::string& name, QualType type) {
  FunctionDecl* f = FunctionDecl::Create(astContext,
                                         astContext.getTranslationUnitDecl(),
                                         invLoc,
                                         invLoc,
                                         createDeclName(name),
                                         type,
                                         nullptr,
                                         SC_None);
  astContext.getTranslationUnitDecl()->addDecl(f);

  return f;
}

RecordDecl* AST::createStruct(const std::string& name) {
  RecordDecl* decl = RecordDecl::Create(astContext,
                                        TTK_Struct,
                                        astContext.getTranslationUnitDecl(),
                                        invLoc,
                                        invLoc,
                                        &getIdentifierInfo(name));
  astContext.getTranslationUnitDecl()->addDecl(decl);
  return decl;
}

FieldDecl*
AST::createField(const std::string& name, QualType type, RecordDecl* strct) {
  FieldDecl* field = FieldDecl::Create(astContext,
                                       strct,
                                       invLoc,
                                       invLoc,
                                       &getIdentifierInfo(name),
                                       type,
                                       nullptr,
                                       nullptr,
                                       true,
                                       ICIS_NoInit);
  strct->addDecl(field);
  return field;
}

VarDecl* AST::createGlobalVariable(const std::string& name, QualType type) {
  TranslationUnitDecl* tu = astContext.getTranslationUnitDecl();
  VarDecl* g = VarDecl::Create(astContext,
                               tu,
                               invLoc,
                               invLoc,
                               &getIdentifierInfo(name),
                               type,
                               nullptr,
                               SC_None);
  tu->addDecl(g);

  return g;
}

VarDecl* AST::createLocalVariable(const std::string& name,
                                  QualType type,
                                  FunctionDecl* func) {
  VarDecl* local = VarDecl::Create(astContext,
                                   func,
                                   invLoc,
                                   invLoc,
                                   &getIdentifierInfo(name),
                                   type,
                                   nullptr,
                                   SC_None);
  func->addDecl(local);

  return local;
}

VarDecl* AST::createVariable(const std::string& name,
                             QualType type,
                             DeclContext* declContext) {
  VarDecl* var = VarDecl::Create(astContext,
                                 declContext,
                                 invLoc,
                                 invLoc,
                                 &getIdentifierInfo(name),
                                 type,
                                 nullptr,
                                 SC_None);

  return var;
}

CXXBoolLiteralExpr* AST::createBoolLiteral(bool b) {
  return createBoolLiteral(b, astContext.BoolTy);
}

CXXBoolLiteralExpr* AST::createBoolLiteral(bool b, QualType type) {
  return new(astContext) CXXBoolLiteralExpr(b, type, invLoc);
}

CharacterLiteral* AST::createCharacterLiteral(unsigned c) {
  return new(astContext)
      CharacterLiteral(c, CharacterLiteral::Ascii, astContext.CharTy, invLoc);
}

IntegerLiteral* AST::createIntLiteral(const llvm::APInt& i, QualType type) {
  return IntegerLiteral::Create(astContext, i, type, invLoc);
}

IntegerLiteral* AST::createIntLiteral(short i) {
  return createIntLiteral(llvm::APInt(16, i, true), astContext.ShortTy);
}

IntegerLiteral* AST::createIntLiteral(unsigned short i) {
  return createIntLiteral(llvm::APInt(16, i, false),
                          astContext.UnsignedShortTy);
}

IntegerLiteral* AST::createIntLiteral(int i) {
  return createIntLiteral(llvm::APInt(32, i, true), astContext.IntTy);
}

IntegerLiteral* AST::createIntLiteral(unsigned int i) {
  return createIntLiteral(llvm::APInt(32, i, false), astContext.UnsignedIntTy);
}

IntegerLiteral* AST::createIntLiteral(long i) {
  return createIntLiteral(llvm::APInt(64, i, true), astContext.LongTy);
}

IntegerLiteral* AST::createIntLiteral(unsigned long i) {
  return createIntLiteral(llvm::APInt(64, i, false), astContext.UnsignedLongTy);
}

FloatingLiteral* AST::createFloatLiteral(const llvm::APFloat& f,
                                         QualType type) {
  return FloatingLiteral::Create(astContext, f, false, type, invLoc);
}

FloatingLiteral* AST::createFloatLiteral(float f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.FloatTy);
}

FloatingLiteral* AST::createFloatLiteral(double f) {
  return createFloatLiteral(llvm::APFloat(f), astContext.DoubleTy);
}

FloatingLiteral* AST::createFloatLiteral(long double) {
  fatal(error() << "UNIMPLEMENTED: Float literal for long double\n");
  return nullptr;
}

StringLiteral* AST::createStringLiteral(llvm::StringRef str, QualType type) {
  return StringLiteral::Create(
      astContext, str, StringLiteral::Ascii, false, type, invLoc);
}

StringLiteral* AST::createStringLiteral(const std::string& str, QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

StringLiteral* AST::createStringLiteral(const char* str, QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

CXXNullPtrLiteralExpr* AST::createNullptr(QualType type) {
  return new(astContext) CXXNullPtrLiteralExpr(type, invLoc);
}

InitListExpr* AST::createInitListExpr(const Vector<Expr*>& exprs) {
  return new(astContext)
      InitListExpr(astContext, invLoc, LLVM::makeArrayRef(exprs), invLoc);
}

UnaryOperator*
AST::createUnaryOperator(Expr* expr, UnaryOperator::Opcode opc, QualType type) {
  return new(astContext)
      UnaryOperator(expr, opc, type, VK_RValue, OK_Ordinary, invLoc, false);
}

BinaryOperator* AST::createBinaryOperator(Expr* lhs,
                                          Expr* rhs,
                                          BinaryOperator::Opcode opc,
                                          QualType type) {
  return new(astContext) BinaryOperator(
      lhs, rhs, opc, type, VK_RValue, OK_Ordinary, invLoc, FPOptions());
}

ConditionalOperator*
AST::createConditionalOperator(Expr* cond, Expr* t, Expr* f, QualType type) {
  return new(astContext) ConditionalOperator(
      cond, invLoc, t, invLoc, f, type, VK_LValue, OK_Ordinary);
}

ArraySubscriptExpr*
AST::createArraySubscriptExpr(Expr* base, Expr* idx, QualType type) {
  return new(astContext)
      ArraySubscriptExpr(base, idx, type, VK_RValue, OK_Ordinary, invLoc);
}

CallExpr*
AST::createCallExpr(Expr* callee, const Vector<Expr*>& args, QualType type) {
  return CallExpr::Create(
      astContext, callee, LLVM::makeArrayRef(args), type, VK_RValue, invLoc);
}

CStyleCastExpr* AST::createCastExpr(Expr* expr, QualType qty) {
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

MemberExpr*
AST::createMemberExpr(Expr* base, ValueDecl* member, QualType type) {
  return MemberExpr::Create(astContext,
                            base,
                            false,
                            invLoc,
                            NestedNameSpecifierLoc(),
                            invLoc,
                            member,
                            DeclAccessPair::make(member, AS_public),
                            getDeclarationNameInfo(member->getName()),
                            nullptr,
                            type,
                            VK_RValue,
                            OK_Ordinary);
}

DeclRefExpr* AST::createDeclRefExpr(ValueDecl* decl) {
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

NullStmt* AST::createNullStmt() {
  return new(astContext) NullStmt(invLoc);
}

CompoundStmt* AST::createCompoundStmt(const Vector<Stmt*>& stmts) {
  return CompoundStmt::Create(
      astContext, LLVM::makeArrayRef(stmts), invLoc, invLoc);
}

CompoundStmt* AST::createCompoundStmt(Stmt* stmt) {
  return CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmt), invLoc, invLoc);
}

IfStmt* AST::createIfStmt(Expr* cond, Stmt* thn, Stmt* els) {
  return IfStmt::Create(
      astContext, invLoc, false, nullptr, nullptr, cond, thn, invLoc, els);
}

SwitchStmt* AST::createSwitchStmt(Expr* cond, Stmt* body) {
  SwitchStmt* switchStmt
      = SwitchStmt::Create(astContext, nullptr, nullptr, cond);
  switchStmt->setBody(body);

  return switchStmt;
}

CaseStmt* AST::createCaseStmt(Expr* value, Stmt* body) {
  CaseStmt* caseStmt
      = CaseStmt::Create(astContext, value, nullptr, invLoc, invLoc, invLoc);
  caseStmt->setSubStmt(body);

  return caseStmt;
}

DefaultStmt* AST::createDefaultStmt(Stmt* body) {
  return new(astContext) DefaultStmt(invLoc, invLoc, body);
}

DoStmt* AST::createDoStmt(Stmt* body, Expr* cond) {
  return new(astContext) DoStmt(body, cond, invLoc, invLoc, invLoc);
}

ForStmt* AST::createForStmt(Stmt* init, Expr* cond, Expr* inc, Stmt* body) {
  return new(astContext) ForStmt(
      astContext, init, cond, nullptr, inc, body, invLoc, invLoc, invLoc);
}

WhileStmt* AST::createWhileStmt(Expr* cond, Stmt* body) {
  return WhileStmt::Create(astContext, nullptr, cond, body, invLoc);
}

ReturnStmt* AST::createReturnStmt(Expr* expr) {
  return ReturnStmt::Create(astContext, invLoc, expr, nullptr);
}

GotoStmt* AST::createGotoStmt(LabelDecl* label) {
  return new(astContext) GotoStmt(label, invLoc, invLoc);
}

BreakStmt* AST::createBreakStmt() {
  return new(astContext) BreakStmt(invLoc);
}

ContinueStmt* AST::createContinueStmt() {
  return new(astContext) ContinueStmt(invLoc);
}

LabelStmt* AST::createLabelStmt(LabelDecl* label) {
  // Need to add a null statement to the label because the CFG generation code
  // expects a non-null sub-statement in the LabelStmt. Not setting on ends
  // up causing a silent failure in the CFG construction which breaks nearly
  // all the AST cleanup passes
  return new(astContext) LabelStmt(invLoc, label, createNullStmt());
}

DeclStmt* AST::createDeclStmt(Decl* decl) {
  return new(astContext)
      DeclStmt(DeclGroupRef::Create(astContext, &decl, 1), invLoc, invLoc);
}

DeclStmt* AST::createDeclStmt(Vector<Decl*>& decls) {
  return new(astContext)
      DeclStmt(DeclGroupRef::Create(astContext, decls.data(), decls.size()),
               invLoc,
               invLoc);
}

Stmt* AST::clone(CXXNullPtrLiteralExpr* nlit) {
  return createNullptr(nlit->getType());
}

Stmt* AST::clone(CXXBoolLiteralExpr* blit) {
  return createBoolLiteral(blit->getValue());
}

Stmt* AST::clone(CharacterLiteral* clit) {
  return createCharacterLiteral(clit->getValue());
}

Stmt* AST::clone(IntegerLiteral* ilit) {
  return createIntLiteral(ilit->getValue(), ilit->getType());
}

Stmt* AST::clone(FloatingLiteral* flit) {
  return createFloatLiteral(flit->getValue(), flit->getType());
}

Stmt* AST::clone(StringLiteral* slit) {
  return createStringLiteral(slit->getString(), slit->getType());
}

Stmt* AST::clone(DeclRefExpr* declRef) {
  return createDeclRefExpr(declRef->getDecl());
}

Stmt* AST::clone(UnaryOperator* unOp) {
  return createUnaryOperator(
      cloneExpr(unOp->getSubExpr()), unOp->getOpcode(), unOp->getType());
}

Stmt* AST::clone(BinaryOperator* binOp) {
  return createBinaryOperator(cloneExpr(binOp->getLHS()),
                              cloneExpr(binOp->getRHS()),
                              binOp->getOpcode(),
                              binOp->getType());
}

Stmt* AST::clone(ConditionalOperator* condOp) {
  return createConditionalOperator(cloneExpr(condOp->getCond()),
                                   cloneExpr(condOp->getTrueExpr()),
                                   cloneExpr(condOp->getFalseExpr()),
                                   condOp->getType());
}

Stmt* AST::clone(ArraySubscriptExpr* arrExpr) {
  return createArraySubscriptExpr(cloneExpr(arrExpr->getBase()),
                                  cloneExpr(arrExpr->getIdx()),
                                  arrExpr->getType());
}

Stmt* AST::clone(CallExpr* callExpr) {
  Vector<Expr*> args;
  for(Expr* arg : callExpr->arguments())
    args.push_back(cloneExpr(arg));

  return createCallExpr(
      cloneExpr(callExpr->getCallee()), args, callExpr->getType());
}

Stmt* AST::clone(CStyleCastExpr* castExpr) {
  return createCastExpr(cloneExpr(castExpr->getSubExpr()), castExpr->getType());
}

Stmt* AST::clone(MemberExpr* memberExpr) {
  return createMemberExpr(cloneExpr(memberExpr->getBase()),
                          memberExpr->getMemberDecl(),
                          memberExpr->getType());
}

Stmt* AST::clone(InitListExpr* initList) {
  Vector<Expr*> inits;
  for(Expr* init : initList->inits())
    inits.push_back(cloneExpr(init));
  return createInitListExpr(inits);
}

Stmt* AST::clone(CompoundStmt* compoundStmt) {
  Vector<Stmt*> body;
  for(Stmt* stmt : compoundStmt->body())
    body.push_back(clone(stmt));
  return createCompoundStmt(body);
}

Stmt* AST::clone(IfStmt* ifStmt) {
  if(Stmt* els = ifStmt->getElse())
    return createIfStmt(
        cloneExpr(ifStmt->getCond()), clone(ifStmt->getThen()), clone(els));
  else
    return createIfStmt(
        cloneExpr(ifStmt->getCond()), clone(ifStmt->getThen()), nullptr);
}

Stmt* AST::clone(DoStmt* doStmt) {
  return createDoStmt(clone(doStmt->getBody()), cloneExpr(doStmt->getCond()));
}

Stmt* AST::clone(ForStmt* forStmt) {
  return createForStmt(clone(forStmt->getInit()),
                       cloneExpr(forStmt->getCond()),
                       cloneExpr(forStmt->getInc()),
                       clone(forStmt->getBody()));
}

Stmt* AST::clone(WhileStmt* whileStmt) {
  return createWhileStmt(cloneExpr(whileStmt->getCond()),
                         clone(whileStmt->getBody()));
}

Stmt* AST::clone(SwitchStmt* switchStmt) {
  return createSwitchStmt(cloneExpr(switchStmt->getCond()),
                          clone(switchStmt->getBody()));
}

Stmt* AST::clone(CaseStmt* caseStmt) {
  return createCaseStmt(cloneExpr(caseStmt->getLHS()),
                        clone(caseStmt->getSubStmt()));
}

Stmt* AST::clone(DefaultStmt* defaultStmt) {
  return createDefaultStmt(clone(defaultStmt->getSubStmt()));
}

Stmt* AST::clone(BreakStmt*) {
  return createBreakStmt();
}

Stmt* AST::clone(ContinueStmt*) {
  return createContinueStmt();
}

Stmt* AST::clone(GotoStmt* gotoStmt) {
  return createGotoStmt(gotoStmt->getLabel());
}

Stmt* AST::clone(ReturnStmt* retStmt) {
  if(Expr* retValue = retStmt->getRetValue())
    return createReturnStmt(cloneExpr(retValue));
  else
    return createReturnStmt(nullptr);
}

Stmt* AST::clone(LabelStmt* labelStmt) {
  return createLabelStmt(labelStmt->getDecl());
}

Stmt* AST::clone(NullStmt*) {
  return createNullStmt();
}

Stmt* AST::clone(Stmt* stmt) {
  if(auto* boolLit = dyn_cast<CXXBoolLiteralExpr>(stmt))
    return clone(boolLit);
  else if(auto* charLit = dyn_cast<CharacterLiteral>(stmt))
    return clone(charLit);
  else if(auto* intLit = dyn_cast<IntegerLiteral>(stmt))
    return clone(intLit);
  else if(auto* fpLit = dyn_cast<FloatingLiteral>(stmt))
    return clone(fpLit);
  else if(auto* stringLit = dyn_cast<StringLiteral>(stmt))
    return clone(stringLit);
  else if(auto* cnull = dyn_cast<CXXNullPtrLiteralExpr>(stmt))
    return clone(cnull);
  else if(auto* initList = dyn_cast<InitListExpr>(stmt))
    return clone(initList);
  else if(auto* declRefExpr = dyn_cast<DeclRefExpr>(stmt))
    return clone(declRefExpr);
  else if(auto* unOp = dyn_cast<UnaryOperator>(stmt))
    return clone(unOp);
  else if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    return clone(binOp);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    return clone(condOp);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(stmt))
    return clone(arrExpr);
  else if(auto* callExpr = dyn_cast<CallExpr>(stmt))
    return clone(callExpr);
  else if(auto* castExpr = dyn_cast<CStyleCastExpr>(stmt))
    return clone(castExpr);
  else if(auto* memberExpr = dyn_cast<MemberExpr>(stmt))
    return clone(memberExpr);
  else if(auto* compoundStmt = dyn_cast<CompoundStmt>(stmt))
    return clone(compoundStmt);
  else if(auto* ifStmt = dyn_cast<IfStmt>(stmt))
    return clone(ifStmt);
  else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt))
    return clone(switchStmt);
  else if(auto* castStmt = dyn_cast<CaseStmt>(stmt))
    return clone(castStmt);
  else if(auto* defaultStmt = dyn_cast<DefaultStmt>(stmt))
    return clone(defaultStmt);
  else if(auto* doStmt = dyn_cast<DoStmt>(stmt))
    return clone(doStmt);
  else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    return clone(whileStmt);
  else if(auto* forStmt = dyn_cast<ForStmt>(stmt))
    return clone(forStmt);
  else if(auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    return clone(retStmt);
  else if(auto* nullStmt = dyn_cast<NullStmt>(stmt))
    return clone(nullStmt);
  else if(auto* breakStmt = dyn_cast<BreakStmt>(stmt))
    return clone(breakStmt);
  else if(auto* continueStmt = dyn_cast<ContinueStmt>(stmt))
    return clone(continueStmt);
  else if(auto* labelStmt = dyn_cast<LabelStmt>(stmt))
    return clone(labelStmt);
  else if(auto* gotoStmt = dyn_cast<GotoStmt>(stmt))
    return clone(gotoStmt);
  else
    fatal(error() << "Unknown statement to clone: "
                  << stmt->getStmtClassName());

  return nullptr;
}

Expr* AST::cloneExpr(Expr* expr) {
  return cast<Expr>(clone(expr));
}

} // namespace cish
