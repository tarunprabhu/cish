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

#include <clang/AST/RecursiveASTVisitor.h>

#include <algorithm>

using namespace clang;

namespace cish {

AST::AST(CishContext& cishContext)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      decl(nullptr), pm(cishContext, decl), en(cishContext),
      dt(new DominatorTree()) {
  ;
}

AST::AST(CishContext& cishContext, FunctionDecl* decl, const AST& topLevel)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      decl(decl), pm(cishContext, decl), en(cishContext),
      dt(new DominatorTree()) {
  globals = topLevel.globals;
  for(VarDecl* g : globals) {
    useMap[g].clear();
    defMap[g].clear();
  }
  for(ParmVarDecl* param : decl->parameters()) {
    useMap[param].clear();
    defMap[param].clear();
  }
}

FunctionDecl* AST::getFunction() const {
  return decl;
}

const DominatorTree& AST::getDominatorTree() const {
  return *dt;
}

CFG* AST::getCFG() const {
  return cfg.get();
}

CFGBlock* AST::getCFGBlock(Stmt* stmt) const {
  return cfgStmtMap->getBlock(stmt);
}

const CFG::BuildOptions& AST::getCFGBuildOpts() const {
  return cfgBuildOpts;
}

bool AST::replace(UnaryOperator* unOp, Expr* repl) {
  remove(unOp->getSubExpr());
  unOp->setSubExpr(repl);
  add(repl, unOp);

  return true;
}

bool AST::replace(BinaryOperator* binOp, Expr* repl, bool replaceLHS) {
  if(replaceLHS) {
    remove(binOp->getLHS());
    if(binOp->getOpcode() == BO_Assign) {
      if(VarDecl* var = Clang::getVar(repl))
        addDef(var, binOp);
      pm.addOrReplace(repl, binOp);
    } else {
      add(repl, binOp);
    }
    binOp->setLHS(repl);
  } else {
    remove(binOp->getRHS());
    add(repl, binOp);
    binOp->setRHS(repl);
  }

  return true;
}

bool AST::replace(ArraySubscriptExpr* arrExpr, Expr* repl, bool replaceBase) {
  if(replaceBase) {
    remove(arrExpr->getBase());
    arrExpr->setLHS(repl);
  } else {
    remove(arrExpr->getIdx());
    arrExpr->setRHS(repl);
  }
  add(repl, arrExpr);

  return true;
}

bool AST::replace(CallExpr* callExpr, Expr* repl, long i) {
  if(i < 0) {
    remove(callExpr->getCallee());
    callExpr->setCallee(repl);
  } else {
    remove(callExpr->getArg(i));
    callExpr->setArg(i, repl);
  }
  add(repl, callExpr);

  return true;
}

bool AST::replace(CStyleCastExpr* castExpr, Expr* repl) {
  remove(castExpr->getSubExpr());
  castExpr->setSubExpr(repl);
  add(repl, castExpr);

  return true;
}

bool AST::replace(MemberExpr* memberExpr, Expr* repl) {
  remove(memberExpr->getBase());
  memberExpr->setBase(repl);
  add(repl, memberExpr);

  return true;
}

bool AST::replace(ReturnStmt* retStmt, Expr* repl) {
  remove(retStmt->getRetValue());
  retStmt->setRetValue(repl);
  add(repl, retStmt);

  return true;
}

bool AST::replace(IfStmt* ifStmt, Expr* repl) {
  remove(ifStmt->getCond());
  ifStmt->setCond(repl);
  add(repl, ifStmt);

  return true;
}

bool AST::replace(SwitchStmt* switchStmt, Expr* repl) {
  remove(switchStmt->getCond());
  switchStmt->setCond(repl);
  add(repl, switchStmt);

  return true;
}

bool AST::replace(DoStmt* doStmt, Expr* repl) {
  remove(doStmt->getCond());
  doStmt->setCond(repl);
  add(repl, doStmt);

  return true;
}

bool AST::replace(ForStmt* forStmt, Expr* repl) {
  remove(forStmt->getCond());
  forStmt->setCond(repl);
  add(repl, forStmt);

  return true;
}

bool AST::replace(WhileStmt* whileStmt, Expr* repl) {
  remove(whileStmt->getCond());
  whileStmt->setCond(repl);
  add(repl, whileStmt);

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
      changed |= replace(pm.getParent(condOp),
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
      changed |= replace(static_cast<Stmt*>(binOp), expr, repl);
    if(forStmt->getCond() == expr)
      changed |= replace(forStmt, cloneExpr(repl));
    for(BinaryOperator* binOp : Clang::getForIncs(forStmt))
      changed |= replace(static_cast<Stmt*>(binOp), expr, repl);
  } else if(auto* whileStmt = dyn_cast<WhileStmt>(parent)) {
    changed |= replace(whileStmt, cloneExpr(repl));
  } else if(isa<DeclRefExpr>(expr)) {
    ;
  } else {
    fatal(error() << "Unexpected use for expression being replaced: "
                  << parent->getStmtClassName());
  }

  return changed;
}

bool AST::replaceEqvUsesWith(Expr* expr, Expr* repl) {
  bool changed = false;

  if(isa<DeclRefExpr>(expr))
    fatal(error() << "Cannot replace a DeclRefExpr. Replace the underlying "
                     "ValueDecl instead");

  Set<VarDecl*> vars = Clang::getVarsInStmt(expr);
  for(Expr* e : en.getEqv(expr).clone())
    if(e != expr)
      if(pm.hasParent(e)) {
        llvm::outs() << "Replacing eqv: " << Clang::toString(e, astContext)
                     << "\n";
        changed |= replace(pm.getParent(e), e, repl);
      }

  return changed;
}

bool AST::replaceAllUsesWith(VarDecl* var, Expr* repl) {
  bool changed = false;

  for(Expr* expr : en.getEqv(en.get(var)).clone())
    for(Stmt* use : getUses(var).clone())
      changed |= replace(use, expr, repl);

  return changed;
}

bool AST::replaceExprWith(Expr* expr, Expr* repl) {
  bool changed = false;

  changed |= replace(pm.getParent(expr), expr, repl);

  return changed;
}

bool AST::replaceStmtWith(Stmt* stmt, Stmt* repl) {
  bool changed = false;

  CompoundStmt* body = cast<CompoundStmt>(pm.getParent(stmt));
  Stmt** stmts = body->body_begin();
  for(unsigned i = 0; i < body->size(); i++) {
    if(stmts[i] == stmt) {
      if(repl)
        stmts[i] = repl;
      else
        stmts[i] = createNullStmt();
      pm.add(stmts[i], body);
      remove(stmt);
      changed |= true;
    }
  }

  return changed;
}

bool AST::erase(Stmt* key) {
  bool changed = false;

  changed |= replaceStmtWith(key, nullptr);

  return changed;
}

bool AST::erase(VarDecl* var) {
  if(isDefined(var) or isUsed(var))
    fatal(error() << "Cannot delete variable: " << var->getName() << "("
                  << getNumDefs(var) << " defs, " << getNumUses(var)
                  << " uses)");

  bool changed = false;

  // This has to be done in this order. locals.erase() will return the size of
  // the container after potentially removing the variable
  changed |= (locals.size() != locals.erase(var));

  return changed;
}

void AST::add(Expr* expr, Stmt* user) {
  pm.addOrReplace(expr, user);
  if(VarDecl* var = Clang::getVar(expr))
    addUse(var, user);
}

void AST::remove(CXXBoolLiteralExpr*) {
  ;
}

void AST::remove(CharacterLiteral*) {
  ;
}

void AST::remove(IntegerLiteral*) {
  ;
}

void AST::remove(FloatingLiteral*) {
  ;
}

void AST::remove(StringLiteral*) {
  ;
}

void AST::remove(CXXNullPtrLiteralExpr*) {
  ;
}

void AST::remove(NullStmt*) {
  ;
}

void AST::remove(LabelStmt* labelStmt) {
  remove(labelStmt->getSubStmt());
}

void AST::remove(GotoStmt*) {
  ;
}

void AST::remove(BreakStmt*) {
  ;
}

void AST::remove(ContinueStmt*) {
  ;
}

void AST::remove(CompoundStmt* compoundStmt) {
  for(Stmt* stmt : compoundStmt->body())
    remove(stmt);
}

void AST::remove(IfStmt* ifStmt) {
  remove(ifStmt->getCond());
  remove(ifStmt->getThen());
  if(Stmt* els = ifStmt->getElse())
    remove(els);
}

void AST::remove(SwitchStmt* switchStmt) {
  remove(switchStmt->getCond());
  remove(switchStmt->getBody());
}

void AST::remove(CaseStmt* caseStmt) {
  remove(caseStmt->getSubStmt());
}

void AST::remove(DefaultStmt* defaultStmt) {
  remove(defaultStmt->getSubStmt());
}

void AST::remove(DoStmt* doStmt) {
  remove(doStmt->getCond());
  remove(doStmt->getBody());
}

void AST::remove(ForStmt* forStmt) {
  if(Stmt* inits = forStmt->getInit())
    remove(inits);
  remove(forStmt->getCond());
  if(Stmt* incs = forStmt->getInc())
    remove(incs);
  remove(forStmt->getBody());
}

void AST::remove(WhileStmt* whileStmt) {
  remove(whileStmt->getCond());
  remove(whileStmt->getBody());
}

void AST::remove(DeclRefExpr*) {
  ;
}

void AST::remove(ReturnStmt* retStmt) {
  if(Expr* retValue = retStmt->getRetValue())
    remove(retValue);
}

void AST::remove(UnaryOperator* unOp) {
  remove(unOp->getSubExpr());
}

void AST::remove(BinaryOperator* binOp) {
  remove(binOp->getLHS());
  remove(binOp->getRHS());
}

void AST::remove(ConditionalOperator* condOp) {
  remove(condOp->getCond());
  remove(condOp->getTrueExpr());
  remove(condOp->getFalseExpr());
}

void AST::remove(ArraySubscriptExpr* arrExpr) {
  remove(arrExpr->getBase());
  remove(arrExpr->getIdx());
}

void AST::remove(CallExpr* callExpr) {
  remove(callExpr->getCallee());
  for(Expr* arg : callExpr->arguments())
    remove(arg);
}

void AST::remove(CStyleCastExpr* castExpr) {
  remove(castExpr->getSubExpr());
}

void AST::remove(MemberExpr* memberExpr) {
  remove(memberExpr->getBase());
}

void AST::remove(InitListExpr* initList) {
  for(Expr* init : initList->inits())
    remove(init);
}

void AST::remove(Stmt* stmt) {
  if(auto* boolLit = dyn_cast<CXXBoolLiteralExpr>(stmt))
    remove(boolLit);
  else if(auto* charLit = dyn_cast<CharacterLiteral>(stmt))
    remove(charLit);
  else if(auto* intLit = dyn_cast<IntegerLiteral>(stmt))
    remove(intLit);
  else if(auto* fpLit = dyn_cast<FloatingLiteral>(stmt))
    remove(fpLit);
  else if(auto* stringLit = dyn_cast<StringLiteral>(stmt))
    remove(stringLit);
  else if(auto* cnull = dyn_cast<CXXNullPtrLiteralExpr>(stmt))
    remove(cnull);
  else if(auto* initList = dyn_cast<InitListExpr>(stmt))
    remove(initList);
  else if(auto* declRefExpr = dyn_cast<DeclRefExpr>(stmt))
    remove(declRefExpr);
  else if(auto* unOp = dyn_cast<UnaryOperator>(stmt))
    remove(unOp);
  else if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    remove(binOp);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    remove(condOp);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(stmt))
    remove(arrExpr);
  else if(auto* callExpr = dyn_cast<CallExpr>(stmt))
    remove(callExpr);
  else if(auto* castExpr = dyn_cast<CStyleCastExpr>(stmt))
    remove(castExpr);
  else if(auto* memberExpr = dyn_cast<MemberExpr>(stmt))
    remove(memberExpr);
  else if(auto* compoundStmt = dyn_cast<CompoundStmt>(stmt))
    remove(compoundStmt);
  else if(auto* ifStmt = dyn_cast<IfStmt>(stmt))
    remove(ifStmt);
  else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt))
    remove(switchStmt);
  else if(auto* castStmt = dyn_cast<CaseStmt>(stmt))
    remove(castStmt);
  else if(auto* defaultStmt = dyn_cast<DefaultStmt>(stmt))
    remove(defaultStmt);
  else if(auto* doStmt = dyn_cast<DoStmt>(stmt))
    remove(doStmt);
  else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    remove(whileStmt);
  else if(auto* forStmt = dyn_cast<ForStmt>(stmt))
    remove(forStmt);
  else if(auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    remove(retStmt);
  else if(auto* nullStmt = dyn_cast<NullStmt>(stmt))
    remove(nullStmt);
  else if(auto* breakStmt = dyn_cast<BreakStmt>(stmt))
    remove(breakStmt);
  else if(auto* continueStmt = dyn_cast<ContinueStmt>(stmt))
    remove(continueStmt);
  else if(auto* labelStmt = dyn_cast<LabelStmt>(stmt))
    remove(labelStmt);
  else if(auto* gotoStmt = dyn_cast<GotoStmt>(stmt))
    remove(gotoStmt);
  else
    fatal(error() << "Unknown statement to remove: "
                  << stmt->getStmtClassName());

  if(Expr* expr = dyn_cast<Expr>(stmt)) {
    if(VarDecl* var = Clang::getVar(expr)) {
      removeUse(var, pm.getParent(expr));
      removeDef(var, pm.getParent(expr));
    }
    en.remove(expr);
  }
  pm.remove(stmt);
}

void AST::addDef(VarDecl* var, Stmt* user) {
  if(not var)
    fatal(error() << "Cannot add nullptr");
  defMap.at(var).insert(user);
}

void AST::addUse(VarDecl* var, Stmt* user) {
  useMap.at(var).insert(user);
}

void AST::removeUse(VarDecl* var, Stmt* stmt) {
  useMap.at(var).erase(stmt);
}

void AST::removeDef(VarDecl* var, Stmt* stmt) {
  defMap.at(var).erase(stmt);
}

bool AST::isTopLevel(Stmt* stmt) const {
  return pm.isTopLevel(stmt);
}

bool AST::isDirectlyContainedIn(Stmt* needle, Stmt* haystack) const {
  return pm.isDirectlyContainedIn(needle, haystack);
}

bool AST::isContainedIn(Stmt* needle, Stmt* haystack) const {
  return pm.isContainedIn(needle, haystack);
}

const Set<Expr*>& AST::getEqvExprs(Expr* expr) const {
  return en.getEqv(expr);
}

bool AST::hasAddressTaken(VarDecl* var) const {
  return addrTaken.contains(var);
}

void AST::resetAddressTaken(VarDecl* var) {
  addrTaken.erase(var);
}

const Set<VarDecl*>& AST::getVars() const {
  return locals;
}

Set<Stmt*> AST::getTopLevelDefs(VarDecl* var) const {
  Set<Stmt*> defs;
  for(Stmt* def : getDefs(var))
    if(pm.isTopLevel(def))
      defs.insert(def);
    else
      defs.insert(pm.getTopLevelAncestor(def));
  return defs;
}

const Set<Stmt*>& AST::getDefs(VarDecl* var) const {
  return defMap.at(var);
}

unsigned AST::getNumDefs(VarDecl* var) const {
  return getDefs(var).size();
}

bool AST::isDefined(VarDecl* var) const {
  return getNumDefs(var);
}

bool AST::hasZeroDefs(VarDecl* var) const {
  return getNumDefs(var) == 0;
}

bool AST::hasSingleDef(VarDecl* var) const {
  return getNumDefs(var) == 1;
}

Stmt* AST::getSingleDef(VarDecl* var) const {
  if(hasSingleDef(var))
    return *getDefs(var).begin();
  return nullptr;
}

Expr* AST::getSingleDefRHS(VarDecl* var) const {
  if(hasSingleDef(var))
    return cast<BinaryOperator>(getSingleDef(var))->getRHS();
  return nullptr;
}

Set<Stmt*> AST::getTopLevelUses(VarDecl* var) const {
  Set<Stmt*> uses;
  for(Stmt* use : getUses(var))
    if(pm.isTopLevel(use))
      uses.insert(use);
    else
      uses.insert(pm.getTopLevelAncestor(use));
  return uses;
}

const Set<Stmt*>& AST::getUses(VarDecl* var) const {
  return useMap.at(var);
}

unsigned AST::getNumUses(VarDecl* var) const {
  return useMap.at(var).size();
}

bool AST::isUsed(VarDecl* var) const {
  return getNumUses(var);
}

bool AST::hasZeroUses(VarDecl* var) const {
  return getNumUses(var) == 0;
}

bool AST::hasSingleUse(VarDecl* var) const {
  return getNumUses(var) == 1;
}

Stmt* AST::getSingleUse(VarDecl* var) const {
  if(hasSingleUse(var))
    return *useMap.at(var).begin();
  return nullptr;
}

void AST::recalculateCFG() {
  stmtParents.reset(nullptr);
  cfg.reset(nullptr);
  cfgStmtMap.reset(nullptr);

  Stmt* body = decl->getBody();
  stmtParents.reset(new clang::ParentMap(body));
  cfg = CFG::buildCFG(decl, body, &astContext, cfgBuildOpts);
  cfgStmtMap.reset(CFGStmtMap::Build(cfg.get(), stmtParents.get()));

  // FIXME: The dominator tree calculation fails because there is some
  // inconsistency when constructing the CFG. This is possible because the AST
  // may not be in the form that it expects because I am screwing up somewhere
  // along the way. At some point this should be fixed
  //
  // dt->getBase().recalculate(*cfg);
}

void AST::recalculate() {
  recalculateCFG();
}

void AST::setFunctionBody(Stmt* body) {
  decl->setBody(body);
  pm.add(body, nullptr);
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
  useMap[param].clear();
  defMap[param].clear();
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
  globals.insert(g);
  useMap[g].clear();
  defMap[g].clear();

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
  locals.insert(local);
  useMap[local].clear();
  defMap[local].clear();

  return local;
}

DeclRefExpr* AST::createVariable(const std::string& name,
                                 QualType type,
                                 DeclContext* parent) {
  VarDecl* var = VarDecl::Create(astContext,
                                 parent,
                                 invLoc,
                                 invLoc,
                                 &getIdentifierInfo(name),
                                 type,
                                 nullptr,
                                 SC_None);

  return createDeclRefExpr(var);
}

CXXBoolLiteralExpr* AST::createBoolLiteral(bool b) {
  return createBoolLiteral(b, astContext.BoolTy);
}

CXXBoolLiteralExpr* AST::createBoolLiteral(bool b, QualType type) {
  CXXBoolLiteralExpr* blit
      = new(astContext) CXXBoolLiteralExpr(b, type, invLoc);
  en.add(blit);

  return blit;
}

CharacterLiteral* AST::createCharacterLiteral(unsigned c) {
  CharacterLiteral* clit = new(astContext)
      CharacterLiteral(c, CharacterLiteral::Ascii, astContext.CharTy, invLoc);
  en.add(clit);

  return clit;
}

IntegerLiteral* AST::createIntLiteral(const llvm::APInt& i, QualType type) {
  IntegerLiteral* ilit = IntegerLiteral::Create(astContext, i, type, invLoc);
  en.add(ilit);

  return ilit;
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
  FloatingLiteral* flit
      = FloatingLiteral::Create(astContext, f, false, type, invLoc);
  en.add(flit);

  return flit;
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
  StringLiteral* slit = StringLiteral::Create(
      astContext, str, StringLiteral::Ascii, false, type, invLoc);
  en.add(slit);

  return slit;
}

StringLiteral* AST::createStringLiteral(const std::string& str, QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

StringLiteral* AST::createStringLiteral(const char* str, QualType type) {
  return createStringLiteral(llvm::StringRef(str), type);
}

CXXNullPtrLiteralExpr* AST::createNullptr(QualType type) {
  CXXNullPtrLiteralExpr* nlit
      = new(astContext) CXXNullPtrLiteralExpr(type, invLoc);
  en.add(nlit);

  return nlit;
}

InitListExpr* AST::createInitListExpr(const Vector<Expr*>& exprs) {
  InitListExpr* initList = new(astContext)
      InitListExpr(astContext, invLoc, LLVM::makeArrayRef(exprs), invLoc);
  for(Expr* expr : exprs)
    pm.add(expr, initList);
  en.add(initList);

  return initList;
}

UnaryOperator*
AST::createUnaryOperator(Expr* expr, UnaryOperator::Opcode opc, QualType type) {
  UnaryOperator* unOp = new(astContext)
      UnaryOperator(expr, opc, type, VK_RValue, OK_Ordinary, invLoc, false);
  pm.add(expr, unOp);
  en.add(unOp);
  if(VarDecl* var = Clang::getVar(expr)) {
    addUse(var, unOp);
    if(opc == UO_AddrOf)
      addrTaken.insert(var);
  }

  return unOp;
}

BinaryOperator* AST::createBinaryOperator(Expr* lhs,
                                          Expr* rhs,
                                          BinaryOperator::Opcode opc,
                                          QualType type) {
  BinaryOperator* binOp = new(astContext) BinaryOperator(
      lhs, rhs, opc, type, VK_RValue, OK_Ordinary, invLoc, FPOptions());
  pm.add(lhs, binOp);
  pm.add(rhs, binOp);
  en.add(binOp);
  if(binOp->getOpcode() == BO_Assign) {
    if(VarDecl* var = Clang::getVar(lhs))
      addDef(var, binOp);
  } else {
    if(VarDecl* var = Clang::getVar(lhs))
      addUse(var, binOp);
  }
  if(VarDecl* var = Clang::getVar(rhs))
    addUse(var, binOp);

  return binOp;
}

ConditionalOperator*
AST::createConditionalOperator(Expr* cond, Expr* t, Expr* f, QualType type) {
  ConditionalOperator* condOp = new(astContext) ConditionalOperator(
      cond, invLoc, t, invLoc, f, type, VK_LValue, OK_Ordinary);
  pm.add(cond, condOp);
  pm.add(t, condOp);
  pm.add(f, condOp);
  en.add(condOp);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, condOp);
  if(VarDecl* var = Clang::getVar(t))
    addUse(var, condOp);
  if(VarDecl* var = Clang::getVar(f))
    addUse(var, condOp);

  return condOp;
}

ArraySubscriptExpr*
AST::createArraySubscriptExpr(Expr* base, Expr* idx, QualType type) {
  ArraySubscriptExpr* arrExpr = new(astContext)
      ArraySubscriptExpr(base, idx, type, VK_RValue, OK_Ordinary, invLoc);
  pm.add(base, arrExpr);
  pm.add(idx, arrExpr);
  en.add(arrExpr);
  if(VarDecl* var = Clang::getVar(base))
    addUse(var, arrExpr);
  if(VarDecl* var = Clang::getVar(idx))
    addUse(var, arrExpr);

  return arrExpr;
}

CallExpr*
AST::createCallExpr(Expr* callee, const Vector<Expr*>& args, QualType type) {
  CallExpr* callExpr = CallExpr::Create(
      astContext, callee, LLVM::makeArrayRef(args), type, VK_RValue, invLoc);
  pm.add(callee, callExpr);
  for(Expr* arg : callExpr->arguments())
    pm.add(arg, callExpr);
  en.add(callExpr);
  if(VarDecl* var = Clang::getVar(callee))
    addUse(var, callExpr);
  for(Expr* arg : args)
    if(VarDecl* var = Clang::getVar(arg))
      addUse(var, callExpr);

  return callExpr;
}

CStyleCastExpr* AST::createCastExpr(Expr* expr, QualType qty) {
  CStyleCastExpr* castExpr
      = CStyleCastExpr::Create(astContext,
                               qty,
                               VK_RValue,
                               CastKind::CK_BitCast,
                               expr,
                               nullptr,
                               astContext.CreateTypeSourceInfo(qty),
                               invLoc,
                               invLoc);
  pm.add(expr, castExpr);
  en.add(castExpr);
  if(VarDecl* var = Clang::getVar(expr))
    addUse(var, castExpr);

  return castExpr;
}

MemberExpr*
AST::createMemberExpr(Expr* base, ValueDecl* member, QualType type) {
  MemberExpr* memberExpr
      = MemberExpr::Create(astContext,
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
  pm.add(base, memberExpr);
  en.add(memberExpr);
  if(VarDecl* var = Clang::getVar(base))
    addUse(var, memberExpr);

  return memberExpr;
}

DeclRefExpr* AST::createDeclRefExpr(ValueDecl* decl) {
  DeclRefExpr* declRef = DeclRefExpr::Create(astContext,
                                             NestedNameSpecifierLoc(),
                                             invLoc,
                                             decl,
                                             false,
                                             invLoc,
                                             decl->getType(),
                                             VK_LValue,
                                             decl);
  en.add(declRef);

  return declRef;
}

NullStmt* AST::createNullStmt() {
  return new(astContext) NullStmt(invLoc);
}

CompoundStmt* AST::createCompoundStmt(const Vector<Stmt*>& stmts) {
  CompoundStmt* compoundStmt = CompoundStmt::Create(
      astContext, LLVM::makeArrayRef(stmts), invLoc, invLoc);
  for(Stmt* stmt : stmts) {
    pm.add(stmt, compoundStmt);
    if(auto* binOp = dyn_cast<BinaryOperator>(stmt)) {
      if(binOp->getOpcode() == BO_Assign) {

      } else {
      }
    }
  }

  return compoundStmt;
}

CompoundStmt* AST::createCompoundStmt(Stmt* stmt) {
  CompoundStmt* compoundStmt = CompoundStmt::Create(
      astContext, llvm::ArrayRef<Stmt*>(stmt), invLoc, invLoc);
  pm.add(stmt, compoundStmt);

  return compoundStmt;
}

IfStmt* AST::createIfStmt(Expr* cond, Stmt* thn, Stmt* els) {
  IfStmt* ifStmt = IfStmt::Create(
      astContext, invLoc, false, nullptr, nullptr, cond, thn, invLoc, els);
  pm.add(cond, ifStmt);
  pm.add(thn, ifStmt);
  if(els)
    pm.add(els, ifStmt);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, ifStmt);

  return ifStmt;
}

SwitchStmt* AST::createSwitchStmt(Expr* cond, Stmt* body) {
  SwitchStmt* switchStmt
      = SwitchStmt::Create(astContext, nullptr, nullptr, cond);
  switchStmt->setBody(body);
  pm.add(cond, switchStmt);
  pm.add(body, switchStmt);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, switchStmt);

  return switchStmt;
}

CaseStmt* AST::createCaseStmt(Expr* value, Stmt* body) {
  CaseStmt* caseStmt
      = CaseStmt::Create(astContext, value, nullptr, invLoc, invLoc, invLoc);
  caseStmt->setSubStmt(body);
  pm.add(body, caseStmt);

  return caseStmt;
}

DefaultStmt* AST::createDefaultStmt(Stmt* body) {
  DefaultStmt* defaultStmt = new(astContext) DefaultStmt(invLoc, invLoc, body);
  pm.add(body, defaultStmt);

  return defaultStmt;
}

DoStmt* AST::createDoStmt(Stmt* body, Expr* cond) {
  DoStmt* doStmt = new(astContext) DoStmt(body, cond, invLoc, invLoc, invLoc);
  pm.add(cond, doStmt);
  pm.add(body, doStmt);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, doStmt);

  return doStmt;
}

ForStmt* AST::createForStmt(Stmt* init, Expr* cond, Expr* inc, Stmt* body) {
  ForStmt* forStmt = new(astContext) ForStmt(
      astContext, init, cond, nullptr, inc, body, invLoc, invLoc, invLoc);
  if(init)
    pm.add(init, forStmt);

  pm.add(cond, forStmt);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, forStmt);

  if(inc)
    pm.add(inc, forStmt);
  pm.add(body, forStmt);

  return forStmt;
}

WhileStmt* AST::createWhileStmt(Expr* cond, Stmt* body) {
  WhileStmt* whileStmt
      = WhileStmt::Create(astContext, nullptr, cond, body, invLoc);
  pm.add(cond, whileStmt);
  pm.add(body, whileStmt);
  if(VarDecl* var = Clang::getVar(cond))
    addUse(var, whileStmt);

  return whileStmt;
}

ReturnStmt* AST::createReturnStmt(Expr* expr) {
  ReturnStmt* retStmt = ReturnStmt::Create(astContext, invLoc, expr, nullptr);
  if(expr) {
    pm.add(expr, retStmt);
    if(VarDecl* var = Clang::getVar(expr))
      addUse(var, retStmt);
  }

  return retStmt;
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
  auto* group = new(astContext) DeclGroupRef(decl);

  return new(astContext) DeclStmt(*group, invLoc, invLoc);
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
