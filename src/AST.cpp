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

#include <clang/AST/RecursiveASTVisitor.h>

#include <algorithm>

using namespace clang;

namespace cish {

static const Set<Expr*> emptyEqvExprs;

class FindVarsAddressTaken : public RecursiveASTVisitor<FindVarsAddressTaken> {
protected:
  Set<VarDecl*>& vars;

public:
  explicit FindVarsAddressTaken(Set<VarDecl*>& vars) : vars(vars) {
    ;
  }

  bool VisitUnaryOperator(UnaryOperator* unOp) {
    if(unOp->getOpcode() == UO_AddrOf)
      if(auto* ref = dyn_cast<DeclRefExpr>(unOp->getSubExpr()))
        if(auto* var = dyn_cast<VarDecl>(ref->getFoundDecl()))
          vars.insert(var);
    return true;
  }
};

AST::AST(CishContext& cishContext, FunctionDecl* decl)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      builder(cishContext.getASTBuilder()), decl(decl),
      defUseCalculator(createASTDefUseCalculatorPass(cishContext)),
      exprNumberingCalculator(createASTExprNumberingPass(cishContext)),
      dt(new DominatorTree()) {
  for(ParmVarDecl* param : decl->parameters()) {
    defMap[param].clear();
    useMap[param].clear();
    tlDefMap[param].clear();
    tlUseMap[param].clear();
  }
  for(Decl* d : decl->decls()) {
    if(auto* var = dyn_cast<VarDecl>(d)) {
      useMap[var].clear();
      defMap[var].clear();
      tlUseMap[var].clear();
      tlDefMap[var].clear();
    }
  }
}

AST::~AST() {
  delete defUseCalculator;
  delete exprNumberingCalculator;
}

unsigned AST::getDepth(Stmt* stmt) const {
  return stmtInfo.at(stmt).depth;
}

bool AST::isTopLevel(Stmt* stmt) const {
  return stmtInfo.contains(stmt);
}

clang::CompoundStmt* AST::getBodyFor(Stmt* stmt) const {
  return stmtInfo.at(stmt).body;
}

clang::Stmt* AST::getConstructFor(Stmt* stmt) const {
  return stmtInfo.at(stmt).ctrl;
}

bool AST::isDirectlyContainedIn(Stmt* needle, Stmt* haystack) const {
  return subStmts.contains(haystack) and subStmts.at(haystack).contains(needle);
}

bool AST::isContainedIn(Stmt* needle, Stmt* haystack) const {
  return descendants.contains(haystack)
         and descendants.at(haystack).contains(needle);
}

bool AST::isStructureFor(const FunctionDecl* f) const {
  return f == decl;
}

FunctionDecl* AST::getFunction() const {
  return decl;
}

const DominatorTree& AST::getDominatorTree() const {
  return *dt;
}

Stmt* AST::getParent(clang::Stmt* stmt) const {
  return stmtParents->getParent(stmt);
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

void AST::addChild(Stmt* stmt, CompoundStmt* body, Stmt* ctrl, unsigned depth) {
  subStmts[stmt];
  stmtInfo.emplace(stmt, StmtInfo(body, ctrl, depth));
  subStmts[ctrl].insert(stmt);
}

void AST::associateStmts(CompoundStmt* body, Stmt* ctrl, unsigned depth) {
  ctrls[body] = ctrl;
  subStmts[ctrl];
  for(Stmt* stmt : body->body()) {
    addChild(stmt, body, ctrl, depth);
    if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
      associateStmts(cast<CompoundStmt>(ifStmt->getThen()), ifStmt, depth + 1);
      if(auto* els = cast_or_null<CompoundStmt>(ifStmt->getElse()))
        associateStmts(els, ifStmt, depth + 1);
    } else if(auto* doStmt = dyn_cast<DoStmt>(stmt)) {
      loopStmts.push_back(doStmt);
      associateStmts(cast<CompoundStmt>(doStmt->getBody()), doStmt, depth + 1);
    } else if(auto* forStmt = dyn_cast<ForStmt>(stmt)) {
      loopStmts.push_back(forStmt);
      associateStmts(
          cast<CompoundStmt>(forStmt->getBody()), forStmt, depth + 1);
    } else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt)) {
      loopStmts.push_back(whileStmt);
      associateStmts(
          cast<CompoundStmt>(whileStmt->getBody()), whileStmt, depth + 1);
    } else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt)) {
      for(SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
          kase = kase->getNextSwitchCase()) {
        associateStmts(cast<CompoundStmt>(kase->getSubStmt()), kase, depth + 1);
      }
    }
  }
}

AST::ExprNum AST::addExpr(Expr* expr) {
  AST::ExprNum exprId = exprNums.size() + 1;
  eqvExprs[exprId].insert(expr);

  return exprNums[expr] = exprId;
}

bool AST::hasExprNum(Expr* expr) const {
  return exprNums.contains(expr);
}

AST::ExprNum AST::getExprNum(Expr* expr) const {
  return exprNums.at(expr);
}

const Set<Expr*>& AST::getEqvExprs(Expr* expr) const {
  if(not eqvExprs.contains(exprNums.at(expr)))
    return emptyEqvExprs;
  return eqvExprs.at(exprNums.at(expr));
}

void AST::addDef(VarDecl* var, Stmt* user) {
  defMap[var].insert(user);
}

void AST::addUse(VarDecl* var, Stmt* user) {
  useMap[var].insert(user);
}

void AST::addExprUse(Expr* expr, Stmt* user) {
  exprUseMap[expr].insert(user);
}

void AST::addTopLevelDef(VarDecl* var, Stmt* user) {
  tlDefMap[var].insert(user);
}

void AST::addTopLevelUse(Stmt* stmt, Stmt* user) {
  for(VarDecl* var : getVarsInStmt(stmt))
    tlUseMap[var].insert(user);
}

void AST::removeUse(VarDecl* var, Stmt* stmt) {
  useMap.at(var).erase(stmt);
  tlUseMap.at(var).erase(stmt);
}

void AST::removeDef(VarDecl* var, Stmt* stmt) {
  defMap.at(var).erase(stmt);
  tlDefMap.at(var).erase(stmt);
}

bool AST::replace(UnaryOperator* unOp, Expr* repl) {
  bool changed = false;

  changed |= (unOp->getSubExpr() != repl);
  unOp->setSubExpr(repl);

  return changed;
}

bool AST::replace(BinaryOperator* binOp, Expr* repl, bool lhs) {
  bool changed = false;

  if(lhs) {
    changed |= (binOp->getLHS() != repl);
    binOp->setLHS(repl);
  } else {
    changed |= (binOp->getRHS() != repl);
    llvm::errs() << "Replacing " << toString(binOp->getRHS(), astContext)
                 << " => " << toString(repl, astContext) << " in "
                 << toString(binOp, astContext) << "\n";
    binOp->setRHS(repl);
  }

  return changed;
}

bool AST::replace(ArraySubscriptExpr* arrExpr, Expr* repl, bool base) {
  bool changed = false;

  if(base) {
    changed |= (arrExpr->getBase() != repl);
    arrExpr->setLHS(repl);
  } else {
    changed |= (arrExpr->getIdx() != repl);
    arrExpr->setRHS(repl);
  }

  return changed;
}

bool AST::replace(CallExpr* callExpr, Expr* repl, long arg) {
  bool changed = false;

  if(arg < 0) {
    changed |= (callExpr->getCallee() != repl);
    callExpr->setCallee(repl);
  } else {
    changed |= (callExpr->getArg(arg) != repl);
    callExpr->setArg(arg, repl);
  }

  return changed;
}

bool AST::replace(CStyleCastExpr* castExpr, Expr* repl) {
  bool changed = false;

  changed |= (castExpr->getSubExpr() != repl);
  castExpr->setSubExpr(repl);

  return changed;
}

bool AST::replace(MemberExpr* memberExpr, Expr* repl) {
  bool changed = false;

  changed |= (memberExpr->getBase() != repl);
  memberExpr->setBase(repl);

  return changed;
}

bool AST::replace(ReturnStmt* retStmt, Expr* repl) {
  bool changed = false;

  changed |= (retStmt->getRetValue() != repl);
  retStmt->setRetValue(repl);

  return changed;
}

bool AST::replace(IfStmt* ifStmt, Expr* repl) {
  bool changed = false;

  changed |= (ifStmt->getCond() != repl);
  ifStmt->setCond(repl);

  return changed;
}

bool AST::replace(SwitchStmt* switchStmt, Expr* repl) {
  bool changed = false;

  changed |= (switchStmt->getCond() != repl);
  switchStmt->setCond(repl);

  return changed;
}

bool AST::replace(DoStmt* doStmt, Expr* repl) {
  bool changed = false;

  changed |= (doStmt->getCond() != repl);
  doStmt->setCond(repl);

  return changed;
}

bool AST::replace(ForStmt* forStmt, Expr* repl, bool cond) {
  bool changed = false;

  if(cond) {
    changed |= (forStmt->getCond() != repl);
    forStmt->setCond(repl);
  } else {
    changed |= (forStmt->getInc() != repl);
    forStmt->setInc(repl);
  }

  return changed;
}

bool AST::replace(WhileStmt* whileStmt, Expr* repl) {
  bool changed = false;

  changed |= (whileStmt->getCond() != repl);
  whileStmt->setCond(repl);

  return changed;
}

bool AST::replace(Stmt* parent, Expr* expr, Expr* repl) {
  bool changed = false;

  if(auto* unOp = dyn_cast<UnaryOperator>(parent)) {
    if(unOp->getSubExpr() == expr)
      changed |= replace(unOp, repl);
  } else if(auto* binOp = dyn_cast<BinaryOperator>(parent)) {
    if(binOp->getLHS() == expr)
      changed |= replace(binOp, repl, true);
    else if(binOp->getRHS() == expr)
      changed |= replace(binOp, repl, false);
  } else if(auto* condOp = dyn_cast<ConditionalOperator>(parent)) {
    Expr* newCond = condOp->getCond();
    if(newCond == expr)
      newCond = repl;
    Expr* newTrue = condOp->getTrueExpr();
    if(newTrue == expr)
      newTrue = repl;
    Expr* newFalse = condOp->getFalseExpr();
    if(newFalse == expr)
      newFalse = repl;
    if((newCond == repl) or (newTrue == repl) or (newFalse == repl))
      changed |= replace(getParent(condOp),
                         condOp,
                         builder.createConditionalOperator(
                             newCond, newTrue, newFalse, newTrue->getType()));
  } else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(parent)) {
    if(arrExpr->getBase() == expr)
      changed |= replace(arrExpr, repl, true);
    else if(arrExpr->getIdx() == expr)
      changed |= replace(arrExpr, repl, false);
  } else if(auto* callExpr = dyn_cast<CallExpr>(parent)) {
    if(callExpr->getCallee() == expr)
      changed |= replace(callExpr, repl, -1);
    for(unsigned i = 0; i < callExpr->getNumArgs(); i++)
      if(callExpr->getArg(i) == expr)
        changed |= replace(callExpr, repl, i);
  } else if(auto* castExpr = dyn_cast<CStyleCastExpr>(parent)) {
    if(castExpr->getSubExpr() == expr)
      changed |= replace(castExpr, repl);
  } else if(auto* memberExpr = dyn_cast<MemberExpr>(parent)) {
    if(memberExpr->getBase() == expr)
      changed |= replace(memberExpr, repl);
  } else if(auto* retStmt = dyn_cast<ReturnStmt>(parent)) {
    if(retStmt->getRetValue() == expr)
      changed |= replace(retStmt, repl);
  } else if(auto* ifStmt = dyn_cast<IfStmt>(parent)) {
    if(ifStmt->getCond() == expr)
      changed |= replace(ifStmt, repl);
  } else if(auto* switchStmt = dyn_cast<SwitchStmt>(parent)) {
    if(switchStmt->getCond() == expr)
      changed |= replace(switchStmt, repl);
  } else if(auto* doStmt = dyn_cast<DoStmt>(parent)) {
    if(doStmt->getCond() == expr)
      changed |= replace(doStmt, repl);
  } else if(auto* forStmt = dyn_cast<ForStmt>(parent)) {
    for(BinaryOperator* binOp : getForInits(forStmt))
      changed |= replace((Stmt*)binOp, expr, repl);
    if(forStmt->getCond() == expr)
      changed |= replace(forStmt, repl, true);
    if(forStmt->getInc() == expr)
      changed |= replace(forStmt, repl, false);
  } else if(auto* whileStmt = dyn_cast<WhileStmt>(parent)) {
    if(whileStmt->getCond() == expr)
      changed |= replace(whileStmt, repl);
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

  Set<VarDecl*> vars = getVarsInStmt(expr);
  for(Expr* e : getEqvExprs(expr)) {
    if(e != expr) {
      changed |= replace(getParent(e), e, repl);
    }
  }

  return changed;
}

bool AST::replaceAllUsesWith(VarDecl* var, Expr* repl) {
  bool changed = false;

  // The DeclRef's are uniqued, so this will always return the same value for
  // a given VarDecl
  DeclRefExpr* ref = builder.createDeclRefExpr(var);
  for(Stmt* use : getUses(var)) {
    changed |= replace(use, ref, repl);
    removeUse(var, use);
  }

  return changed;
}

bool AST::replaceExprWith(Expr* expr, Expr* repl) {
  bool changed = false;

  changed |= replace(getParent(expr), expr, repl);

  return changed;
}

bool AST::replaceStmtWith(Stmt* old, Stmt* repl) {
  bool changed = false;

  CompoundStmt* body = cast<CompoundStmt>(getParent(old));
  Stmt** stmts = body->body_begin();
  for(unsigned i = 0; i < body->size(); i++) {
    if(stmts[i] == old) {
      stmts[i] = repl;
      changed |= true;
    }
  }

  return changed;
}

bool AST::erase(Stmt* key) {
  bool changed = false;

  CompoundStmt* body = cast<CompoundStmt>(getParent(key));
  Stmt** stmts = body->body_begin();
  for(unsigned i = 0; i < body->size(); i++) {
    if(stmts[i] == key) {
      stmts[i] = builder.createNullStmt();
      for(VarDecl* var : getVarsInStmt(key)) {
        removeUse(var, key);
        removeDef(var, key);
      }
      changed |= true;
    }
  }

  return changed;
}

bool AST::erase(VarDecl* var) {
  if(not(hasZeroTopLevelUses(var) and hasZeroTopLevelDefs(var)))
    fatal(error() << "Cannot delete variable: " << var->getName() << "("
                  << getNumTopLevelDefs(var) << " defs, "
                  << getNumTopLevelUses(var) << " uses)");

  bool changed = false;
  for(VarDecl* local : vars())
    if(local == var)
      changed = true;

  locals.erase(var);
  decl->removeDecl(var);

  return changed;
}

Vector<VarDecl*> AST::getVars() const {
  return Vector<VarDecl*>(locals.begin(), locals.end());
}

Vector<Stmt*> AST::getLoops() const {
  return Vector<Stmt*>(loops().begin(), loops().end());
}

AST::var_range AST::vars() const {
  return var_range(locals.begin(), locals.end());
}

AST::tluse_range AST::tluses(VarDecl* var) const {
  return tluse_range(tlUseMap.at(var).begin(), tlUseMap.at(var).end());
}

AST::tldef_range AST::tldefs(VarDecl* var) const {
  return tldef_range(tlDefMap.at(var).begin(), tlDefMap.at(var).end());
}

AST::loop_range AST::loops() const {
  return loop_range(loopStmts.begin(), loopStmts.end());
}

AST::child_range AST::children(Stmt* stmt) const {
  return child_range(subStmts.at(stmt).begin(), subStmts.at(stmt).end());
}

AST::tl_range AST::tlstmts() const {
  return tl_range(stmtInfo.keys().begin(), stmtInfo.keys().end());
}

bool AST::hasAddressTaken(VarDecl* var) const {
  return addrTaken.contains(var);
}

Set<Stmt*> AST::getDefs(VarDecl* var) {
  return Set<Stmt*>(defMap.at(var).begin(), defMap.at(var).end());
}

unsigned AST::getNumDefs(VarDecl* var) const {
  return defMap.at(var).size();
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
    return *defMap.at(var).begin();
  return nullptr;
}

Expr* AST::getSingleDefRHS(VarDecl* var) const {
  if(hasSingleDef(var))
    return cast<BinaryOperator>(getSingleDef(var))->getRHS();
  return nullptr;
}

unsigned AST::getNumTopLevelDefs(VarDecl* var) const {
  return tlDefMap.at(var).size();
}

bool AST::hasZeroTopLevelDefs(VarDecl* var) const {
  return getNumTopLevelDefs(var) == 0;
}

bool AST::hasSingleTopLevelDef(VarDecl* var) const {
  return getNumTopLevelDefs(var) == 1;
}

Vector<Stmt*> AST::getTopLevelDefs(VarDecl* var) const {
  return Vector<Stmt*>(tldefs(var).begin(), tldefs(var).end());
}

Set<Stmt*> AST::getTopLevelDefsSet(VarDecl* var) const {
  return Set<Stmt*>(tldefs(var).begin(), tldefs(var).end());
}

Set<Stmt*> AST::getUses(VarDecl* var) {
  return Set<Stmt*>(useMap.at(var).begin(), useMap.at(var).end());
}

bool AST::isUsed(VarDecl* var) const {
  return getNumUses(var);
}

bool AST::hasZeroUses(VarDecl* var) const {
  return getNumUses(var) == 0;
}

bool AST::hasZeroTopLevelUses(VarDecl* var) const {
  return getNumTopLevelUses(var) == 0;
}

bool AST::hasSingleTopLevelUse(VarDecl* var) const {
  return getNumTopLevelUses(var) == 1;
}

bool AST::hasSingleUse(VarDecl* var) const {
  return getNumUses(var) == 1;
}

Stmt* AST::getSingleUse(VarDecl* var) const {
  if(hasSingleUse(var))
    return *useMap.at(var).begin();
  return nullptr;
}

unsigned AST::getNumUses(VarDecl* var) const {
  return useMap.at(var).size();
}

Vector<Stmt*> AST::getTopLevelUses(VarDecl* var) const {
  return Vector<Stmt*>(tluses(var).begin(), tluses(var).end());
}

Set<Stmt*> AST::getTopLevelUsesSet(VarDecl* var) const {
  return Set<Stmt*>(tluses(var).begin(), tluses(var).end());
}

unsigned AST::getNumTopLevelUses(VarDecl* var) const {
  return tlUseMap.at(var).size();
}

static bool isConstruct(const Stmt* stmt) {
  return isa<IfStmt>(stmt) or isa<DoStmt>(stmt) or isa<ForStmt>(stmt)
         or isa<WhileStmt>(stmt);
}

void AST::recalculateCFG() {
  stmtParents.reset(nullptr);
  cfg.reset(nullptr);
  cfgStmtMap.reset(nullptr);

  Stmt* body = decl->getBody();
  stmtParents.reset(new ParentMap(body));
  cfg = CFG::buildCFG(decl, body, &astContext, cfgBuildOpts);
  cfgStmtMap.reset(CFGStmtMap::Build(cfg.get(), stmtParents.get()));
  // dt->getBase().recalculate(*cfg);
}

void AST::recalculateDefUse() {
  addrTaken.clear();
  for(VarDecl* var : vars()) {
    useMap[var].clear();
    tlUseMap[var].clear();
    defMap[var].clear();
    tlDefMap[var].clear();
  }

  // Compute statement-level use-def information
  // This will associate a variable with the statement in which it is used
  // directly. The statement may be a subexpression
  defUseCalculator->runOnFunction(decl);

  // Find variables who address is taken. These can never be optimized safely
  FindVarsAddressTaken(addrTaken).TraverseDecl(decl);
}

void AST::recalculateStructure() {
  eqvExprs.clear();
  exprNums.clear();

  stmtInfo.clear();
  ctrls.clear();
  loopStmts.clear();
  subStmts.clear();
  descendants.clear();

  // Compute a map from parent to child. This is for faster lookups because
  // although each Stmt has a children() property, it only iterates over them
  // There is no way to query this easily. Clang has a ParentMap that operates
  // in the other direction, but it's not terribly useful.
  associateStmts(cast<CompoundStmt>(decl->getBody()), nullptr, 0);

  // Compute the closure of the children of each statement. This makes it
  // easier to query parent-descendant relationships. Mostly useful when
  // determining whether or a not a statement is in a (potentially deeply
  // nested) loop
  unsigned maxDepth
      = (*std::max_element(stmtInfo.values().begin(),
                           stmtInfo.values().end(),
                           [](const StmtInfo& a, const StmtInfo& b) {
                             return a.depth < b.depth;
                           }))
            .depth;
  descendants = subStmts;
  for(unsigned depth = maxDepth; depth >= 1; depth--)
    for(Stmt* stmt : subStmts.keys())
      if((not stmt)
         or (isConstruct(stmt) and (stmtInfo.at(stmt).depth == (depth - 1))))
        for(auto& j : subStmts.at(stmt))
          descendants[stmt].insert(descendants.at(j));
  for(auto& j : subStmts.at(nullptr))
    descendants[nullptr].insert(descendants.at(j));
}

void AST::recalculate(bool defUseOnly) {
  locals.clear();
  for(Decl* d : decl->decls())
    if(auto* var = dyn_cast<VarDecl>(d))
      if(not isa<ParmVarDecl>(var))
        locals.push_back(var);

  // If we only need to update the def-use information, then the structure
  // hasn't changed and we don't need to recalculate the parent-child
  // relationships between statements or the CFG
  if(not defUseOnly) {
    recalculateStructure();
    recalculateCFG();
  }
  exprNumberingCalculator->runOnFunction(decl);
  recalculateDefUse();
}

} // namespace cish
