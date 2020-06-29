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
      decl(decl), defUseCalculator(createASTDefUseCalculatorPass(cishContext)) {
  for(ParmVarDecl* param : decl->parameters()) {
    defMap[param].clear();
    useMap[param].clear();
    tlDefMap[param].clear();
    tlUseMap[param].clear();
  }
  for(Decl* decl : decl->decls()) {
    if(auto* var = dyn_cast<VarDecl>(decl)) {
      useMap[var].clear();
      defMap[var].clear();
      tlUseMap[var].clear();
      tlDefMap[var].clear();
    }
  }
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

CFGBlock* AST::getCFGBlock(Stmt* stmt) const {
  return cfgStmtMap->getBlock(stmt);
}

void AST::addChild(Stmt* stmt,
                   CompoundStmt* body,
                   Stmt* construct,
                   unsigned depth) {
  subStmts[stmt];
  stmtInfo.emplace(stmt, StmtInfo(body, construct, depth));
  subStmts[construct].insert(stmt);
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
        addChild(kase,
                 cast<CompoundStmt>(switchStmt->getBody()),
                 switchStmt,
                 depth + 1);
        associateStmts(cast<CompoundStmt>(kase->getSubStmt()), kase, depth + 1);
      }
    }
  }
}

static bool isConstruct(const Stmt* stmt) {
  return isa<IfStmt>(stmt) or isa<DoStmt>(stmt) or isa<ForStmt>(stmt)
         or isa<WhileStmt>(stmt);
}

void AST::addDef(VarDecl* var, Stmt* stmt) {
  defMap[var].push_back(stmt);
}

void AST::addUse(VarDecl* var, Stmt* stmt) {
  useMap[var].push_back(stmt);
}

void AST::addTopLevelDef(VarDecl* var, Stmt* stmt) {
  tlDefMap[var].insert(stmt);
}

void AST::addTopLevelUse(VarDecl* var, Stmt* stmt) {
  if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    if(binOp->getOpcode() == BO_Assign)
      if(auto* ref = dyn_cast<DeclRefExpr>(binOp->getLHS()))
        if(ref->getFoundDecl() == var)
          return addTopLevelDef(var, stmt);
  tlUseMap[var].insert(stmt);
}

void AST::removeUse(VarDecl* var, Stmt* stmt) {
  useMap.at(var).erase(stmt);
}

void AST::removeDef(VarDecl* var, Stmt* stmt) {
  defMap.at(var).erase(stmt);
}

void AST::removeVar(VarDecl* var) {
  if(not(hasZeroDefs(var) and hasZeroUses(var)))
    fatal(error() << "Cannot remove var: " << var->getName() << ". Defs: "
                  << getNumDefs(var) << ". Uses: " << getNumUses(var));
  defMap.erase(var);
  useMap.erase(var);
}

bool AST::replace(Expr* expr, VarDecl* var) {
  if(auto* declRefExpr = dyn_cast<DeclRefExpr>(expr))
    return declRefExpr->getFoundDecl() == var;
  return false;
}

bool AST::replace(BinaryOperator* binOp, VarDecl* var, Expr* repl) {
  bool changed = false;

  if(replace(binOp->getLHS(), var)) {
    binOp->setLHS(repl);
    changed |= true;
  }

  if(replace(binOp->getRHS(), var)) {
    binOp->setRHS(repl);
    changed |= true;
  }

  return changed;
}

bool AST::replace(UnaryOperator* unOp, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(unOp->getSubExpr(), var)))
    unOp->setSubExpr(repl);

  return changed;
}

bool AST::replace(ConditionalOperator* condOp, VarDecl* var, Expr* repl) {
  fatal(error() << "Not implemented\n");
}

bool AST::replace(ArraySubscriptExpr* arrExpr, VarDecl* var, Expr* repl) {
  bool changed = false;

  if(replace(arrExpr->getBase(), var)) {
    arrExpr->setLHS(repl);
    changed |= true;
  }

  if(replace(arrExpr->getIdx(), var)) {
    arrExpr->setRHS(repl);
    changed |= true;
  }

  return changed;
}

bool AST::replace(MemberExpr* memberExpr, VarDecl* var, Expr* repl) {
  return false;
}

bool AST::replace(CallExpr* callExpr, VarDecl* var, Expr* repl) {
  bool changed = false;

  for(unsigned i = 0; i < callExpr->getNumArgs(); i++) {
    if(replace(callExpr->getArg(i), var)) {
      callExpr->setArg(i, repl);
      changed |= true;
    }
  }
  if(replace(callExpr->getCallee(), var)) {
    callExpr->setCallee(repl);
    changed |= true;
  }

  return changed;
}

bool AST::replace(CastExpr* castExpr, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(castExpr->getSubExpr(), var)))
    castExpr->setSubExpr(repl);

  return changed;
}

bool AST::replace(ReturnStmt* retStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if(Expr* retValue = retStmt->getRetValue())
    if((changed |= replace(retValue, var)))
      retStmt->setRetValue(repl);

  return changed;
}

bool AST::replace(IfStmt* ifStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(ifStmt->getCond(), var)))
    ifStmt->setCond(repl);

  return changed;
}

bool AST::replace(DoStmt* doStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(doStmt->getCond(), var)))
    doStmt->setCond(repl);

  return changed;
}

bool AST::replace(ForStmt* forStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(forStmt->getCond(), var)))
    forStmt->setCond(repl);

  return changed;
}

bool AST::replace(WhileStmt* whileStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(whileStmt->getCond(), var)))
    whileStmt->setCond(repl);

  return changed;
}

bool AST::replace(SwitchStmt* switchStmt, VarDecl* var, Expr* repl) {
  bool changed = false;

  if((changed |= replace(switchStmt->getCond(), var)))
    switchStmt->setCond(repl);

  return changed;
}

bool AST::replace(Stmt* stmt, VarDecl* var, Expr* repl) {
  if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    return replace(binOp, var, repl);
  else if(auto* unOp = dyn_cast<UnaryOperator>(stmt))
    return replace(unOp, var, repl);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    return replace(condOp, var, repl);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(stmt))
    return replace(arrExpr, var, repl);
  else if(auto* memberExpr = dyn_cast<MemberExpr>(stmt))
    return replace(memberExpr, var, repl);
  else if(auto* callExpr = dyn_cast<CallExpr>(stmt))
    return replace(callExpr, var, repl);
  else if(auto* castExpr = dyn_cast<CastExpr>(stmt))
    return replace(castExpr, var, repl);
  else if(auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    return replace(retStmt, var, repl);
  else if(auto* ifStmt = dyn_cast<IfStmt>(stmt))
    return replace(ifStmt, var, repl);
  else if(auto* doStmt = dyn_cast<DoStmt>(stmt))
    return replace(doStmt, var, repl);
  else if(auto* forStmt = dyn_cast<ForStmt>(stmt))
    return replace(forStmt, var, repl);
  else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    return replace(whileStmt, var, repl);
  else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt))
    return replace(switchStmt, var, repl);
  else if(isa<CXXBoolLiteralExpr>(stmt) or isa<CharacterLiteral>(stmt)
          or isa<IntegerLiteral>(stmt) or isa<FloatingLiteral>(stmt)
          or isa<StringLiteral>(stmt) or isa<CXXNullPtrLiteralExpr>(stmt)
          or isa<LabelStmt>(stmt) or isa<GotoStmt>(stmt) or isa<BreakStmt>(stmt)
          or isa<ContinueStmt>(stmt) or isa<SwitchCase>(stmt))
    return false;
  else
    fatal(error() << "Unknown statement to replace in: "
                  << stmt->getStmtClassName());

  return false;
}

bool AST::replaceAllUsesWith(VarDecl* var, Expr* repl) {
  bool changed = false;

  for(Stmt* use : getUses(var)) {
    changed |= replace(use, var, repl);
    removeUse(var, use);
  }

  return changed;
}

Vector<VarDecl*> AST::getVars() const {
  return Vector<VarDecl*>(vars().begin(), vars().end());
}

Vector<Stmt*> AST::getLoops() const {
  return Vector<Stmt*>(loops().begin(), loops().end());
}

AST::var_range AST::vars() const {
  return var_range(defMap.keys().begin(), defMap.keys().end());
}

AST::def_range AST::defs(VarDecl* var) const {
  return def_range(defMap.at(var).begin(), defMap.at(var).end());
}

AST::use_range AST::uses(VarDecl* var) const {
  return use_range(useMap.at(var).begin(), useMap.at(var).end());
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

unsigned AST::getNumDefs(VarDecl* var) const {
  return defMap.at(var).size();
}

unsigned AST::getNumTopLevelDefs(VarDecl* var) const {
  return tlDefMap.at(var).size();
}

Vector<Stmt*> AST::getDefs(VarDecl* var) const {
  return Vector<Stmt*>(defs(var).begin(), defs(var).end());
}

Set<Stmt*> AST::getDefsSet(VarDecl* var) const {
  return Set<Stmt*>(defs(var).begin(), defs(var).end());
}

Vector<Stmt*> AST::getTopLevelDefs(VarDecl* var) const {
  return Vector<Stmt*>(tldefs(var).begin(), tldefs(var).end());
}

Set<Stmt*> AST::getTopLevelDefsSet(VarDecl* var) const {
  return Set<Stmt*>(tldefs(var).begin(), tldefs(var).end());
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

unsigned AST::getNumUses(VarDecl* var) const {
  return useMap.at(var).size();
}

Vector<Stmt*> AST::getUses(VarDecl* var) const {
  return Vector<Stmt*>(uses(var).begin(), uses(var).end());
}

Set<Stmt*> AST::getUsesSet(VarDecl* var) const {
  return Set<Stmt*>(uses(var).begin(), uses(var).end());
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

void AST::recalculate() {
  addrTaken.clear();
  stmtInfo.clear();
  ctrls.clear();
  loopStmts.clear();
  subStmts.clear();
  descendants.clear();

  Stmt* body = decl->getBody();
  stmtParents.reset(new ParentMap(body));
  cfg = CFG::buildCFG(decl, body, &astContext, cfgBuildOpts);
  cfgStmtMap.reset(CFGStmtMap::Build(cfg.get(), stmtParents.get()));

  // Compute statement-level use-def information
  // This will associate a variable with the statement in which it is used
  // directly. The statement may be a subexpression
  defUseCalculator->runOnFunction(decl);

  // Compute use-def information with respect to the top-level statements
  for(Stmt* stmt : tlstmts()) {
    if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
      for(VarDecl* var : getVarsInStmt(ifStmt->getCond()))
        addTopLevelUse(var, ifStmt);
    } else if(auto* doStmt = dyn_cast<DoStmt>(stmt)) {
      for(VarDecl* var : getVarsInStmt(doStmt->getCond()))
        addTopLevelUse(var, doStmt);
    } else if(auto* forStmt = dyn_cast<ForStmt>(stmt)) {
      for(Stmt* subStmt : Vector<Stmt*>{
              forStmt->getInit(), forStmt->getCond(), forStmt->getInc()})
        if(subStmt)
          for(VarDecl* var : getVarsInStmt(subStmt))
            addTopLevelUse(var, forStmt);
    } else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt)) {
      for(VarDecl* var : getVarsInStmt(whileStmt->getCond()))
        addTopLevelUse(var, whileStmt);
    } else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt)) {
      for(VarDecl* var : getVarsInStmt(switchStmt->getCond()))
        addTopLevelUse(var, switchStmt);
    } else if(auto* switchCase = dyn_cast<SwitchCase>(stmt)) {
      ;
    } else {
      for(VarDecl* var : getVarsInStmt(stmt))
        addTopLevelUse(var, stmt);
    }
  }

  // Find variables who address is taken. These can never be optimized safely
  FindVarsAddressTaken(addrTaken).TraverseDecl(decl);

  // Compute a map from parent to child. This is for faster lookups because
  // although each Stmt has a children() property, it only iterates over them
  // There is no way to query this easily. Clang has a ParentMap that operates
  // in the other direction, but it's not terribly useful.
  associateStmts(cast<CompoundStmt>(decl->getBody()), nullptr, 0);

  // Compute the closure of the children of each statement. This makes it easier
  // to query parent-descendant relationships. Mostly useful when determining
  // whether or a not a statement is in a (potentially deeply nested) loop
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

} // namespace cish
