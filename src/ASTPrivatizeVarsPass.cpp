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

#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "List.h"
#include "Set.h"

#include <clang/AST/RecursiveASTVisitor.h>

using namespace clang;

extern bool g_dbg;

namespace cish {

// Move variables declarations from the start of the program to somewhere
// lower if they can be safely privatized
class ASTPrivatizeVarsPass : public ASTFunctionPass<ASTPrivatizeVarsPass> {
protected:
  Set<ForStmt*> forLoops;

protected:
  bool allUsesInLoop(VarDecl* var, ForStmt* forStmt) {
    for(Stmt* use : um.getUses(var))
      if(not pm.isContainedIn(use, forStmt))
        return false;
    return true;
  }

  bool allInitsArePrivateToLoop(ForStmt* forStmt) {
    if(forStmt->getInit()) {
      for(BinaryOperator* init : Clang::getForInits(forStmt))
        if(not allUsesInLoop(Clang::getVar(init->getLHS()), forStmt))
          return false;
      return true;
    }
    return false;
  }

  bool allInitsHaveSameType(ForStmt* forStmt) {
    Set<const Type*> types;
    for(BinaryOperator* init : Clang::getForInits(forStmt))
      types.insert(Clang::getVar(init->getLHS())->getType().getTypePtr());
    return types.size() == 1;
  }

  bool isPrivatizable(VarDecl* var, Stmt* def) {
    unsigned depth = pm.getDepth(def);
    if(depth <= 2)
      return false;
    for(Stmt* use : um.getUses(var))
      if(pm.getDepth(use) < depth)
        return false;
    return true;
  }

  void getVarDefs(CompoundStmt* body,
                  List<std::pair<VarDecl*, BinaryOperator*>>& vars) {
    for(Stmt* stmt : body->body()) {
      if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(ifStmt->getThen()), vars);
        if(Stmt* els = ifStmt->getElse())
          getVarDefs(cast<CompoundStmt>(els), vars);
      } else if(auto* doStmt = dyn_cast<DoStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(doStmt->getBody()), vars);
      } else if(auto* forStmt = dyn_cast<ForStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(forStmt->getBody()), vars);
      } else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(whileStmt->getBody()), vars);
      } else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(switchStmt->getBody()), vars);
      } else if(auto* caseStmt = dyn_cast<CaseStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(caseStmt->getSubStmt()), vars);
      } else if(auto* defaultStmt = dyn_cast<DefaultStmt>(stmt)) {
        getVarDefs(cast<CompoundStmt>(defaultStmt->getSubStmt()), vars);
      } else if(auto* binOp = dyn_cast<BinaryOperator>(stmt)) {
        if(VarDecl* var = Clang::getVar(binOp->getLHS()))
          vars.push_back(std::make_pair(var, binOp));
      }
    }
  }

  List<std::pair<VarDecl*, BinaryOperator*>> getVarDefs(FunctionDecl* f) {
    List<std::pair<VarDecl*, BinaryOperator*>> vars;

    getVarDefs(cast<CompoundStmt>(f->getBody()), vars);

    return vars;
  }

public:
  bool process(ForStmt* forStmt, Stmt*) {
    if(allInitsArePrivateToLoop(forStmt) and allInitsHaveSameType(forStmt))
      forLoops.insert(forStmt);

    return false;
  }

  bool process(FunctionDecl* f, Stmt*) {
    bool changed = false;

    for(ForStmt* forStmt : forLoops) {
      if(Stmt* init = forStmt->getInit()) {
        Vector<Decl*> newVars;
        for(BinaryOperator* binOp : Clang::getForInits(forStmt)) {
          VarDecl* var = Clang::getVar(binOp->getLHS());
          VarDecl* newVar
              = ast->createVariable(var->getName(), var->getType(), f);
          Expr* newDeclRef = ast->createDeclRefExpr(newVar);

          for(Expr* expr : em.getEqv(em.get(var)).clone()) {
            for(Stmt* use : um.getUses(var))
              if(pm.isContainedIn(use, forStmt))
                changed |= ast->replaceExprWith(expr, newDeclRef, use);

            for(Stmt* def : um.getDefs(var))
              if(pm.isContainedIn(def, forStmt))
                changed |= ast->replaceExprWith(expr, newDeclRef, def);
          }

          newVar->setInit(ast->cloneExpr(binOp->getRHS()));
          newVars.push_back(newVar);

          ast->erase(binOp, pm.getParent(binOp));
        }

        DeclStmt* declStmt = ast->createDeclStmt(newVars);
        forStmt->setInit(declStmt);
        changed |= true;
      }
    }

    List<std::pair<VarDecl*, BinaryOperator*>> varDefs = getVarDefs(f);
    Map<VarDecl*, unsigned> defs;
    for(const auto& p : varDefs) {
      VarDecl* var = p.first;
      defs[var] += 1;
    }

    for(auto& i : varDefs) {
      VarDecl* var = i.first;
      BinaryOperator* defStmt = i.second;

      llvm::errs() << var->getName() << ": " << defs[var] << " |"
                   << isPrivatizable(var, defStmt->getRHS()) << "|\n"
                   << "       " << Clang::toString(defStmt, astContext) << "\n";
      if((defs[var] == 1) and isPrivatizable(var, defStmt->getRHS())) {
        VarDecl* newVar
            = ast->createVariable(var->getName(), var->getType(), f);
        DeclRefExpr* newDeclRef = ast->createDeclRefExpr(newVar);
        Stmt* parent = pm.getParent(defStmt);

        for(Expr* expr : em.getEqv(em.get(var)).clone())
          for(Stmt* use : um.getUses(var))
            if(pm.isContainedIn(use, parent))
              changed |= ast->replaceExprWith(expr, newDeclRef, use);

        DeclStmt* declStmt = ast->createDeclStmt(newVar);
        changed |= ast->replaceStmtWith(defStmt, declStmt, parent);
        ast->erase(defStmt, parent);

        newVar->setInit(ast->cloneExpr(defStmt->getRHS()));
      }
    }

    return false;
  }

public:
  ASTPrivatizeVarsPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext, RequireExprNums | RequireUses | OnePass) {
    ;
  }

  ASTPrivatizeVarsPass(const ASTPrivatizeVarsPass&) = delete;
  ASTPrivatizeVarsPass(ASTPrivatizeVarsPass&&) = delete;
  virtual ~ASTPrivatizeVarsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-privatize";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish Privatize Vars";
  }
};

} // namespace cish

cish::ASTPass* createASTPrivatizeVarsPass(cish::CishContext& cishContext) {
  return new cish::ASTPrivatizeVarsPass(cishContext);
}
