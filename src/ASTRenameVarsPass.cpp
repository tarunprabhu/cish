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
#include "Map.h"
#include "NameGenerator.h"
#include "Options.h"
#include "Vector.h"

#include <clang/Analysis/CFG.h>

using namespace clang;

namespace cish {

class ASTRenameVarsPass : public ASTFunctionPass<ASTRenameVarsPass> {
protected:
  const Vector<std::string> loopVarsBase;

protected:
  std::string getNameFor(UnaryOperator* unOp) {
    if(unOp->getOpcode() == UO_AddrOf) {
      std::string newName = getNameFor(unOp->getSubExpr());
      if(newName.size())
        return newName + "_a";
    }
    return "";
  }

  std::string getNameFor(ArraySubscriptExpr* arrExpr) {
    std::string newName = getNameFor(arrExpr->getBase());
    if(newName.size())
      return newName + "_e";
    return "";
  }

  void parse(MemberExpr* memberExpr, Vector<std::string>& pieces) {
    pieces.push_back(memberExpr->getMemberDecl()->getName());
    if(auto* next = dyn_cast<MemberExpr>(memberExpr->getBase()))
      parse(next, pieces);
  }

  std::string getNameFor(MemberExpr* memberExpr) {
    Vector<std::string> pieces;
    parse(memberExpr, pieces);
    if(pieces.size()) {
      if(pieces.size() == 1)
        return pieces[0];
      else
        return pieces[1] + "_" + pieces[0];
    }
    return "";
  }

  std::string getNameFor(DeclRefExpr* declRef) {
    NameGenerator& names = cishContext.getNameGenerator();
    if(auto* var = dyn_cast<VarDecl>(declRef->getDecl()))
      if(not names.isGeneratedName(var->getName()))
        return var->getName();
    return "";
  }

  std::string getNameFor(Expr* expr) {
    if(auto* unOp = dyn_cast<UnaryOperator>(expr))
      return getNameFor(unOp);
    else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(expr))
      return getNameFor(arrExpr);
    else if(auto* memberExpr = dyn_cast<MemberExpr>(expr))
      return getNameFor(memberExpr);
    else if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
      return getNameFor(declRef);
    else if(isa<CXXBoolLiteralExpr>(expr))
      return "cb";
    else if(isa<CharacterLiteral>(expr))
      return "cc";
    else if(isa<IntegerLiteral>(expr))
      return "ci";
    else if(isa<FloatingLiteral>(expr))
      return "cf";
    else if(isa<StringLiteral>(expr))
      return "cs";
    return "";
  }

  VarDecl* getVarByName(const std::string& name) {
    for(VarDecl* var : ast->getVars())
      if(var->getName() == name)
        return var;
    return nullptr;
  }

  void
  getLoops(CompoundStmt* body, unsigned depth, Map<Stmt*, unsigned>& loops) {
    for(Stmt* stmt : body->body()) {
      if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
        getLoops(cast<CompoundStmt>(ifStmt->getThen()), depth, loops);
        if(Stmt* els = ifStmt->getElse())
          getLoops(cast<CompoundStmt>(els), depth, loops);
      } else if(auto* doStmt = dyn_cast<DoStmt>(stmt)) {
        loops[stmt] = depth;
        getLoops(cast<CompoundStmt>(doStmt->getBody()), depth + 1, loops);
      } else if(auto* forStmt = dyn_cast<ForStmt>(stmt)) {
        loops[stmt] = depth;
        getLoops(cast<CompoundStmt>(forStmt->getBody()), depth + 1, loops);
      } else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt)) {
        loops[stmt] = depth;
        getLoops(cast<CompoundStmt>(whileStmt->getBody()), depth + 1, loops);
      } else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt)) {
        for(SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
            kase = kase->getNextSwitchCase())
          if(CaseStmt* caseStmt = dyn_cast<CaseStmt>(kase))
            getLoops(cast<CompoundStmt>(caseStmt->getSubStmt()), depth, loops);
          else if(DefaultStmt* defStmt = dyn_cast<DefaultStmt>(kase))
            getLoops(cast<CompoundStmt>(defStmt->getSubStmt()), depth, loops);
      }
    }
  }

  Map<Stmt*, unsigned> getLoops() {
    Map<Stmt*, unsigned> loops;

    getLoops(cast<CompoundStmt>(ast->getFunction()->getBody()), 0, loops);

    return loops;
  }

  std::string getLoopVarName(unsigned depth, bool useSuffix, unsigned suffix) {
    std::string buf;
    llvm::raw_string_ostream ss(buf);
    std::string base = loopVarsBase[depth % loopVarsBase.size()];
    for(unsigned i = 0; i <= depth / loopVarsBase.size(); i++)
      ss << base;
    if(useSuffix)
      ss << suffix;

    return ss.str();
  }

  bool allInitializedVariablesLoopLocal(Stmt* init,
                                        ForStmt* loop,
                                        Vector<VarDecl*>& vars) {
    if(auto* binOp = dyn_cast<BinaryOperator>(init)) {
      Expr* lhs = binOp->getLHS();
      Expr* rhs = binOp->getRHS();
      if(binOp->getOpcode() == BO_Comma) {
        return allInitializedVariablesLoopLocal(lhs, loop, vars)
               and allInitializedVariablesLoopLocal(rhs, loop, vars);
      } else if(binOp->getOpcode() == BO_Assign) {
        VarDecl* var = cast<VarDecl>(cast<DeclRefExpr>(lhs)->getDecl());
        for(Stmt* use : ast->getUses(var))
          if(not ast->isContainedIn(use, loop))
            return false;
        vars.push_back(var);
      } else {
        fatal(error() << "Unexpected operator in loop initializer: "
                      << binOp->getOpcodeStr());
      }
    } else {
      fatal(error() << "Unexpected statement in loop initializer: "
                    << init->getStmtClassName());
    }

    return true;
  }

  Vector<VarDecl*> getLoopVariables(ForStmt* loop) {
    Vector<VarDecl*> vars;
    if(allInitializedVariablesLoopLocal(loop->getInit(), loop, vars))
      return vars;
    return Vector<VarDecl*>();
  }

  bool renameLoopVariables() {
    NameGenerator& names = cishContext.getNameGenerator();

    Map<VarDecl*, std::string> newLoopVars;
    for(auto& i : getLoops()) {
      Stmt* loop = i.first;
      unsigned loopDepth = i.second;
      if(auto* forStmt = dyn_cast<ForStmt>(loop)) {
        // All of the variables in the initializer of a for-stmt must
        // be local
        Vector<VarDecl*> loopVars = getLoopVariables(forStmt);
        for(unsigned i = 0; i < loopVars.size(); i++)
          newLoopVars[loopVars[i]]
              = getLoopVarName(loopDepth, loopVars.size() > 1, i);
      }
    }

    // Rename existing variables
    for(const auto& i : newLoopVars) {
      const std::string& name = i.second;
      if(VarDecl* var = getVarByName(name))
        var->setDeclName(ast->createDeclName(names.getNewVarName()));
    }

    // Rename loop variables
    for(auto& i : newLoopVars) {
      VarDecl* var = i.first;
      const std::string& name = i.second;
      var->setDeclName(ast->createDeclName(name));
    }

    return newLoopVars.size();
  }

public:
  bool process(FunctionDecl* f) {
    bool changed = false;

    NameGenerator& names = cishContext.getNameGenerator();
    changed |= renameLoopVariables();
    Set<VarDecl*> renamed;

    bool iterChanged = false;
    do {
      iterChanged = false;

      for(VarDecl* var : ast->getVars()) {
        if(names.isGeneratedName(var->getName()) and (not renamed.contains(var))
           and ast->hasSingleDef(var)) {
          Expr* rhs = ast->getSingleDefRHS(var);
          std::string newName = getNameFor(rhs);
          if(auto* declRef = dyn_cast<DeclRefExpr>(rhs))
            if(auto* param = dyn_cast<ParmVarDecl>(declRef->getDecl()))
              newName = param->getName().str() + "_p";
          if((var->getName() != newName) and newName.size()) {
            var->setDeclName(
                ast->createDeclName(names.getNewName(newName, false)));
            renamed.insert(var);
            iterChanged |= true;
          }
        }
      }

      changed |= iterChanged;
    } while(iterChanged);

    return changed;
  }

public:
  ASTRenameVarsPass(CishContext& context)
      : ASTFunctionPass(context), loopVarsBase({"i", "j", "k", "l"}) {
    ;
  }

  ASTRenameVarsPass(const CishContext&) = delete;
  ASTRenameVarsPass(CishContext&&) = delete;
  virtual ~ASTRenameVarsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-rename";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Rename Vars";
  }
};

} // namespace cish

cish::ASTPass* createASTRenameVarsPass(cish::CishContext& context) {
  return new cish::ASTRenameVarsPass(context);
}
