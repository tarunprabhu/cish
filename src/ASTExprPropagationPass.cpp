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
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "Map.h"
#include "NameGenerator.h"
#include "Set.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <llvm/Support/raw_ostream.h>

// This pass finds "short" expressions in the code that are assigned to some
// stack variable and "inlines" them instead. The idea is that having a lot of
// things put into a variable with a potentially unhelpful name like _t38 is
// not terribly readable. But at the opposite extreme, pushing all the
// expressions down could result in some truly hideous code. This is
// inherently going to be driven by heuristics. At some point, it might be nice
// to have some knobs that can be fiddled with to make the result look a little
// nicer under specific circumstances, but that might never happen

using namespace clang;

namespace cish {

static Vector<CFGBlock*> getSuccessors(CFGBlock* block) {
  // ***********************************************************************
  //                     FIXME: Possible bug here
  //
  // In some cases, a potentially unreachable block ends up here. This may
  // be the result of the AST getting construction not being correct
  // becausae of a bug in AST.cpp that does not do the replacements
  // correctly, but I can't seem to figure out if that is actually the
  // case because dumping the AST looks correct. As a temporary workaround
  // look for a potentially unreachable block and handle it correctly
  // Once it is properly fixed, this function should go away and wherever it
  // is used, just call the succs() method on the CFGBlock directly
  // ------------------------------------------------------------------------

  Vector<CFGBlock*> succs;
  for(CFGBlock::AdjacentBlock& adj : block->succs()) {
    if(adj.isReachable()) {
      if(CFGBlock* succ = adj.getReachableBlock())
        succs.push_back(succ);
    } else if(CFGBlock* succ = adj.getPossiblyUnreachableBlock()) {
      succs.push_back(succ);
    }
  }

  return succs;
}

class ASTPropagateExprsPass : public ASTFunctionPass<ASTPropagateExprsPass> {
protected:
  bool isDefBeforeUseInRootBlock(Set<Stmt*>& rhsDefs,
                                 CFGBlock* rootBlock,
                                 Stmt* def,
                                 Set<Stmt*>& lhsUses) {
    bool sawLhsDef = false;
    for(CFGElement& cfgElem : *rootBlock) {
      if(CFGStmt* cfgStmt = cfgElem.getAs<CFGStmt>().getPointer()) {
        Stmt* stmt = const_cast<Stmt*>(cfgStmt->getStmt());
        if(stmt == def) {
          sawLhsDef = true;
        } else if(sawLhsDef) {
          if(rhsDefs.contains(stmt))
            return true;
          else if(lhsUses.contains(stmt))
            lhsUses.erase(stmt);
        }
      } else {
        fatal(error() << "Unexpected CFG element kind");
      }
    }
    Stmt* term = rootBlock->getTerminator();
    if(sawLhsDef) {
      if(rhsDefs.contains(term))
        return true;
      else if(lhsUses.contains(term))
        lhsUses.erase(term);
    }

    return false;
  }

  bool isDefBeforeUseInBlock(const Set<Stmt*>& defs,
                             CFGBlock* block,
                             bool defSeen,
                             Set<CFGBlock*>& seen,
                             Set<Stmt*>& uses) {
    if(seen.contains(block))
      return false;
    else if(uses.size() == 0)
      return false;

    for(CFGElement& cfgElem : *block) {
      if(CFGStmt* cfgStmt = cfgElem.getAs<CFGStmt>().getPointer()) {
        Stmt* stmt = const_cast<Stmt*>(cfgStmt->getStmt());
        if(defs.contains(stmt)) {
          defSeen |= true;
        } else if(uses.contains(stmt)) {
          uses.erase(stmt);
          if(defSeen)
            return true;
        }
      } else {
        fatal(error() << "Unexpected CFG element kind");
      }
    }
    Stmt* term = block->getTerminator();
    if(defs.contains(term)) {
      defSeen |= true;
    } else if(uses.contains(term)) {
      uses.erase(term);
      if(defSeen)
        return true;
    }

    seen.insert(block);
    for(CFGBlock* succ : getSuccessors(block))
      if(succ and isDefBeforeUseInBlock(defs, succ, defSeen, seen, uses))
        return true;

    return false;
  }

  bool isShort(const UnaryOperator* unOp) {
    switch(unOp->getOpcode()) {
    case UO_PostInc:
    case UO_PostDec:
    case UO_PreInc:
    case UO_PreDec:
    case UO_Not:
    case UO_LNot:
    case UO_Minus:
      return isShort(unOp->getSubExpr());
    default:
      break;
    }
    return false;
  }

  bool isShort(const Expr* expr) {
    if(isa<CXXBoolLiteralExpr>(expr) or isa<CXXNullPtrLiteralExpr>(expr)
       or isa<IntegerLiteral>(expr) or isa<FloatingLiteral>(expr)
       or isa<CharacterLiteral>(expr))
      return true;
    else if(isa<DeclRefExpr>(expr))
      return true;
    else if(auto* unOp = dyn_cast<UnaryOperator>(expr))
      return isShort(unOp);
    return false;
  }

  bool isReplaceable(const Expr* expr) {
    if(const auto* binOp = dyn_cast<BinaryOperator>(expr)) {
      switch(binOp->getOpcode()) {
      case BO_Add:
      case BO_Sub:
      case BO_Mul:
      case BO_Div:
      case BO_Rem:
      case BO_Shl:
      case BO_Shr:
      case BO_And:
      case BO_Or:
      case BO_Xor:
      case BO_EQ:
      case BO_NE:
      case BO_GT:
      case BO_GE:
      case BO_LT:
      case BO_LE:
      case BO_LAnd:
      case BO_LOr: {
        return isShort(binOp->getLHS()) and isShort(binOp->getRHS());
      }
      default:
        break;
      }
    } else if(const auto* unOp = dyn_cast<UnaryOperator>(expr)) {
      if(unOp->getOpcode() != UO_AddrOf)
        return isShort(unOp->getSubExpr());
    } else if(const auto* castExpr = dyn_cast<CStyleCastExpr>(expr)) {
      return isShort(castExpr->getSubExpr());
    }

    return false;
  }

  bool canPropagateCopy(VarDecl* lhs, VarDecl* rhs, Stmt* def) {
    Set<Stmt*> defs = um.getTopLevelDefs(rhs);
    Set<Stmt*> uses = um.getTopLevelUses(lhs);
    if(uses.size() == 0)
      return true;

    CFGBlock* rootBlock = cm.getCFGBlock(def);

    if(isDefBeforeUseInRootBlock(defs, rootBlock, def, uses))
      return false;
    // If all the uses have already been seen, nothing more to be done
    if(uses.size() == 0)
      return true;

    // Look at the rest of the graph
    Set<CFGBlock*> seen = {rootBlock};
    for(CFGBlock* succ : getSuccessors(rootBlock))
      if(succ and isDefBeforeUseInBlock(defs, succ, false, seen, uses))
        return false;

    // There should be no uses left
    if(uses.size())
      fatal(error() << "Did not encounter all uses: " << lhs->getName());

    return true;
  }

  bool isRecursiveExpr(VarDecl* lhs, Stmt* lhsDef) {
    for(VarDecl* var : Clang::getVarsInStmt(lhsDef)) {
      for(Stmt* def : um.getTopLevelDefs(var)) {
        Expr* rhs = cast<BinaryOperator>(def)->getRHS();
        if(Clang::getVarsInStmt(rhs).contains(lhs))
          return true;
      }
    }
    return false;
  }

  bool isLHSDefConstantForAllUses(VarDecl* lhs, BinaryOperator* def) {
    Set<Stmt*> uses = um.getTopLevelUses(lhs);
    CFGBlock* rootBlock = cm.getCFGBlock(def);

    for(VarDecl* var : Clang::getVarsInStmt(def->getRHS())) {
      Set<Stmt*> defs = um.getTopLevelDefs(var);

      if(isDefBeforeUseInRootBlock(defs, rootBlock, def, uses))
        return false;

      // If all the uses have already been seen, nothing more to be done
      if(uses.size()) {
        // Look at the rest of the graph
        Set<CFGBlock*> seen = {rootBlock};
        for(CFGBlock* succ : getSuccessors(rootBlock))
          if(succ and isDefBeforeUseInBlock(defs, succ, false, seen, uses))
            return false;

        // There should be no uses left
        if(uses.size())
          fatal(error() << "Did not encounter all uses: " << lhs->getName());
      }
    }

    return true;
  }

  bool canPropagateExpr(VarDecl* lhs, BinaryOperator* def) {
    if(isRecursiveExpr(lhs, def->getRHS())
       or isLHSDefConstantForAllUses(lhs, def))
      return true;
    return false;
  }

public:
  bool process(FunctionDecl* f, Stmt*) {
    bool changed = false;

    // Finding vars to be propagated that have more than one definition is
    // possible, but is more difficult because a more complicated
    // flow-sensitive analysis will need to be done to determine whether or
    // not the RHS is constant before uses of the LHS.
    // But since this comes from the output of a compiler that has probably
    // already converted everything to SSA form and then had reg2mem done to
    // it, it is likely that there are few variables that have more than one
    // definition.
    // Also, because the input is from a compiler, there is no need to check
    // if the definition dominates all the uses
    Map<VarDecl*, DeclRefExpr*> replVars;
    for(VarDecl* lhs : Clang::getLocalVars(f))
      if(um.hasSingleDef(lhs) and (not um.hasZeroUses(lhs))
         and (not addrTaken.contains(lhs)))
        if(auto* def = dyn_cast<DeclRefExpr>(um.getSingleDefRHS(lhs)))
          if(auto* rhs = dyn_cast<VarDecl>(def->getDecl()))
            if(canPropagateCopy(lhs, rhs, um.getSingleDef(lhs)))
              replVars[lhs] = def;

    for(auto& i : replVars) {
      VarDecl* var = i.first;
      DeclRefExpr* def = i.second;
      for(Expr* expr : em.getEqv(em.get(var)).clone())
        for(Stmt* use : um.getUses(var).clone())
          changed |= ast.replaceExprWith(expr, def, use);
    }

    // Now that any variables are propagated, check if there are any
    // "simple" expressions that can be propagated. The expressions are
    // intended to be "short" in length and have not more than one operator.
    // This is to strike a balance between having plenty of complicated
    // expressions all over the place which are hard to read and having
    // everything assigned to a variable first which is also hard to read
    // Because the input is from a compiler, there is no need to check if the
    // definition dominates all the uses
    Map<VarDecl*, Expr*> replExprs;
    replExprs.clear();
    for(VarDecl* lhs : Clang::getLocalVars(f))
      if(um.hasSingleDef(lhs) and (not um.hasZeroUses(lhs))
         and (not addrTaken.contains(lhs)))
        if(Expr* def = um.getSingleDefRHS(lhs))
          if(isReplaceable(def)
             and canPropagateExpr(lhs,
                                  cast<BinaryOperator>(um.getSingleDef(lhs))))
            replExprs[lhs] = def;

    for(auto& i : replExprs) {
      VarDecl* var = i.first;
      Expr* repl = i.second;
      for(Expr* expr : em.getEqv(em.get(var)).clone())
        for(Stmt* use : um.getUses(var))
          changed |= ast.replaceExprWith(expr, repl, use);
    }

    return changed;
  }

public:
  ASTPropagateExprsPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext,
                        RequireExprNums | RequireUses | RequireCFG) {
    ;
  }

  ASTPropagateExprsPass(const ASTPropagateExprsPass&) = delete;
  ASTPropagateExprsPass(ASTPropagateExprsPass&&) = delete;
  virtual ~ASTPropagateExprsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-eprop";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Expression Propagation";
  }
};

} // namespace cish

cish::ASTPass* createASTPropagateExprsPass(cish::CishContext& cishContext) {
  return new cish::ASTPropagateExprsPass(cishContext);
}
