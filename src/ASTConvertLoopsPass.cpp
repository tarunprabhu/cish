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

#include <clang/AST/RecursiveASTVisitor.h>

#include <algorithm>

using namespace clang;

namespace cish {

// Convert do-while loops into for-loops and while loops whenever possible
// Currently, this only does very simple pattern matching that converts a
// loop with a single exiting statement and single loop variable

class ASTConvertLoopsPass : public ASTFunctionPass<ASTConvertLoopsPass> {
protected:
  struct LoopInfo {
    VarDecl* var;
    Stmt* initStmt;
    Expr* init;
    IfStmt* exit;
    Stmt* latch;
    Expr* step;
    bool valid;

    operator bool() const {
      return valid;
    }

    LoopInfo()
        : var(nullptr), initStmt(nullptr), init(nullptr), exit(nullptr),
          latch(nullptr), step(nullptr), valid(false) {
      ;
    }

    LoopInfo(VarDecl* var,
             Stmt* initStmt,
             Expr* init,
             IfStmt* exit,
             Stmt* latch,
             Expr* step)
        : var(var), initStmt(initStmt), init(init), exit(exit), latch(latch),
          step(step), valid(true) {
      ;
    }

    bool isForLoop() const {
      return valid and latch;
    }

    bool isWhileLoop() const {
      return valid and (not latch);
    }
  };

protected:
  Vector<DoStmt*> doLoops;
  Vector<WhileStmt*> whileLoops;
  const Map<BinaryOperator::Opcode, BinaryOperator::Opcode> invOps;

protected:
  bool isExiting(Stmt* stmt) {
    if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
      auto* body = cast<CompoundStmt>(ifStmt->getThen());
      if(body->size() == 1)
        return isa<BreakStmt>(*body->child_begin());
    }
    return false;
  }

  bool dominates(Stmt*, Stmt*) {
    // FIXME: Actually calculate the dominator tree
    // Right now, it causes segmentation faults, probably because of some
    // mistake in how the clang AST is being constructed
    return true;

    // const DominatorTree& dt = ast->getDominatorTree();
    // const CFGBlock* aBlock = ast->getCFGBlock(a);
    // const CFGBlock* bBlock = ast->getCFGBlock(b);

    // if(aBlock == bBlock) {
    //   bool seenA = false;
    //   for(const CFGElement& cfgElem : *aBlock) {
    //     if(const CFGStmt* cfgStmt = cfgElem.getAs<CFGStmt>().getPointer()) {
    //       const Stmt* stmt = cfgStmt->getStmt();
    //       if(stmt == a)
    //         seenA = true;
    //       else if(stmt == b)
    //         return seenA;
    //     }
    //   }
    // } else {
    //   // return dt.dominates(aBlock, bBlock);
    // }
    // return false;
  }

  // All uses of the loop variable must be in the loop
  // At most a single definition can be outside it and it should dominate the
  // loop.
  // FIXME: Relax the conditions to be able to simplify more loops
  bool isLoopVariable(VarDecl* var, Stmt* loop) {
    for(Stmt* use : ast->getUses(var))
      if(not ast->isContainedIn(use, loop))
        return false;

    unsigned init = 0;
    unsigned incr = 0;
    for(Stmt* def : ast->getDefs(var))
      if((not ast->isContainedIn(def, loop)) and dominates(def, loop))
        init += 1;
      else if(ast->isContainedIn(def, loop))
        incr += 1;

    return (init <= 1) and (incr == 1);
  }

  LoopInfo getLoopInfo(WhileStmt*) {
    return LoopInfo();
  }

  LoopInfo getLoopInfo(DoStmt* loop) {
    CompoundStmt* body = cast<CompoundStmt>(loop->getBody());
    Vector<IfStmt*> exiting;
    Vector<VarDecl*> loopVars;
    for(Stmt* stmt : body->body()) {
      if(isExiting(stmt)) {
        IfStmt* ifStmt = cast<IfStmt>(stmt);
        exiting.push_back(ifStmt);
        for(VarDecl* var : Clang::getVarsInStmt(ifStmt->getCond()))
          if(isLoopVariable(var, loop))
            loopVars.push_back(var);
      }
    }

    if(not((exiting.size() == 1) and (loopVars.size() == 1)))
      return LoopInfo();

    // The loop variable has a single definition outside the loop and that
    // definition is the initial value of the variable
    VarDecl* var = loopVars.front();
    IfStmt* cond = exiting.front();

    // If there is a single loop variable, then check that there is a single
    // initialization value and a single latch
    BinaryOperator* init = nullptr;
    Vector<BinaryOperator*> latches;
    for(Stmt* def : ast->getDefs(var)) {
      if(not ast->isContainedIn(def, loop))
        init = cast<BinaryOperator>(def);
      else
        latches.push_back(cast<BinaryOperator>(def));
    }

    if(*body->body_begin() == cond) {
      // If the first statement in the loop is the exiting if statement, then
      // the loop should be transformed into a while loop.
      // FIXME: We may be able to do something smarter and convert it into a for
      // loop even in this case
      return LoopInfo(var, init, init->getRHS(), cond, nullptr, nullptr);
    } else if(latches.size() == 1) {
      // There should only be one statement after the loop exit block
      // and the end of the loop. That should be a def of the loop variable
      BinaryOperator* latch = latches.front();
      Expr* step = nullptr;
      if(auto* bin = dyn_cast<BinaryOperator>(latch->getRHS()))
        step = Clang::getVar(bin->getLHS()) ? bin->getRHS() : bin->getLHS();

      if(step) {
        Stmt** stmts = body->body_begin();
        unsigned i = 0;
        for(; i < body->size(); i++)
          if(stmts[i] == cond)
            break;
        if((i == (body->size() - 2)) and (stmts[i + 1] == latch))
          return LoopInfo(var, init, init->getRHS(), cond, latch, step);
      }
    }

    return LoopInfo();
  }

  // This tries to normalize for loops so the LHS of the condition consists
  // either only variable declarations or logical operations
  BinaryOperator* normalizeForLoopCondition(BinaryOperator* cond, Expr* step) {
    Expr* lhs = cond->getLHS();
    Expr* rhs = cond->getRHS();
    BinaryOperator::Opcode op = cond->getOpcode();

    //
    //   (var <op> step) <= end  ...  var <= end <inv-op> step
    //
    if(auto* subOp = dyn_cast<BinaryOperator>(lhs)) {
      if(Clang::getVar(subOp->getLHS())) {
        if((op == BO_LE) and (Clang::isEqual(subOp->getRHS(), step))) {
          switch(subOp->getOpcode()) {
          case BO_Add:
          case BO_Sub:
          case BO_Mul:
          case BO_Div:
            ast->replaceExprWith(cond->getLHS(),
                                 ast->cloneExpr(subOp->getLHS()));
            ast->replaceExprWith(
                cond->getRHS(),
                ast->createBinaryOperator(ast->cloneExpr(rhs),
                                          ast->cloneExpr(step),
                                          invOps.at(subOp->getOpcode()),
                                          step->getType()));
            break;
          default:
            break;
          }
        }
      }
    }

    return cond;
  }

  bool replaceWithForLoop(DoStmt* loop, LoopInfo& li) {
    bool changed = false;
    Stmt* initStmt = li.initStmt;
    Expr* init = li.init;
    IfStmt* exit = li.exit;
    Stmt* latch = li.latch;
    Expr* step = li.step;

    // FIXME: There may be other increment conditions for the loop variable
    // in a for loop
    auto* latchLhs = cast<BinaryOperator>(latch)->getLHS();
    auto* latchRhs
        = dyn_cast<BinaryOperator>(cast<BinaryOperator>(latch)->getRHS());
    auto* cond = dyn_cast<BinaryOperator>(exit->getCond());

    if(latchRhs and cond) {
      BinaryOperator::Opcode stepOp = latchRhs->getOpcode();
      BinaryOperator::Opcode condOp = cond->getOpcode();

      BinaryOperator* newInit
          = ast->createBinaryOperator(ast->cloneExpr(latchLhs),
                                      ast->cloneExpr(init),
                                      BO_Assign,
                                      latchLhs->getType());
      BinaryOperator* newCondLhs
          = ast->createBinaryOperator(ast->cloneExpr(cond->getLHS()),
                                      ast->cloneExpr(step),
                                      invOps.at(stepOp),
                                      step->getType());
      BinaryOperator* newCond
          = ast->createBinaryOperator(newCondLhs,
                                      ast->cloneExpr(cond->getRHS()),
                                      invOps.at(condOp),
                                      newCondLhs->getType());
      newCond = normalizeForLoopCondition(newCond, step);
      BinaryOperator* newLatch
          = ast->createBinaryOperator(ast->cloneExpr(latchLhs),
                                      ast->cloneExpr(latchRhs),
                                      BO_Assign,
                                      latchLhs->getType());

      ast->erase(initStmt);
      ast->erase(exit);
      ast->erase(latch);
      Stmt* body = ast->clone(loop->getBody());
      ForStmt* forStmt = ast->createForStmt(newInit, newCond, newLatch, body);

      changed |= ast->replaceStmtWith(loop, forStmt);
    }

    return changed;
  }

  bool replaceWithWhileLoop(DoStmt*, LoopInfo&) {
    bool changed = false;

    fatal(error() << "Not implemented: replace do loop with while loop\n");

    return changed;
  }

  bool replaceWithForLoop(WhileStmt*, LoopInfo&) {
    bool changed = false;

    fatal(error() << "Not implemented: replace while loop with for loop\n");

    return changed;
  }

public:
  bool process(DoStmt* doStmt) {
    doLoops.push_back(doStmt);

    return false;
  }

  bool process(WhileStmt* whileStmt) {
    whileLoops.push_back(whileStmt);

    return false;
  }

  bool process(FunctionDecl*) {
    bool changed = false;

    // // The loops must be processed from the inside out. The innermost loops
    // // will have the greatest depth
    // std::sort(loops.begin(), loops.end(), [&](Stmt* a, Stmt* b) {
    //   return ast->getDepth(a) < ast->getDepth(b);
    // });
    // std::reverse(loops.begin(), loops.end());

    for(DoStmt* loop : doLoops) {
      if(LoopInfo li = getLoopInfo(loop)) {
        if(li.isForLoop())
          changed |= replaceWithForLoop(loop, li);
        else
          changed |= replaceWithWhileLoop(loop, li);
      }
    }

    for(WhileStmt* loop : whileLoops)
      if(LoopInfo li = getLoopInfo(loop))
        changed |= replaceWithForLoop(loop, li);

    return changed;
  }

public:
  ASTConvertLoopsPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext, ModifiesAST | PostOrder),
        invOps({
            {BO_Add, BO_Sub},
            {BO_Sub, BO_Add},
            {BO_Mul, BO_Div},
            {BO_Div, BO_Mul},

            {BO_EQ, BO_NE},
            {BO_NE, BO_EQ},
            {BO_GT, BO_LE},
            {BO_LE, BO_GT},
            {BO_LT, BO_GE},
            {BO_GE, BO_LT},
        }) {
    ;
  }

  ASTConvertLoopsPass(const ASTConvertLoopsPass&) = delete;
  ASTConvertLoopsPass(ASTConvertLoopsPass&&) = delete;
  virtual ~ASTConvertLoopsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-loop-convert";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Convert Loops";
  }
};

} // namespace cish

cish::ASTPass* createASTConvertLoopsPass(cish::CishContext& context) {
  return new cish::ASTConvertLoopsPass(context);
}
