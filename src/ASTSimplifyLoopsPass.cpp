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
#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "ClangUtils.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <algorithm>

using namespace clang;

namespace cish {

// Convert do-while loops into for-loops and while loops whenever possible
// Currently, this only does very simple pattern matching that converts a
// loop with a single exiting statement and single loop variable

class ASTSimplifyLoopsPass : public ASTFunctionPass<ASTSimplifyLoopsPass> {
protected:
  struct Loop {
    VarDecl* var;
    Stmt* initStmt;
    Expr* init;
    IfStmt* cond;
    Stmt* latch;
    Expr* step;
    bool valid;

    operator bool() const {
      return valid;
    }

    Loop()
        : var(nullptr), initStmt(nullptr), init(nullptr), cond(nullptr),
          latch(nullptr), step(nullptr), valid(false) {
      ;
    }

    Loop(VarDecl* var,
         Stmt* initStmt,
         Expr* init,
         IfStmt* cond,
         Stmt* latch,
         Expr* step)
        : var(var), initStmt(initStmt), init(init), cond(cond), latch(latch),
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
  ASTBuilder builder;

protected:
  bool isExiting(Stmt* stmt) {
    if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
      auto* body = cast<CompoundStmt>(ifStmt->getThen());
      if(body->size() == 1)
        return isa<BreakStmt>(*body->child_begin());
    }
    return false;
  }

  bool dominates(Stmt* a, Stmt* b) {
    // FIXME: Actually calculate the dominator tree
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
    for(Stmt* use : ast->tluses(var))
      if(not ast->isContainedIn(use, loop))
        return false;

    unsigned init = 0;
    unsigned incr = 0;
    for(Stmt* def : ast->tldefs(var))
      if((not ast->isContainedIn(def, loop)) and dominates(def, loop))
        init += 1;
      else if(ast->isContainedIn(def, loop))
        incr += 1;

    return (init <= 1) and (incr == 1);
  }

  Loop getLoopFor(Stmt* loop) {
    Vector<IfStmt*> exiting;
    Vector<VarDecl*> loopVars;
    for(Stmt* stmt : ast->children(loop)) {
      if(isExiting(stmt)) {
        IfStmt* ifStmt = cast<IfStmt>(stmt);
        exiting.push_back(ifStmt);
        for(VarDecl* var : getVarsInStmt(ifStmt->getCond()))
          if(isLoopVariable(var, loop))
            loopVars.push_back(var);
      }
    }

    if(not((exiting.size() == 1) and (loopVars.size() == 1)))
      return Loop();

    // The loop variable has a single definition outside the loop and that
    // definition is the initial value of the variable
    VarDecl* var = loopVars.front();
    IfStmt* cond = exiting.front();

    // If there is a single loop variable, then it checks that there is a single
    // initialization value and a single latch
    BinaryOperator* init = nullptr;
    Vector<BinaryOperator*> latches;
    for(Stmt* def : ast->tldefs(var))
      if(not ast->isContainedIn(def, loop))
        init = cast<BinaryOperator>(def);
      else
        latches.push_back(cast<BinaryOperator>(def));

    CompoundStmt* body = cast<CompoundStmt>(cast<DoStmt>(loop)->getBody());
    if(*body->body_begin() == cond) {
      // If the first statement in the loop is the exiting if statement, then
      // the loop should be transformed into a while loop.
      // FIXME: We may be able to do something smarter and convert it into a for
      // loop even in this case
      return Loop(var, init, init->getRHS(), cond, nullptr, nullptr);
    } else if(latches.size() == 1) {
      // There should only be one statement after the loop exit block
      // and the end of the loop. That should be a def of the loop variable
      BinaryOperator* latch = latches.front();
      Expr* step = nullptr;
      if(auto* bin = dyn_cast<BinaryOperator>(latch->getRHS()))
        step = getVarInExpr(bin->getLHS()) ? bin->getRHS() : bin->getLHS();

      if(step) {
        Stmt** stmts = body->body_begin();
        unsigned i = 0;
        for(; i < body->size(); i++)
          if(stmts[i] == cond)
            break;
        if((i == (body->size() - 2)) and (stmts[i + 1] == latch))
          return Loop(var, init, init->getRHS(), cond, latch, step);
      }
    }

    return Loop();
  }

  Stmt* getLoopBody(Stmt* loop) {
    if(auto* doStmt = dyn_cast<DoStmt>(loop))
      return doStmt->getBody();
    else if(auto* forStmt = dyn_cast<ForStmt>(loop))
      return forStmt->getBody();
    else if(auto* whileStmt = dyn_cast<WhileStmt>(loop))
      return whileStmt->getBody();
    else
      fatal(error() << "Not a loop: " << loop->getStmtClassName());
    return nullptr;
  }

  void setLoopBody(Stmt* loop, Stmt* body) {
    if(auto* doStmt = dyn_cast<DoStmt>(loop))
      return doStmt->setBody(body);
    else if(auto* forStmt = dyn_cast<ForStmt>(loop))
      return forStmt->setBody(body);
    else if(auto* whileStmt = dyn_cast<WhileStmt>(loop))
      return whileStmt->setBody(body);
    else
      fatal(error() << "Not a loop: " << loop->getStmtClassName());
  }

  VarDecl* getVarInExpr(Expr* expr) {
    if(auto* ref = dyn_cast<DeclRefExpr>(expr))
      if(auto* var = dyn_cast<VarDecl>(ref->getFoundDecl()))
        return var;
    return nullptr;
  }

  bool replaceLoopWithForLoop(Stmt* loop,
                              VarDecl* var,
                              Stmt* initStmt,
                              Expr* init,
                              IfStmt* exit,
                              Stmt* latch,
                              Expr* step) {
    static const Map<BinaryOperator::Opcode, BinaryOperator::Opcode> invOps = {
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
    };

    bool changed = false;

    // FIXME: There may be other increment conditions for the loop variable
    // in a for loop
    auto* latchLhs = cast<BinaryOperator>(latch)->getLHS();
    auto* latchRhs
        = dyn_cast<BinaryOperator>(cast<BinaryOperator>(latch)->getRHS());
    auto* cond = dyn_cast<BinaryOperator>(exit->getCond());

    if(latchRhs and cond) {
      BinaryOperator::Opcode stepOp = latchRhs->getOpcode();
      BinaryOperator::Opcode condOp = cond->getOpcode();

      Expr* newInit = builder.createBinaryOperator(
          latchLhs, init, BO_Assign, latchLhs->getType());
      Expr* newCondLhs = builder.createBinaryOperator(
          cond->getLHS(), step, invOps.at(stepOp), step->getType());
      Expr* newCond = builder.createBinaryOperator(
          newCondLhs, cond->getRHS(), invOps.at(condOp), newCondLhs->getType());
      Expr* newLatch = builder.createBinaryOperator(
          latchLhs, latchRhs, BO_Assign, latchLhs->getType());
      ForStmt* forStmt = builder.createForStmt(
          newInit, newCond, newLatch, getLoopBody(loop));

      setLoopBody(loop, nullptr);
      changed |= ast->replaceStmtWith(loop, forStmt);
      changed |= ast->erase(initStmt);
      changed |= ast->erase(exit);
      changed |= ast->erase(latch);
    }

    return changed;
  }

  bool replaceLoopWithWhileLoop(Stmt* loop, VarDecl* var, IfStmt* cond) {
    fatal(error() << "Not implemented: replace while loop\n");
    return true;
  }

public:
  bool process(FunctionDecl* f) {
    bool changed = false;

    Vector<Stmt*> loops = ast->getLoops();
    std::sort(loops.begin(), loops.end(), [&](Stmt* a, Stmt* b) {
      return ast->getDepth(a) < ast->getDepth(b);
    });
    std::reverse(loops.begin(), loops.end());
    for(Stmt* l : loops) {
      if(Loop loop = getLoopFor(l)) {
        if(loop.isForLoop())
          changed |= replaceLoopWithForLoop(l,
                                            loop.var,
                                            loop.initStmt,
                                            loop.init,
                                            loop.cond,
                                            loop.latch,
                                            loop.step);
        else
          changed |= replaceLoopWithWhileLoop(l, loop.var, loop.cond);
      }
    }

    return changed;
  }

public:
  ASTSimplifyLoopsPass(CishContext& cishContext)
      : ASTFunctionPass(cishContext, true),
        builder(cishContext.getASTContext()) {
    ;
  }

  ASTSimplifyLoopsPass(const ASTSimplifyLoopsPass&) = delete;
  ASTSimplifyLoopsPass(ASTSimplifyLoopsPass&&) = delete;
  virtual ~ASTSimplifyLoopsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "Cish AST Simplify Loops";
  }
};

} // namespace cish

cish::ASTPass* createASTSimplifyLoopsPass(cish::CishContext& context) {
  return new cish::ASTSimplifyLoopsPass(context);
}
