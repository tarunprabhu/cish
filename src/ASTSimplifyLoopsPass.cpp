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

class ASTSimplifyLoopsPass : public ASTFunctionPass<ASTSimplifyLoopsPass> {
protected:
  bool isExiting(Stmt* stmt);
  bool canConvert(Stmt* loop);
  bool isLoopVariable(VarDecl* var, Stmt* loop);

public:
  ASTSimplifyLoopsPass(CishContext& context);
  ASTSimplifyLoopsPass(const ASTSimplifyLoopsPass&) = delete;
  ASTSimplifyLoopsPass(ASTSimplifyLoopsPass&&) = delete;
  virtual ~ASTSimplifyLoopsPass() = default;

  virtual llvm::StringRef getPassName() const override;
  bool process(FunctionDecl* f);
};

ASTSimplifyLoopsPass::ASTSimplifyLoopsPass(CishContext& context)
    : ASTFunctionPass(context) {
  ;
}

llvm::StringRef ASTSimplifyLoopsPass::getPassName() const {
  return "Cish AST Simplify Loops";
}

bool ASTSimplifyLoopsPass::isExiting(Stmt* stmt) {
  if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
    auto* body = cast<CompoundStmt>(ifStmt->getThen());
    if(body->size() == 1)
      return isa<BreakStmt>(*body->child_begin());
  }
  return false;
}

bool ASTSimplifyLoopsPass::isLoopVariable(VarDecl* var, Stmt* loop) {
  for(Stmt* use : ast->tluses(var))
    if(not ast->isContainedIn(use, loop))
      return false;

  // FIXME: Instead of just checking if the variable is defined outside the
  // loop, check that it is in a dominator of the loop. It should never happen
  // that it isn't, but probably safer that way
  unsigned outside = 0;
  for(Stmt* def : ast->tldefs(var))
    if(not ast->isContainedIn(def, loop))
      outside += 1;

  return outside <= 1;
}

bool ASTSimplifyLoopsPass::canConvert(Stmt* loop) {
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
  llvm::errs() << "|exiting| = " << exiting.size() << "\n";
  llvm::errs() << "|vars| = " << loopVars.size() << "\n";

  return (exiting.size() == 1) and (loopVars.size() == 1);
}

bool ASTSimplifyLoopsPass::process(FunctionDecl* f) {
  bool changed = false;

  Vector<Stmt*> loops = ast->getLoops();
  std::sort(loops.begin(), loops.end(), [&](Stmt* a, Stmt* b) {
    return ast->getDepth(a) < ast->getDepth(b);
  });
  std::reverse(loops.begin(), loops.end());
  for(Stmt* loop : loops) {
    if(canConvert(loop))
      llvm::errs() << ast->getDepth(loop) << "\n";
  }

  return changed;
}

} // namespace cish

cish::ASTPass* createASTSimplifyLoopsPass(cish::CishContext& context) {
  return new cish::ASTSimplifyLoopsPass(context);
}
