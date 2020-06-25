#include "ASTLookup.h"

using namespace clang;

namespace cish {

ASTLookup::ASTLookup(const DefUse& du) : du(du) {
  ;
}

clang::CompoundStmt* ASTLookup::getCompoundStmtFor(Stmt* stmt) const {
  return parents.at(stmt);
}

clang::Stmt* ASTLookup::getConstructFor(Stmt* stmt) const {
  return constructs.at(parents.at(stmt));
}

void ASTLookup::associateStmts(CompoundStmt* body, Stmt* construct) {
  constructs[body] = construct;
  for(Stmt* stmt : body->body()) {
    if(auto* ifStmt = dyn_cast<IfStmt>(stmt)) {
      auto* thn = cast<CompoundStmt>(ifStmt->getThen());
      associateStmts(thn, ifStmt);
      if(auto* els = cast_or_null<CompoundStmt>(ifStmt->getElse()))
        associateStmts(els, ifStmt);
    } else if(auto* doStmt = dyn_cast<DoStmt>(stmt)) {
      auto* body = cast<CompoundStmt>(doStmt->getBody());
      associateStmts(body, doStmt);
    } else if(auto* forStmt = dyn_cast<ForStmt>(stmt)) {
      auto* body = cast<CompoundStmt>(forStmt->getBody());
      associateStmts(body, forStmt);
    } else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt)) {
      auto* body = cast<CompoundStmt>(whileStmt->getBody());
      associateStmts(body, whileStmt);
    } else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt)) {
      for(SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
          kase = kase->getNextSwitchCase()) {
        auto* body = cast<CompoundStmt>(kase->getSubStmt());
        associateStmts(body, kase);
      }
    }
    parents[stmt] = body;
  }
}

void ASTLookup::runOnFunction(FunctionDecl* f) {
  parents.clear();
  constructs.clear();
  associateStmts(cast<CompoundStmt>(f->getBody()), nullptr);
}

// bool ASTLookup::allUsesInConstruct(const VarDecl* var, DoStmt* loop) const {
//   ;
// }

// bool ASTLookup::allUsesInConstruct(const VarDecl* var, ForStmt* loop) const {
//   ;
// }

// bool ASTLookup::allUsesInConstruct(const VarDecl* var, WhileStmt* loop) const
// {
//   ;
// }

} // namespace cish
