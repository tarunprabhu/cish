#include "ASTExprPass.h"
#include "Diagnostics.h"

using namespace clang;

namespace cish {

ASTExprPass::ASTExprPass(ASTContext& astContext) : ASTFunctionPass(astContext) {
  ;
}

void ASTExprPass::process(CompoundStmt* compoundStmt) {
  for(Stmt* stmt : compoundStmt->body())
    process(stmt);
}

void ASTExprPass::process(IfStmt* ifStmt) {
  ifStmt->setCond(process(ifStmt->getCond()));
  if(Stmt* thn = ifStmt->getThen())
    process(thn);
  if(Stmt* els = ifStmt->getElse())
    process(els);
}

void ASTExprPass::process(DoStmt* doStmt) {
  doStmt->setCond(process(doStmt->getCond()));
  process(doStmt->getBody());
}

void ASTExprPass::process(ForStmt* forStmt) {
  process(forStmt->getInit());
  forStmt->setCond(process(forStmt->getCond()));
  forStmt->setInc(process(forStmt->getInc()));
}

void ASTExprPass::process(WhileStmt* whileStmt) {
  whileStmt->setCond(process(whileStmt->getCond()));
  process(whileStmt->getBody());
}

void ASTExprPass::process(SwitchStmt* switchStmt) {
  switchStmt->setCond(process(switchStmt->getCond()));
  process(switchStmt->getBody());
}

void ASTExprPass::process(CaseStmt* castStmt) {
  process(castStmt->getSubStmt());
}

void ASTExprPass::process(DefaultStmt* defaultStmt) {
  process(defaultStmt->getSubStmt());
}

void ASTExprPass::process(ReturnStmt* retStmt) {
  if(Expr* retValue = retStmt->getRetValue())
    retStmt->setRetValue(process(retValue));
}

void ASTExprPass::process(LabelStmt*) {
  return;
}

void ASTExprPass::process(GotoStmt*) {
  return;
}

void ASTExprPass::process(BreakStmt*) {
  return;
}

void ASTExprPass::process(ContinueStmt*) {
  return;
}

void ASTExprPass::process(DeclStmt* declStmt) {
  for(Decl* decl : declStmt->decls()) {
    if(auto* var = dyn_cast<VarDecl>(decl)) {
      if(var->hasInit())
        var->setInit(process(var->getInit()));
    } else {
      fatal(error() << "Unknown decl in DeclStmt: " << decl->getDeclKindName());
    }
  }
}

void ASTExprPass::process(Stmt* stmt) {
  if(auto* compoundStmt = dyn_cast<CompoundStmt>(stmt))
    return process(compoundStmt);
  else if(auto* ifStmt = dyn_cast<IfStmt>(stmt))
    return process(ifStmt);
  else if(auto* doStmt = dyn_cast<DoStmt>(stmt))
    return process(doStmt);
  else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    return process(whileStmt);
  else if(auto* forStmt = dyn_cast<ForStmt>(stmt))
    return process(forStmt);
  else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt))
    return process(switchStmt);
  else if(auto* castStmt = dyn_cast<CaseStmt>(stmt))
    return process(castStmt);
  else if(auto* defaultStmt = dyn_cast<DefaultStmt>(stmt))
    return process(defaultStmt);
  else if(auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    return process(retStmt);
  else if(auto* breakStmt = dyn_cast<BreakStmt>(stmt))
    return process(breakStmt);
  else if(auto* continueStmt = dyn_cast<ContinueStmt>(stmt))
    return process(continueStmt);
  else if(auto* labelStmt = dyn_cast<LabelStmt>(stmt))
    return process(labelStmt);
  else if(auto* gotoStmt = dyn_cast<GotoStmt>(stmt))
    return process(gotoStmt);
  else if(auto* declStmt = dyn_cast<DeclStmt>(stmt))
    return process(declStmt);
  else if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    process(binOp);
  else if(auto* unOp = dyn_cast<UnaryOperator>(stmt))
    process(unOp);
  else if(auto* callExpr = dyn_cast<CallExpr>(stmt))
    process(callExpr);
  else
    fatal(error() << "Unknown statement: " << stmt->getStmtClassName());
}

Expr* ASTExprPass::process(CXXBoolLiteralExpr* boolLiteral) {
  return boolLiteral;
}

Expr* ASTExprPass::process(CharacterLiteral* charLiteral) {
  return charLiteral;
}

Expr* ASTExprPass::process(IntegerLiteral* intLiteral) {
  return intLiteral;
}

Expr* ASTExprPass::process(FloatingLiteral* floatLiteral) {
  return floatLiteral;
}

Expr* ASTExprPass::process(StringLiteral* stringLiteral) {
  return stringLiteral;
}

Expr* ASTExprPass::process(CXXNullPtrLiteralExpr* nullLiteral) {
  return nullLiteral;
}

Expr* ASTExprPass::process(DeclRefExpr* declRefExpr) {
  return declRefExpr;
}

Expr* ASTExprPass::process(InitListExpr* initList) {
  for(unsigned i = 0; i < initList->getNumInits(); i++)
    initList->setInit(i, process(initList->getInit(i)));
  return initList;
}

Expr* ASTExprPass::process(BinaryOperator* binOp) {
  binOp->setLHS(process(binOp->getLHS()));
  binOp->setRHS(process(binOp->getRHS()));
  return binOp;
}

Expr* ASTExprPass::process(UnaryOperator* unOp) {
  unOp->setSubExpr(process(unOp->getSubExpr()));
  return unOp;
}

Expr* ASTExprPass::process(ConditionalOperator* condOp) {
  Expr* cond = condOp->getCond();
  Expr* trueExpr = condOp->getTrueExpr();
  Expr* falseExpr = condOp->getFalseExpr();
  Expr* newCond = process(cond);
  Expr* newTrue = process(trueExpr);
  Expr* newFalse = process(falseExpr);
  if((cond != newCond) or (trueExpr != newTrue) or (falseExpr != newFalse))
    return new(astContext) ConditionalOperator(newCond,
                                               invLoc,
                                               newTrue,
                                               invLoc,
                                               newFalse,
                                               newTrue->getType(),
                                               VK_RValue,
                                               OK_Ordinary);
  return cond;
}

Expr* ASTExprPass::process(CallExpr* callExpr) {
  for(unsigned i = 0; i < callExpr->getNumArgs(); i++)
    callExpr->setArg(i, process(callExpr->getArg(i)));
  return callExpr;
}

Expr* ASTExprPass::process(CStyleCastExpr* castExpr) {
  castExpr->setSubExpr(process(castExpr->getSubExpr()));
  return castExpr;
}

Expr* ASTExprPass::process(ArraySubscriptExpr* arrExpr) {
  arrExpr->setLHS(process(arrExpr->getBase()));
  arrExpr->setRHS(process(arrExpr->getIdx()));
  return arrExpr;
}

Expr* ASTExprPass::process(Expr* expr) {
  if(auto* boolLit = dyn_cast<CXXBoolLiteralExpr>(expr))
    return process(boolLit);
  else if(auto* charLit = dyn_cast<CharacterLiteral>(expr))
    return process(charLit);
  else if(auto* intLit = dyn_cast<IntegerLiteral>(expr))
    return process(intLit);
  else if(auto* fpLit = dyn_cast<FloatingLiteral>(expr))
    return process(fpLit);
  else if(auto* stringLit = dyn_cast<StringLiteral>(expr))
    return process(stringLit);
  else if(auto* cnull = dyn_cast<CXXNullPtrLiteralExpr>(expr))
    return process(cnull);
  else if(auto* initList = dyn_cast<InitListExpr>(expr))
    return process(initList);
  else if(auto* binOp = dyn_cast<BinaryOperator>(expr))
    return process(binOp);
  else if(auto* unOp = dyn_cast<UnaryOperator>(expr))
    return process(unOp);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(expr))
    return process(condOp);
  else if(auto* callExpr = dyn_cast<CallExpr>(expr))
    return process(callExpr);
  else if(auto* castExpr = dyn_cast<CStyleCastExpr>(expr))
    return process(castExpr);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(expr))
    return process(arrExpr);
  else if(auto* declRefExpr = dyn_cast<DeclRefExpr>(expr))
    return process(declRefExpr);
  fatal(error() << "Unknown exprssion: " << expr->getStmtClassName());

  return expr;
}

void ASTExprPass::runOnFunction(FunctionDecl* f) {
  process(f->getBody());
}

} // namespace cish
