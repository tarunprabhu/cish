#include "ASTDefUseInfo.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class DefUseVisitor : public RecursiveASTVisitor<DefUseVisitor> {
protected:
  ASTContext& astContext;
  ASTDefUseInfo& du;
  const Set<UnaryOperator::Opcode> defOps1;
  const Set<BinaryOperator::Opcode> defOps2;

protected:
  void findDefd(Expr* expr, Stmt* user);
  void findUsed(Expr* expr, Stmt* user);
  void addParent(Stmt* stmt, Stmt* parent);

public:
  explicit DefUseVisitor(ASTContext& astContext, ASTDefUseInfo& du);

  bool VisitSwitchStmt(SwitchStmt* switchStmt);
  bool VisitReturnStmt(ReturnStmt* retStmt);
  bool VisitConditionalOperator(ConditionalOperator* condOp);
  bool VisitBinaryOperator(BinaryOperator* binOp);
  bool VisitUnaryOperator(UnaryOperator* unOp);
  bool VisitCastExpr(CastExpr* castExpr);
  bool VisitCallExpr(CallExpr* callExpr);
  bool VisitArraySubscriptExpr(ArraySubscriptExpr* arrExpr);
  bool VisitMemberExpr(MemberExpr* memberExpr);
  bool VisitInitListExpr(InitListExpr* initList);

  bool VisitIfStmt(IfStmt* ifStmt);
  bool VisitDoStmt(DoStmt* doStmt);
  bool VisitForStmt(ForStmt* forStmt);
  bool VisitWhileStmt(WhileStmt* whileStmt);

  bool VisitFunctionDecl(FunctionDecl* f);
};

DefUseVisitor::DefUseVisitor(ASTContext& astContext, ASTDefUseInfo& du)
    : astContext(astContext), du(du),
      defOps1({UO_PreInc, UO_PreDec, UO_PostInc, UO_PostDec}),
      defOps2({BO_Assign,
               BO_AddAssign,
               BO_SubAssign,
               BO_MulAssign,
               BO_DivAssign,
               BO_RemAssign,
               BO_ShlAssign,
               BO_ShrAssign,
               BO_AndAssign,
               BO_OrAssign,
               BO_XorAssign}) {
  ;
}

void DefUseVisitor::findDefd(Expr* expr, Stmt* user) {
  if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
    if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
      du.defMap[var].push_back(user);
}

void DefUseVisitor::findUsed(Expr* expr, Stmt* user) {
  if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
    if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
      du.useMap[var].push_back(user);
}

void DefUseVisitor::addParent(Stmt* child, Stmt* parent) {
  auto* compound = cast<CompoundStmt>(child);
  for(Stmt* stmt : compound->body())
    du.parents[stmt] = compound;
  du.containers[compound] = parent;
}

bool DefUseVisitor::VisitConditionalOperator(ConditionalOperator* condOp) {
  findUsed(condOp->getTrueExpr(), condOp);
  findUsed(condOp->getFalseExpr(), condOp);

  return true;
}

bool DefUseVisitor::VisitBinaryOperator(BinaryOperator* binOp) {
  if(defOps2.contains(binOp->getOpcode())) {
    findDefd(binOp->getLHS(), binOp);
    // If this is an OperatorAssign (+=, -= etc.), the LHS will be definitely
    // also be read
    if(binOp->getOpcode() != BO_Assign)
      findUsed(binOp->getLHS(), binOp);
  } else {
    findUsed(binOp->getLHS(), binOp);
  }
  findUsed(binOp->getRHS(), binOp);

  return true;
}

bool DefUseVisitor::VisitUnaryOperator(UnaryOperator* unOp) {
  Expr* expr = unOp->getSubExpr();
  if(defOps1.contains(unOp->getOpcode()))
    findDefd(expr, unOp);
  findUsed(expr, unOp);

  return true;
}

bool DefUseVisitor::VisitCallExpr(CallExpr* callExpr) {
  // FIXME: Call expressions may have variables passed by pointer/reference
  // which might get mutated in the call. Not sure whether or not they should
  // be considered defs
  for(Expr* arg : callExpr->arguments())
    findUsed(arg, callExpr);
  return true;
}

bool DefUseVisitor::VisitCastExpr(CastExpr* castExpr) {
  findUsed(castExpr->getSubExpr(), castExpr);
  return true;
}

bool DefUseVisitor::VisitArraySubscriptExpr(ArraySubscriptExpr* arrExpr) {
  findUsed(arrExpr->getBase(), arrExpr);
  findUsed(arrExpr->getIdx(), arrExpr);

  return true;
}

bool DefUseVisitor::VisitMemberExpr(MemberExpr* memberExpr) {
  findUsed(memberExpr->getBase(), memberExpr);

  return true;
}

bool DefUseVisitor::VisitInitListExpr(InitListExpr* initList) {
  for(Expr* expr : initList->inits())
    findUsed(expr, initList);
  return true;
}

bool DefUseVisitor::VisitSwitchStmt(SwitchStmt* switchStmt) {
  findUsed(switchStmt->getCond(), switchStmt);

  for(SwitchCase* kase = switchStmt->getSwitchCaseList(); kase;
      kase = kase->getNextSwitchCase())
    addParent(kase->getSubStmt(), kase);

  return true;
}

bool DefUseVisitor::VisitReturnStmt(ReturnStmt* retStmt) {
  if(Expr* expr = retStmt->getRetValue())
    findUsed(expr, retStmt);

  return true;
}

bool DefUseVisitor::VisitIfStmt(IfStmt* ifStmt) {
  findUsed(ifStmt->getCond(), ifStmt);
  addParent(ifStmt->getThen(), ifStmt);
  if(Stmt* els = ifStmt->getElse())
    addParent(els, ifStmt);

  return true;
}

bool DefUseVisitor::VisitDoStmt(DoStmt* doStmt) {
  findUsed(doStmt->getCond(), doStmt);
  addParent(doStmt->getBody(), doStmt);

  return true;
}

bool DefUseVisitor::VisitForStmt(ForStmt* forStmt) {
  findUsed(forStmt->getCond(), forStmt);
  addParent(forStmt->getBody(), forStmt);

  return true;
}

bool DefUseVisitor::VisitWhileStmt(WhileStmt* whileStmt) {
  findUsed(whileStmt->getCond(), whileStmt);
  addParent(whileStmt->getBody(), whileStmt);

  return true;
}

bool DefUseVisitor::VisitFunctionDecl(FunctionDecl* f) {
  for(ParmVarDecl* param : f->parameters()) {
    du.defMap[param] = {};
    du.useMap[param] = {};
  }
  for(Decl* decl : f->decls()) {
    if(auto* var = dyn_cast<VarDecl>(decl)) {
      du.useMap[var] = {};
      du.defMap[var] = {};
    }
  }

  if(Stmt* body = f->getBody())
    addParent(body, nullptr);

  return true;
}

ASTDefUseInfo::ASTDefUseInfo(ASTContext& astContext) : astContext(astContext) {
  ;
}

void ASTDefUseInfo::remove(Stmt* stmt) {
  for(auto& i : useMap)
    i.second.erase(stmt);
  for(auto& i : defMap)
    i.second.erase(stmt);
}

void ASTDefUseInfo::remove(const VarDecl* var) {
  defMap.erase(var);
  useMap.erase(var);
}

CompoundStmt* ASTDefUseInfo::getParent(Stmt* stmt) const {
  return parents.at(stmt);
}

std::pair<CompoundStmt*, Stmt*> ASTDefUseInfo::getContainer(Stmt* stmt) const {
  return std::make_pair(getParent(stmt), containers.at(getParent(stmt)));
}

llvm::iterator_range<ASTDefUseInfo::def_iterator>
ASTDefUseInfo::defs(const VarDecl* var) const {
  return llvm::make_range(defMap.at(var).begin(), defMap.at(var).end());
}

llvm::iterator_range<ASTDefUseInfo::use_iterator>
ASTDefUseInfo::uses(const VarDecl* var) const {
  return llvm::make_range(useMap.at(var).begin(), useMap.at(var).end());
}

bool ASTDefUseInfo::isDefined(const VarDecl* var) const {
  return getNumDefs(var);
}

bool ASTDefUseInfo::hasSingleDef(const VarDecl* var) const {
  return getNumDefs(var) == 1;
}

bool ASTDefUseInfo::isUsed(const VarDecl* var) const {
  return getNumUses(var);
}

bool ASTDefUseInfo::hasSingleUse(const VarDecl* var) const {
  return getNumUses(var) == 1;
}

unsigned ASTDefUseInfo::getNumDefs(const VarDecl* var) const {
  return defMap.at(var).size();
}

unsigned ASTDefUseInfo::getNumUses(const VarDecl* var) const {
  return useMap.at(var).size();
}

void ASTDefUseInfo::runOnFunction(FunctionDecl* f) {
  DefUseVisitor visitor(astContext, *this);
  visitor.TraverseDecl(f);
}

} // namespace cish
