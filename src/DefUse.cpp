#include "DefUse.h"
#include "ASTFunctionAnalysisPass.h"
#include "List.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTDefUsePass : public ASTFunctionAnalysisPass<ASTDefUsePass> {
protected:
  DefUse& du;
  const Set<clang::UnaryOperator::Opcode> defOps1;
  const Set<clang::BinaryOperator::Opcode> defOps2;

protected:
  void findDefd(clang::Expr* expr, clang::Stmt* user);
  void findUsed(clang::Expr* expr, clang::Stmt* user);

public:
  void process(clang::IfStmt* ifStmt);
  void process(clang::DoStmt* doStmt);
  void process(clang::WhileStmt* whileStmt);
  void process(clang::ForStmt* forStmt);
  void process(clang::SwitchStmt* swtchStmt);
  void process(clang::ReturnStmt* retStmt);
  void process(clang::InitListExpr* initList);
  void process(clang::BinaryOperator* binOp);
  void process(clang::UnaryOperator* unOp);
  void process(clang::ConditionalOperator* cond);
  void process(clang::CallExpr* callExpr);
  void process(clang::MemberExpr* memberExpr);
  void process(clang::CStyleCastExpr* castExpr);
  void process(clang::ArraySubscriptExpr* arrayExpr);
  void process(clang::FunctionDecl* f);

public:
  ASTDefUsePass(CishContext& context, DefUse& du);
  ASTDefUsePass(const ASTDefUsePass&) = delete;
  ASTDefUsePass(ASTDefUsePass&&) = delete;
  virtual ~ASTDefUsePass() = default;

  virtual llvm::StringRef getPassName() const override;
};

ASTDefUsePass::ASTDefUsePass(CishContext& context, DefUse& du)
    : ASTFunctionAnalysisPass<ASTDefUsePass>(context), du(du),
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

llvm::StringRef ASTDefUsePass::getPassName() const {
  return "Cish AST Def Use Pass";
}

void ASTDefUsePass::findDefd(Expr* expr, Stmt* user) {
  if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
    if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
      du.defMap[var].push_back(user);
}

void ASTDefUsePass::findUsed(Expr* expr, Stmt* user) {
  if(auto* declRef = dyn_cast<DeclRefExpr>(expr))
    if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
      du.useMap[var].push_back(user);
}

void ASTDefUsePass::process(ConditionalOperator* condOp) {
  findUsed(condOp->getCond(), condOp);
  findUsed(condOp->getTrueExpr(), condOp);
  findUsed(condOp->getFalseExpr(), condOp);
}

void ASTDefUsePass::process(BinaryOperator* binOp) {
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
}

void ASTDefUsePass::process(UnaryOperator* unOp) {
  Expr* expr = unOp->getSubExpr();
  if(defOps1.contains(unOp->getOpcode()))
    findDefd(expr, unOp);
  findUsed(expr, unOp);
}

void ASTDefUsePass::process(CallExpr* callExpr) {
  // FIXME: Call expressions may have variables passed by pointer/reference
  // which might get mutated in the call. Not sure whether or not they should
  // be considered defs
  for(Expr* arg : callExpr->arguments())
    findUsed(arg, callExpr);
}

void ASTDefUsePass::process(CStyleCastExpr* castExpr) {
  findUsed(castExpr->getSubExpr(), castExpr);
}

void ASTDefUsePass::process(ArraySubscriptExpr* arrExpr) {
  findUsed(arrExpr->getBase(), arrExpr);
  findUsed(arrExpr->getIdx(), arrExpr);
}

void ASTDefUsePass::process(MemberExpr* memberExpr) {
  findUsed(memberExpr->getBase(), memberExpr);
}

void ASTDefUsePass::process(InitListExpr* initList) {
  for(Expr* expr : initList->inits())
    findUsed(expr, initList);
}

void ASTDefUsePass::process(SwitchStmt* switchStmt) {
  findUsed(switchStmt->getCond(), switchStmt);
}

void ASTDefUsePass::process(ReturnStmt* retStmt) {
  if(Expr* expr = retStmt->getRetValue())
    findUsed(expr, retStmt);
}

void ASTDefUsePass::process(IfStmt* ifStmt) {
  findUsed(ifStmt->getCond(), ifStmt);
}

void ASTDefUsePass::process(DoStmt* doStmt) {
  findUsed(doStmt->getCond(), doStmt);
}

void ASTDefUsePass::process(ForStmt* forStmt) {
  findUsed(forStmt->getCond(), forStmt);
}

void ASTDefUsePass::process(WhileStmt* whileStmt) {
  findUsed(whileStmt->getCond(), whileStmt);
}

void ASTDefUsePass::process(FunctionDecl* f) {
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
}

DefUse::DefUse(CishContext& context) : context(context) {
  ;
}

void DefUse::removeUse(const VarDecl* var, Stmt* stmt) {
  useMap.at(var).erase(stmt);
}

void DefUse::removeDef(const VarDecl* var, Stmt* stmt) {
  defMap.at(var).erase(stmt);
}

void DefUse::removeVar(const VarDecl* var) {
  if(not(hasZeroDefs(var) and hasZeroUses(var)))
    fatal(error() << "Cannot remove var: " << var->getName() << ". Defs: "
                  << getNumDefs(var) << ". Uses: " << getNumUses(var));
  defMap.erase(var);
  useMap.erase(var);
}

llvm::iterator_range<DefUse::def_iterator>
DefUse::defs(const VarDecl* var) const {
  return llvm::make_range(defMap.at(var).begin(), defMap.at(var).end());
}

llvm::iterator_range<DefUse::use_iterator>
DefUse::uses(const VarDecl* var) const {
  return llvm::make_range(useMap.at(var).begin(), useMap.at(var).end());
}

bool DefUse::isDefined(const VarDecl* var) const {
  return getNumDefs(var);
}

bool DefUse::hasZeroDefs(const VarDecl* var) const {
  return getNumDefs(var) == 0;
}

bool DefUse::hasSingleDef(const VarDecl* var) const {
  return getNumDefs(var) == 1;
}

Stmt* DefUse::getSingleDef(const VarDecl* var) const {
  if(hasSingleDef(var))
    return *defMap.at(var).begin();
  return nullptr;
}

unsigned DefUse::getNumDefs(const VarDecl* var) const {
  return defMap.at(var).size();
}

bool DefUse::isUsed(const VarDecl* var) const {
  return getNumUses(var);
}

bool DefUse::hasZeroUses(const VarDecl* var) const {
  return getNumUses(var) == 0;
}

bool DefUse::hasSingleUse(const VarDecl* var) const {
  return getNumUses(var) == 1;
}

Stmt* DefUse::getSingleUse(const VarDecl* var) const {
  if(hasSingleUse(var))
    return *useMap.at(var).begin();
  return nullptr;
}

unsigned DefUse::getNumUses(const VarDecl* var) const {
  return useMap.at(var).size();
}

bool DefUse::runOnFunction(FunctionDecl* f) {
  ASTDefUsePass(context, *this).runOnFunction(f);

  return true;
}

} // namespace cish
