#include "ASTFunctionPass.h"
#include "ASTStreamer.h"
#include "DefUse.h"
#include "Diagnostics.h"
#include "Map.h"
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

class AddrTakenVisitor : public RecursiveASTVisitor<AddrTakenVisitor> {
protected:
  Set<VarDecl*>& vars;

public:
  explicit AddrTakenVisitor(Set<VarDecl*>& vars) : vars(vars) {
    ;
  }

  bool VisitUnaryOperator(UnaryOperator* unOp) {
    if(unOp->getOpcode() == UO_AddrOf)
      if(auto* declRef = dyn_cast<DeclRefExpr>(unOp->getSubExpr()))
        if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
          vars.insert(var);
    return true;
  }
};

class ASTPropagateExprsPass : public ASTFunctionPass {
protected:
  bool shouldReplace(const Stmt* stmt);
  bool isShort(const UnaryOperator* unOp);
  bool isShort(const Expr* expr);
  bool replace(Expr* expr, VarDecl* var);
  void replace(BinaryOperator* binOp, VarDecl* var, Expr* repl);
  void replace(UnaryOperator* unOp, VarDecl* var, Expr* repl);
  void replace(ConditionalOperator* condOp, VarDecl* var, Expr* repl);
  void replace(ArraySubscriptExpr* arrExpr, VarDecl* var, Expr* repl);
  void replace(MemberExpr* memberExpr, VarDecl* var, Expr* repl);
  void replace(CallExpr* callExpr, VarDecl* var, Expr* repl);
  void replace(CastExpr* castExpr, VarDecl* var, Expr* repl);
  void replace(ReturnStmt* retStmt, VarDecl* var, Expr* repl);
  void replace(IfStmt* ifStmt, VarDecl* var, Expr* repl);
  void replace(DoStmt* doStmt, VarDecl* var, Expr* repl);
  void replace(ForStmt* forStmt, VarDecl* var, Expr* repl);
  void replace(WhileStmt* whileStmt, VarDecl* var, Expr* repl);
  void replace(SwitchStmt* switchStmt, VarDecl* var, Expr* repl);
  void replace(Stmt* dst, VarDecl* var, Expr* repl);

public:
  ASTPropagateExprsPass(CishContext& context);
  ASTPropagateExprsPass(const ASTPropagateExprsPass&) = delete;
  ASTPropagateExprsPass(ASTPropagateExprsPass&&) = delete;
  virtual ~ASTPropagateExprsPass() = default;

  virtual llvm::StringRef getPassName() const override;
  virtual bool runOnFunction(FunctionDecl* f) override;
};

ASTPropagateExprsPass::ASTPropagateExprsPass(CishContext& context)
    : ASTFunctionPass(context) {
  ;
}

llvm::StringRef ASTPropagateExprsPass::getPassName() const {
  return "Cish AST Propagate Exprs";
}

bool ASTPropagateExprsPass::replace(Expr* expr, VarDecl* var) {
  if(auto* declRefExpr = dyn_cast<DeclRefExpr>(expr))
    return declRefExpr->getFoundDecl() == var;
  return false;
}

void ASTPropagateExprsPass::replace(BinaryOperator* binOp,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(binOp->getLHS(), var))
    binOp->setLHS(repl);
  if(replace(binOp->getRHS(), var))
    binOp->setRHS(repl);
}

void ASTPropagateExprsPass::replace(UnaryOperator* unOp,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(unOp->getSubExpr(), var))
    unOp->setSubExpr(repl);
}

void ASTPropagateExprsPass::replace(ConditionalOperator* condOp,
                                    VarDecl* var,
                                    Expr* repl) {
  fatal(error() << "Not implemented\n");
}

void ASTPropagateExprsPass::replace(ArraySubscriptExpr* arrExpr,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(arrExpr->getBase(), var))
    arrExpr->setLHS(repl);
  if(replace(arrExpr->getIdx(), var))
    arrExpr->setRHS(repl);
}

void ASTPropagateExprsPass::replace(MemberExpr* memberExpr,
                                    VarDecl* var,
                                    Expr* repl) {
  ;
}

void ASTPropagateExprsPass::replace(CallExpr* callExpr,
                                    VarDecl* var,
                                    Expr* repl) {
  for(unsigned i = 0; i < callExpr->getNumArgs(); i++)
    if(replace(callExpr->getArg(i), var))
      callExpr->setArg(i, repl);
  if(replace(callExpr->getCallee(), var))
    callExpr->setCallee(repl);
}

void ASTPropagateExprsPass::replace(CastExpr* castExpr,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(castExpr->getSubExpr(), var))
    castExpr->setSubExpr(repl);
}

void ASTPropagateExprsPass::replace(ReturnStmt* retStmt,
                                    VarDecl* var,
                                    Expr* repl) {
  if(Expr* retValue = retStmt->getRetValue())
    if(replace(retValue, var))
      retStmt->setRetValue(repl);
}

void ASTPropagateExprsPass::replace(IfStmt* ifStmt, VarDecl* var, Expr* repl) {
  if(replace(ifStmt->getCond(), var))
    ifStmt->setCond(repl);
}

void ASTPropagateExprsPass::replace(DoStmt* doStmt, VarDecl* var, Expr* repl) {
  if(replace(doStmt->getCond(), var))
    doStmt->setCond(repl);
}

void ASTPropagateExprsPass::replace(ForStmt* forStmt,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(forStmt->getCond(), var))
    forStmt->setCond(repl);
}

void ASTPropagateExprsPass::replace(WhileStmt* whileStmt,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(whileStmt->getCond(), var))
    whileStmt->setCond(repl);
}

void ASTPropagateExprsPass::replace(SwitchStmt* switchStmt,
                                    VarDecl* var,
                                    Expr* repl) {
  if(replace(switchStmt->getCond(), var))
    switchStmt->setCond(repl);
}

void ASTPropagateExprsPass::replace(Stmt* stmt, VarDecl* var, Expr* repl) {
  if(auto* binOp = dyn_cast<BinaryOperator>(stmt))
    return replace(binOp, var, repl);
  else if(auto* unOp = dyn_cast<UnaryOperator>(stmt))
    return replace(binOp, var, repl);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(stmt))
    return replace(condOp, var, repl);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(stmt))
    return replace(arrExpr, var, repl);
  else if(auto* memberExpr = dyn_cast<MemberExpr>(stmt))
    return replace(memberExpr, var, repl);
  else if(auto* callExpr = dyn_cast<CallExpr>(stmt))
    return replace(callExpr, var, repl);
  else if(auto* castExpr = dyn_cast<CastExpr>(stmt))
    return replace(castExpr, var, repl);
  else if(auto* retStmt = dyn_cast<ReturnStmt>(stmt))
    return replace(retStmt, var, repl);
  else if(auto* ifStmt = dyn_cast<IfStmt>(stmt))
    return replace(ifStmt, var, repl);
  else if(auto* doStmt = dyn_cast<DoStmt>(stmt))
    return replace(doStmt, var, repl);
  else if(auto* forStmt = dyn_cast<ForStmt>(stmt))
    return replace(forStmt, var, repl);
  else if(auto* whileStmt = dyn_cast<WhileStmt>(stmt))
    return replace(whileStmt, var, repl);
  else if(auto* switchStmt = dyn_cast<SwitchStmt>(stmt))
    return replace(switchStmt, var, repl);
}

bool ASTPropagateExprsPass::isShort(const UnaryOperator* unOp) {
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

bool ASTPropagateExprsPass::isShort(const Expr* expr) {
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

bool ASTPropagateExprsPass::shouldReplace(const Stmt* stmt) {
  if(const auto* binOp = dyn_cast<BinaryOperator>(stmt)) {
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
  }

  return false;
}

bool ASTPropagateExprsPass::runOnFunction(FunctionDecl* f) {
  Set<VarDecl*> addrTaken;
  AddrTakenVisitor(addrTaken).TraverseDecl(f);

  DefUse& du = getDefUse();

  Map<VarDecl*, Stmt*> repls;
  for(Decl* decl : f->decls())
    if(auto* var = dyn_cast<VarDecl>(decl))
      if(Stmt* def = du.getSingleDef(var))
        if((not addrTaken.contains(var))
           and shouldReplace(cast<BinaryOperator>(def)->getRHS()))
          repls[var] = cast<BinaryOperator>(def)->getRHS();

  for(auto& i : repls) {
    VarDecl* var = i.first;
    Expr* repl = cast<Expr>(i.second);
    Vector<Stmt*> uses(du.uses(var).begin(), du.uses(var).end());
    for(Stmt* use : uses) {
      replace(use, var, repl);
      du.removeUse(var, use);
    }
  }

  return repls.size();
}

} // namespace cish

cish::ASTFunctionPass* createASTPropagateExprsPass(cish::CishContext& context) {
  return new cish::ASTPropagateExprsPass(context);
}
