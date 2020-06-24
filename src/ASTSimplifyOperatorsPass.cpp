#include "ASTExprPass.h"
#include "ASTStreamer.h"

#include <clang/Analysis/Analyses/ExprMutationAnalyzer.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTSimplifyOperatorsPass : public ASTExprPass {
protected:
  virtual Expr* process(BinaryOperator* binOp) override;
  virtual Expr* process(UnaryOperator* unOp) override;
  virtual Expr* process(MemberExpr* memberExpr) override;

public:
  ASTSimplifyOperatorsPass(ASTContext& astContext);

  virtual llvm::StringRef getPassName() const override;
};

ASTSimplifyOperatorsPass::ASTSimplifyOperatorsPass(ASTContext& astContext)
    : ASTExprPass(astContext) {
  ;
}

llvm::StringRef ASTSimplifyOperatorsPass::getPassName() const {
  return "Cish AST Simplify Operators";
}

static bool identical(const Expr* e1, const Expr* e2) {
  return e1 == e2;
}

Expr* ASTSimplifyOperatorsPass::process(MemberExpr* memberExpr) {
  ASTExprPass::process(memberExpr);

  memberExpr->setArrow(
      isa<PointerType>(memberExpr->getBase()->getType().getTypePtr()));
  return memberExpr;
}

Expr* ASTSimplifyOperatorsPass::process(BinaryOperator* binOp) {
  ASTExprPass::process(binOp);

  BinaryOperator::Opcode opc = binOp->getOpcode();
  Expr* lhs = binOp->getLHS();
  Expr* rhs = binOp->getRHS();
  switch(opc) {
  case BO_Assign:
    if(auto* rhsOp = dyn_cast<BinaryOperator>(rhs)) {
      Expr* rhs0 = rhsOp->getLHS();
      Expr* rhs1 = rhsOp->getRHS();
      if(identical(lhs, rhs0)) {
        switch(rhsOp->getOpcode()) {
        case BO_Add:
          binOp->setOpcode(BO_AddAssign);
          binOp->setRHS(rhs1);
          break;
        case BO_Sub:
          binOp->setOpcode(BO_SubAssign);
          binOp->setRHS(rhs1);
        case BO_Mul:
          binOp->setOpcode(BO_MulAssign);
          binOp->setRHS(rhs1);
        case BO_Div:
          binOp->setOpcode(BO_DivAssign);
          binOp->setRHS(rhs1);
        case BO_Rem:
          binOp->setOpcode(BO_RemAssign);
          binOp->setRHS(rhs1);
        case BO_Shl:
          binOp->setOpcode(BO_ShlAssign);
          binOp->setRHS(rhs1);
        case BO_Shr:
          binOp->setOpcode(BO_ShrAssign);
          binOp->setRHS(rhs1);
        case BO_And:
          binOp->setOpcode(BO_AndAssign);
          binOp->setRHS(rhs1);
        case BO_Or:
          binOp->setOpcode(BO_OrAssign);
          binOp->setRHS(rhs1);
        case BO_Xor:
          binOp->setOpcode(BO_XorAssign);
          binOp->setRHS(rhs1);
          break;
        default:
          break;
        }
      }
    }
    break;
  default:
    break;
  }
  // TODO: Can merge a unary minus or negative number on the RHS
  // of an addition or subtraction

  return binOp;
}

Expr* ASTSimplifyOperatorsPass::process(UnaryOperator* unOp) {
  ASTExprPass::process(unOp);

  UnaryOperator::Opcode op = unOp->getOpcode();
  if(op == UO_Deref) {
    // Cancel out consecutive occurences of * and &
    if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr()))
      if(subOp->getOpcode() == UO_AddrOf)
        return subOp->getSubExpr();
  } else if(op == UO_LNot) {
    // Convert relational operators of the form
    //
    // !(a <rel> b)
    //
    // to
    //
    // (a <inv-rel> b)
    //
    if(auto* binOp = dyn_cast<BinaryOperator>(unOp->getSubExpr())) {
      switch(binOp->getOpcode()) {
      case BO_EQ:
        binOp->setOpcode(BO_NE);
        return binOp;
      case BO_NE:
        binOp->setOpcode(BO_EQ);
        return binOp;
      case BO_GT:
        binOp->setOpcode(BO_LE);
        return binOp;
      case BO_GE:
        binOp->setOpcode(BO_LT);
        return binOp;
      case BO_LT:
        binOp->setOpcode(BO_GE);
        return binOp;
      case BO_LE:
        binOp->setOpcode(BO_GT);
        return binOp;
      default:
        break;
      }
    }
  }

  return unOp;
}

} // namespace cish

cish::ASTFunctionPass* createASTSimplifyOperatorsPass(ASTContext& astContext) {
  return new cish::ASTSimplifyOperatorsPass(astContext);
}
