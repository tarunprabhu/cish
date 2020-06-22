#include "ASTExprPass.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTSimplifyOperatorsPass : public ASTExprPass {
protected:
  virtual Expr* process(BinaryOperator* binOp) override;
  virtual Expr* process(UnaryOperator* unOp) override;

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

Expr* ASTSimplifyOperatorsPass::process(BinaryOperator* binOp) {
  ASTExprPass::process(binOp);
  // TODO: Can merge a unary minus or negative number on the RHS
  // of an addition or subtraction

  return binOp;
}

Expr* ASTSimplifyOperatorsPass::process(UnaryOperator* unOp) {
  ASTExprPass::process(unOp);

  UnaryOperator::Opcode op = unOp->getOpcode();
  if(op == UO_Deref) {
    if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr()))
      if(subOp->getOpcode() == UO_AddrOf)
        return subOp->getSubExpr();
  } else if(op == UO_LNot) {
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
