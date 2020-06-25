#include "ASTBuilder.h"
#include "ASTExprPass.h"
#include "Diagnostics.h"

using namespace clang;

namespace cish {

class ASTConstantFoldingPass : public ASTExprPass<ASTConstantFoldingPass> {
protected:
  ASTBuilder builder;

protected:
  bool isConstant(const Expr* expr) const;
  Expr*
  evaluate(BinaryOperator::Opcode op, IntegerLiteral* lhs, IntegerLiteral* rhs);
  Expr* evaluate(BinaryOperator::Opcode op,
                 FloatingLiteral* lhs,
                 FloatingLiteral* rhs);
  Expr* evaluate(BinaryOperator::Opcode op,
                 CXXBoolLiteralExpr* lhs,
                 CXXBoolLiteralExpr* rhs);
  Expr* evaluate(BinaryOperator::Opcode op, Expr* lhs, Expr* rhs);
  Expr* evaluateIdentity(BinaryOperator* binOp);

  bool canAssociate(BinaryOperator::Opcode, BinaryOperator::Opcode);
  bool isOne(const Expr* expr);
  bool isZero(const Expr* expr);
  bool isTrue(const Expr* expr);
  bool isFalse(const Expr* expr);

public:
  Expr* process(BinaryOperator* binOp);
  Expr* process(UnaryOperator* unOp);

public:
  ASTConstantFoldingPass(CishContext& context);
  ASTConstantFoldingPass(const ASTConstantFoldingPass&) = delete;
  ASTConstantFoldingPass(ASTConstantFoldingPass&&) = delete;
  virtual ~ASTConstantFoldingPass() = default;

  virtual llvm::StringRef getPassName() const override;
  virtual bool runOnFunction(FunctionDecl* f) override;
};

ASTConstantFoldingPass::ASTConstantFoldingPass(CishContext& context)
    : ASTExprPass(context), builder(astContext) {
  ;
}

llvm::StringRef ASTConstantFoldingPass::getPassName() const {
  return "Cish AST Constant Folding";
}

bool ASTConstantFoldingPass::isOne(const Expr* expr) {
  if(const auto* i = dyn_cast<IntegerLiteral>(expr))
    return i->getValue().isOneValue();
  return false;
}

bool ASTConstantFoldingPass::isZero(const Expr* expr) {
  if(const auto* i = dyn_cast<IntegerLiteral>(expr))
    return i->getValue().getLimitedValue() == 0;
  else if(const auto* f = dyn_cast<FloatingLiteral>(expr))
    return f->getValue().isZero();
  return false;
}

bool ASTConstantFoldingPass::isFalse(const Expr* expr) {
  if(const auto* b = dyn_cast<CXXBoolLiteralExpr>(expr))
    return not b->getValue();
  return false;
}

bool ASTConstantFoldingPass::isTrue(const Expr* expr) {
  if(const auto* b = dyn_cast<CXXBoolLiteralExpr>(expr))
    return b->getValue();
  return false;
}

bool ASTConstantFoldingPass::isConstant(const Expr* expr) const {
  return isa<CXXBoolLiteralExpr>(expr) or isa<IntegerLiteral>(expr)
         or isa<FloatingLiteral>(expr);
}

Expr* ASTConstantFoldingPass::evaluate(BinaryOperator::Opcode op,
                                       CXXBoolLiteralExpr* lhs,
                                       CXXBoolLiteralExpr* rhs) {
  bool l = lhs->getValue();
  bool r = rhs->getValue();
  QualType type = lhs->getType();
  switch(op) {
  case BO_And:
    return builder.createBoolLiteral(l & r, type);
  case BO_Or:
    return builder.createBoolLiteral(l | r, type);
  case BO_Xor:
    return builder.createBoolLiteral(l ^ r, type);
  default:
    fatal(error() << "Unexpected operator for binary operands: "
                  << BinaryOperator::getOpcodeStr(op));
  }
  return nullptr;
}

Expr* ASTConstantFoldingPass::evaluate(BinaryOperator::Opcode op,
                                       IntegerLiteral* lhs,
                                       IntegerLiteral* rhs) {
  llvm::APInt l = lhs->getValue();
  llvm::APInt r = rhs->getValue();
  llvm::APInt res(l);
  QualType type = lhs->getType();
  bool isSigned = l.isSignedIntN(8) or l.isSignedIntN(16) or l.isSignedIntN(32)
                  or l.isSignedIntN(64);
  switch(op) {
  case BO_Add:
    res += r;
    return builder.createIntLiteral(res, type);
  case BO_Sub:
    res -= r;
    return builder.createIntLiteral(res, type);
  case BO_Mul:
    res *= r;
    return builder.createIntLiteral(res, type);
  case BO_Div:
  case BO_Rem:
    fatal(error() << "Not implemented: Operation in constant propagation: "
                  << BinaryOperator::getOpcodeStr(op));
    break;
  case BO_And:
    res &= r;
    return builder.createIntLiteral(res, type);
  case BO_Or:
    res |= r;
    return builder.createIntLiteral(res, type);
  case BO_Xor:
    res ^= r;
    return builder.createIntLiteral(res, type);
  case BO_Shl:
    res <<= r;
    return builder.createIntLiteral(res, type);
  case BO_Shr:
    fatal(error() << "Not implemented: >> in constant propagation");
    break;
    // return builder.createIntLiteral(res, type);
  case BO_EQ:
    return builder.createBoolLiteral(l == r);
  case BO_NE:
    return builder.createBoolLiteral(l != r);
  case BO_LT:
    if(isSigned)
      return builder.createBoolLiteral(l.slt(r));
    else
      return builder.createBoolLiteral(l.ult(r));
  case BO_LE:
    if(isSigned)
      return builder.createBoolLiteral(l.sle(r));
    else
      return builder.createBoolLiteral(l.ule(r));
  case BO_GT:
    if(isSigned)
      return builder.createBoolLiteral(l.sgt(r));
    else
      return builder.createBoolLiteral(l.ugt(r));
  case BO_GE:
    if(isSigned)
      return builder.createBoolLiteral(l.sge(r));
    else
      return builder.createBoolLiteral(l.uge(r));
  default:
    fatal(error() << "Unknown binary operator: "
                  << BinaryOperator::getOpcodeStr(op));
    break;
  }

  return nullptr;
}

Expr* ASTConstantFoldingPass::evaluate(BinaryOperator::Opcode op,
                                       FloatingLiteral* lhs,
                                       FloatingLiteral* rhs) {
  const llvm::APFloat l = lhs->getValue();
  const llvm::APFloat r = rhs->getValue();
  QualType type = lhs->getType();
  switch(op) {
  case BO_Add:
    return builder.createFloatLiteral(l + r, type);
  case BO_Sub:
    return builder.createFloatLiteral(l - r, type);
  case BO_Mul:
    return builder.createFloatLiteral(l * r, type);
  case BO_Div:
    return builder.createFloatLiteral(l / r, type);
  case BO_Rem:
    fatal(error() << "Operator % not implemneted for floats");
  case BO_EQ:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpEqual:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  case BO_NE:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpUnordered:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  case BO_LT:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpLessThan:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  case BO_LE:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpLessThan:
    case llvm::APFloat::cmpResult::cmpEqual:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  case BO_GT:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpGreaterThan:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  case BO_GE:
    switch(l.compare(r)) {
    case llvm::APFloat::cmpResult::cmpGreaterThan:
    case llvm::APFloat::cmpResult::cmpEqual:
      return builder.createBoolLiteral(true);
    default:
      return builder.createBoolLiteral(false);
    }
  default:
    fatal(error() << "Unexpected binary operator for floating point: "
                  << BinaryOperator::getOpcodeStr(op));
  }
}

Expr* ASTConstantFoldingPass::evaluate(BinaryOperator::Opcode op,
                                       Expr* lhs,
                                       Expr* rhs) {
  if(auto* ilhs = dyn_cast<IntegerLiteral>(lhs)) {
    if(auto* irhs = dyn_cast<IntegerLiteral>(rhs))
      return evaluate(op, ilhs, irhs);
  } else if(auto* flhs = dyn_cast<FloatingLiteral>(lhs)) {
    if(auto* frhs = dyn_cast<FloatingLiteral>(rhs))
      return evaluate(op, flhs, frhs);
  } else if(auto* blhs = dyn_cast<CXXBoolLiteralExpr>(lhs)) {
    if(auto* brhs = dyn_cast<CXXBoolLiteralExpr>(rhs))
      return evaluate(op, blhs, brhs);
  }
  return nullptr;
}

Expr* ASTConstantFoldingPass::evaluateIdentity(BinaryOperator* binOp) {
  switch(binOp->getOpcode()) {
  case BO_Add:
    if(isZero(binOp->getLHS()))
      return binOp->getRHS();
    else if(isZero(binOp->getRHS()))
      return binOp->getLHS();
    break;
  case BO_Sub:
    if(isZero(binOp->getRHS()))
      return binOp->getLHS();
    else if(isZero(binOp->getLHS()))
      return builder.createUnaryOperator(
          binOp->getRHS(), UO_Minus, binOp->getRHS()->getType());
    break;
  case BO_Mul:
    if(isOne(binOp->getLHS()))
      return binOp->getRHS();
    else if(isOne(binOp->getRHS()))
      return binOp->getLHS();
    break;
  case BO_Div:
    if(isOne(binOp->getRHS()))
      return binOp->getLHS();
    break;
  case BO_LAnd:
    if(isTrue(binOp->getLHS()))
      return binOp->getRHS();
    else if(isTrue(binOp->getRHS()))
      return binOp->getLHS();
    break;
  case BO_LOr:
    if(isFalse(binOp->getLHS()))
      return binOp->getRHS();
    else if(isFalse(binOp->getRHS()))
      return binOp->getLHS();
    break;
  default:
    break;
  }

  return nullptr;
}

bool ASTConstantFoldingPass::canAssociate(BinaryOperator::Opcode left,
                                          BinaryOperator::Opcode right) {
  static const Map<BinaryOperator::Opcode, Set<BinaryOperator::Opcode>> assocMap
      = {{BO_Add, {BO_Add, BO_Sub}},
         {BO_Sub, {BO_Sub, BO_Add}},
         {BO_Mul, {BO_Mul, BO_Div}},
         {BO_Div, {BO_Div, BO_Div}},
         {BO_And, {BO_And}},
         {BO_Or, {BO_Or}},
         {BO_Xor, {BO_Xor}}};
  return assocMap.contains(left) and assocMap.at(left).contains(right);
}

Expr* ASTConstantFoldingPass::process(UnaryOperator* unOp) {
  switch(unOp->getOpcode()) {
  case UO_Minus:
    break;
  default:
    break;
  }

  return unOp;
}

Expr* ASTConstantFoldingPass::process(BinaryOperator* binOp) {
  Expr* lhs = binOp->getLHS();
  Expr* rhs = binOp->getRHS();
  BinaryOperator::Opcode op = binOp->getOpcode();
  switch(op) {
  case BO_Add:
  case BO_Sub:
  case BO_Mul:
  case BO_Div:
  case BO_Rem:
  case BO_And:
  case BO_Or:
  case BO_Xor:
  case BO_Shl:
  case BO_Shr:
  case BO_EQ:
  case BO_NE:
  case BO_LT:
  case BO_LE:
  case BO_GT:
  case BO_GE:
    if(Expr* ret = evaluateIdentity(binOp)) {
      return ret;
    } else if(isConstant(lhs) and isConstant(rhs)) {
      if(Expr* ret = evaluate(binOp->getOpcode(), lhs, rhs))
        return ret;
    } else if(isConstant(rhs)) {
      if(auto* lhsOp = dyn_cast<BinaryOperator>(lhs)) {
        if(isConstant(lhsOp->getRHS())
           and canAssociate(lhsOp->getOpcode(), op)) {
          if(Expr* ret = evaluate(op, lhsOp->getRHS(), rhs)) {
            binOp->setLHS(lhsOp->getLHS());
            binOp->setRHS(ret);
          }
        }
      }
    } else if(isConstant(lhs)) {
      if(auto* rhsOp = dyn_cast<BinaryOperator>(rhs)) {
        if(isConstant(rhsOp->getLHS())
           and canAssociate(op, rhsOp->getOpcode())) {
          if(Expr* ret = evaluate(op, lhs, rhsOp->getLHS())) {
            binOp->setLHS(ret);
            binOp->setRHS(rhsOp->getRHS());
          }
        }
      }
    }
    break;
  default:
    break;
  }

  return binOp;
}

bool ASTConstantFoldingPass::runOnFunction(FunctionDecl* f) {
  bool folded = false;
  do {
    bool saveChanged = changed;
    changed = false;
    ASTExprPass::process(f->getBody());
    folded = changed;
    changed = saveChanged;
  } while(folded);

  return changed;
}

} // namespace cish

cish::ASTFunctionPass*
createASTConstantFoldingPass(cish::CishContext& context) {
  return new cish::ASTConstantFoldingPass(context);
}
