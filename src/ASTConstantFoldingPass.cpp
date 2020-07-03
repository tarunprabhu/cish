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

#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "ClangUtils.h"
#include "Diagnostics.h"
#include "Map.h"
#include "Set.h"

using namespace clang;

namespace cish {

class ASTConstantFoldingPass : public ASTFunctionPass<ASTConstantFoldingPass> {
protected:
  bool isOne(const Expr* expr) {
    if(const auto* i = dyn_cast<IntegerLiteral>(expr))
      return i->getValue().isOneValue();
    return false;
  }

  bool isZero(const Expr* expr) {
    if(const auto* i = dyn_cast<IntegerLiteral>(expr))
      return i->getValue().getLimitedValue() == 0;
    else if(const auto* f = dyn_cast<FloatingLiteral>(expr))
      return f->getValue().isZero();
    return false;
  }

  bool isFalse(const Expr* expr) {
    if(const auto* b = dyn_cast<CXXBoolLiteralExpr>(expr))
      return not b->getValue();
    return false;
  }

  bool isTrue(const Expr* expr) {
    if(const auto* b = dyn_cast<CXXBoolLiteralExpr>(expr))
      return b->getValue();
    return false;
  }

  bool isConstant(const Expr* expr) const {
    return isa<CXXBoolLiteralExpr>(expr) or isa<IntegerLiteral>(expr)
           or isa<FloatingLiteral>(expr);
  }

  Expr* evaluate(BinaryOperator::Opcode op,
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

  Expr* evaluate(BinaryOperator::Opcode op,
                 IntegerLiteral* lhs,
                 IntegerLiteral* rhs) {
    llvm::APInt l = lhs->getValue();
    llvm::APInt r = rhs->getValue();
    llvm::APInt res(l);
    QualType type = lhs->getType();
    bool isSigned = l.isSignedIntN(8) or l.isSignedIntN(16)
                    or l.isSignedIntN(32) or l.isSignedIntN(64);
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

  Expr* evaluate(BinaryOperator::Opcode op,
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

  Expr* evaluate(BinaryOperator::Opcode op, Expr* lhs, Expr* rhs) {
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

  Expr* evaluateIdentity(BinaryOperator* binOp) {
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

  bool canAssociate(BinaryOperator::Opcode left, BinaryOperator::Opcode right) {
    static const Map<BinaryOperator::Opcode, Set<BinaryOperator::Opcode>>
        assocMap = {{BO_Add, {BO_Add, BO_Sub}},
                    {BO_Sub, {BO_Sub, BO_Add}},
                    {BO_Mul, {BO_Mul, BO_Div}},
                    {BO_Div, {BO_Div, BO_Div}},
                    {BO_And, {BO_And}},
                    {BO_Or, {BO_Or}},
                    {BO_Xor, {BO_Xor}}};
    return assocMap.contains(left) and assocMap.at(left).contains(right);
  }

public:
  bool process(BinaryOperator* binOp) {
    bool changed = false;

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
      if(Expr* eval = evaluateIdentity(binOp)) {
        changed |= ast->replaceExprWith(binOp, eval);
      } else if(isConstant(lhs) and isConstant(rhs)) {
        if(Expr* eval = evaluate(binOp->getOpcode(), lhs, rhs))
          changed |= ast->replaceExprWith(binOp, eval);
      } else if(isConstant(rhs)) {
        if(auto* lhsOp = dyn_cast<BinaryOperator>(lhs)) {
          if(isConstant(lhsOp->getRHS())
             and canAssociate(lhsOp->getOpcode(), op)) {
            if(Expr* eval = evaluate(op, lhsOp->getRHS(), rhs)) {
              changed |= ast->replaceExprWith(binOp->getLHS(), lhsOp->getLHS());
              changed |= ast->replaceExprWith(binOp->getRHS(), eval);
            }
          }
        }
      } else if(isConstant(lhs)) {
        if(auto* rhsOp = dyn_cast<BinaryOperator>(rhs)) {
          if(isConstant(rhsOp->getLHS())
             and canAssociate(op, rhsOp->getOpcode())) {
            if(Expr* eval = evaluate(op, lhs, rhsOp->getLHS())) {
              changed |= ast->replaceExprWith(binOp->getLHS(), eval);
              changed |= ast->replaceExprWith(binOp->getRHS(), rhsOp->getRHS());
            }
          }
        }
      }
      break;
    default:
      break;
    }

    return changed;
  }

public:
  ASTConstantFoldingPass(CishContext& context)
      : ASTFunctionPass(context) {
    ;
  }

  ASTConstantFoldingPass(const ASTConstantFoldingPass&) = delete;
  ASTConstantFoldingPass(ASTConstantFoldingPass&&) = delete;
  virtual ~ASTConstantFoldingPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-cprop";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Constant Folding";
  }
};

} // namespace cish

cish::ASTPass* createASTConstantFoldingPass(cish::CishContext& context) {
  return new cish::ASTConstantFoldingPass(context);
}
