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

#include "ASTExprPass.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTSimplifyOperatorsPass : public ASTExprPass<ASTSimplifyOperatorsPass> {
public:
  // Clang doesn't unique expressions. But because this is coming from LLVM
  // which does unique values, if two expressions have different addresses,
  // they are different
  // It might be better to be more rigorous about this because if we ever
  // support GCC, then this might not necessarily be true
  bool identical(const Expr* e1, const Expr* e2) {
    return e1 == e2;
  }

  Expr* process(MemberExpr* memberExpr) {
    bool arrow
        = isa<PointerType>(memberExpr->getBase()->getType().getTypePtr());
    changed |= (arrow != memberExpr->isArrow());
    memberExpr->setArrow(arrow);
    return memberExpr;
  }

  Expr* process(BinaryOperator* binOp) {
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
            changed |= true;
            break;
          case BO_Sub:
            binOp->setOpcode(BO_SubAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Mul:
            binOp->setOpcode(BO_MulAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Div:
            binOp->setOpcode(BO_DivAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Rem:
            binOp->setOpcode(BO_RemAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Shl:
            binOp->setOpcode(BO_ShlAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Shr:
            binOp->setOpcode(BO_ShrAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_And:
            binOp->setOpcode(BO_AndAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Or:
            binOp->setOpcode(BO_OrAssign);
            binOp->setRHS(rhs1);
            changed |= true;
            break;
          case BO_Xor:
            binOp->setOpcode(BO_XorAssign);
            binOp->setRHS(rhs1);
            changed |= true;
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

  Expr* process(UnaryOperator* unOp) {
    switch(unOp->getOpcode()) {
    case UO_Deref:
      // Cancel out consecutive occurences of * and &
      if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr()))
        if(subOp->getOpcode() == UO_AddrOf)
          return subOp->getSubExpr();
      break;
    case UO_LNot:
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
      } else if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr())) {
        if(subOp->getOpcode() == UO_LNot)
          return subOp->getSubExpr();
      }
      break;
    default:
      break;
    }

    return unOp;
  }

public:
  ASTSimplifyOperatorsPass(CishContext& context) : ASTExprPass(context) {
    ;
  }

  ASTSimplifyOperatorsPass(const ASTSimplifyOperatorsPass&) = delete;
  ASTSimplifyOperatorsPass(ASTSimplifyOperatorsPass&&) = delete;
  virtual ~ASTSimplifyOperatorsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "Cish AST Simplify Operators";
  }
}; // namespace cish

} // namespace cish

cish::ASTPass* createASTSimplifyOperatorsPass(cish::CishContext& context) {
  return new cish::ASTSimplifyOperatorsPass(context);
}
