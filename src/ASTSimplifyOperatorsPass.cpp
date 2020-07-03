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

#include "ASTFunctionPass.h"
#include "ClangUtils.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTSimplifyOperatorsPass
    : public ASTFunctionPass<ASTSimplifyOperatorsPass> {
public:
  bool process(MemberExpr* memberExpr) {
    bool changed = false;

    bool arrow
        = isa<PointerType>(memberExpr->getBase()->getType().getTypePtr());
    changed |= (arrow != memberExpr->isArrow());
    memberExpr->setArrow(arrow);

    return changed;
  }

  bool process(BinaryOperator* binOp) {
    bool changed = false;

    BinaryOperator::Opcode opc = binOp->getOpcode();
    Expr* lhs = binOp->getLHS();
    Expr* rhs = binOp->getRHS();
    switch(opc) {
    case BO_Add:
      //
      // Match:   op1 + -op2
      // Replace: op1 - op2
      //
      if(auto* unOp = dyn_cast<UnaryOperator>(rhs)) {
        if(unOp->getOpcode() == UO_Minus) {
          binOp->setOpcode(BO_Sub);
          ast->replaceExprWith(binOp->getRHS(), unOp->getSubExpr());
          changed |= true;
        }
      } else if(auto* intLit = dyn_cast<IntegerLiteral>(rhs)) {
        llvm::APInt ival = intLit->getValue();
        if(ival.isNegative()) {
          ival.negate();
          binOp->setOpcode(BO_Sub);
          ast->replaceExprWith(
              binOp->getRHS(),
              builder.createIntLiteral(ival, intLit->getType()));
          changed |= true;
        }
      }
      break;
    case BO_Sub:
      //
      // Match:   op1 - -op2
      // Replace: op1 + op2
      //
      if(auto* unOp = dyn_cast<UnaryOperator>(rhs)) {
        if(unOp->getOpcode() == UO_Minus) {
          binOp->setOpcode(BO_Add);
          ast->replaceExprWith(binOp->getRHS(), unOp->getSubExpr());
          changed |= true;
        }
      } else if(auto* intLit = dyn_cast<IntegerLiteral>(rhs)) {
        llvm::APInt ival = intLit->getValue();
        if(ival.isNegative()) {
          ival.negate();
          binOp->setOpcode(BO_Add);
          ast->replaceExprWith(
              binOp->getRHS(),
              builder.createIntLiteral(ival, intLit->getType()));
          changed |= true;
        }
      }
      break;
    case BO_LT:
      //
      // Match:   (op - 1) < op2
      // Replace: op1 <= op2
      //
      if(auto* subOp = dyn_cast<BinaryOperator>(lhs)) {
        if((subOp->getOpcode() == BO_Sub) and isOne(subOp->getRHS())) {
          binOp->setOpcode(BO_LE);
          ast->replaceExprWith(binOp->getLHS(), subOp->getRHS());
          changed |= true;
        }
      }
      break;
    default:
      break;
    }

    return changed;
  }

  bool process(UnaryOperator* unOp) {
    bool changed = false;

    switch(unOp->getOpcode()) {
    case UO_Deref:
      if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr()))
        //
        // Match:   *&op
        // Replace: op
        //
        if(subOp->getOpcode() == UO_AddrOf)
          changed |= ast->replaceExprWith(unOp, subOp->getSubExpr());
      break;
    case UO_LNot:
      if(auto* subOp = dyn_cast<UnaryOperator>(unOp->getSubExpr())) {
        //
        // Match:   !!op
        // Replace: op
        //
        if(subOp->getOpcode() == UO_LNot)
          changed |= ast->replaceExprWith(unOp, subOp->getSubExpr());
      } else if(auto* binOp = dyn_cast<BinaryOperator>(unOp->getSubExpr())) {
        //
        // Match:   !(a <rel> b)
        // Replace: (a <inv-rel> b)
        //
        switch(binOp->getOpcode()) {
        case BO_EQ:
          binOp->setOpcode(BO_NE);
          changed |= true;
        case BO_NE:
          binOp->setOpcode(BO_EQ);
          changed |= true;
        case BO_GT:
          binOp->setOpcode(BO_LE);
          changed |= true;
        case BO_GE:
          binOp->setOpcode(BO_LT);
          changed |= true;
        case BO_LT:
          binOp->setOpcode(BO_GE);
          changed |= true;
        case BO_LE:
          binOp->setOpcode(BO_GT);
          changed |= true;
        default:
          break;
        }
      }
      break;
    default:
      break;
    }

    return changed;
  }

public:
  ASTSimplifyOperatorsPass(CishContext& context) : ASTFunctionPass(context) {
    ;
  }

  ASTSimplifyOperatorsPass(const ASTSimplifyOperatorsPass&) = delete;
  ASTSimplifyOperatorsPass(ASTSimplifyOperatorsPass&&) = delete;
  virtual ~ASTSimplifyOperatorsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-operators";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "Cish AST Simplify Operators";
  }
};

} // namespace cish

cish::ASTPass* createASTSimplifyOperatorsPass(cish::CishContext& context) {
  return new cish::ASTSimplifyOperatorsPass(context);
}
