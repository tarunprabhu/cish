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

#include "Operators.h"
#include "Map.h"
#include "Map2.h"

using namespace clang;

static const cish::Map<UnaryOperator::Opcode, unsigned> prec1 = {
    {UO_Minus, 14},
    {UO_AddrOf, 14},
    {UO_Deref, 14},
    {UO_LNot, 14},
    {UO_Not, 14},
};

static const cish::Map<BinaryOperator::Opcode, unsigned> prec2 = {
    {BO_PtrMemD, 15}, {BO_PtrMemI, 15}, {BO_Mul, 13}, {BO_Div, 13},
    {BO_Rem, 13},     {BO_Add, 12},     {BO_Sub, 12}, {BO_Shl, 11},
    {BO_Shr, 11},     {BO_LE, 10},      {BO_LT, 10},  {BO_GE, 10},
    {BO_GT, 10},      {BO_EQ, 9},       {BO_NE, 9},   {BO_And, 8},
    {BO_Xor, 7},      {BO_Or, 6},       {BO_LAnd, 5}, {BO_LOr, 4},
    {BO_Assign, 2},   {BO_Comma, 1},
};

// Compare the precedences of operators
bool operator>(clang::BinaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) > prec2.at(op2);
}

bool operator>=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) >= prec2.at(op2);
}

bool operator<(clang::BinaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) < prec2.at(op2);
}

bool operator<=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) <= prec2.at(op2);
}

bool operator==(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) == prec2.at(op2);
}

bool operator!=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2) {
  return prec2.at(op1) != prec2.at(op2);
}

bool operator>(clang::UnaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) > prec1.at(op2);
}

bool operator>=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) >= prec1.at(op2);
}

bool operator<(clang::UnaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) < prec1.at(op2);
}

bool operator<=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) <= prec1.at(op2);
}

bool operator==(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) == prec1.at(op2);
}

bool operator!=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2) {
  return prec1.at(op1) != prec1.at(op2);
}

bool operator>(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return true;
}

bool operator>=(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return true;
}

bool operator<(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return false;
}

bool operator<=(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return false;
}

bool operator==(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return false;
}

bool operator!=(clang::UnaryOperator::Opcode, clang::BinaryOperator::Opcode) {
  return false;
}

bool operator>(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return false;
}

bool operator>=(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return false;
}

bool operator<(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return true;
}

bool operator<=(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return true;
}

bool operator==(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return false;
}

bool operator!=(clang::BinaryOperator::Opcode, clang::UnaryOperator::Opcode) {
  return false;
}

namespace cish {

namespace Operator {

bool isArithmetic(BinaryOperator::Opcode op) {
  switch(op) {
  case BO_Add:
  case BO_Sub:
  case BO_Mul:
  case BO_Div:
  case BO_Rem:
    return true;
  default:
    return false;
  }
}

bool isRelational(BinaryOperator::Opcode op) {
  switch(op) {
  case BO_EQ:
  case BO_NE:
  case BO_LT:
  case BO_LE:
  case BO_GT:
  case BO_GE:
    return true;
  default:
    return false;
  }
}

bool isBitwise(BinaryOperator::Opcode op) {
  return (op == BO_And) or (op == BO_Or) or (op == BO_Xor);
}

bool isLogical(BinaryOperator::Opcode op) {
  return (op == BO_LAnd) or (op == BO_LOr);
}

BinaryOperator::Opcode getInverse(BinaryOperator::Opcode opc) {
  static const Map2S<BinaryOperator::Opcode> ops = {
      {BO_Add, BO_Sub},
      {BO_Mul, BO_Div},

      {BO_EQ, BO_NE},
      {BO_GT, BO_LE},
      {BO_GE, BO_LT},
  };

  return ops.at(opc);
}

} // namespace Operator

} // namespace cish
