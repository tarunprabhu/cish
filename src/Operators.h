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

#ifndef CISH_OPERATORS_H
#define CISH_OPERATORS_H

#include "Map.h"

#include <clang/AST/ExprCXX.h>

// Compare the precedences of operators
bool operator>(clang::BinaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2);
bool operator>=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator<(clang::BinaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2);
bool operator<=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator==(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator!=(clang::BinaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);

bool operator>(clang::UnaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2);
bool operator>=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator<(clang::UnaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2);
bool operator<=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator==(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator!=(clang::UnaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);

bool operator>(clang::UnaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2);
bool operator>=(clang::UnaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator<(clang::UnaryOperator::Opcode op1,
               clang::BinaryOperator::Opcode op2);
bool operator<=(clang::UnaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator==(clang::UnaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);
bool operator!=(clang::UnaryOperator::Opcode op1,
                clang::BinaryOperator::Opcode op2);

bool operator>(clang::BinaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2);
bool operator>=(clang::BinaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator<(clang::BinaryOperator::Opcode op1,
               clang::UnaryOperator::Opcode op2);
bool operator<=(clang::BinaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator==(clang::BinaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);
bool operator!=(clang::BinaryOperator::Opcode op1,
                clang::UnaryOperator::Opcode op2);

namespace cish {

namespace Operator {

bool isArithmetic(clang::BinaryOperator::Opcode op);
bool isBitwise(clang::BinaryOperator::Opcode op);
bool isLogical(clang::BinaryOperator::Opcode op);
bool isRelational(clang::BinaryOperator::Opcode op);

clang::BinaryOperator::Opcode getInverse(clang::BinaryOperator::Opcode);

} // namespace Operator

} // namespace cish

#endif // CISH_OPERATORS_H
