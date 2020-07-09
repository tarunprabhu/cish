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

#ifndef CISH_AST_PASS_H
#define CISH_AST_PASS_H

#include "ExprNumberMap.h"
#include "ParentMap.h"
#include "Set.h"
#include "UsesMap.h"

#include <clang/AST/ASTContext.h>

namespace cish {

class CishContext;
class AST;

class ASTPass {
protected:
  static constexpr unsigned PreOrder = 1 << 0;
  static constexpr unsigned RequireExprNums = 1 << 1;
  static constexpr unsigned RequireUses = 1 << 2;
  static constexpr unsigned RequireCFG = 1 << 3;
  static constexpr unsigned OnePass = 1 << 4;

protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;
  unsigned flags;
  AST* ast;
  ExprNumberMap em;
  ParentMap pm;
  UsesMap um;
  Set<clang::VarDecl*> addrTaken;

private:
  bool hasFlag(unsigned flag) const;

protected:
  ASTPass(CishContext& cishContext, unsigned flags);
  ASTPass(const ASTPass&) = delete;
  ASTPass(ASTPass&&) = delete;

  bool hasPreOrder() const;
  bool hasRequireExprNums() const;
  bool hasRequireUses() const;
  bool hasRequireCFG() const;
  bool hasOnePass() const;

public:
  virtual ~ASTPass() = default;

  virtual llvm::StringRef getPassName() const = 0;
  virtual llvm::StringRef getPassLongName() const;

  virtual bool runOnFunction(clang::FunctionDecl* f) = 0;
};

} // namespace cish

#endif // CISH_AST_PASS_H
