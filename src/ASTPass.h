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

#include <clang/AST/ASTContext.h>

namespace cish {

class CishContext;
class AST;

class ASTPass {
protected:
  static constexpr unsigned ModifiesAST = 0x1;
  static constexpr unsigned PostOrder = 0x2;
  static constexpr unsigned IterateUntilConvergence = 0x4;

protected:
  CishContext& cishContext;
  clang::ASTContext& astContext;
  unsigned flags;
  AST* ast;

private:
  bool hasFlag(unsigned flag) const;

protected:
  ASTPass(CishContext& cishContext, unsigned flags);
  ASTPass(const ASTPass&) = delete;
  ASTPass(ASTPass&&) = delete;

  bool hasModifiesAST() const;
  bool hasPostorder() const;
  bool hasIterateUntilConvergence() const;

public:
  virtual ~ASTPass() = default;

  virtual llvm::StringRef getPassName() const = 0;
  virtual llvm::StringRef getPassLongName() const;

  virtual bool runOnFunction(clang::FunctionDecl* f) = 0;
};

} // namespace cish

#endif // CISH_AST_PASS_H
