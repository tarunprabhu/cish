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

#ifndef AST_FUNCTION_ANALYSIS_PASS_H
#define AST_FUNCTION_ANALYSIS_PASS_H

#include "ASTFunctionPass.h"
#include "Diagnostics.h"

using clang::dyn_cast;

namespace cish {

// specialization that does the checking

// Base class to walk over all statements and expressions in a function. This
// is intended for analysis passses only, so the AST nodes are all marked
// const

template <typename DerivedT>
class ASTFunctionAnalysisPass : public ASTFunctionPass {
private:
  template <typename ClangStmt>
  struct has_process {
  private:
    template <typename T>
    static constexpr auto check(T*) -> typename std::is_same<
        decltype(std::declval<T>().processBase(std::declval<ClangStmt>())),
        void>::type;

    template <typename>
    static constexpr std::false_type check(...);

    typedef decltype(check<DerivedT>(0)) type;

  public:
    static constexpr bool value = type::value;
  };

public:
  ASTFunctionAnalysisPass(CishContext& context) : ASTFunctionPass(context) {
    ;
  }

  ASTFunctionAnalysisPass(ASTFunctionAnalysisPass&) = delete;
  ASTFunctionAnalysisPass(ASTFunctionAnalysisPass&&) = delete;
  virtual ~ASTFunctionAnalysisPass() = default;

  virtual llvm::StringRef getPassName() const override = 0;
  virtual bool runOnFunction(clang::FunctionDecl* f) override {
    processBase(f);

    return false;
  }
};

} // namespace cish

#endif // AST_FUNCTION_ANALYSIS_PASS_H
