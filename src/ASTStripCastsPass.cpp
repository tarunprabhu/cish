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
#include "Diagnostics.h"
#include "Options.h"

#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTStripCastsPass : public ASTFunctionPass<ASTStripCastsPass> {
public:
  bool process(CStyleCastExpr* castExpr) {
    const Type* type = castExpr->getType().getTypePtr();
    Expr* expr = castExpr->getSubExpr();
    if(opts().has(StripCasts::Never)) {
      return false;
    } else if(opts().has(StripCasts::All)) {
      return ast->replaceExprWith(castExpr, expr);
    } else if(const auto* pty = dyn_cast<PointerType>(type)) {
      if(isa<FunctionProtoType>(pty->getPointeeType())) {
        if(opts().has(StripCasts::Function))
          return ast->replaceExprWith(castExpr, expr);
      } else if(isa<VectorType>(pty->getPointeeType())) {
        if(opts().has(StripCasts::Vector))
          return ast->replaceExprWith(castExpr, expr);
      } else {
        if(opts().has(StripCasts::Pointer))
          return ast->replaceExprWith(castExpr, expr);
      }
    } else if(type->isScalarType()) {
      if(opts().has(StripCasts::Scalar))
        return ast->replaceExprWith(castExpr, expr);
    } else if(isa<VectorType>(type)) {
      if(opts().has(StripCasts::Vector))
        return ast->replaceExprWith(castExpr, expr);
    }

    return false;
  }

public:
  ASTStripCastsPass(CishContext& cishContext) : ASTFunctionPass(cishContext) {
    ;
  }

  ASTStripCastsPass(const ASTStripCastsPass&) = delete;
  ASTStripCastsPass(ASTStripCastsPass&&) = delete;
  virtual ~ASTStripCastsPass() = default;

  virtual llvm::StringRef getPassName() const override {
    return "cish-casts";
  }

  virtual llvm::StringRef getPassLongName() const override {
    return "AST Strip Casts Pass";
  }
};

} // namespace cish

cish::ASTPass* createASTStripCastsPass(cish::CishContext& context) {
  return new cish::ASTStripCastsPass(context);
}
