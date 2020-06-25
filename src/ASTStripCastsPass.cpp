#include "ASTExprPass.h"
#include "Diagnostics.h"
#include "Options.h"

#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTStripCastsPass : public ASTExprPass<ASTStripCastsPass> {
public:
  Expr* process(CStyleCastExpr* cexpr);

public:
  ASTStripCastsPass(CishContext& context);
  ASTStripCastsPass(const ASTStripCastsPass&) = delete;
  ASTStripCastsPass(ASTStripCastsPass&&) = delete;
  virtual ~ASTStripCastsPass() = default;

  virtual llvm::StringRef getPassName() const override;
};

ASTStripCastsPass::ASTStripCastsPass(CishContext& context)
    : ASTExprPass(context) {
  ;
}

llvm::StringRef ASTStripCastsPass::getPassName() const {
  return "AST Strip Casts Pass";
}

Expr* ASTStripCastsPass::process(CStyleCastExpr* castExpr) {
  const Type* type = castExpr->getType().getTypePtr();
  Expr* expr = castExpr->getSubExpr();
  if(opts().has(StripCasts::Never)) {
    return castExpr;
  } else if(opts().has(StripCasts::All)) {
    return expr;
  } else if(const auto* pty = dyn_cast<PointerType>(type)) {
    if(isa<FunctionProtoType>(pty->getPointeeType())) {
      if(opts().has(StripCasts::Function))
        return expr;
    } else if(isa<VectorType>(pty->getPointeeType())) {
      if(opts().has(StripCasts::Vector))
        return expr;
    } else {
      if(opts().has(StripCasts::Pointer))
        return expr;
    }
  } else if(type->isScalarType()) {
    if(opts().has(StripCasts::Scalar))
      return expr;
  } else if(isa<VectorType>(type)) {
    if(opts().has(StripCasts::Vector))
      return expr;
  }

  return castExpr;
}

} // namespace cish

cish::ASTFunctionPass* createASTStripCastsPass(cish::CishContext& context) {
  return new cish::ASTStripCastsPass(context);
}
