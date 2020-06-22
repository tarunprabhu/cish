#include "ASTExprPass.h"
#include "Diagnostics.h"
#include "Options.h"

#include <clang/AST/ExprCXX.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTStripCastsPass : public ASTExprPass {
protected:
  virtual Expr* process(CStyleCastExpr* cexpr) override;

public:
  ASTStripCastsPass(ASTContext& astContext);
  ASTStripCastsPass(const ASTStripCastsPass&) = delete;
  ASTStripCastsPass(ASTStripCastsPass&&) = delete;
  virtual ~ASTStripCastsPass() = default;

  virtual llvm::StringRef getPassName() const override;
};

ASTStripCastsPass::ASTStripCastsPass(ASTContext& astContext)
    : ASTExprPass(astContext) {
  ;
}

llvm::StringRef ASTStripCastsPass::getPassName() const {
  return "AST Strip Casts Pass";
}

Expr* ASTStripCastsPass::process(CStyleCastExpr* castExpr) {
  const Type* type = castExpr->getType().getTypePtr();
  Expr* expr = castExpr->getSubExpr();
  if(opts().has(StripCasts::Never)) {
    return ASTExprPass::process(castExpr);
  } else if(opts().has(StripCasts::All)) {
    return ASTExprPass::process(expr);
  } else if(const auto* pty = dyn_cast<PointerType>(type)) {
    if(isa<FunctionProtoType>(pty->getPointeeType())) {
      if(opts().has(StripCasts::Function))
        return ASTExprPass::process(expr);
    } else if(isa<VectorType>(pty->getPointeeType())) {
      if(opts().has(StripCasts::Vector))
        return ASTExprPass::process(expr);
    } else {
      if(opts().has(StripCasts::Pointer))
        return ASTExprPass::process(expr);
    }
  } else if(type->isScalarType()) {
    if(opts().has(StripCasts::Scalar))
      return ASTExprPass::process(expr);
  } else if(isa<VectorType>(type)) {
    if(opts().has(StripCasts::Vector))
      return ASTExprPass::process(expr);
  }

  return ASTExprPass::process(castExpr);
}

} // namespace cish

cish::ASTFunctionPass* createASTStripCastsPass(ASTContext& astContext) {
  return new cish::ASTStripCastsPass(astContext);
}
