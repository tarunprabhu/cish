#include "ClangUtils.h"

using namespace clang;

const Type* getBaseType(const ArrayType* aty) {
  const Type* ety = aty->getElementType().getTypePtr();
  if(const auto* bty = dyn_cast<ArrayType>(ety))
    return getBaseType(bty);
  return ety;
}

clang::Expr* stripCasts(clang::Expr* expr) {
  if(auto* cst = dyn_cast<CastExpr>(expr))
    return stripCasts(cst->getSubExpr());
  return expr;
}
