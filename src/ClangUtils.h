#ifndef CISH_CLANG_UTILS_H
#define CISH_CLANG_UTILS_H

#include <clang/AST/ExprCXX.h>

/// @param aty The clang::ArrayType
/// @returns The innermost non-array type within @aty
const clang::Type* getBaseType(const clang::ArrayType* aty);

clang::Expr* stripCasts(clang::Expr* expr);

#endif // CISH_CLANG_UTILS_H
