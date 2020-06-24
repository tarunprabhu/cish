#ifndef CISH_AST_PASSES_H
#define CISH_AST_PASSES_H

#include <clang/AST/ASTContext.h>

#include "ASTFunctionPass.h"

cish::ASTFunctionPass* createASTStripCastsPass(clang::ASTContext&);
cish::ASTFunctionPass* createASTSimplifyOperatorsPass(clang::ASTContext&);
cish::ASTFunctionPass* createASTDeadCodeEliminationPass(clang::ASTContext&);

#endif // CISH_AST_PASSES_H
