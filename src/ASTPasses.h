#ifndef CISH_AST_PASSES_H
#define CISH_AST_PASSES_H

namespace cish {

class ASTFunctionPass;
class CishContext;

} // namespace cish

cish::ASTFunctionPass* createASTStripCastsPass(cish::CishContext&);
cish::ASTFunctionPass* createASTSimplifyOperatorsPass(cish::CishContext&);
cish::ASTFunctionPass* createASTDeadCodeEliminationPass(cish::CishContext&);
cish::ASTFunctionPass* createASTPropagateExprsPass(cish::CishContext&);

#endif // CISH_AST_PASSES_H
