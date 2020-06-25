#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "ASTLookup.h"
#include "DefUse.h"
#include "Diagnostics.h"

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

class ASTDCEPass : public ASTFunctionPass {
protected:
  ASTBuilder builder;

public:
  ASTDCEPass(CishContext& context);
  ASTDCEPass(const ASTDCEPass&) = delete;
  ASTDCEPass(ASTDCEPass&&) = delete;
  virtual ~ASTDCEPass() = default;

  virtual llvm::StringRef getPassName() const override;
  virtual bool modifiesAST() const override;
  virtual bool runOnFunction(FunctionDecl* f) override;
};

ASTDCEPass::ASTDCEPass(CishContext& context)
    : ASTFunctionPass(context), builder(astContext) {
  ;
}

llvm::StringRef ASTDCEPass::getPassName() const {
  return "Cish AST Dead Code Elimination Pass";
}

bool ASTDCEPass::modifiesAST() const {
  return true;
}

bool ASTDCEPass::runOnFunction(FunctionDecl* f) {
  DefUse& du = getDefUse();
  ASTLookup& ast = getASTLookup();

  Set<Stmt*> removeStmts;
  Set<const VarDecl*> removeVars;
  bool changed = false;
  do {
    changed = false;
    for(Decl* decl : f->decls()) {
      if(auto* var = dyn_cast<VarDecl>(decl)) {
        if(not du.isUsed(var)) {
          auto defs = du.defs(var);
          List<Stmt*> remove(defs.begin(), defs.end());
          changed |= remove.size();
          for(Stmt* def : remove) {
            du.removeDef(var, def);
            removeStmts.insert(def);
          }
          removeVars.insert(var);
        }
      }
    }
  } while(changed);

  Set<std::pair<CompoundStmt*, Stmt*>> containers;
  for(Stmt* stmt : removeStmts)
    containers.emplace(ast.getCompoundStmtFor(stmt), ast.getConstructFor(stmt));

  for(auto& i : containers) {
    CompoundStmt* body = i.first;
    Stmt* container = i.second;
    Vector<Stmt*> newStmts;
    for(Stmt* stmt : i.first->body())
      if(not removeStmts.contains(stmt))
        newStmts.push_back(stmt);
    CompoundStmt* newBody = builder.createCompoundStmt(newStmts);
    if(not container)
      f->setBody(newBody);
    else if(auto* ifStmt = dyn_cast<IfStmt>(container))
      if(ifStmt->getThen() == body)
        ifStmt->setThen(newBody);
      else
        ifStmt->setElse(newBody);
    else if(auto* doStmt = dyn_cast<DoStmt>(container))
      doStmt->setBody(newBody);
    else if(auto* forStmt = dyn_cast<ForStmt>(container))
      forStmt->setBody(newBody);
    else if(auto* whileStmt = dyn_cast<WhileStmt>(container))
      whileStmt->setBody(newBody);
    else if(auto* caseStmt = dyn_cast<CaseStmt>(container))
      caseStmt->setSubStmt(newBody);
    else if(auto* defaultStmt = dyn_cast<DefaultStmt>(container))
      defaultStmt->setSubStmt(newBody);
    else
      fatal(error() << "Unexpected container statement: "
                    << container->getStmtClassName());
  }

  Set<VarDecl*> orphanVars;
  for(auto* decl : f->decls())
    if(auto* var = dyn_cast<VarDecl>(decl))
      if(du.hasZeroDefs(var) and du.hasZeroUses(var))
        orphanVars.insert(var);
  for(VarDecl* var : orphanVars) {
    du.removeVar(var);
    f->removeDecl(var);
  }

  return removeStmts.size() or removeVars.size() or orphanVars.size();
}

} // namespace cish

cish::ASTFunctionPass*
createASTDeadCodeEliminationPass(cish::CishContext& context) {
  return new cish::ASTDCEPass(context);
}
