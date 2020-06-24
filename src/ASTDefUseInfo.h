#ifndef CISH_AST_DEF_USE_INFO_H
#define CISH_AST_DEF_USE_INFO_H

#include "ASTFunctionPass.h"
#include "List.h"
#include "Map.h"
#include "Set.h"
#include "Vector.h"

#include <llvm/ADT/iterator_range.h>

namespace cish {

class DefUseVisitor;

class ASTDefUseInfo {
protected:
  clang::ASTContext& astContext;
  Map<const clang::VarDecl*, List<clang::Stmt*>> defMap;
  Map<const clang::VarDecl*, List<clang::Stmt*>> useMap;
  Map<const clang::CompoundStmt*, clang::Stmt*> containers;
  Map<const clang::Stmt*, clang::CompoundStmt*> parents;
  Map<const clang::Stmt*, Set<clang::Stmt*>> children;

public:
  using def_iterator = decltype(defMap)::mapped_type::const_iterator;
  using use_iterator = decltype(useMap)::mapped_type::const_iterator;

public:
  ASTDefUseInfo(clang::ASTContext& astContext);
  ASTDefUseInfo(const ASTDefUseInfo&) = delete;
  ASTDefUseInfo(ASTDefUseInfo&&) = delete;
  virtual ~ASTDefUseInfo() = default;

  void remove(const clang::VarDecl* var);
  void remove(clang::Stmt* stmt);

  // The first element of the pair is the compound statement of which the
  // given statement is a part and the second element is the container
  // (if, for, while, do, switch) for that compound statement. If the second
  // statement is null, then the compound statement is the body of the function
  std::pair<clang::CompoundStmt*, clang::Stmt*>
  getContainer(clang::Stmt* stmt) const;

  llvm::iterator_range<def_iterator> defs(const clang::VarDecl* var) const;
  llvm::iterator_range<use_iterator> uses(const clang::VarDecl* var) const;
  unsigned getNumDefs(const clang::VarDecl* var) const;
  unsigned getNumUses(const clang::VarDecl* var) const;
  bool isDefined(const clang::VarDecl* var) const;
  bool isUsed(const clang::VarDecl* var) const;
  bool hasSingleDef(const clang::VarDecl* var) const;
  bool hasSingleUse(const clang::VarDecl* var) const;
  clang::CompoundStmt* getParent(clang::Stmt* stmt) const;
  std::pair<clang::Stmt*, unsigned> getOwner(clang::CompoundStmt* stmt) const;
  bool allUsesInLoop(const clang::VarDecl* var, clang::DoStmt* stmt) const;
  bool allUsesInLoop(const clang::VarDecl* var, clang::ForStmt* stmt) const;
  bool allUsesInLoop(const clang::VarDecl* var, clang::WhileStmt* stmt) const;

  void runOnFunction(clang::FunctionDecl* f);

public:
  friend class DefUseVisitor;
};

} // namespace cish

#endif // CISH_AST_DEF_USE_INFO_H
