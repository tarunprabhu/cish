#ifndef CISH_DEF_USE_H
#define CISH_DEF_USE_H

#include "ASTFunctionAnalysisPass.h"
#include "List.h"
#include "Map.h"
#include "Set.h"
#include "Vector.h"

#include <llvm/ADT/iterator_range.h>

namespace cish {

class ASTDefUsePass;

class DefUse {
protected:
  CishContext& context;
  Map<const clang::VarDecl*, List<clang::Stmt*>> defMap;
  Map<const clang::VarDecl*, List<clang::Stmt*>> useMap;

public:
  using def_iterator = decltype(defMap)::mapped_type::const_iterator;
  using use_iterator = decltype(useMap)::mapped_type::const_iterator;

public:
  DefUse(CishContext& context);
  DefUse(const DefUse&) = delete;
  DefUse(DefUse&&) = delete;
  virtual ~DefUse() = default;

  void removeUse(const clang::VarDecl* var, clang::Stmt* stmt);
  void removeDef(const clang::VarDecl* var, clang::Stmt* stmt);
  void removeVar(const clang::VarDecl* var);

  llvm::iterator_range<def_iterator> defs(const clang::VarDecl* var) const;
  llvm::iterator_range<use_iterator> uses(const clang::VarDecl* var) const;
  unsigned getNumDefs(const clang::VarDecl* var) const;
  unsigned getNumUses(const clang::VarDecl* var) const;
  bool isDefined(const clang::VarDecl* var) const;
  bool isUsed(const clang::VarDecl* var) const;
  bool hasSingleDef(const clang::VarDecl* var) const;
  bool hasSingleUse(const clang::VarDecl* var) const;
  bool hasZeroDefs(const clang::VarDecl* var) const;
  bool hasZeroUses(const clang::VarDecl* var) const;
  clang::Stmt* getSingleDef(const clang::VarDecl* var) const;
  clang::Stmt* getSingleUse(const clang::VarDecl* var) const;

  bool runOnFunction(clang::FunctionDecl* f);

public:
  friend class ASTDefUsePass;
};

} // namespace cish

#endif // CISH_DEF_USE_H
