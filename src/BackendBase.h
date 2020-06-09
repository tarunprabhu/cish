#ifndef CISH_BACKEND_BASE_H
#define CISH_BACKEND_BASE_H

#include "CishContext.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class BackendBase {
protected:
  CishContext& context;

  // All of these are needed to set up the clang::ASTContext object
  // std::unique_ptr<clang::ASTContext> astContext;
  clang::ASTContext& astContext;

  std::string varPrefix;
  uint64_t varSuffix;

  // The statements comprising the body of the current function being
  // converted
  std::vector<clang::Stmt*> stmts;

  // We obviously don't have any SourceLocations, so just use this wherever
  // we need a clang::SourceLocation
  const clang::FullSourceLoc invLoc;

protected:
  BackendBase(CishContext& context);
  BackendBase() = delete;
  BackendBase(const BackendBase&) = delete;
  BackendBase(BackendBase&&) = delete;

  void beginFunction(clang::FunctionDecl* f);
  void endFunction(clang::FunctionDecl* f);
  void beginBlock(clang::LabelDecl* bb);
  void endBlock(clang::LabelDecl* bb);

public:
  virtual ~BackendBase() = default;

  /// Returns a new entity name that (hopefully) will not collide with
  /// any other entity name that already exists in the code being Cish'ed
  ///
  /// @param[optional] prefix An additional prefix to add to the variable
  ///                         name. Useful to disambiguate between local
  ///                         variables, args, blocks, labels etc.
  /// @returns The new variable name
  std::string getNewVar(const std::string& prefix = "v");
};

} // namespace cish

#endif // CISH_BACKEND_BASE_H
