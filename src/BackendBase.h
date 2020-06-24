#ifndef CISH_BACKEND_BASE_H
#define CISH_BACKEND_BASE_H

#include "ASTBuilder.h"
#include "Map.h"
#include "Stack.h"
#include "Vector.h"

#include <clang/AST/ExprCXX.h>

namespace cish {

class CishContext;

class BackendBase {
private:
  std::string varPrefix;
  uint64_t varSuffix;

protected:
  clang::ASTContext& astContext;

  ASTBuilder ast;

  // The statements comprising the body of the current function being
  // converted
  Stack<Vector<clang::Stmt*>> stmts;
  Map<std::string, clang::LabelDecl*> labels;

protected:
  BackendBase(CishContext& context);
  BackendBase() = delete;
  BackendBase(const BackendBase&) = delete;
  BackendBase(BackendBase&&) = delete;

  void beginFunction(clang::FunctionDecl* f);
  void endFunction(clang::FunctionDecl* f);

  clang::Stmt* add(clang::Stmt* stmt);
  clang::Stmt& add(clang::Stmt& stmt);

  clang::Expr* replace(clang::Expr* src, clang::Expr* old, clang::Expr* repl);

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
