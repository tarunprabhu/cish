#ifndef CISH_CONTEXT_H
#define CISH_CONTEXT_H

#include "ASTBase.h"
#include "Decl.h"
#include "Expr.h"
#include "Stmt.h"
#include "Type.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Value.h>

#include <map>
#include <memory>

namespace cish {

/// This is a single object that contains the maps from LLVM entities
/// (types and values) to their corresponding Cish AST nodes and keeps track
/// of any temporary identifiers that have been generated.
class Context {
private:
  uint64_t nextVarSuffix;

  // The same LLVM value could have more than one C representation
  // For instance, a function parameter could have a
  // declaration (with a type) and a use (which would be just the name).
  // Similarly, local variables would be allocas which would be a cish::Stmt
  // when declared but a cish::Expr when used
  std::map<const llvm::Value*, std::unique_ptr<Expr>> exprs;
  std::map<const llvm::Value*, std::unique_ptr<Stmt>> stmts;
  std::map<const llvm::Value*, std::unique_ptr<Decl>> decls;
  std::map<llvm::Type*, std::unique_ptr<Type>> types;

public:
  Context();
  Context(const Context&) = delete;
  Context(Context&&) = delete;
  ~Context() = default;

  /// param val A llvm::Value
  /// @returns true if the @val has a corresponding AST node
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  bool has(const llvm::Value* val) const;

  /// param val A llvm::Value
  /// @returns true if the @val has a corresponding AST node
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  bool has(const llvm::Value& val) const {
    return has<CishT>(&val);
  }

  /// param val A llvm::Type
  /// @returns true if the @type has a corresponding AST node
  bool has(llvm::Type* type) const;

  /// Creates an AST node of the specified kind with the given C
  /// representation for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Expr>::value, int> = 0>
  const CishT& add(const llvm::Value* val, RestT&&... rest) {
    exprs[val].reset(new Expr(*val, rest...));
    return get<Expr>(val);
  }

  /// Creates an AST node of the specified kind with the given C
  /// representation for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Stmt>::value, int> = 0>
  const CishT& add(const llvm::Value* val, RestT&&... rest) {
    stmts[val].reset(new Stmt(*val, rest...));
    return get<Stmt>(val);
  }

  /// Creates an AST node of the specified kind with the given C
  /// representation for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Decl>::value, int> = 0>
  const CishT& add(const llvm::Value* val, RestT&&... rest) {
    decls[val].reset(new Decl(*val, rest...));
    return get<Decl>(val);
  }

  // template <typename... RestT>
  // const Expr& add(const llvm::Value* val, RestT&&... rest) {
  //   exprs[val].reset(new Expr(*val, rest...));
  //   return get<Expr>(val);
  // }

  // template <typename... RestT>
  // const Stmt& add(const llvm::Value* val, RestT&&... rest) {
  //   stmts[val].reset(new Stmt(*val, rest...));
  //   return get<Stmt>(val);
  // }

  // template <typename... RestT>
  // const Decl& add(const llvm::Value* val, RestT&&... rest) {
  //   decls[val].reset(new Decl(*val, rest...));
  //   return get<Decl>(val);
  // }

  /// Creates an AST node of the specified kind with the given C
  /// representation for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT = Expr,
            typename... RestT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& add(const llvm::Value& val, RestT&&... rest) {
    return add<CishT>(&val, rest...);
  }

  /// Creates an AST node for an llvm::Type
  /// @param type The llvm::Type
  /// @s The C representation of the llvm::Type
  /// @returns A newly allocated AST node
  const Type& add(llvm::Type* type, const std::string& s);

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template<typename CishT = Expr,
           std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Value* val) const;

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template<typename CishT = Expr,
           std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Value& val) const {
    return get<CishT>(&val);
  }

  /// @param[in] type A llvm::Type
  /// @returns The AST node for the llvm::Type @type
  const Type& get(llvm::Type* type) const;

  /// Returns a new entity name that (hopefully) will not collide with
  /// any other entity name that already exists in the code being Cish'ed
  ///
  /// @param[optional] prefix An additional prefix to add to the variable
  ///                         name. Useful to disambiguate between local
  ///                         variables, args, blocks, labels etc.
  /// @returns The new variable name
  std::string getNewVar(const std::string& prefix = "");
};

} // namespace cish

#endif // CISH_CONTEXT_H
