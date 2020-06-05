#ifndef CISH_CONTEXT_H
#define CISH_CONTEXT_H

#include "ASTBase.h"
#include "Decl.h"
#include "Expr.h"
#include "For.h"
#include "If.h"
#include "Map.h"
#include "Map2.h"
#include "Stmt.h"
#include "Type.h"
#include "While.h"

#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/DataLayout.h>
#include <llvm/IR/DebugInfo.h>
#include <llvm/IR/Metadata.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <memory>

namespace cish {

/// This is a single object that contains the maps from LLVM entities
/// (types and values) to their corresponding Cish AST nodes and keeps track
/// of names of entities that were determined from DebugInfo or elsewehere
/// and any that are generated
class Context {
private:
  std::string varPrefix;
  uint64_t varSuffix;

  // The same LLVM value could have more than one C representation
  // For instance, a function parameter could have a
  // declaration (with a type) and a use (which would be just the name).
  // Similarly, local variables would be allocas which would be a cish::Stmt
  // when declared but a cish::Expr when used
  Map<const llvm::Value*, std::unique_ptr<Decl>> decls;
  Map<const llvm::Value*, std::unique_ptr<Expr>> exprs;
  Map<const llvm::Loop*, std::unique_ptr<For>> fors;
  Map<const llvm::Value*, std::unique_ptr<If>> ifs;
  Map<const llvm::Value*, std::unique_ptr<Stmt>> stmts;
  Map<const llvm::Loop*, std::unique_ptr<While>> whiles;
  Map<llvm::Type*, std::unique_ptr<Type>> types;

  // The original translations of the LLVM values that were overwritten
  Map<const llvm::Value*, std::unique_ptr<Expr>> ovrs;

  // The user names for any values or structs that can be gleaned
  // from the DebugInfo or elsewhere
  Map<const llvm::Value*, std::string> valueNames;
  Map<llvm::StructType*, std::string> structNames;
  Map<llvm::StructType*, Vector<std::string>> elemNames;

protected:
  void parseFunctionSource(const llvm::Function& f);
  void parseGlobalSource(const llvm::GlobalVariable& g);
  void parseStructSource(
      llvm::StructType* sty,
      const llvm::DICompositeType* di,
      const llvm::StructLayout& sl);
  void parseClassSource(
      llvm::StructType* sty,
      const llvm::DICompositeType* di,
      const llvm::StructLayout& sl);
  void parseUnionSource(
      llvm::StructType* sty,
      const llvm::DICompositeType* di,
      const llvm::StructLayout& sl);

public:
  /// @param prefix The prefix to use when generating names
  Context(const std::string& prefix);
  Context(const Context&) = delete;
  Context(Context&&) = delete;
  ~Context() = default;

  /// Returns a new entity name that (hopefully) will not collide with
  /// any other entity name that already exists in the code being Cish'ed
  ///
  /// @param[optional] prefix An additional prefix to add to the variable
  ///                         name. Useful to disambiguate between local
  ///                         variables, args, blocks, labels etc.
  /// @returns The new variable name
  std::string getNewVar(const std::string& prefix = "v");

  /// Overwrite an existing Expr with the temporary variable name.
  /// @param val  The llvm::Value to be overwritten
  /// @param name The temporary variable name with which to overwrite
  /// @returns    The Expr with the overwritten name
  Expr& overwrite(const llvm::Value& val, const std::string& name);

  /// Overwrite an existing Expr with the temporary variable name.
  /// @param val  The llvm::Value to be overwritten
  /// @param name The temporary variable name with which to overwrite
  /// @returns    The Expr with the overwritten name
  Expr& overwrite(const llvm::Value* val, const std::string& name);

  /// Creates a Decl node for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Decl>::value, int> = 0>
  CishT& add(const llvm::Value* val, RestT&&... rest) {
    decls[val].reset(new Decl(*this, *val, rest...));
    return get<Decl>(val);
  }

  /// Creates an Expr node for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Expr>::value, int> = 0>
  CishT& add(const llvm::Value* val, RestT&&... rest) {
    exprs[val].reset(new Expr(*this, *val, rest...));
    return get<Expr>(val);
  }

  /// Create a For node for the give llvm::Loop.
  ///
  /// @param loop The llvm::Loop
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, For>::value, int> = 0>
  CishT& add(const llvm::Loop* loop, RestT&&... rest) {
    fors[loop].reset(new For(*this, *loop, rest...));
    return get<For>(loop);
  }

  /// Creates an If node for the given llvm::Value.
  ///
  /// @param br   The llvm::BranchInst
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, If>::value, int> = 0>
  CishT& add(const llvm::BranchInst* br, RestT&&... rest) {
    ifs[br].reset(new If(*this, *br, rest...));
    return get<If>(br);
  }

  /// Creates a Stmt node for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, Stmt>::value, int> = 0>
  CishT& add(const llvm::Value* val, RestT&&... rest) {
    stmts[val].reset(new Stmt(*this, *val, rest...));
    return get<Stmt>(val);
  }

  /// Create a While node for the give llvm::Loop
  ///
  /// @param loop The llvm::Loop
  /// @returns A newly allocated AST node
  template <typename CishT,
            typename... RestT,
            std::enable_if_t<std::is_same<CishT, While>::value, int> = 0>
  CishT& add(const llvm::Loop* loop, RestT&&... rest) {
    whiles[loop].reset(new While(*this, *loop, rest...));
    return get<While>(loop);
  }

  /// Creates an AST node of the specified kind with the given C
  /// representation for the given llvm::Value
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename CishT = Expr,
            typename... RestT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& add(const llvm::Value& val, RestT&&... rest) {
    return add<CishT>(&val, rest...);
  }

  /// Creates loop AST node(for or while) of the specified kind for a llvm::Loop
  ///
  /// @param loop The llvm::Loop
  /// @returns A newly allocated AST node
  template <typename CishT = Expr,
            typename... RestT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& add(const llvm::Loop& loop, RestT&&... rest) {
    return add<CishT>(&loop, rest...);
  }

  /// Create an If AST node for the given llvm::BranchInst
  ///
  /// @param val The llvm::Value
  /// @returns A newly allocated AST node
  template <typename... RestT>
  If& add(const llvm::BranchInst& br, RestT&&... rest) {
    return add<If>(&br, rest...);
  }

  /// Creates an AST node for an llvm::Type
  /// @param type The llvm::Type
  /// @s The C representation of the llvm::Type
  /// @returns A newly allocated AST node
  const Type& add(llvm::Type* type, const std::string& s);

  /// Adds a field name to an llvm::StructType. The name may be from the source
  /// or it may have been generated
  ///
  /// @param type The llvm::StructType
  /// @param field The field name
  void addElementName(llvm::StructType* type, const std::string& field);

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& get(const llvm::Value* val);

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& get(const llvm::Value& val) {
    return get<CishT>(&val);
  }

  /// @param loop A llvm::Loop
  /// @returns The AST node for the llvm::Loop @loop
  template <typename CishT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& get(const llvm::Loop* loop);

  /// @param val A llvm::Loop
  /// @returns The AST node for the llvm::Loop @val
  template <typename CishT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  CishT& get(const llvm::Loop& loop) {
    return get<CishT>(&loop);
  }

  /// @param val A llvm::Value
  /// @returns true if the value has been overwritten
  bool isOverwrite(const llvm::Value& val) const;

  /// @param val A llvm::Value
  /// @returns true if the value has been overwritten
  bool isOverwrite(const llvm::Value* val) const;

  /// @param val A llvm::Value
  /// @returns The original conversion of the overwritten @val
  const Expr& getOverwrite(const llvm::Value& val) const;

  /// @param val A llvm::Value
  /// @returns The original conversion of the overwritten @val
  const Expr& getOverwrite(const llvm::Value* val) const;

  /// @param val A llvm::Value
  /// @returns true if the @val has a corresponding AST node
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  bool has(const llvm::Value* val) const;

  /// @param val A llvm::Value
  /// @returns true if the @val has a corresponding AST node
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  bool has(const llvm::Value& val) const {
    return has<CishT>(&val);
  }

  /// @param loop A llvm::Loop
  /// @returns True if the @loop has a corresponding AST node
  template <typename CishT,
            std::enable_if_t<std::is_same<CishT, For>::value
                                 || std::is_same<CishT, While>::value,
                             int> = 0>
  bool has(const llvm::Loop* loop) const;

  /// @param loop A llvm::Loop
  /// @returns True if the @loop has a corresponding AST node
  template <typename CishT,
            std::enable_if_t<std::is_same<CishT, For>::value
                                 || std::is_same<CishT, While>::value,
                             int> = 0>
  bool has(const llvm::Loop& loop) const {
    return has<CishT>(&loop);
  }

  /// @param val A llvm::Type
  /// @returns true if the @type has a corresponding AST node
  bool has(llvm::Type* type) const;

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Value* val) const;

  /// @param val A llvm::Value
  /// @returns The AST node for the llvm::Value @val
  template <typename CishT = Expr,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Value& val) const {
    return get<CishT>(&val);
  }

  /// @param loop A llvm::Loop
  /// @returns The AST node for the llvm::Loop @loop
  template <typename CishT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Loop* loop) const;

  /// @param val A llvm::Loop
  /// @returns The AST node for the llvm::Loop @val
  template <typename CishT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  const CishT& get(const llvm::Loop& loop) const {
    return get<CishT>(&loop);
  }

  /// @param[in] type A llvm::Type
  /// @returns The AST node for the llvm::Type @type
  const Type& get(llvm::Type* type) const;

  /// Parses a llvm::Module for any debug information that might be present
  /// to associate user names with the entities in the module
  ///
  /// @module The llvm::Module
  void parseSourceInformation(const llvm::Module&);

  /// @param val The llvm::Value
  /// @returns true if @val has a source name
  bool hasSourceName(const llvm::Value& val) const;

  /// @param val The llvm::Value
  /// @returns true if @val has a source name
  bool hasSourceName(const llvm::Value* val) const;

  /// @param type The llvm::StructType
  /// @returns true if @type has a source name
  bool hasSourceName(llvm::StructType* type) const;

  /// Checks if the struct has any field names registered. If the struct is
  /// empty, this will still return true
  ///
  /// @param type The llvm::StructType
  /// @returns true if @type has been seen and may have field names set
  bool hasElementNames(llvm::StructType* type) const;

  /// @param val The llvm::Value
  /// @returns The source name for @val
  const std::string& getSourceName(const llvm::Value& val) const;

  /// @param val The llvm::Value
  /// @returns The source name for @val
  const std::string& getSourceName(const llvm::Value* val) const;

  /// @param type The llvm::StructType
  /// @returns The source name for @type
  const std::string& getSourceName(llvm::StructType* type) const;

  /// @param type The llvm::StructType
  /// @param i    The field of the struct being looked up
  /// @returns    The name of the field
  const std::string& getElementName(llvm::StructType* type, unsigned i) const;
};

} // namespace cish

#endif // CISH_CONTEXT_H
