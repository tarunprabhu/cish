#ifndef CISH_AST_BASE_H
#define CISH_AST_BASE_H

#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <sstream>

namespace cish {

class Context;

enum class ASTKind {
  Decl,
  Expr,
  For,
  If,
  Stmt,
  While,
  Type,
};

/// Abstract base class for all Cish AST nodes
class ASTBase {
private:
  Context& ctxt;
  const ASTKind kind;
  std::string buf;

private:
  void add_impl_0(llvm::raw_string_ostream&) {
    ;
  }

  void add_impl_0(llvm::raw_string_ostream& ss, const char* cstr) {
    ss << cstr;
  }

  void add_impl_0(llvm::raw_string_ostream& ss, const std::string& s) {
    ss << s;
  }

  template <typename T, std::enable_if_t<std::is_arithmetic<T>::value, int> = 0>
  void add_impl_0(llvm::raw_string_ostream& ss, T val) {
    ss << val;
  }

  void add_impl_0(llvm::raw_string_ostream ss, long double g) {
    std::stringstream iss;
    iss << g;
    ss << iss.str();
  }

  template <typename CishT,
            std::enable_if_t<std::is_base_of<ASTBase, CishT>::value, int> = 0>
  void add_impl_0(llvm::raw_string_ostream& ss, CishT&& t) {
    ss << t.str();
  }

  template <typename FirstT, typename... RestT>
  void add_impl(llvm::raw_string_ostream& ss, FirstT&& first, RestT&&... rest) {
    add_impl_0(ss, first);
    add_impl(ss, rest...);
  }

  void add_impl(llvm::raw_string_ostream&) {
    ;
  }

protected:
  /// @param ctxt The context object containing all the AST nodes
  /// @param kind The kind of the AST node
  ASTBase(Context& ctxt, ASTKind kind);

  /// @param ctxt The context object containing all the AST nodes
  /// @param kind The kind of the AST node
  /// @param s    The Cish string representation of the node
  ASTBase(Context& ctxt, ASTKind kind, const std::string& s);

  template <typename T>
  void add(T&& expr) {
    llvm::raw_string_ostream ss(buf);
    add_impl_0(ss, expr);
    ss.flush();
  }

  template <typename FirstT, typename... RestT>
  void add(FirstT& first, RestT&&... rest) {
    llvm::raw_string_ostream ss(buf);
    add_impl_0(ss, first);
    add_impl(ss, rest...);
    ss.flush();
  }

public:
  ASTBase(const ASTBase&) = delete;
  ASTBase(ASTBase&&) = delete;
  virtual ~ASTBase() = default;

  /// @returns The AST context object that owns this node
  Context& getContext();

  /// @returns The AST context object that owns this node
  const Context& getContext() const;

  /// @returns The kind of this AST node
  ASTKind getKind() const;

  /// @returns The string representation of this AST node
  operator std::string() const;

  /// @returns The string representation of this AST node
  const std::string& str() const;
};

} // namespace cish

#endif // CISH_AST_BASE_H
