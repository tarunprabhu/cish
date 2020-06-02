#ifndef CISH_AST_BASE_H
#define CISH_AST_BASE_H

#include <llvm/Support/Casting.h>
#include <llvm/Support/raw_ostream.h>

#include <sstream>

namespace cish {

enum class ASTKind {
  Decl,
  Expr,
  Stmt,
  Type,
};

/// Abstract base class for all Cish AST nodes
class ASTBase {
private:
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

  void add_impl_0(llvm::raw_string_ostream& ss, uint64_t i) {
    ss << i;
  }

  void add_impl_0(llvm::raw_string_ostream& ss, float f) {
    ss << f;
  }

  void add_impl_0(llvm::raw_string_ostream& ss, double d) {
    ss << d;
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
  ASTBase(ASTKind kind);
  ASTBase(ASTKind kind, const std::string& s);

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

  /// @returns The kind of this AST node
  ASTKind getKind() const;

  /// @returns The string representation of this AST node
  operator std::string() const;

  /// @returns The string representation of this AST node
  const std::string& str() const;
};

} // namespace cish

#endif // CISH_AST_BASE_H
