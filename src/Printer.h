#ifndef CISH_PRINTER_H
#define CISH_PRINTER_H

#include "Context.h"

#include <llvm/IR/Function.h>
#include <llvm/Support/raw_ostream.h>

namespace cish {

class Expr;
class Decl;
class Stmt;
class Type;

/// Class that pretty prints the Cish program
class Printer {
protected:
  const Context& ctxt;
  std::string buf;
  llvm::raw_string_ostream ss;

  /// @member depth The current indentation level
  unsigned ilevel;

  /// @member tabWidth The number of spaces to use in indentation. Defaults to 4
  unsigned tabWidth;

public:
  /// @param ctxt The Cish context containing mappings for the LLVM values
  Printer(const Context& ctxt);
  Printer(const Printer&) = delete;
  Printer(Printer&&) = delete;
  virtual ~Printer() = default;

  /// Sets the tab width to use when pretty-printing
  /// @param The tab width to use
  Printer& setTabWidth(unsigned width);

  /// Flush the output stream to the underlying string buffer
  /// If this is not called, calls to str() may return incomplete results
  Printer& flush();

  /// Clears the entire buffer and the underlying string
  Printer& clear();

  /// Reposition the cursor from the start of a line to the current
  /// indentation level.
  Printer& reposition();

  /// Increments the current indentation level and also repositions the cursor
  /// to the correct point
  Printer& tab();

  /// Decrements the current indentation level
  Printer& untab();

  /// End the current line
  Printer& endl();

  /// Insert spaces into the stream
  /// @param[optional] numSpaces The number of spaces to insert
  Printer& space(unsigned numSpaces = 1);

  /// Adds a comment to the stream
  /// @param comment The comment to add
  Printer& comment(const std::string& comment);

  Printer& begin_func(const llvm::Function& f);
  Printer& end_func(const llvm::Function& f);
  Printer& begin_block(const std::string& label = "");
  Printer& end_block(const std::string& label = "");

  Printer& add(const std::string&);
  Printer& add(const ASTBase& ast);

  /// @returns The string buffer into which everything has been streamed
  const std::string& str();

  /// Returns the string buffer into which everything has been streamed.
  /// @note If this is called without first calling flush(), it may return
  /// incomplete results. Use the non-const version of this function to
  /// guarantee that everything is flushed to the underlying string correctly
  const std::string& str() const;
};

} // namespace cish

#endif // CISH_PRINTER_H
