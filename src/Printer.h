#ifndef CISH_PRINTER_H
#define CISH_PRINTER_H

#include "CishContext.h"
#include "FormatOptions.h"

#include <clang/AST/ASTContext.h>

#include <llvm/Support/raw_ostream.h>

namespace cish {

class Printer {
protected:
  clang::ASTContext& astContext;
  const FormatOptions& fmtOpts;

public:
  Printer(CishContext& context);
  Printer(const Printer&) = delete;
  Printer(Printer&&) = delete;
  ~Printer() = default;

  void run(llvm::raw_ostream& os);
};

} // namespace cish

#endif // CISH_PRINTER_H
