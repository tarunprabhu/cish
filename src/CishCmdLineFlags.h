#ifndef CISH_CMD_LINE_FLAGS_H
#define CISH_CMD_LINE_FLAGS_H

#include "FormatOptions.h"

#include <llvm/Support/CommandLine.h>

namespace cish {

extern llvm::cl::OptionCategory cishOptionCategory;

extern llvm::cl::list<cish::StripCasts> optStripCasts;
extern llvm::cl::opt<std::string> optPrefix;
extern llvm::cl::list<cish::Annotations> optAnnotations;
extern llvm::cl::opt<cish::IndentStyle> optIndentStyle;
extern llvm::cl::opt<unsigned> optOffset;
extern llvm::cl::opt<cish::Parens> optParens;
extern llvm::cl::opt<bool> optQuiet;

extern llvm::cl::opt<std::string> optOutput;
extern llvm::cl::opt<std::string> optFilename;

} // namespace cish

#endif // CISH_CMD_LINE_FLAGS_H
