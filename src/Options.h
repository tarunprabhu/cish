//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu <tarun.prabhu@acm.org>
//
//  This file is part of Cish.
//
//  Cish is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  Cish is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with Cish.  If not, see <https://www.gnu.org/licenses/>.
//  ---------------------------------------------------------------------------

#ifndef CISH_OPTIONS_H
#define CISH_OPTIONS_H

#include <string>

#include <llvm/Support/CommandLine.h>

namespace cish {

// All enums that behave as flags should begin with All and end with None
// The None option is not available to the user and is for internal use
// only.
enum class StripCasts {
  All,
  Function,
  Pointer,
  Scalar,
  Vector,
  None,
};

enum class Annotations {
  All,
  Source,
  Cish,
  None,
};

enum class IndentStyle {
  KR,         // K&R style
  Allman,     // Allman style
  Stroustrup, // Stroustrup style (like K&R but without the "cuddled else")
};

enum class Parens {
  Always, // Always add parentheses to operands of oeprators
  Fuzzy,  // Adds parenthese based on operator precedence and readability
  Strict, // Adds parentheses strictly based on operator precedence
};

enum class LogCategory {
  All,
  IR,        // Log the prepared LLVM IR prior to structure analysis
  Structure, // Log each reduction during structure analysis
  AST,       // Log the output of each AST transformation pass
  CFG,       // Log the CFG after each AST transformation pass
  None,
};

class Options {
private:
  template <typename Enum>
  unsigned parse(unsigned flags) {
    if(flags & (1 << (unsigned)Enum::All))
      flags = ~0x0U;
    return flags;
  }

  template <typename Enum>
  bool has(Enum e, const unsigned flags) const {
    if(e == Enum::All)
      return flags == ~0x0U;
    else if(e == Enum::None)
      return flags == 0;
    else
      return flags & (1 << (unsigned)e);
  }

public:
  const std::string fileIn;
  const std::string fileOut;

  // The prefix string to use for generated variable names
  const std::string prefix;
  const IndentStyle indentStyle;
  const unsigned indentOffset;
  const Parens parens;
  const bool verbose;
  const std::string logDir;

private:
  const unsigned stripCasts;
  const unsigned annotations;
  const unsigned log;

public:
  Options();
  Options(const Options&) = delete;
  Options(Options&&) = delete;

  bool has(StripCasts) const;
  bool has(Annotations) const;
  bool has(LogCategory) const;
};

// Exposes the singular global object containing the parsed command line
// parameters
const Options& opts();
bool isOpt(const llvm::cl::Option& cat);
void parseOpts();

} // namespace cish

#endif // CISH_OPTIONS_H
