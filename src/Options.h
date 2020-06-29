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

namespace llvm {
namespace cl {
class OptionCategory;
}
} // namespace llvm

namespace cish {

enum class StripCasts {
  Never = 0x0,
  Function = 0x1,
  Pointer = 0x2,
  Scalar = 0x4,
  Vector = 0x8,
  All = 0xffff,
};

enum class Annotations {
  None = 0x0,
  Source = 0x1,
  Cish = 0x2,
  All = 0xffff,
};

enum class IndentStyle {
  KR,         // K&R style
  Allman,     // Allman style
  Stroustrup, // Stroustrup style (like K&R but without the "cuddled else")
};

enum class Parens {
  Always, // Always add parentheses to operands of oeprators
  Smart,  // Be "smart" about adding parentheses to operands of operators
};

class Options {
private:
  unsigned stripCasts;
  unsigned annotations;

private:
  void set(StripCasts cst);
  void set(Annotations ann);

public:
  std::string fileIn;
  std::string fileOut;

  // The prefix string to use for generated variable names
  std::string prefix;
  IndentStyle indentStyle;
  unsigned indentOffset;
  Parens parens;
  bool verbose;
  bool log;
  std::string logDir;

public:
  Options();
  Options(const Options&) = delete;
  Options(Options&&) = delete;

  bool has(StripCasts cst) const;
  bool has(Annotations ann) const;
};

// Exposes the singular global object containing the parsed command line
// parameters
const Options& opts();
bool isOpt(const llvm::cl::Option& cat);
void parseOpts();

} // namespace cish

#endif // CISH_OPTIONS_H
