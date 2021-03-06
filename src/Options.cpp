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

#include "Options.h"
#include "Diagnostics.h"
#include "Set.h"

using namespace llvm;
namespace cl = llvm::cl;

namespace cish {

static cl::OptionCategory cishOptionCategory("Cish Options", "");

static cl::bits<cish::StripCasts> optStripCasts(
    "strip-casts",
    cl::desc("Which casts, if any, to strip from the output"),
    cl::values(
        clEnumValN(cish::StripCasts::Function,
                   "function",
                   "Strip casts to function types"),
        clEnumValN(cish::StripCasts::Pointer, "pointer", "Strip pointer casts"),
        clEnumValN(cish::StripCasts::Scalar, "scalar", "Strip scalar casts"),
        clEnumValN(cish::StripCasts::Vector,
                   "vector",
                   "Strip casts to vector types"),
        clEnumValN(cish::StripCasts::All, "all", "Strip all casts")),
    cl::cat(cishOptionCategory));

static cl::opt<std::string> optPrefix(
    "prefix",
    cl::desc("The prefix to use for any names auto-generated by cish"),
    cl::init("_"),
    cl::cat(cishOptionCategory));

static cl::bits<cish::Annotations> optAnnotations(
    "annotate",
    cl::desc("Add annotations to the output"),
    cl::values(
        clEnumValN(cish::Annotations::Source,
                   "source",
                   "Annotate with source code if available"),
        clEnumValN(cish::Annotations::Cish,
                   "cish",
                   "Annotations from cish that may or may not be helpful"),
        clEnumValN(cish::Annotations::All, "all", "Add all annotations")),
    cl::cat(cishOptionCategory));

static cl::opt<cish::IndentStyle> optIndentStyle(
    "indent-style",
    cl::desc("The indentation style to use"),
    cl::values(
        clEnumValN(cish::IndentStyle::KR, "kr", "K&R style"),
        clEnumValN(cish::IndentStyle::Allman, "allman", "Allman style"),
        clEnumValN(
            cish::IndentStyle::Stroustrup,
            "stroustrup",
            "Stroustrup style (like K&R but without the \"cuddled else\")")),
    cl::value_desc("style"),
    cl::init(IndentStyle::KR),
    cl::cat(cishOptionCategory));

static cl::opt<unsigned>
    optOffset("indent-offset",
              cl::desc("Number of spaces to use for indentation. If 0, tabs "
                       "are used instead of spaces"),
              cl::value_desc("n"),
              cl::init(4),
              cl::cat(cishOptionCategory));

static cl::opt<cish::Parens> optParens(
    "parens",
    cl::desc("How to use parentheses in expressions"),
    cl::values(
        clEnumValN(cish::Parens::Always, "always", "Always use parentheses"),
        clEnumValN(
            cish::Parens::Fuzzy,
            "fuzzy",
            "Respects operator precedence but tries adds some parentheses for "
            "clarity when combining arithmetic and relational operators"),
        clEnumValN(cish::Parens::Strict,
                   "strict",
                   "Adds parentheses strictly based on operator precedence.")),
    cl::init(Parens::Fuzzy),
    cl::cat(cishOptionCategory));

static cl::opt<bool> optVerbose("verbose",
                                cl::desc("Print messages during conversion"),
                                cl::init(false),
                                cl::cat(cishOptionCategory));

static cl::bits<LogCategory> optLog(
    "log",
    cl::desc("Create logs of the conversion (WARNING - "
             "Creates many files in the current directory)"),
    cl::values(
        clEnumValN(cish::LogCategory::IR,
                   "ir",
                   "Log the prepared LLVM IR before structure analysis"),
        clEnumValN(cish::LogCategory::Structure,
                   "structure",
                   "Log each step of the structure analysis"),
        clEnumValN(cish::LogCategory::AST,
                   "ast",
                   "Record the program after each AST transformation pass"),
        clEnumValN(cish::LogCategory::CFG,
                   "cfg",
                   "Record the CFG after each AST transformation pass"),
        clEnumValN(cish::LogCategory::All, "all", "Log everything")),
    cl::cat(cishOptionCategory));

static cl::opt<std::string>
    optLogDir("log-dir",
              cl::desc("Create the log files in the specified directory"),
              cl::init(""),
              cl::cat(cishOptionCategory));

static cl::opt<std::string> optOutput("o",
                                      cl::desc("Output file"),
                                      cl::value_desc("filename"),
                                      cl::init("-"));

static cl::opt<std::string> optFilename(cl::Positional,
                                        cl::desc("<input>"),
                                        cl::init("-"),
                                        cl::value_desc("filename"));

// Single global object containing all the options
std::unique_ptr<Options> g_opts;

bool isOpt(const cl::Option& opt) {
  return opt.Category == &cishOptionCategory;
}

void parseOpts() {
  g_opts.reset(new Options());
}

const Options& opts() {
  return *g_opts;
}

Options::Options()
    : fileIn(optFilename), fileOut(optOutput), prefix(optPrefix),
      indentStyle(optIndentStyle), indentOffset(optOffset), parens(optParens),
      verbose(optVerbose), logDir(optLogDir),
      stripCasts(parse<StripCasts>(optStripCasts.getBits())),
      annotations(parse<Annotations>(optAnnotations.getBits())),
      log(parse<LogCategory>(optLog.getBits())) {
  // Sanity checks
  if(indentOffset > 8)
    fatal(error() << "Invalid value for offset. Min 0, Max 8");
}

bool Options::has(StripCasts cst) const {
  return has(cst, stripCasts);
}

bool Options::has(Annotations ann) const {
  return has(ann, annotations);
}

bool Options::has(LogCategory cat) const {
  return has(cat, log);
}

} // namespace cish
