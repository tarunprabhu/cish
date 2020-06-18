#include "CishCmdLineFlags.h"

using namespace llvm;
namespace cl = llvm::cl;

namespace cish {

cl::OptionCategory cishOptionCategory("Cish Options", "");

cl::list<cish::StripCasts> optStripCasts(
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
        clEnumValN(cish::StripCasts::Never, "never", "Never strip casts"),
        clEnumValN(cish::StripCasts::All, "all", "Strip all casts")),
    cl::cat(cishOptionCategory));

cl::opt<std::string> optPrefix(
    "prefix",
    cl::desc("The prefix to use for any names auto-generated by cish"),
    cl::init(cish::FormatOptions::defPrefix),
    cl::cat(cishOptionCategory));

cl::list<cish::Annotations> optAnnotations(
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

cl::opt<cish::IndentStyle> optIndentStyle(
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
    cl::init(cish::FormatOptions::defIndentStyle),
    cl::cat(cishOptionCategory));

cl::opt<unsigned>
    optOffset("indent-offset",
              cl::desc("Number of spaces to use for indentation. If 0, tabs "
                       "are used instead of spaces"),
              cl::value_desc("<n>"),
              cl::init(cish::FormatOptions::defOffset),
              cl::cat(cishOptionCategory));

cl::opt<cish::Parens>
    optParens("parens",
              cl::desc("How to use parentheses in expressions"),
              cl::values(clEnumValN(cish::Parens::Always,
                                    "always",
                                    "Always use parentheses"),
                         clEnumValN(cish::Parens::Smart,
                                    "smart",
                                    "Be smart about using parentheses")),
              cl::init(cish::FormatOptions::defParens),
              cl::cat(cishOptionCategory));

cl::opt<bool> optQuiet("quiet",
                       cl::desc("Do not print any messages"),
                       cl::init(cish::FormatOptions::defQuiet),
                       cl::cat(cishOptionCategory));

cl::opt<std::string> optOutput("o",
                               cl::desc("Output file"),
                               cl::value_desc("filename"),
                               cl::init("-"));

cl::opt<std::string> optFilename(cl::Positional,
                                 cl::desc("<input>"),
                                 cl::init("-"),
                                 cl::value_desc("filename"));

} // namespace cish