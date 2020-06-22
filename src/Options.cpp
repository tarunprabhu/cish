#include "Options.h"
#include "Diagnostics.h"

using namespace llvm;
namespace cl = llvm::cl;

namespace cish {

static cl::OptionCategory cishOptionCategory("Cish Options", "");

static cl::list<cish::StripCasts> optStripCasts(
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

static cl::opt<std::string> optPrefix(
    "prefix",
    cl::desc("The prefix to use for any names auto-generated by cish"),
    cl::init("_"),
    cl::cat(cishOptionCategory));

static cl::list<cish::Annotations> optAnnotations(
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
              cl::value_desc("<n>"),
              cl::init(4),
              cl::cat(cishOptionCategory));

static cl::opt<cish::Parens>
    optParens("parens",
              cl::desc("How to use parentheses in expressions"),
              cl::values(clEnumValN(cish::Parens::Always,
                                    "always",
                                    "Always use parentheses"),
                         clEnumValN(cish::Parens::Smart,
                                    "smart",
                                    "Be smart about using parentheses")),
              cl::init(Parens::Smart),
              cl::cat(cishOptionCategory));

static cl::opt<bool> optVerbose("verbose",
                                cl::desc("Print messages during conversion"),
                                cl::init(false),
                                cl::cat(cishOptionCategory));

static cl::opt<bool>
    optLog("log",
           cl::desc("Create logs of the conversion (WARNING - "
                    "Creates many files in the current directory)"),
           cl::init(false),
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
const Options* g_opts;

bool isOpt(const cl::Option& opt) {
  return opt.Category == &cishOptionCategory;
}

void parseOpts() {
  g_opts = new Options();
}

const Options& opts() {
  return *g_opts;
}

Options::Options()
    : stripCasts(0), annotations(0), fileIn(optFilename), fileOut(optOutput),
      prefix(optPrefix), indentStyle(optIndentStyle), indentOffset(optOffset),
      parens(optParens), verbose(optVerbose), log(optLog), logDir(optLogDir) {
  if(optStripCasts.size())
    for(StripCasts cst : optStripCasts)
      set(cst);
  else
    set(StripCasts::Never);

  if(optAnnotations.size())
    for(Annotations ann : optAnnotations)
      set(ann);
  else
    set(Annotations::None);

  // Sanity checks
  if(indentOffset > 8)
    fatal(error() << "Invalid value for offset. Min 0, Max 8");
}

void Options::set(StripCasts cst) {
  stripCasts |= (unsigned)cst;
}

void Options::set(Annotations ann) {
  annotations |= (unsigned)ann;
}

bool Options::has(StripCasts cst) const {
  if(cst == StripCasts::Never) {
    return stripCasts == 0;
  } else {
    unsigned i = (unsigned)cst;
    return (stripCasts & i) == i;
  }
}

bool Options::has(Annotations ann) const {
  unsigned i = (unsigned)ann;
  return (annotations & i) == i;
}

} // namespace cish