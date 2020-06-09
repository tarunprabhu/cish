#ifndef CISH_FORMAT_OPTIONS_H
#define CISH_FORMAT_OPTIONS_H

#include "Set.h"

#include <string>

namespace cish {

enum class IgnoreCasts {
  None,
  Function,
  Vector,
  All,
};

enum class Annotations {
  Source,
  Cish,
  All,
};

enum class Indentation {
  KR,         // K&R style
  Allman,     // Allman style
  Stroustrup, // Stroustrup style (like K&R but without the "cuddled else")
};

enum class Parens {
  Always, // Always add parentheses to operands of oeprators
  Smart,  // Be "smart" about adding parentheses to operands of operators
};

struct FormatOptions {
  // The prefix string to use for generated variable names
  std::string prefix;
  Set<IgnoreCasts> ignoreCasts;
  Set<Annotations> annotations;
  Indentation indentation;
  Parens parens;
  unsigned offset;
};

} // namespace cish

#endif // CISH_FORMAT_OPTIONS_H
