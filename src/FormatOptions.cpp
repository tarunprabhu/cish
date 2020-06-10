#include "FormatOptions.h"

namespace cish {

FormatOptions::FormatOptions()
  : ignoreCasts(0), annotations(0), prefix(FormatOptions::defPrefix),
      indentation(FormatOptions::defIndentStyle),
      parens(FormatOptions::defParens), offset(FormatOptions::defOffset),
      quiet(FormatOptions::defQuiet) {
  set(FormatOptions::defStripCasts);
  set(FormatOptions::defAnnotations);
}

void FormatOptions::set(StripCasts cst) {
  ignoreCasts |= (unsigned)cst;
}

void FormatOptions::set(Annotations ann) {
  annotations |= (unsigned)ann;
}

bool FormatOptions::has(StripCasts cst) const {
  if(cst == StripCasts::Never) {
    return ignoreCasts == 0;
  } else {
    unsigned i = (unsigned)cst;
    return (ignoreCasts & i) == i;
  }
}

bool FormatOptions::has(Annotations ann) const {
  unsigned i = (unsigned)ann;
  return (annotations & i) == i;
}

} // namespace cish
