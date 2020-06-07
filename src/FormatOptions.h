#ifndef CISH_FORMAT_OPTIONS_H
#define CISH_FORMAT_OPTIONS_H

#include "Set.h"

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
  KR,
  Allman,
  Stroustrup,
};

struct FormatOptions {
  Set<IgnoreCasts> ignoreCasts;
  Set<Annotations> annotations;
  Indentation indentation;
  unsigned offset;
};

} // namespace cish

#endif // CISH_FORMAT_OPTIONS_H
