#include "Diagnostics.h"

using namespace llvm;

namespace cish {

WithColor warning(raw_ostream& os) {
  WithColor::warning(os);
  return WithColor(os);
}

WithColor error(raw_ostream& os) {
  WithColor::error(os);
  return WithColor(os);
}

void fatal(raw_ostream& os) {
  os << "\n";
  exit(1);
}

} // namespace cish
