#include "Diagnostics.h"
#include "Options.h"

using namespace llvm;

namespace cish {

static raw_null_ostream nullStream;

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

WithColor message(raw_ostream& os) {
  if(not opts().verbose)
    return WithColor(nullStream);

  WithColor(os, raw_ostream::Colors::GREEN, true) << "Message";
  WithColor(os) << ": ";
  return WithColor(os);
}

} // namespace cish
