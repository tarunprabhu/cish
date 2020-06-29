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
