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

#ifndef CISH_DIAGNOSTICS_H
#define CISH_DIAGNOSTICS_H

#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

namespace cish {

llvm::WithColor message(llvm::raw_ostream& = llvm::outs());
llvm::WithColor warning(llvm::raw_ostream& = llvm::errs());
llvm::WithColor error(llvm::raw_ostream& = llvm::errs());
[[noreturn]] void fatal(llvm::raw_ostream& = llvm::errs());

std::string getBacktrace();

} // namespace cish

#endif // CISH_DIAGNOSTICS_H
