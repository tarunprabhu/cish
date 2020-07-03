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

#ifndef CISH_LOGGING_H
#define CISH_LOGGING_H

#include <llvm/Support/raw_ostream.h>

namespace cish {

class Logger {
private:
  std::unique_ptr<llvm::raw_ostream> nullStream;
  std::unique_ptr<llvm::raw_fd_ostream> fdStream;

protected:
  Logger(const std::string& filename);

public:
  Logger();
  Logger(const Logger&) = delete;
  Logger(Logger&&);
  ~Logger();

  operator bool() const;
  llvm::raw_ostream& operator()();

public:
  static Logger
  openFile(const std::string& base, unsigned tag, const std::string& ext);
  static Logger openFile(const std::string& base,
                         const std::string& tag,
                         const std::string& ext);
};

} // namespace cish

#endif // CISH_LOGGING_H
