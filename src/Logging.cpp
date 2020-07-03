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

#include "Logging.h"
#include "Diagnostics.h"
#include "Options.h"

#include <cstdio>

using namespace llvm;

namespace cish {

Logger::Logger() : nullStream(new raw_null_ostream()) {
  ;
}

Logger::Logger(const std::string& filename) : Logger() {
  std::error_code ec;
  fdStream.reset(new raw_fd_ostream(filename, ec));
  if(not ec) {
    message() << "Writing to log file " << filename << "\n";
  } else {
    warning() << "Could not open log file: " << filename << ". " << ec.message()
              << "\n";
  }
}

Logger::Logger(Logger&& o) {
  this->nullStream = std::move(o.nullStream);
  this->fdStream = std::move(o.fdStream);
}

Logger::~Logger() {
  if(fdStream) {
    fdStream->flush();
    fdStream->close();
  }
  nullStream.reset(nullptr);
  fdStream.reset(nullptr);
}

Logger::operator bool() const {
  return fdStream.get();
}

raw_ostream& Logger::operator()() {
  if(fdStream)
    return *fdStream.get();
  return *nullStream.get();
}

Logger Logger::openFile(const std::string& base,
                        const std::string& tag,
                        const std::string& ext) {
  if(not opts().log)
    return Logger();

  std::string buf;
  raw_string_ostream fname(buf);
  if(opts().logDir.length())
    fname << opts().logDir << "/";
  if(tag.length())
    fname << base << "." << tag << "." << ext;

  return Logger(fname.str());
}

Logger Logger::openFile(const std::string& base,
                        unsigned tag,
                        const std::string& ext) {
  char stag[20];
  sprintf(stag, "%05d", tag);
  return Logger::openFile(base, std::string(stag), ext);
}

} // namespace cish
