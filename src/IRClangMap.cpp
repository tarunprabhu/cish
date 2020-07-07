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

#include "IRClangMap.h"

namespace cish {

const std::string& IRClangMap::getUniqueName(clang::FunctionDecl* decl) const {
  return fnames.at(decl);
}

const std::string& IRClangMap::getUniqueName(clang::VarDecl* decl) const {
  return gnames.at(decl);
}

void IRClangMap::addFunction(const std::string& fname,
                             clang::FunctionDecl* decl) {
  fnames.insert(fname, decl);
}

void IRClangMap::addGlobal(const std::string& gname, clang::VarDecl* decl) {
  gnames.insert(gname, decl);
}

clang::FunctionDecl* IRClangMap::getFunction(const std::string& name) const {
  return fnames.at(name);
}

clang::VarDecl* IRClangMap::getGlobal(const std::string& name) const {
  return gnames.at(name);
}

} // namespace cish
