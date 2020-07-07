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

#include "NameGenerator.h"
#include "Options.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

NameGenerator::NameGenerator() : globalPrefix(opts().prefix) {
  ;
}

NameGenerator::NameGenerator(const NameGenerator& parent) : NameGenerator() {
  allNames = parent.allNames;
  userNames = parent.userNames;
  ambigNames = parent.ambigNames;
  genNames = parent.genNames;
  suffixes = parent.suffixes;
}

bool NameGenerator::contains(const std::string& var) const {
  return allNames.find(var) != allNames.end();
}

const std::string& NameGenerator::registerUserName(const std::string& name) {
  userNames.insert(name);
  allNames.insert(name);
  suffixes[name] = 0;

  return name;
}

const std::string& NameGenerator::getAmbiguousUserName(const std::string& name,
                                                       bool forceSuffix) {
  if((not contains(name)) and (not forceSuffix)) {
    ambigNames.insert(name);
    suffixes[name] = 0;

    return *allNames.insert(name).first;
  }

  do {
    std::string newName = name + std::to_string(suffixes[name]++);
    if(not contains(newName)) {
      ambigNames.insert(newName);
      return *allNames.insert(newName).first;
    }
  } while(true);
}

const std::string& NameGenerator::getNewName(const std::string& prefix,
                                             bool useGlobalPrefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  if(useGlobalPrefix)
    ss << globalPrefix;
  ss << prefix;
  std::string base = ss.str();
  do {
    std::string newName = base + std::to_string(suffixes[prefix]++);
    if(not contains(newName)) {
      genNames.insert(newName);
      return *allNames.insert(newName).first;
    }
  } while(true);
}

const std::string& NameGenerator::getNewVarName(bool useGlobalPrefix) {
  return getNewName("t", useGlobalPrefix);
}

bool NameGenerator::isUserName(const std::string& name) const {
  return userNames.contains(name);
}

bool NameGenerator::isAmbiguousUserName(const std::string& name) const {
  return ambigNames.contains(name);
}

bool NameGenerator::isGeneratedName(const std::string& name) const {
  return genNames.contains(name);
}

llvm::raw_ostream& NameGenerator::dump(const Set<std::string>& names,
                                       const std::string& label,
                                       llvm::raw_ostream& os) const {
  os << "  " << label << ":\n";
  for(const std::string& name : names)
    os << "    " << name << "\n";

  return os;
}

void NameGenerator::dump(llvm::raw_ostream& os) const {
  os << "names:\n";
  dump(userNames, "user", os);
  dump(ambigNames, "ambiguous", os);
  dump(genNames, "generated", os);
}

} // namespace cish
