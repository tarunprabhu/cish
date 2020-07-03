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

#ifndef CISH_NAME_GENERATOR_H
#define CISH_NAME_GENERATOR_H

#include "Map.h"
#include "Set.h"

namespace cish {

class CishContext;

// Keeps track of known and generated names and temporary variables
// Used to determine priorities when running the cleanup passes
class NameGenerator {
private:
  CishContext& cishContext;
  const std::string globalPrefix;

  // The only reason this is a standard std::set is because the insert function
  // returns an iterator to the just inserted element while the Set wrapper
  // returns true/false depending on whether or not an element was actually
  // inserted into the set
  std::set<std::string> allNames;
  Set<std::string> userNames;
  Set<std::string> ambigNames;
  Set<std::string> genNames;
  Map<std::string, unsigned> suffixes;

private:
  bool contains(const std::string& var) const;

public:
  NameGenerator(CishContext& cishContext);
  NameGenerator(const NameGenerator&) = delete;
  NameGenerator(NameGenerator&&) = delete;

  // These are variables that are explicitly known and can be safely associated
  // with an entity like a function argument or a local variable. There is
  // also no ambiguity here because there is a single program entity associated
  // with the variable and it will not be duplicated.
  // These have the highest priority when it comes to deciding which names
  // should be maintained in the final cleaned up code
  const std::string& registerUserName(const std::string& var);

  // These are used when there is user variable information available in the
  // debug information but it cannot be unambiguously associated with a
  // program element. This happens as a result of compiler optimizations
  // like inlining. The reason these are maintained separately is that
  // because they bear a resemblance to some variable in the user code,
  // even if they end up with a suffix attached to them, they may make it easier
  // to associate the decompiled code with the original.
  const std::string& getAmbiguousUserName(const std::string& var,
                                          bool forceSuffix);

  // These are names of variables and other entities that are entirely
  // generated by Cish and have absolutely no corresponding program entity.
  // These are intended to be the first in line to be optimized away
  const std::string& getNewName(const std::string& prefix,
                                bool useGlobalPrefix = true);

  // Convenience function for a new variable name
  const std::string& getNewVarName(bool useGlobalPrefix = true);

  bool isGeneratedName(const std::string& name) const;
  bool isAmbiguousUserName(const std::string& name) const;
  bool isUserName(const std::string& name) const;
};

} // namespace cish

#endif // CISH_NAME_GENERATOR_H