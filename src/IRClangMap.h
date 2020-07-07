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

#ifndef IR_CLANG_MAP_H
#define IR_CLANG_MAP_H

#include "Map2.h"

#include <clang/AST/ExprCXX.h>

#include <string>

namespace cish {

// Interface for objects that maintain a mapping between the low level IR
// elements to the Clang AST nodes
class IRClangMap {
protected:
  Map2<std::string, clang::FunctionDecl*> fnames;
  Map2<std::string, clang::VarDecl*> gnames;

protected:
  IRClangMap() = default;
  IRClangMap(const IRClangMap&) = delete;
  IRClangMap(IRClangMap&&) = delete;

public:
  virtual ~IRClangMap() = default;

  virtual void addFunction(const std::string& name, clang::FunctionDecl* decl);
  virtual void addGlobal(const std::string& name, clang::VarDecl* decl);

  // Return the unique (possibly mangled) name of the function
  virtual const std::string& getUniqueName(clang::FunctionDecl* decl) const;

  // Return the unique (possibly mangled) name of the global variable
  virtual const std::string& getUniqueName(clang::VarDecl* decl) const;

  virtual clang::FunctionDecl* getFunction(const std::string& name) const;
  virtual clang::VarDecl* getGlobal(const std::string& name) const;
};

} // namespace cish

#endif // IR_CLANG_MAP_H
