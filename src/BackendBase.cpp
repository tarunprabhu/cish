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

#include "BackendBase.h"
#include "AST.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "LLVMUtils.h"
#include "Options.h"

using namespace clang;

namespace cish {

BackendBase::BackendBase(CishContext& cishContext)
    : cishContext(cishContext), astContext(cishContext.getASTContext()),
      builder(astContext), varPrefix(opts().prefix), varSuffix(0) {
  ;
}

std::string BackendBase::getNewVar(const std::string& prefix) {
  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << varPrefix << prefix << varSuffix;

  varSuffix++;

  return ss.str();
}

void BackendBase::beginFunction(FunctionDecl*) {
  varSuffix = 0;
  stmts.emplace();
}

void BackendBase::endFunction(FunctionDecl* f) {
  varSuffix = 0;
  stmts.clear();
  cishContext.addAST(f).recalculate();
}

Stmt* BackendBase::add(Stmt* stmt) {
  stmts.top().push_back(stmt);
  return stmt;
}

Stmt& BackendBase::add(Stmt& stmt) {
  return *add(&stmt);
}

} // namespace cish
