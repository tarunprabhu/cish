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

#ifndef CISH_CISH_CONTEXT_H
#define CISH_CISH_CONTEXT_H

#include "IRClangMap.h"
#include "Map.h"
#include "NameGenerator.h"

#include <clang/AST/ASTContext.h>
#include <clang/Basic/Builtins.h>

namespace cish {

class CishContext {
protected:
  clang::FileSystemOptions fileOpts;
  clang::FileManager fileMgr;
  clang::IntrusiveRefCntPtr<clang::DiagnosticIDs> diagIDs;
  clang::IntrusiveRefCntPtr<clang::DiagnosticOptions> diagOpts;
  clang::DiagnosticsEngine diagEngine;
  clang::SourceManager srcMgr;
  clang::LangOptions langOpts;
  std::shared_ptr<clang::IdentifierTable> idents;
  clang::Builtin::Context builtins;
  clang::SelectorTable sels;
  std::shared_ptr<clang::TargetOptions> targetOpts;
  std::shared_ptr<clang::TargetInfo> targetInfo;
  std::unique_ptr<clang::ASTContext> astContext;
  std::unique_ptr<NameGenerator> topLevelNames;

  Map<std::string, std::unique_ptr<NameGenerator>> nameGens;
  std::unique_ptr<IRClangMap> irClangMap;

protected:
  CishContext(const std::string& triple);
  CishContext() = delete;
  CishContext(const CishContext&) = delete;
  CishContext(CishContext&&) = delete;

  NameGenerator& addNameGenerator(const std::string& name);

public:
  virtual ~CishContext() = default;

  clang::ASTContext& getASTContext() const;
  const clang::LangOptions& getLangOptions() const;
  NameGenerator& getNameGenerator(const std::string& name) const;
  NameGenerator& getNameGenerator(clang::FunctionDecl* f) const;
  Vector<clang::FunctionDecl*> getFunctions() const;
};

} // namespace cish

#endif // CISH_CISH_CONTEXT_H
