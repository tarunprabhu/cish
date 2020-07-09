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

#include "CishContext.h"
#include "Diagnostics.h"

using namespace clang;

namespace cish {

CishContext::CishContext(const std::string& triple)
    : fileMgr(fileOpts), diagIDs(new DiagnosticIDs),
      diagOpts(new DiagnosticOptions), diagEngine(diagIDs, diagOpts),
      srcMgr(diagEngine, fileMgr), targetOpts(new TargetOptions), targetInfo() {
  langOpts.CPlusPlus11 = true;
  langOpts.Bool = true;

  idents.reset(new IdentifierTable(langOpts));

  targetOpts->Triple = triple;
  targetInfo.reset(TargetInfo::CreateTargetInfo(diagEngine, targetOpts));

  astContext.reset(new ASTContext(langOpts, srcMgr, *idents, sels, builtins));
  astContext->InitBuiltinTypes(*targetInfo);

  topLevelNames.reset(new NameGenerator);
}

ASTContext& CishContext::getASTContext() const {
  return *astContext;
}

const LangOptions& CishContext::getLangOptions() const {
  return langOpts;
}

NameGenerator& CishContext::addNameGenerator(const std::string& name) {
  if(not nameGens.contains(name))
    nameGens[name].reset(new NameGenerator(*topLevelNames));
  return *nameGens.at(name);
}

NameGenerator& CishContext::getNameGenerator(const std::string& name) const {
  return *nameGens.at(name);
}

NameGenerator& CishContext::getNameGenerator(FunctionDecl* f) const {
  if(not f)
    return *topLevelNames;
  return *nameGens.at(irClangMap->getUniqueName(f));
}

Vector<FunctionDecl*> CishContext::getFunctions() const {
  Vector<FunctionDecl*> funcs;
  for(Decl* decl : astContext->getTranslationUnitDecl()->decls())
    if(auto* func = dyn_cast<FunctionDecl>(decl))
      if(func->getBody())
        funcs.push_back(func);
  return funcs;
}

} // namespace cish
