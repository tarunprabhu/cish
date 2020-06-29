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

#include "ASTBuilder.h"
#include "ASTFunctionPass.h"
#include "Map.h"
#include "Vector.h"

#include <clang/Analysis/CFG.h>

using namespace clang;

namespace cish {

class ASTRenameVarsPass : public ASTFunctionPass<ASTRenameVarsPass> {
protected:
  ASTBuilder builder;
  Map<std::string, unsigned> varNames;

protected:
  void parse(MemberExpr* memberExpr, Vector<std::string>& pieces);
  Vector<std::string> parse(Stmt* stmt);
  std::string getName(const std::string& base);

public:
  ASTRenameVarsPass(CishContext& context);
  ASTRenameVarsPass(const CishContext&) = delete;
  ASTRenameVarsPass(CishContext&&) = delete;
  virtual ~ASTRenameVarsPass() = default;

  virtual llvm::StringRef getPassName() const override;
  bool process(FunctionDecl* f);
};

ASTRenameVarsPass::ASTRenameVarsPass(CishContext& context)
    : ASTFunctionPass(context), builder(astContext) {
  ;
}

llvm::StringRef ASTRenameVarsPass::getPassName() const {
  return "Cish AST Rename Vars";
}

void ASTRenameVarsPass::parse(MemberExpr* memberExpr,
                              Vector<std::string>& pieces) {
  pieces.push_back(memberExpr->getMemberDecl()->getName());
  if(auto* next = dyn_cast<MemberExpr>(memberExpr->getBase()))
    parse(next, pieces);
}

Vector<std::string> ASTRenameVarsPass::parse(Stmt* stmt) {
  Vector<std::string> pieces;
  if(auto* memberExpr = dyn_cast<MemberExpr>(stmt)) {
    parse(memberExpr, pieces);
  } else if(auto* unOp = dyn_cast<UnaryOperator>(stmt)) {
    if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(unOp->getSubExpr()))
      if(auto* declRef = dyn_cast<DeclRefExpr>(arrExpr->getBase()))
        if(auto* var = dyn_cast<VarDecl>(declRef->getFoundDecl()))
          pieces.push_back(var->getName());
  }

  return pieces;
}

std::string ASTRenameVarsPass::getName(const std::string& base) {
  if(not varNames.contains(base)) {
    varNames[base] = 0;
    return base;
  }

  std::string buf;
  llvm::raw_string_ostream ss(buf);
  ss << base << ++varNames[base];
  return ss.str();
}

bool ASTRenameVarsPass::process(FunctionDecl* f) {
  bool changed = false;

  Vector<VarDecl*> vars;
  for(Decl* decl : f->decls()) {
    if(auto* var = dyn_cast<VarDecl>(decl)) {
      varNames[var->getName()] = 0;
      vars.push_back(var);
    }
  }

  bool varChanged = false;
  do {
    varChanged = false;
    for(VarDecl* var : vars) {
      if(var->getName().startswith("_") and ast->hasSingleDef(var)) {
        Stmt* def = cast<BinaryOperator>(ast->getSingleDef(var))->getRHS();
        Vector<std::string> pieces = parse(def);
        if(pieces.size()) {
          if(pieces.size() == 1)
            var->setDeclName(builder.createDeclName(getName(pieces[0])));
          else
            var->setDeclName(
                builder.createDeclName(getName(pieces[1] + "_" + pieces[0])));
          changed |= true;
          varChanged |= true;
        }
      }
    }
  } while(varChanged);

  return changed;
}

} // namespace cish

cish::ASTPass* createASTRenameVarsPass(cish::CishContext& context) {
  return new cish::ASTRenameVarsPass(context);
}
