//  ---------------------------------------------------------------------------
//  Copyright (C) 2020 Tarun Prabhu
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

#include "ExprNumberMap.h"
#include "CishContext.h"
#include "ClangUtils.h"
#include "Diagnostics.h"

#include <llvm/ADT/APFloat.h>

using namespace clang;

namespace cish {

static const Set<Expr*> emptyEqvExprs;

ExprNumberMap::ExprNumberMap(CishContext& cishContext)
    : cishContext(cishContext) {
  clear();
}

void ExprNumberMap::clear() {
  nextExprNum = 1;

  blits.clear();
  clits.clear();
  ilits16.clear();
  ilits32.clear();
  ilits64.clear();
  ulits16.clear();
  ulits32.clear();
  ulits64.clear();
  flits.clear();
  dlits.clear();
  glits.clear();
  slits.clear();
  unOps.clear();
  binOps.clear();
  condOps.clear();
  arrExprs.clear();
  callExprs.clear();
  castExprs.clear();
  memberExprs.clear();
  declRefs.clear();

  varNums.clear();
  exprNums.clear();
  eqvExprs.clear();
}

ExprNum ExprNumberMap::getNewExprNum() {
  return nextExprNum++;
}

ExprNum ExprNumberMap::add(Expr* expr, ExprNum exprNum) {
  if(VarDecl* var = Clang::getVar(expr))
    varNums[var] = exprNum;
  eqvExprs[exprNum].insert(expr);
  return exprNums[expr] = exprNum;
}

bool ExprNumberMap::has(VarDecl* var) const {
  return varNums.contains(var);
}

bool ExprNumberMap::has(Expr* expr) const {
  return exprNums.contains(expr);
}

ExprNum ExprNumberMap::get(VarDecl* var) const {
  return varNums.at(var);
}

ExprNum ExprNumberMap::get(Expr* expr) const {
  return exprNums.at(expr);
}

void ExprNumberMap::remove(Expr* expr) {
  if(exprNums.contains(expr))
    eqvExprs[exprNums.at(expr)].erase(expr);
  exprNums.erase(expr);
}

void ExprNumberMap::refresh(Expr* expr) {
  remove(expr);
  add(expr);
}

const Set<Expr*>& ExprNumberMap::getEqv(Expr* expr) const {
  if(not eqvExprs.contains(exprNums.at(expr)))
    return emptyEqvExprs;
  return eqvExprs.at(exprNums.at(expr));
}

const Set<Expr*>& ExprNumberMap::getEqv(ExprNum exprNum) const {
  if(not eqvExprs.contains(exprNum))
    return emptyEqvExprs;
  return eqvExprs.at(exprNum);
}

ExprNum ExprNumberMap::add(CXXBoolLiteralExpr* blit) {
  if(not has(blit)) {
    ExprNum v = blit->getValue();
    if(not blits.contains(v))
      blits[v] = add(blit, getNewExprNum());
    return add(blit, blits[v]);
  }

  return get(blit);
}

ExprNum ExprNumberMap::add(CharacterLiteral* clit) {
  if(not has(clit)) {
    unsigned c = clit->getValue();
    if(not clits.contains(c))
      clits[c] = add(clit, getNewExprNum());
    return add(clit, clits[c]);
  }

  return get(clit);
}

ExprNum ExprNumberMap::add(IntegerLiteral* ilit) {
  if(not has(ilit)) {
    llvm::APInt api = ilit->getValue();
    if(api.isSignedIntN(16))
      return add(ilit, ilits16);
    else if(api.isSignedIntN(32))
      return add(ilit, ilits32);
    else if(api.isSignedIntN(64))
      return add(ilit, ilits64);
    else if(api.isIntN(16))
      return add(ilit, ulits16);
    else if(api.isIntN(32))
      return add(ilit, ulits32);
    else if(api.isIntN(64))
      return add(ilit, ulits64);
    else
      fatal(error() << "Unsupported integer literal: " << api);
  }

  return get(ilit);
}

ExprNum ExprNumberMap::add(FloatingLiteral* flit) {
  if(not has(flit)) {
    llvm::APFloat apf = flit->getValue();
    switch(llvm::APFloat::getSizeInBits(apf.getSemantics())) {
    case 32:
      return add(flit, apf.convertToFloat(), flits);
    case 64:
      return add(flit, apf.convertToDouble(), dlits);
    case 80:
    case 128:
    default:
      fatal(error() << "Unsupported floating point kind: "
                    << Clang::toString(flit, cishContext.getASTContext()));
      break;
    }
  }

  return get(flit);
}

ExprNum ExprNumberMap::add(StringLiteral* slit) {
  if(not has(slit)) {
    std::string s = slit->getString();
    if(not slits.contains(s))
      slits[s] = add(slit, getNewExprNum());
    return add(slit, slits[s]);
  }

  return get(slit);
}

ExprNum ExprNumberMap::add(CXXNullPtrLiteralExpr* nlit) {
  // Don't make nullptr literals equivalent because they aren't actually
  // equivalent
  return add(nlit, getNewExprNum());
}

ExprNum ExprNumberMap::add(UnaryOperator* unOp) {
  if(not has(unOp)) {
    UnaryOperator::Opcode op = unOp->getOpcode();
    auto k = get(unOp->getSubExpr());
    if(not unOps[op].contains(k))
      unOps[op][k] = add(unOp, getNewExprNum());
    return add(unOp, unOps[op][k]);
  }

  return get(unOp);
}

ExprNum ExprNumberMap::add(BinaryOperator* binOp) {
  if(not has(binOp)) {
    BinaryOperator::Opcode op = binOp->getOpcode();
    auto p = std::make_pair(get(binOp->getLHS()), get(binOp->getRHS()));
    if(not binOps[op].contains(p))
      binOps[op][p] = add(binOp, getNewExprNum());
    return add(binOp, binOps[op][p]);
  }

  return get(binOp);
}

ExprNum ExprNumberMap::add(ConditionalOperator* condOp) {
  if(not has(condOp)) {
    Vector<ExprNum> v = {get(condOp->getCond()),
                         get(condOp->getTrueExpr()),
                         get(condOp->getFalseExpr())};
    if(not condOps.contains(v))
      condOps[v] = add(condOp, getNewExprNum());
    return add(condOp, condOps[v]);
  }

  return get(condOp);
}

ExprNum ExprNumberMap::add(ArraySubscriptExpr* arrExpr) {
  if(not has(arrExpr)) {
    auto p = std::make_pair(get(arrExpr->getBase()), get(arrExpr->getIdx()));
    if(not arrExprs.contains(p))
      arrExprs[p] = add(arrExpr, getNewExprNum());
    return add(arrExpr, arrExprs[p]);
  }

  return get(arrExpr);
}

ExprNum ExprNumberMap::add(CallExpr* callExpr) {
  if(not has(callExpr)) {
    Vector<ExprNum> v = {get(callExpr->getCallee())};
    for(Expr* arg : callExpr->arguments())
      v.push_back(get(arg));
    if(not callExprs.contains(v))
      callExprs[v] = add(callExpr, getNewExprNum());
    return add(callExpr, callExprs[v]);
  }

  return get(callExpr);
}

ExprNum ExprNumberMap::add(CStyleCastExpr* castExpr) {
  if(not has(castExpr)) {
    ExprNum k = get(castExpr->getSubExpr());
    if(not castExprs.contains(k))
      castExprs[k] = add(castExpr, getNewExprNum());
    return add(castExpr, castExprs[k]);
  }

  return get(castExpr);
}

ExprNum ExprNumberMap::add(MemberExpr* memberExpr) {
  if(not has(memberExpr)) {
    auto p = std::make_pair(get(memberExpr->getBase()),
                            memberExpr->getMemberDecl());
    if(not memberExprs.contains(p))
      memberExprs[p] = add(memberExpr, getNewExprNum());
    return add(memberExpr, memberExprs[p]);
  }

  return get(memberExpr);
}

ExprNum ExprNumberMap::add(InitListExpr* initList) {
  // Not sure if there is anything to be gained by uniquing an initializer
  // list so don't bother. But it needs to be added because it may appear
  // in a BinaryOperator
  return add(initList, getNewExprNum());
}

ExprNum ExprNumberMap::add(DeclRefExpr* declRefExpr) {
  if(not has(declRefExpr)) {
    ValueDecl* decl = declRefExpr->getDecl();
    if(not declRefs.contains(decl))
      declRefs[decl] = add(declRefExpr, getNewExprNum());
    return add(declRefExpr, declRefs[decl]);
  }

  return get(declRefExpr);
}

ExprNum ExprNumberMap::add(Expr* expr) {
  if(auto* blit = dyn_cast<CXXBoolLiteralExpr>(expr))
    return add(blit);
  else if(auto* clit = dyn_cast<CharacterLiteral>(expr))
    return add(clit);
  else if(auto* ilit = dyn_cast<IntegerLiteral>(expr))
    return add(ilit);
  else if(auto* flit = dyn_cast<FloatingLiteral>(expr))
    return add(flit);
  else if(auto* slit = dyn_cast<StringLiteral>(expr))
    return add(slit);
  else if(auto* nlit = dyn_cast<CXXNullPtrLiteralExpr>(expr))
    return add(nlit);
  else if(auto* unOp = dyn_cast<UnaryOperator>(expr))
    return add(unOp);
  else if(auto* binOp = dyn_cast<BinaryOperator>(expr))
    return add(binOp);
  else if(auto* condOp = dyn_cast<ConditionalOperator>(expr))
    return add(condOp);
  else if(auto* arrExpr = dyn_cast<ArraySubscriptExpr>(expr))
    return add(arrExpr);
  else if(auto* callExpr = dyn_cast<CallExpr>(expr))
    return add(callExpr);
  else if(auto* castExpr = dyn_cast<CStyleCastExpr>(expr))
    return add(castExpr);
  else if(auto* memberExpr = dyn_cast<MemberExpr>(expr))
    return add(memberExpr);
  else if(auto* initList = dyn_cast<InitListExpr>(expr))
    return add(initList);
  else
    fatal(error() << "Unknown expression in number map: "
                  << expr->getStmtClassName());

  return 0;
}

void ExprNumberMap::dump(llvm::raw_ostream& os) const {
  os << "varNums:\n";
  for(const auto& i : varNums) {
    VarDecl* var = i.first;
    ExprNum n = i.second;
    os << "    " << var->getName() << " => " << n << "\n";
  }
  os << "exprs:\n";
  for(const auto& i : exprNums) {
    Expr* expr = i.first;
    ExprNum n = i.second;
    os << "    " << Clang::toString(expr, cishContext.getASTContext()) << " => "
       << n << "\n";
  }
}

} // namespace cish
