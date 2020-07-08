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

#include "ClangUtils.h"
#include "ASTStreamer.h"
#include "Diagnostics.h"

#include <clang/AST/RecursiveASTVisitor.h>

#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

namespace Clang {

class FindVarsInStmt : public RecursiveASTVisitor<FindVarsInStmt> {
protected:
  Set<VarDecl*>& vars;

public:
  explicit FindVarsInStmt(Set<VarDecl*>& vars) : vars(vars) {
    ;
  }

  bool VisitDeclRefExpr(DeclRefExpr* ref) {
    if(auto* var = dyn_cast<VarDecl>(ref->getDecl()))
      if(not isa<ParmVarDecl>(var))
        vars.insert(var);
    return true;
  }
};

const Type* getBaseType(const ArrayType* aty) {
  const Type* ety = aty->getElementType().getTypePtr();
  if(const auto* bty = dyn_cast<ArrayType>(ety))
    return getBaseType(bty);
  return ety;
}

Expr* stripCasts(Expr* expr) {
  if(auto* cst = dyn_cast<CastExpr>(expr))
    return stripCasts(cst->getSubExpr());
  return expr;
}

static bool isEqualInt(Expr* lhs, Expr* rhs) {
  if(auto* lint = dyn_cast<IntegerLiteral>(lhs))
    if(auto* rint = dyn_cast<IntegerLiteral>(rhs))
      return lint->getValue().getLimitedValue()
             == rint->getValue().getLimitedValue();
  return false;
}

static bool isEqualFloat(Expr* lhs, Expr* rhs) {
  if(auto* lf = dyn_cast<FloatingLiteral>(lhs))
    if(auto* rf = dyn_cast<FloatingLiteral>(rhs))
      return lf->getValue().compare(rf->getValue()) == llvm::APFloat::cmpEqual;
  return false;
}

bool isEqual(Expr* lhs, Expr* rhs) {
  if(lhs->getType()->isIntegerType())
    return isEqualInt(lhs, rhs);
  else if(lhs->getType()->isFloatingType())
    return isEqualFloat(lhs, rhs);
  return false;
}

bool isConstant(Expr* expr, uint64_t val) {
  if(auto* intLit = dyn_cast<IntegerLiteral>(expr))
    return intLit->getValue().getLimitedValue() == val;
  return false;
}

bool isZero(Expr* expr) {
  return isConstant(expr, 0);
}

bool isOne(Expr* expr) {
  return isConstant(expr, 1);
}

bool isLiteral(const Expr* expr) {
  return isa<CXXBoolLiteralExpr>(expr) or isa<IntegerLiteral>(expr)
         or isa<FloatingLiteral>(expr) or isa<CXXNullPtrLiteralExpr>(expr)
         or isa<CharacterLiteral>(expr) or isa<StringLiteral>(expr);
}

VarDecl* getVar(Expr* expr) {
  if(auto* ref = dyn_cast<DeclRefExpr>(expr))
    if(auto* var = dyn_cast<VarDecl>(ref->getDecl()))
      return var;
  return nullptr;
}

Vector<VarDecl*> getVarsInStmtAsVector(Stmt* stmt) {
  Set<VarDecl*> vars = getVarsInStmt(stmt);
  return Vector<VarDecl*>(vars.begin(), vars.end());
}

Set<VarDecl*> getVarsInStmt(Stmt* stmt) {
  Set<VarDecl*> vars;
  FindVarsInStmt(vars).TraverseStmt(stmt);
  return vars;
}

static void collectCommaExprs(BinaryOperator* binOp,
                              Vector<BinaryOperator*>& exprs) {
  switch(binOp->getOpcode()) {
  case BO_Comma:
    collectCommaExprs(cast<BinaryOperator>(binOp->getLHS()), exprs);
    collectCommaExprs(cast<BinaryOperator>(binOp->getRHS()), exprs);
    break;
  default:
    exprs.push_back(binOp);
    break;
  }
}

Vector<BinaryOperator*> getCommaExprs(BinaryOperator* binOp) {
  Vector<BinaryOperator*> exprs;

  collectCommaExprs(binOp, exprs);

  return exprs;
}

static void collectForInits(BinaryOperator* binOp,
                            Vector<BinaryOperator*>& inits) {
  switch(binOp->getOpcode()) {
  case BO_Assign:
    inits.push_back(binOp);
    break;
  case BO_Comma:
    collectCommaExprs(binOp, inits);
    break;
  default:
    fatal(error() << "Initializer in a for must be either an assignment or a "
                     "comma operator with assignments");
  }
}

Vector<BinaryOperator*> getForInits(ForStmt* forStmt) {
  Vector<BinaryOperator*> inits;
  if(Stmt* init = forStmt->getInit()) {
    if(auto* binOp = dyn_cast<BinaryOperator>(forStmt->getInit()))
      collectForInits(binOp, inits);
    else
      fatal(error() << "Unexpected statement in for loop initializer: "
                    << init->getStmtClassName());
  }
  return inits;
}

Vector<BinaryOperator*> getForIncs(ForStmt* forStmt) {
  Vector<BinaryOperator*> incs;
  if(Stmt* inc = forStmt->getInc()) {
    if(auto* binOp = dyn_cast<BinaryOperator>(inc))
      collectCommaExprs(binOp, incs);
    else
      fatal(error() << "Unexpected statement in for loop increment: "
                    << inc->getStmtClassName());
  }
  return incs;
}

std::string toString(QualType type, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << type;

  return ss.str();
}

std::string toString(Stmt* stmt, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << stmt;

  return ss.str();
}

std::string toString(FunctionDecl* f, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << f;

  return ss.str();
}

std::string toString(VarDecl* var, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << var;

  return ss.str();
}

std::string toString(RecordDecl* record, ASTContext& astContext) {
  ASTStreamer ss(astContext);

  ss << record;

  return ss.str();
}

} // namespace Clang

} // namespace cish
