#include "LLVMParser.h"
#include "CishContext.h"
#include "DIParser.h"
#include "LLVMUtils.h"
#include "Vector.h"

#include <llvm/ADT/SmallVector.h>
// #include <llvm/Analysis/LoopInfo.h>
// #include <llvm/Analysis/LoopPass.h>
// #include <llvm/Analysis/Passes.h>
// #include <llvm/Analysis/ScalarEvolution.h>
// #include <llvm/IR/Dominators.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

namespace cish {

LLVMParser::LLVMParser(CishContext& ctxt, const DIParser& di)
    : ctxt(ctxt), di(di) {
  ;
}

// void LLVMParser::getAnalysisUsage(AnalysisUsage& AU) const {
//   // AU.addRequired<LoopInfoWrapperPass>();
//   // AU.addRequired<DominatorTreeWrapperPass>();
//   // AU.addRequired<ScalarEvolutionWrapperPass>();
//   AU.setPreservesAll();
// }

const Instruction&
LLVMParser::getInstructionForConstantExpr(const ConstantExpr& cexpr) {
  if(not cexprs.contains(&cexpr))
    // ********************* HORRENDOUS EVIL AHEAD *************************
    //
    // FIXME: Make this go away ASAP!
    //
    // ConstantExpr for LLVM 8.0 (that this was originally developed with)
    // is not marked const, but it is in later versions. I don't think
    // anything has actually changed in the code between the two versions.
    // And I really don't want to "un-const" everything here. So just
    // cast away the const and forge ahead.
    //
    // ---------------------------------------------------------------------
    cexprs[&cexpr]
        = std::unique_ptr<Instruction, std::function<void(Instruction*)>>(
            const_cast<ConstantExpr&>(cexpr).getAsInstruction(),
            [](Instruction* inst) { inst->deleteValue(); });

  return *cexprs.at(&cexpr);
}

const ConstantDataArray*
LLVMParser::getStringLiteral(const llvm::Instruction& inst) {
  if(const auto* gep = dyn_cast<GetElementPtrInst>(&inst))
    if(di.isStringLiteral(gep->getPointerOperand())
       and gep->hasAllZeroIndices())
      return dyn_cast<ConstantDataArray>(
          dyn_cast<GlobalVariable>(gep->getPointerOperand())->getInitializer());
  return nullptr;
}

void LLVMParser::handle(const ConstantInt& cint) {
  handle(cint.getType());
  ctxt.add(cint);
}

void LLVMParser::handle(const ConstantFP& cfp) {
  handle(cfp.getType());
  ctxt.add(cfp);
}

void
LLVMParser::handle(const ConstantPointerNull& cnull) {
  handle(cnull.getType());
  ctxt.add(cnull);
}

void LLVMParser::handle(const ConstantAggregateZero& czero) {
  handle(czero.getType());
  ctxt.add(czero);
}

// This is a somewhat grotesque abuse of things. But there isn't an equivalent
// clang Expr for LLVM's undef value. Until (if) I actually make one, just
// use the GNUNullExpr instead to refer to this undef value
void LLVMParser::handle(const UndefValue& undef) {
  handle(undef.getType());
  ctxt.add(undef);
}

void LLVMParser::handle(const ConstantArray& carray) {
  handle(carray.getType());
  for(const Use& op : carray.operands())
    handle(op.get());

  ctxt.add(carray);
}

void LLVMParser::handle(const ConstantDataArray& cda) {
  handle(cda.getType());
  if(not(cda.isCString() or cda.isString()))
    for(unsigned i = 0; i < cda.getNumElements(); i++)
      handle(cda.getElementAsConstant(i));
  ctxt.add(cda);
}

void LLVMParser::handle(const ConstantStruct& cstruct) {
  handle(cstruct.getType());
  for(const Use& op : cstruct.operands())
    handle(op.get());

  ctxt.add(cstruct);
}

void LLVMParser::handle(const ConstantVector& cvec) {
  // We may be able to use something else to explicitly represent a vector
  // since obviously nothing precisely like it exists in C/C++, but an
  // initializer list ought to be good enough and once annotations are
  // implemented, a comment could be added near where these are used
  // indicating that they are actually vectors
  WithColor::error(errs()) << "UNIMPLEMENTED: CONSTANT VECTOR\n";
  exit(1);
}

void LLVMParser::handle(const ConstantExpr& cexpr) {
  const llvm::Instruction& inst = getInstructionForConstantExpr(cexpr);
  // If we are looking up a string literal, just use the literal because
  // that is almost always what we want, I think. I am not sure there is a lot
  // of value in exposing the intricacies of string literals being created
  // in "constant" memory and having their addresses taken when being used.
  if(const ConstantDataArray* lit = getStringLiteral(inst)) {
    handle(lit);
    llvm::cast<clang::Expr>(ctxt.add(cexpr, *lit));
  } else {
    handle(&inst);
    llvm::cast<clang::Expr>(ctxt.add(cexpr, inst));
  }
}

void LLVMParser::handle(const AllocaInst& alloca) {
  handle(alloca.getAllocatedType());
  ctxt.add(alloca, getName(alloca, "local"));
}

void LLVMParser::handle(const CastInst& cst) {
  handle(cst.getDestTy());
  handle(cst.getOperand(0));
  ctxt.add(cst);
}

void LLVMParser::handle(const BranchInst& br) {
  if(const Value* cond = br.getCondition())
    handle(cond);
  for(const llvm::BasicBlock* bb : br.successors())
    handle(bb);

  ctxt.add(br);
}

void LLVMParser::handle(const LoadInst& load) {
  handle(load.getPointerOperand());
  ctxt.add(load);
}

void LLVMParser::handle(const StoreInst& store) {
  handle(store.getPointerOperand());
  handle(store.getValueOperand());
  ctxt.add(store);
}

void LLVMParser::handleIndices(Type* ty,
                               unsigned idx,
                               const Vector<const Value*>& indices,
                               const Instruction& inst,
                               raw_string_ostream& ss) {
  const Value* op = indices[idx];
  Type* next = nullptr;
  // if(auto* pty = dyn_cast<PointerType>(ty)) {
  //   ss << "[" << handle(op) << "]";
  //   next = pty->getElementType();
  // } else if(auto* aty = dyn_cast<ArrayType>(ty)) {
  //   ss << "[" << handle(op) << "]";
  //   next = aty->getElementType();
  // } else if(auto* sty = dyn_cast<StructType>(ty)) {
  //   if(auto* cint = dyn_cast<ConstantInt>(op)) {
  //     unsigned field = cint->getLimitedValue();
  //     ss << "." << ctxt.getElementName(sty, field);
  //     next = sty->getElementType(field);
  //   } else {
  //     WithColor::error(errs()) << "Expected constant index in GEP\n"
  //                              << "          idx: " << idx << "\n"
  //                              << "           op: " << *op << "\n"
  //                              << "         type: " << *ty << "\n"
  //                              << "         inst: " << inst << "\n";
  //     exit(1);
  //   }
  // } else {
  //   WithColor::error(errs())
  //       << "GEP Indices not implemented for type: " << *ty << "\n";
  //   exit(1);
  // }

  if((idx + 1) < indices.size())
    handleIndices(next, idx + 1, indices, inst, ss);
}

void LLVMParser::handle(const GetElementPtrInst& gep) {
  std::string buf;
  raw_string_ostream ss(buf);
  const llvm::Value* ptr = gep.getPointerOperand();
  auto* pty = dyn_cast<PointerType>(ptr->getType());
  // const std::string& s = handle(gep.getPointerOperand());

  WithColor::error(errs()) << "UNIMPLEMENTED: GetElementPtrInst\n";
  exit(1);
  // // If the pointer operand is also a GEP and is not overwritten with a
  // // temporary value, it will begin with a &. Obviously, we don't want that
  // if(isa<GetElementPtrInst>(ptr) and not ctxt.isOverwrite(ptr))
  //   ss << s.substr(1);
  // else
  //   ss << s;
  // Vector<const Value*> indices(gep.idx_begin(), gep.idx_end());
  // handleIndices(pty, 0, indices, gep, ss);
  // return ctxt.add(gep, "&", ss.str());
}

void LLVMParser::handle(const PHINode& phi) {
  // FIXME: This is not correct
  WithColor::warning(errs()) << "PHI nodes not correctly handled\n";
  ctxt.add(phi, getName(phi, "phi"));
}

void LLVMParser::handle(const CallInst& call) {
  handle(call.getCalledValue());
  for(const Value* arg : call.arg_operands())
    handle(arg);
  ctxt.add(call);
}

void LLVMParser::handle(const InvokeInst& invoke) {
  WithColor::error(errs()) << "UNIMPLEMENTED: " << invoke << "\n";
  exit(1);
}

void LLVMParser::handle(const BinaryOperator& inst) {
  clang::BinaryOperator::Opcode op = (clang::BinaryOperator::Opcode)-1;

  switch(inst.getOpcode()) {
  case BinaryOperator::Add:
  case BinaryOperator::FAdd:
    op = clang::BO_Add;
    break;
  case BinaryOperator::Sub:
  case BinaryOperator::FSub:
    op = clang::BO_Sub;
    break;
  case BinaryOperator::Mul:
  case BinaryOperator::FMul:
    op = clang::BO_Mul;
    break;
  case BinaryOperator::UDiv:
  case BinaryOperator::FDiv:
  case BinaryOperator::SDiv:
    op = clang::BO_Div;
    break;
  case BinaryOperator::URem:
  case BinaryOperator::SRem:
  case BinaryOperator::FRem:
    op = clang::BO_Rem;
    break;
  case BinaryOperator::Shl:
    op = clang::BO_Shl;
    break;
  case BinaryOperator::LShr:
    op = clang::BO_Shr;
    break;
  case BinaryOperator::And:
    op = clang::BO_And;
    break;
  case BinaryOperator::Or:
    op = clang::BO_Or;
    break;
  case BinaryOperator::Xor:
    op = clang::BO_Xor;
    break;
  default:
    WithColor::error(errs()) << "Unknown binary operator: " << inst << "\n";
    exit(1);
    break;
  }

  handle(inst.getOperand(0));
  handle(inst.getOperand(1));
  ctxt.add(inst, op);
}

void LLVMParser::handle(const UnaryOperator& inst) {
  clang::UnaryOperator::Opcode op = (clang::UnaryOperator::Opcode)-1;
  switch(inst.getOpcode()) {
  case UnaryOperator::FNeg:
    op = clang::UO_Minus;
    break;
  default:
    WithColor::error(errs()) << "Unknown unary operator: " << inst << "\n";
    exit(1);
    break;
  }

  handle(inst.getOperand(0));
  ctxt.add(inst, op);
}

void LLVMParser::handle(const CmpInst& cmp) {
  clang::BinaryOperator::Opcode op = (clang::BinaryOperator::Opcode)-1;
  switch(cmp.getPredicate()) {
  case CmpInst::FCMP_OEQ:
  case CmpInst::FCMP_UEQ:
  case CmpInst::ICMP_EQ:
    op = clang::BO_EQ;
    break;
  case CmpInst::FCMP_ONE:
  case CmpInst::FCMP_UNE:
  case CmpInst::ICMP_NE:
    op = clang::BO_NE;
    break;
  case CmpInst::FCMP_OGT:
  case CmpInst::FCMP_UGT:
  case CmpInst::ICMP_UGT:
  case CmpInst::ICMP_SGT:
    op = clang::BO_GT;
    break;
  case CmpInst::FCMP_OGE:
  case CmpInst::FCMP_UGE:
  case CmpInst::ICMP_UGE:
  case CmpInst::ICMP_SGE:
    op = clang::BO_GE;
    break;
  case CmpInst::FCMP_OLT:
  case CmpInst::FCMP_ULT:
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT:
    op = clang::BO_LT;
    break;
  case CmpInst::FCMP_OLE:
  case CmpInst::FCMP_ULE:
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE:
    op = clang::BO_LE;
    break;
  default:
    WithColor::error(errs()) << "Unknown compare predicate: " << cmp << "\n";
    exit(1);
    break;
  }

  handle(cmp.getOperand(0));
  handle(cmp.getOperand(1));
  ctxt.add(cmp, op);
}

void LLVMParser::handle(const SelectInst& select) {
  handle(select.getCondition());
  handle(select.getTrueValue());
  handle(select.getFalseValue());
  ctxt.add(select);
}

void LLVMParser::handle(const SwitchInst& sw) {
  WithColor::error(errs()) << "UNIMPLEMENTED: " << sw << "\n";
  exit(1);
}

void LLVMParser::handle(const ReturnInst& ret) {
  if(Value* val = ret.getReturnValue())
    handle(ret.getReturnValue());
  ctxt.add(ret);
}

void LLVMParser::handle(const BasicBlock& bb) {
  if(bb.hasNPredecessors(0))
    ctxt.add(bb);
  else
    ctxt.add(bb, ctxt.getNewVar("bb"));
}

void LLVMParser::handle(const Instruction* inst) {
  if(const auto* load = dyn_cast<LoadInst>(inst))
    handle(*load);
  else if(const auto* store = dyn_cast<StoreInst>(inst))
    handle(*store);
  else if(const auto* phi = dyn_cast<PHINode>(inst))
    handle(*phi);
  else if(const auto* call = dyn_cast<CallInst>(inst))
    handle(*call);
  else if(const auto* invoke = dyn_cast<InvokeInst>(inst))
    handle(*invoke);
  else if(const auto* gep = dyn_cast<GetElementPtrInst>(inst))
    handle(*gep);
  else if(const auto* cst = dyn_cast<CastInst>(inst))
    handle(*cst);
  else if(const auto* sw = dyn_cast<SwitchInst>(inst))
    handle(*sw);
  else if(const auto* select = dyn_cast<SelectInst>(inst))
    handle(*select);
  else if(const auto* ret = dyn_cast<ReturnInst>(inst))
    handle(*ret);
  else if(const auto* cmp = dyn_cast<CmpInst>(inst))
    handle(*cmp);
  else if(const auto* alloca = dyn_cast<AllocaInst>(inst))
    handle(*alloca);
  else if(const auto* binop = dyn_cast<BinaryOperator>(inst))
    handle(*binop);
  else if(const auto* unop = dyn_cast<UnaryOperator>(inst))
    handle(*unop);
  else {
    WithColor::error(errs()) << "UNKNOWN INSTRUCTION: " << *inst << "\n";
    exit(1);
  }

  // If there is zero or more than one use of the instruction, then create a
  // temporary variable for it. This is particularly important in the case
  // of function calls because if we don't do it this way and the result of
  // a call is used more than once, the call will appear to be made multiple
  // times, and if it is never used, then it will appear as if the call is
  // never made. But if we always use a temporary, the result will never
  // look remotely reasonable and we might as well just read LLVM.
  bool useTemp = false;
  size_t uses = inst->getNumUses();
  if((isa<CallInst>(inst) or isa<InvokeInst>(inst))
     and (not inst->getType()->isVoidTy()) and ((uses == 0) or (uses > 1)))
    useTemp = true;
  else if(not isa<AllocaInst>(inst) and (uses > 1))
    useTemp = true;

  if(useTemp)
    ctxt.addTemp(*inst, getName(inst));
}

void LLVMParser::handle(const GlobalValue* gv) {
  WithColor::error(errs()) << "UNKNOWN GLOBAL: " << *gv << "\n";
  exit(1);
}

void LLVMParser::handle(const Constant* c) {
  if(const auto* cint = dyn_cast<ConstantInt>(c))
    return handle(*cint);
  else if(const auto* cfp = dyn_cast<ConstantFP>(c))
    return handle(*cfp);
  else if(const auto* cnull = dyn_cast<ConstantPointerNull>(c))
    return handle(*cnull);
  else if(const auto* czero = dyn_cast<ConstantAggregateZero>(c))
    return handle(*czero);
  else if(const auto* cexpr = dyn_cast<ConstantExpr>(c))
    return handle(*cexpr);
  else if(const auto* undef = dyn_cast<UndefValue>(c))
    return handle(*undef);
  else if(const auto* carray = dyn_cast<ConstantArray>(c))
    return handle(*carray);
  else if(const auto* cda = dyn_cast<ConstantDataArray>(c))
    return handle(*cda);
  else if(const auto* cstruct = dyn_cast<ConstantStruct>(c))
    return handle(*cstruct);
  else if(const auto* cvec = dyn_cast<ConstantVector>(c))
    return handle(*cvec);
  else
    WithColor::error(errs()) << "UNKNOWN CONSTANT: " << *c << "\n";

  exit(1);
}

void LLVMParser::handle(const Value* v) {
  handle(v->getType());
  if(ctxt.has(*v))
    return;

  // The order of the statements is important. All globals are constants
  // but they need to be handled differently from the other constants
  if(const auto* inst = dyn_cast<Instruction>(v))
    return handle(inst);
  else if(const auto* g = dyn_cast<GlobalValue>(v))
    return handle(g);
  else if(const auto* c = dyn_cast<Constant>(v))
    return handle(c);
  else if(const auto* bb = dyn_cast<BasicBlock>(v))
    return handle(*bb);
  else
    WithColor::error(errs()) << "UNHANDLED: " << *v << "\n";

  exit(1);
}

void LLVMParser::handle(PointerType* pty) {
  handle(pty->getElementType());
  ctxt.add(pty);
}

void LLVMParser::handle(ArrayType* aty) {
  handle(aty->getElementType());
  ctxt.add(aty);
}

void LLVMParser::handle(FunctionType* fty) {
  handle(fty->getReturnType());
  for(Type* param : fty->params())
    handle(param);
  ctxt.add(fty);
}

void LLVMParser::handle(Type* type) {
  if(ctxt.has(type))
    return;

  if(auto* pty = dyn_cast<PointerType>(type))
    handle(pty);
  else if(auto* aty = dyn_cast<ArrayType>(type))
    handle(aty);
  else if(auto* fty = dyn_cast<FunctionType>(type))
    handle(fty);
  else if(isa<IntegerType>(type) or type->isFloatingPointTy()
          or type->isVoidTy())
    ctxt.add(type);
}

std::string LLVMParser::getName(llvm::StructType* sty) {
  if(di.hasName(sty))
    return di.getName(sty);
  else if(sty->hasName())
    if(sty->getName().find("struct.") == 0)
      return sty->getName().substr(7);
    else if(sty->getName().find("class.") == 0)
      return sty->getName().substr(6);
    else if(sty->getName().find("union.") == 0)
      return sty->getName().substr(6);
    else
      return sty->getName();
  else
    return ctxt.getNewVar("struct");
}

std::string LLVMParser::getName(const llvm::Value& val,
                                const std::string& prefix) {
  if(di.hasName(val))
    return di.getName(val);
  else if(val.hasName())
    return val.getName();
  else if(prefix.length())
    return ctxt.getNewVar(prefix);
  else
    return ctxt.getNewVar();
}

std::string LLVMParser::getName(const llvm::Value* val,
                                const std::string& prefix) {
  return getName(*val);
}

bool LLVMParser::allUsesIgnored(const Value* v) const {
  if(v->getNumUses() == 0)
    return false;
  for(const Use& u : v->uses())
    if(not ignoreValues.contains(u.getUser()))
      return false;
  return true;
}

void LLVMParser::runOnStruct(StructType* sty) {
  Vector<std::string> fields;
  for(unsigned i = 0; i < sty->getNumElements(); i++) {
    // Shouldn't handle struct types because the only "handling" is done
    // by this method. And it's not clear
    Type* ety = sty->getElementType(i);
    if(not isa<StructType>(ety))
      handle(sty->getElementType(i));
    if(di.hasElementName(sty, i))
      fields.push_back(di.getElementName(sty, i));
    else
      fields.push_back("elem_" + std::to_string(i));
  }
  ctxt.add(sty, fields);
}

void LLVMParser::runOnGlobal(const GlobalVariable& g) {
  handle(g.getType());
  if(const Constant* init = g.getInitializer())
    handle(init);

  if(not di.isStringLiteral(g))
    ctxt.add(g, getName(g, "g"));
}

void LLVMParser::runOnDeclaration(const Function& f) {
  handle(f.getFunctionType());

  ctxt.add(f, getName(f));
}

void LLVMParser::runOnFunction(const Function& f) {
  handle(f.getFunctionType());

  Vector<std::string> argNames;
  for(const Argument& arg : f.args())
    if(di.hasName(arg))
      argNames.push_back(di.getName(arg));
    else if(arg.hasName())
      argNames.push_back(arg.getName());
    else
      argNames.push_back("arg_" + std::to_string(arg.getArgNo()));
  if(di.hasName(f))
    ctxt.add(f, di.getName(f), argNames);
  else
    ctxt.add(f, f.getName(), argNames);
  for(const BasicBlock& bb : f)
    handle(bb);

  // LoopInfo& li = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  // errs() << "loops: " << li.getLoopsInPreorder().size() << "\n";
  // for(const Loop* loop : li) {
  //   errs() << "Found a loop\n";
  //   // for(const BasicBlock* bb : loop->blocks())
  //   //   errs() << *bb << "\n";
  // }

  ctxt.beginFunction(f);

  for(const BasicBlock& bb : f) {
    ctxt.beginBlock(bb);
    for(const Instruction& inst : bb) {
      if(ignoreValues.contains(&inst))
        continue;

      if(const auto* alloca = dyn_cast<AllocaInst>(&inst)) {
        handle(*alloca);
      } else if(const auto* call = dyn_cast<CallInst>(&inst)) {
        handle(*call);
      } else if(const auto* invoke = dyn_cast<InvokeInst>(&inst)) {
        handle(*invoke);
      } else if(const auto* load = dyn_cast<LoadInst>(&inst)) {
        handle(*load);
      } else if(const auto* store = dyn_cast<StoreInst>(&inst)) {
        handle(*store);
      } else if(const auto* phi = dyn_cast<PHINode>(&inst)) {
        handle(*phi);
      } else if(const auto* br = dyn_cast<BranchInst>(&inst)) {
        handle(br);
      } else if(const auto* ret = dyn_cast<ReturnInst>(&inst)) {
        handle(ret);
      }
    }
    ctxt.endBlock(bb);
  }

  ctxt.endFunction(f);
}

static bool isMetadataFunction(const Function& f) {
  FunctionType* fty = f.getFunctionType();
  if(fty->getReturnType()->isMetadataTy())
    return true;
  for(Type* param : fty->params())
    if(param->isMetadataTy())
      return true;
  return false;
}

void LLVMParser::runOnModule(const Module& m) {
  // First find anything that we know are never going to be
  // converted. These would be any LLVM debug and lifetime intrinsics but
  // could be other things as well
  for(const Function& f : m.functions()) {
    if(isMetadataFunction(f)
       or f.getName().startswith("llvm.lifetime")
       or f.getName().startswith("llvm.dbg.")) {
      ignoreValues.insert(&f);
      for(const Use& u : f.uses())
        ignoreValues.insert(u.getUser());
    }
  }

  // Grow the ignore list to include anything that is only used by values in
  // the ignore list
  Set<const llvm::Value*> wl = ignoreValues;
  while(wl.size()) {
    Set<const Value*> next;
    for(const Value* v : wl)
      if(const auto* user = dyn_cast<User>(v))
        for(const Use& op : user->operands())
          if(allUsesIgnored(op.get()))
            next.insert(op.get());
    for(const Value* v : next)
      ignoreValues.insert(v);
    wl = std::move(next);
  }


  // Add all the structs to the context first so a decl exists for each of
  // them first. Then add bodies for them
  for(StructType* sty : m.getIdentifiedStructTypes())
    ctxt.add(sty, getName(sty));
  for(StructType* sty : m.getIdentifiedStructTypes())
    runOnStruct(sty);

  for(const Function& f : m.functions())
    if(not f.size() and not ignoreValues.contains(&f))
      runOnDeclaration(f);

  for(const GlobalVariable& g : m.globals())
    runOnGlobal(g);
  for(const Function& f : m.functions())
    if(f.size())
      runOnFunction(f);
}

} // namespace cish
