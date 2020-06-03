#include <llvm/Analysis/LoopInfo.h>
#include <llvm/IR/Dominators.h>
#include <llvm/Analysis/LoopPass.h>
#include <llvm/Analysis/Passes.h>
#include <llvm/IR/InstIterator.h>
#include <llvm/IR/Module.h>
#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>
#include <llvm/ADT/SmallVector.h>

#include <map>
#include <set>

#include "ASTBase.h"
#include "Context.h"
#include "Printer.h"

using namespace llvm;

class CishPass : public FunctionPass {
public:
  static char ID;

protected:
  cish::Context ctxt;

  // The values to tbe ignored when converting
  std::set<const llvm::Value*> ignore;

  // The actual C code for the current function is here. It should be
  // cleared either after the function is done or before a new function is
  // started
  cish::Printer cc;

protected:
  void initialize(const Function& f);
  bool allUsesIgnored(const Value* v) const;
  bool shouldUseTemporary(const Instruction& inst);
  const std::string& format(const cish::Expr& expr);

  const cish::ASTBase& handle(const AllocaInst& alloca);
  const cish::ASTBase& handle(const LoadInst& load);
  const cish::ASTBase& handle(const StoreInst& store);
  const cish::ASTBase& handle(const GetElementPtrInst& gep);
  const cish::ASTBase& handle(const PHINode& phi);
  const cish::ASTBase& handle(const CallInst& call);
  const cish::ASTBase& handle(const InvokeInst& invoke);
  const cish::ASTBase& handle(const CmpInst& cmp);
  const cish::ASTBase& handle(const BinaryOperator& op);
  const cish::ASTBase& handle(const UnaryOperator& op);
  const cish::ASTBase& handle(const CastInst& cst);
  const cish::ASTBase& handle(const BranchInst& br);
  const cish::ASTBase& handle(const SwitchInst& sw);
  const cish::ASTBase& handle(const SelectInst& select);
  const cish::ASTBase& handle(const ReturnInst& ret);
  const cish::ASTBase& handle(const ShuffleVectorInst& shuffle);

  const cish::ASTBase& handle(const Function& f);
  const cish::ASTBase& handle(const GlobalVariable& g);
  const cish::ASTBase& handle(const Argument& arg);

  const cish::ASTBase& handle(const ConstantInt& cint);
  const cish::ASTBase& handle(const ConstantFP& cfp);
  const cish::ASTBase& handle(const ConstantExpr& cexpr);

  const cish::ASTBase& handle(const BasicBlock& bb);

  const cish::ASTBase& handle(const Value* v);

  const cish::ASTBase& handle(IntegerType* ity);
  const cish::ASTBase& handle(PointerType* pty);
  const cish::ASTBase& handle(ArrayType* aty);
  const cish::ASTBase& handle(FunctionType* fty);
  const cish::ASTBase& handle(StructType* sty);
  const cish::ASTBase& handle(VectorType* vty);
  const cish::ASTBase& handle(Type* type);

public:
  CishPass();

  virtual StringRef getPassName() const override;
  void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual void print(raw_ostream& o, const Module* module) const override;

  virtual bool runOnFunction(Function& f) override;
};

CishPass::CishPass() : FunctionPass(ID), cc(ctxt) {
  ;
}

StringRef CishPass::getPassName() const {
  return "CISH Loop Pass";
}

void CishPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<LoopInfoWrapperPass>();
  AU.addRequired<DominatorTreeWrapperPass>();
  AU.setPreservesAll();
}

void CishPass::print(raw_ostream& o, const Module* module) const {
  o << cc.str() << "\n";
}

const std::string& CishPass::format(const cish::Expr& expr) {
  const llvm::Value& val = expr.getLLVM();
  if(ctxt.isOverwrite(expr.getLLVM()))
    return expr.str();
  else if(isa<BinaryOperator>(val) or isa<CmpInst>(val))
    return expr.getParenthetized();
  else
    return expr.str();
}

const cish::ASTBase& CishPass::handle(const Function& f) {
  FunctionType* fty = f.getFunctionType();
  handle(fty->getReturnType());
  for(Type* type : fty->params())
    handle(type);

  // FIXME: Lookup the function name in the debug info if available and use
  // that name instead
  return ctxt.add(f, f.getName().str());
}

const cish::ASTBase& CishPass::handle(const GlobalVariable& g) {
  // FIXME: Lookup the global name in the debug info if available and use
  // that name instead
  if(g.hasName()) {
    return ctxt.add(g, g.getName().str());
  } else {
    return ctxt.add(g, ctxt.getNewVar("g"));
  }
}

const cish::ASTBase& CishPass::handle(const Argument& arg) {
  // FIXME: Lookup the argument name in the debug info if available and use
  // that name instead
  std::string name;
  if(arg.hasName()) {
    name = arg.getName().str();
  } else {
    std::string buf;
    raw_string_ostream ss(buf);
    ss << "arg_" << arg.getArgNo();
    name = ss.str();
  }

  ctxt.add<cish::Decl>(arg, handle(arg.getType()), " ", name);
  return ctxt.add<cish::Expr>(arg, name);
}

const cish::ASTBase& CishPass::handle(const ConstantInt& cint) {
  const APInt& value = cint.getValue();
  if(value.isNegative())
    // Cast to int64_t so we actually get the sign because
    // value.getLimitedValue() returns a uint64_t.
    return ctxt.add(cint, (int64_t)value.getLimitedValue());
  else
    return ctxt.add(cint, value.getLimitedValue());
}

const cish::ASTBase& CishPass::handle(const ConstantFP& cfp) {
  const APFloat& value = cfp.getValueAPF();
  Type* type = cfp.getType();
  if(type->isFloatTy())
    return ctxt.add(cfp, value.convertToFloat());
  else if(type->isDoubleTy())
    return ctxt.add(cfp, value.convertToDouble());
  else
    return ctxt.add(cfp, "0xUNKNOWN_FLOAT");
}

const cish::ASTBase& CishPass::handle(const ConstantExpr& cexpr) {
  std::string op = cexpr.getOpcodeName();
  if(op == "bitcast") {
    return handle(cexpr.getOperand(0));
  } else {
    errs() << "UNHANDLED CONSTANT EXPR: " << cexpr << "\n";
    return ctxt.add(cexpr, "<<UNKNOWN>>");
  }
}

const cish::ASTBase& CishPass::handle(const AllocaInst& alloca) {
  // FIXME: Lookup the debug info to find the name of the variable allocated
  // here if any
  std::string name;
  if(alloca.hasName())
    name = alloca.getName();
  else
    name = ctxt.getNewVar("local");

  ctxt.add<cish::Stmt>(alloca, handle(alloca.getAllocatedType()), " ", name);
  return ctxt.add<cish::Expr>(alloca, name);
}

const cish::ASTBase& CishPass::handle(const CastInst& cst) {
  return ctxt.add(
      cst, "((", handle(cst.getDestTy()), ")", handle(cst.getOperand(0)), ")");
}

const cish::ASTBase& CishPass::handle(const BranchInst& br) {
  for(const llvm::BasicBlock* bb : br.successors())
    handle(bb);
  if(br.isConditional()) {
    handle(br.getCondition());
    return ctxt.add(br);
  } else {
    return ctxt.add<cish::Stmt>(br, "goto ", handle(br.getSuccessor(0)));
  }
}

const cish::ASTBase& CishPass::handle(const LoadInst& load) {
  const Value* ptr = load.getPointerOperand();
  if(isa<GetElementPtrInst>(ptr))
    // GetElementPtr will always be the address of something, so it will
    // always have '&' at its start. If we are loading from the address, it's
    // cleaner to just get rid of it since * and & will cancel each other out
    // anyway
    return ctxt.add(load, handle(ptr).str().substr(1));
  else
    return ctxt.add(load, "*", handle(ptr));
}

const cish::ASTBase& CishPass::handle(const StoreInst& store) {
  return ctxt.add<cish::Stmt>(store,
                              handle(store.getPointerOperand()),
                              " = ",
                              handle(store.getValueOperand()));
}

const cish::ASTBase& CishPass::handle(const GetElementPtrInst& gep) {
  std::string buf;
  raw_string_ostream ss(buf);
  const llvm::Value* ptr = gep.getPointerOperand();
  auto* pty = dyn_cast<PointerType>(ptr->getType());
  Type* ety = pty->getElementType();
  const std::string& s = handle(gep.getPointerOperand()).str();

  // If the pointer operand is also a GEP and is not overwritten with a
  // temporary value, it will begin with a &. Obviously, we don't want that
  if(isa<GetElementPtrInst>(ptr) and not ctxt.isOverwrite(ptr))
    ss << s.substr(1);
  else
    ss << s;
  if(ety->isIntegerTy() or ety->isFloatingPointTy() or ety->isPointerTy()) {
    for(Value* op : gep.indices())
      ss << "[" << handle(op) << "]";
  } else {
    ss << "<<GEP INDICES NOT IMPLEMENTED FOR TYPE>>";
  }
  return ctxt.add(gep, "&", ss.str());
}

const cish::ASTBase& CishPass::handle(const PHINode& phi) {
  // errs() << "UNIMPLEMENTED: " << phi << "\n";
  return ctxt.add(phi, "PHI");
}

const cish::ASTBase& CishPass::handle(const CallInst& call) {
  std::string buf;
  raw_string_ostream ss(buf);
  ss << handle(call.getCalledValue()) << "(";
  if(call.getNumArgOperands()) {
    ss << handle(call.getArgOperand(0));
    for(unsigned i = 1; i < call.getNumArgOperands(); i++)
      ss << ", " << handle(call.getArgOperand(i));
  }
  ss << ")";
  if(call.getType()->isVoidTy())
    return ctxt.add<cish::Stmt>(call, ss.str());
  else
    return ctxt.add(call, ss.str());
}

const cish::ASTBase& CishPass::handle(const InvokeInst& invoke) {
  errs() << "UNIMPLEMENTED: " << invoke << "\n";
  return ctxt.add(invoke, "");
}

const cish::ASTBase& CishPass::handle(const BinaryOperator& inst) {
  std::string op = "<<UNKNOWN_BINOP>>";

  switch(inst.getOpcode()) {
  case BinaryOperator::Add:
  case BinaryOperator::FAdd:
    op = "+";
    break;
  case BinaryOperator::Sub:
  case BinaryOperator::FSub:
    op = "-";
    break;
  case BinaryOperator::Mul:
  case BinaryOperator::FMul:
    op = "*";
    break;
  case BinaryOperator::UDiv:
  case BinaryOperator::FDiv:
  case BinaryOperator::SDiv:
    op = "/";
    break;
  case BinaryOperator::URem:
  case BinaryOperator::SRem:
  case BinaryOperator::FRem:
    op = "%";
    break;
  case BinaryOperator::Shl:
    op = "<<";
    break;
  case BinaryOperator::LShr:
    op = ">>";
    break;
  case BinaryOperator::And:
    op = "&&";
    break;
  case BinaryOperator::Or:
    op = "||";
    break;
  case BinaryOperator::Xor:
    op = "^";
    break;
  default:
    errs() << "Unknown binary operator: " << inst << "\n";
  }

  const std::string& op0 = format(cast<cish::Expr>(handle(inst.getOperand(0))));
  const std::string& op1 = format(cast<cish::Expr>(handle(inst.getOperand(1))));
  return ctxt.add(inst, op0, " ", op, " ", op1);
}

const cish::ASTBase& CishPass::handle(const UnaryOperator& inst) {
  std::string op = "<<UNKNOWN_UNOP>>";
  switch(inst.getOpcode()) {
  case UnaryOperator::FNeg:
    op = "-";
    break;
  default:
    errs() << "Unknown unary operator: " << inst << "\n";
    break;
  }

  return ctxt.add(inst, op, handle(inst.getOperand(0)));
}

const cish::ASTBase& CishPass::handle(const CmpInst& cmp) {
  std::string op = "<<UNKNOWN_CMP>>";
  switch(cmp.getPredicate()) {
  case CmpInst::FCMP_OEQ:
  case CmpInst::FCMP_UEQ:
  case CmpInst::ICMP_EQ:
    op = "==";
    break;
  case CmpInst::FCMP_ONE:
  case CmpInst::FCMP_UNE:
  case CmpInst::ICMP_NE:
    op = "!=";
    break;
  case CmpInst::FCMP_OGT:
  case CmpInst::FCMP_UGT:
  case CmpInst::ICMP_UGT:
  case CmpInst::ICMP_SGT:
    op = ">";
    break;
  case CmpInst::FCMP_OGE:
  case CmpInst::FCMP_UGE:
  case CmpInst::ICMP_UGE:
  case CmpInst::ICMP_SGE:
    op = ">=";
    break;
  case CmpInst::FCMP_OLT:
  case CmpInst::FCMP_ULT:
  case CmpInst::ICMP_ULT:
  case CmpInst::ICMP_SLT:
    op = "<";
    break;
  case CmpInst::FCMP_OLE:
  case CmpInst::FCMP_ULE:
  case CmpInst::ICMP_ULE:
  case CmpInst::ICMP_SLE:
    op = "<=";
    break;
  default:
    errs() << "Unknown compare predicate: " << cmp << "\n";
  }

  const std::string& op0 = format(cast<cish::Expr>(handle(cmp.getOperand(0))));
  const std::string& op1 = format(cast<cish::Expr>(handle(cmp.getOperand(1))));
  return ctxt.add(cmp, op0, " ", op, " ", op1);
}

const cish::ASTBase& CishPass::handle(const SelectInst& select) {
  return ctxt.add(select,
                  "(",
                  format(cast<cish::Expr>(handle(select.getCondition()))),
                  " ? ",
                  format(cast<cish::Expr>(handle(select.getTrueValue()))),
                  " : ",
                  format(cast<cish::Expr>(handle(select.getFalseValue()))),
                  ")");
}

const cish::ASTBase& CishPass::handle(const SwitchInst& sw) {
  errs() << "UNIMPLEMENTED: " << sw << "\n";
  return ctxt.add(sw, "");
}

const cish::ASTBase& CishPass::handle(const ReturnInst& ret) {
  if(Value* value = ret.getReturnValue())
    return ctxt.add<cish::Stmt>(ret, "return ", handle(value));
  else
    return ctxt.add<cish::Stmt>(ret, "return");
}

const cish::ASTBase& CishPass::handle(const BasicBlock& bb) {
  return ctxt.add(bb, ctxt.getNewVar("bb"));
}

const cish::ASTBase& CishPass::handle(const Value* v) {
  if(ctxt.has(*v))
    return ctxt.get(*v);

  if(const auto* load = dyn_cast<LoadInst>(v))
    return handle(*load);
  else if(const auto* store = dyn_cast<StoreInst>(v))
    return handle(*store);
  else if(const auto* phi = dyn_cast<PHINode>(v))
    return handle(*phi);
  else if(const auto* call = dyn_cast<CallInst>(v))
    return handle(*call);
  else if(const auto* invoke = dyn_cast<InvokeInst>(v))
    return handle(*invoke);
  else if(const auto* gep = dyn_cast<GetElementPtrInst>(v))
    return handle(*gep);
  else if(const auto* cst = dyn_cast<CastInst>(v))
    return handle(*cst);
  else if(const auto* sw = dyn_cast<SwitchInst>(v))
    return handle(*sw);
  else if(const auto* select = dyn_cast<SelectInst>(v))
    return handle(*select);
  else if(const auto* ret = dyn_cast<ReturnInst>(v))
    return handle(*ret);
  else if(const auto* cmp = dyn_cast<CmpInst>(v))
    return handle(*cmp);
  else if(const auto* alloca = dyn_cast<AllocaInst>(v))
    return handle(*alloca);
  else if(const auto* binop = dyn_cast<BinaryOperator>(v))
    return handle(*binop);
  else if(const auto* unop = dyn_cast<UnaryOperator>(v))
    return handle(*unop);
  else if(const auto* f = dyn_cast<Function>(v))
    return handle(*f);
  else if(const auto* g = dyn_cast<GlobalVariable>(v))
    return handle(*g);
  else if(const auto* cint = dyn_cast<ConstantInt>(v))
    return handle(*cint);
  else if(const auto* cfp = dyn_cast<ConstantFP>(v))
    return handle(*cfp);
  else if(const auto* cexpr = dyn_cast<ConstantExpr>(v))
    return handle(*cexpr);
  else if(const auto* bb = dyn_cast<BasicBlock>(v))
    return handle(*bb);
  else
    errs() << "UNHANDLED: " << *v << "\n";

  return ctxt.add(*v, "");
}

const cish::ASTBase& CishPass::handle(IntegerType* ity) {
  switch(ity->getBitWidth()) {
  case 1:
    return ctxt.add(ity, "bool");
  case 8:
    return ctxt.add(ity, "char");
  case 16:
    return ctxt.add(ity, "short");
  case 32:
    return ctxt.add(ity, "int");
  case 64:
    return ctxt.add(ity, "long");
  case 128:
    return ctxt.add(ity, "__int128_t");
  default:
    errs() << "Unexpected integer type: " << *ity << "\n";
  }

  std::string buf;
  raw_string_ostream ss(buf);
  ss << "int" << ity->getBitWidth() << "_t";
  return ctxt.add(ity, ss.str());
}

const cish::ASTBase& CishPass::handle(PointerType* pty) {
  std::string buf;
  raw_string_ostream ss(buf);
  ss << handle(pty->getElementType()) << "*";

  return ctxt.add(pty, ss.str());
}

const cish::ASTBase& CishPass::handle(ArrayType* aty) {
  // Arrays are problematic in C/C++ because the size of the array
  // ends up being attached to a variable. So this will return something
  // that looks C-ish, but definitely isn't valid C
  std::string buf;
  raw_string_ostream ss(buf);
  ss << handle(aty->getElementType()) << "[" << aty->getNumElements() << "]";

  return ctxt.add(aty, ss.str());
}

const cish::ASTBase& CishPass::handle(StructType* sty) {
  if(sty->hasName()) {
    // FIXME: If the struct can be associated with a user-defined type using
    // debug info, that user name should be returned instead
    std::string buf;
    raw_string_ostream ss(buf);
    StringRef sname = sty->getName();
    if(sname.find("struct.") == 0) {
      ss << "struct " << sname.substr(7);
    } else if(sname.find("class.") == 0) {
      ss << sname.substr(6);
    } else if(sname.find("union")) {
      ss << "union " << sname.substr(6);
    } else {
      ss << sname;
    }
    ss.flush();
    for(size_t i = 0; i < buf.length(); i++)
      if(buf[i] == '.')
        buf[i] = '_';
    return ctxt.add(sty, buf);
  } else {
    return ctxt.add(sty, ctxt.getNewVar("struct"));
  }
}

const cish::ASTBase& CishPass::handle(FunctionType* fty) {
  // Function types are probably even more of a headache in C and there
  // probably is a way of getting a a reasonable type without resorting to
  // typedefs but I don't know it. So just return something that looks more
  // like the LLVM string representation of a function type
  std::string buf;
  raw_string_ostream ss(buf);
  ss << handle(fty->getReturnType()) << "(";
  if(fty->getNumParams()) {
    ss << handle(fty->getParamType(0));
    for(unsigned i = 1; i < fty->getNumParams(); i++)
      ss << ", " << handle(fty->getParamType(i));
  }
  ss << ")";

  return ctxt.add(fty, ss.str());
}

const cish::ASTBase& CishPass::handle(VectorType* vty) {
  std::string buf;
  raw_string_ostream ss(buf);
  ss << handle(vty->getElementType()) << "<" << vty->getNumElements() << ">";

  return ctxt.add(vty, ss.str());
}

const cish::ASTBase& CishPass::handle(Type* type) {
  if(ctxt.has(type))
    return ctxt.get(type);

  std::string buf;
  raw_string_ostream ss(buf);
  if(auto* ity = dyn_cast<IntegerType>(type)) {
    return handle(ity);
  } else if(type->isVoidTy()) {
    return ctxt.add(type, "void");
  } else if(type->isFloatTy()) {
    return ctxt.add(type, "float");
  } else if(type->isDoubleTy()) {
    return ctxt.add(type, "double");
  } else if(type->isX86_FP80Ty()) {
    return ctxt.add(type, "long double");
  } else if(type->isFP128Ty()) {
    return ctxt.add(type, "long double");
  } else if(auto* pty = dyn_cast<PointerType>(type)) {
    return handle(pty);
  } else if(auto* aty = dyn_cast<ArrayType>(type)) {
    return handle(aty);
  } else if(auto* sty = dyn_cast<StructType>(type)) {
    return handle(sty);
  } else if(auto* fty = dyn_cast<FunctionType>(type)) {
    return handle(fty);
  } else if(auto* vty = dyn_cast<VectorType>(type)){
    return handle(vty);
  } else {
    errs() << "Unknown type: " << *type << "\n";
  }

  return ctxt.add(type, "UNKNOWN_TYPE");
}

void CishPass::initialize(const Function& f) {
  cc.clear();

  std::set<const llvm::Value*> wl;

  // In the preprocessing step, tag anything that we know are never going to be
  // converted. These would be any LLVM debug and lifetime intrinsics but
  // could be other things as well
  for(const Instruction& inst : instructions(f))
    if(const auto* call = dyn_cast<CallInst>(&inst))
      if(const Function* callee = call->getCalledFunction())
        if((callee->getName().find("llvm.dbg") == 0)
           or (callee->getName().find("llvm.lifetime") == 0)) {
          ignore.insert(call);
          wl.insert(call);
        }

  // Grow the ignore list to include anything that is only used by values in
  // the ignore list
  while(wl.size()) {
    std::set<const Value*> next;
    for(const Value* v : wl)
      if(const auto* user = dyn_cast<User>(v))
        for(const Use& op : user->operands())
          if(allUsesIgnored(op.get()))
            next.insert(op.get());
    wl.clear();
    for(const Value* v : next) {
      ignore.insert(v);
      wl.insert(v);
    }
  }
}

bool CishPass::allUsesIgnored(const Value* v) const {
  if(v->getNumUses() == 0)
    return false;
  for(const Use& u : v->uses())
    if(ignore.find(u.getUser()) == ignore.end())
      return false;
  return true;
}

bool CishPass::shouldUseTemporary(const Instruction& inst) {
  // If there is zero or more than one use of the instruction, then create a
  // temporary variable for it. This is particularly important in the case
  // of function calls because if we don't do it this way and the result of
  // a call is used more than once, the call will appear to be made multiple
  // times, and if it is never used, then it will appear as if the call is
  // never made. But if we always use a temporary, the result will never
  // look remotely reasonable and we might as well just read LLVM.
  size_t uses = inst.getNumUses();
  // FIXME: Use llvm::CallBase for newer versions of LLVM
  if((isa<CallInst>(inst) or isa<InvokeInst>(inst))
          and (not inst.getType()->isVoidTy())
          and ((uses == 0) or (uses > 1)))
    return true;
  else if(isa<GetElementPtrInst>(inst))
    return false;
  else if(not isa<AllocaInst>(inst) and (uses > 1))
    return true;
  return false;
}

bool CishPass::runOnFunction(Function& f) {
  initialize(f);

  handle(f);
  for(const Argument& arg : f.args())
    handle(arg);

  // LoopInfo& li = getAnalysis<LoopInfoWrapperPass>().getLoopInfo();
  // errs() << "loops: " << li.getLoopsInPreorder().size() << "\n";
  // for(const Loop* loop : li) {
  //   errs() << "Found a loop\n";
  //   // for(const BasicBlock* bb : loop->blocks())
  //   //   errs() << *bb << "\n";
  // }

  cc.begin_func(f);
  for(const BasicBlock& bb : f) {
    // Create a label for this basic block only if it is not the entry block
    // of the function. Unreachable basic blocks will probably only happen
    // in broken code, but that doesn't really matter
    if(not bb.hasNPredecessors(0))
      // The basic block may have been encountered before because it may
      // have appeared in a branch instruction that was seen earlier. In that
      // case, call handle() with BasicBlock* because that will check if
      // the basic block has already been converted
      cc.label(handle(&bb));
    for(const Instruction& inst : bb) {
      if(ignore.find(&inst) != ignore.end())
        continue;

      if(const auto* alloca = dyn_cast<AllocaInst>(&inst)) {
        handle(*alloca);
        cc.add(ctxt.get<cish::Stmt>(alloca));
      } else if(const auto* call = dyn_cast<CallInst>(&inst)) {
        handle(*call);
        if(call->getType()->isVoidTy())
          cc.add(ctxt.get<cish::Stmt>(call));
      } else if(const auto* invoke = dyn_cast<InvokeInst>(&inst)) {
        handle(*invoke);
      } else if(const auto* load = dyn_cast<LoadInst>(&inst)) {
        handle(*load);
      } else if(const auto* store = dyn_cast<StoreInst>(&inst)) {
        cc.add(handle(*store));
      } else if(const auto* br = dyn_cast<BranchInst>(&inst)) {
        cc.add(handle(*br));
      } else if(const auto* ret = dyn_cast<ReturnInst>(&inst)) {
        cc.add(handle(ret));
      }

      if(shouldUseTemporary(inst)) {
        std::string varName = ctxt.getNewVar();
        cc.reposition()
            .add(handle(inst.getType()))
            .space()
            .add(varName)
            .add(" = ")
            .add(handle(&inst))
            .add(";")
            .endl();
        ctxt.overwrite(inst, varName);
      }
    }
  }
  cc.end_func(f);
  cc.flush();

  return false;
}

char CishPass::ID = 0;

static RegisterPass<CishPass> X("cish", "Cish Pass", true, true);
