#include "CishContext.h"
#include "Diagnostics.h"
#include "LLVMBackend.h"
#include "LLVMFrontend.h"
#include "Options.h"
#include "SourceInfo.h"

using namespace llvm;

class CishModulePass : public ModulePass {
public:
  static char ID;

public:
  CishModulePass();

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnModule(Module& m) override;
};

CishModulePass::CishModulePass() : ModulePass(ID) {
  ;
}

StringRef CishModulePass::getPassName() const {
  return "Cish Module Pass";
}

void CishModulePass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<SourceInfoWrapperPass>();
  AU.addRequired<CishContextWrapperPass>();
  AU.setPreservesAll();
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

bool CishModulePass::runOnModule(Module& m) {
  cish::message() << "Running " << getPassName() << "\n";

  const cish::SourceInfo& si
      = getAnalysis<SourceInfoWrapperPass>().getSourceInfo();
  const cish::CishContext& context
      = getAnalysis<CishContextWrapperPass>().getCishContext();
  cish::LLVMFrontend& fe = context.getLLVMFrontend();
  cish::LLVMBackend& be = context.getLLVMBackend();

  if(cish::opts().log) {
    std::string buf;
    raw_string_ostream filename(buf);
    if(cish::opts().logDir.size())
      filename << cish::opts().logDir << "/";
    std::string file = m.getSourceFileName();
    size_t start = file.rfind('/');
    size_t end = file.rfind('.');
    if(start == std::string::npos)
      start = 0;
    filename << file.substr(start, end - start) << ".prepared.ll";
    std::error_code ec;
    raw_fd_ostream fs(filename.str(), ec);
    if(not ec) {
      fs << m << "\n";
      fs.close();
      cish::message() << "Wrote prepare LLVM IR to " << filename.str() << "\n";
    } else {
      cish::warning() << "Could not write to log file " << filename.str()
                      << "\n";
    }
  }

  // First find anything that we know are never going to be
  // converted. These would be any LLVM debug and lifetime intrinsics but
  // could be other things as well
  for(const Function& f : m.functions()) {
    if(isMetadataFunction(f) or f.getName().startswith("llvm.lifetime")
       or f.getName().startswith("llvm.dbg.")) {
      fe.addIgnoreValue(&f);
      for(const Use& u : f.uses())
        fe.addIgnoreValue(u.getUser());
    }
  }

  // Add all the structs to the context first so a decl exists for each of
  // them first because there may be recursive structs
  for(StructType* sty : m.getIdentifiedStructTypes())
    be.add(sty, fe.getName(sty));

  // Then add bodies for them
  for(StructType* sty : m.getIdentifiedStructTypes()) {
    cish::Vector<std::string> fields;
    for(unsigned i = 0; i < sty->getNumElements(); i++) {
      Type* ety = sty->getElementType(i);
      if(not isa<StructType>(ety))
        fe.handle(sty->getElementType(i));
      if(si.hasElementName(sty, i))
        fields.push_back(si.getElementName(sty, i));
      else
        fields.push_back("elem_" + std::to_string(i));
    }
    be.add(sty, fields);
  }

  for(const GlobalVariable& g : m.globals())
    fe.handle(&g);

  for(const Function& f : m.functions())
    if(not isMetadataFunction(f))
      fe.handle(&f);

  for(const GlobalAlias& alias : m.aliases()) {
    cish::fatal(cish::error() << "NOT IMPLEMENTED: " << alias);
  }

  return false;
}

char CishModulePass::ID = 0;

static RegisterPass<CishModulePass>
    X("cish-module", "Cish Module Pass", true, true);

Pass* createCishModulePass() {
  return new CishModulePass();
}
