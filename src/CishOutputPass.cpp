#include "CishContext.h"
#include "Diagnostics.h"
#include "ASTStreamer.h"
#include "Vector.h"

#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

class CishOutputPass : public ModulePass {
public:
  static char ID;

private:
  std::string outFile;

private:
  void run(clang::ASTContext& astContext, raw_ostream& os);

public:
  explicit CishOutputPass(const std::string& outFile);

  virtual StringRef getPassName() const override;
  virtual void getAnalysisUsage(AnalysisUsage& AU) const override;
  virtual bool runOnModule(Module& m) override;
};

CishOutputPass::CishOutputPass(const std::string& outFile)
    : ModulePass(ID), outFile(outFile) {
  ;
}

StringRef CishOutputPass::getPassName() const {
  return "Cish Printer Pass";
}

void CishOutputPass::getAnalysisUsage(AnalysisUsage& AU) const {
  AU.addRequired<CishContextWrapperPass>();
  AU.setPreservesAll();
}

void CishOutputPass::run(clang::ASTContext& astContext, raw_ostream& os) {
  cish::ASTStreamer stream(astContext, os);

  cish::Vector<const clang::RecordDecl*> structs;
  cish::Vector<const clang::VarDecl*> globals;
  cish::Vector<const clang::FunctionDecl*> decls;
  cish::Vector<const clang::FunctionDecl*> funcs;

  for(clang::Decl* decl : astContext.getTranslationUnitDecl()->decls())
    if(const auto* record = dyn_cast<clang::RecordDecl>(decl))
      structs.push_back(record);
    else if(const auto* global = dyn_cast<clang::VarDecl>(decl))
      globals.push_back(global);
    else if(const auto* f = dyn_cast<clang::FunctionDecl>(decl))
      if(f->hasBody())
        funcs.push_back(f);
      else
        decls.push_back(f);
    else
      cish::error() << "Unexpected decl found: " << decl->getDeclKindName()
                    << "\n";

  // TODO: Could consider creating a definition order for structs so that
  // structs are defined or forward declared before use because that would be
  // nice to have in a C-ish output
  for(const clang::RecordDecl* record : structs)
    stream << record << stream.endl();
  if(structs.size())
    stream.endl();

  // Function declarations
  for(const clang::FunctionDecl* f : decls)
    stream << f << stream.endl();
  if(decls.size())
    stream.endl();

  // Global variables
  for(const clang::VarDecl* g : globals)
    stream << g << stream.endl();
  if(globals.size())
    stream.endl();

  // Function definitions
  for(const clang::FunctionDecl* f : funcs)
    stream << f << stream.endl() << stream.endl();
}

bool CishOutputPass::runOnModule(Module& m) {
  const cish::CishContext& context
      = getAnalysis<CishContextWrapperPass>().getCishContext();
  clang::ASTContext& astContext = context.getASTContext();

  if(outFile == "-") {
    run(astContext, outs());
  } else {
    std::error_code ec;
    raw_fd_ostream fs(outFile, ec);
    if(ec) {
      cish::fatal(cish::error() << ec.message());
    } else {
      run(astContext, fs);
      fs.close();
    }
  }

  return false;
}

char CishOutputPass::ID = 0;

Pass* createCishOutputPass(const std::string& outFile) {
  return new CishOutputPass(outFile);
}
