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

#include "ASTStreamer.h"
#include "CishContext.h"
#include "Diagnostics.h"
#include "Options.h"
#include "Vector.h"

#include <llvm/Pass.h>
#include <llvm/Support/raw_ostream.h>

using namespace llvm;

class CishASTWriterPass : public ModulePass {
public:
  static char ID;

private:
  cish::CishContext& cishContext;
  std::string outFile;

private:
  std::string run() {
    clang::ASTContext& astContext = cishContext.getASTContext();
    cish::ASTStreamer stream(astContext);

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

    return stream.str();
  }

public:
  explicit CishASTWriterPass(cish::CishContext& cishContext)
      : ModulePass(ID), cishContext(cishContext),
        outFile(cish::opts().fileOut) {
    ;
  }

  virtual StringRef getPassName() const override {
    return "Cish AST Writer Pass";
  }

  virtual void getAnalysisUsage(AnalysisUsage& AU) const override {
    AU.setPreservesAll();
  }

  virtual bool runOnModule(Module&) override {
    if(outFile == "-") {
      outs() << run() << "\n";
    } else {
      std::error_code ec;
      raw_fd_ostream fs(outFile, ec);
      if(ec) {
        cish::fatal(cish::error() << ec.message());
      } else {
        fs << run();
        fs.close();
      }
    }

    return false;
  }
};

char CishASTWriterPass::ID = 0;

Pass* createCishASTWriterPass(cish::CishContext& cishContext) {
  return new CishASTWriterPass(cishContext);
}
