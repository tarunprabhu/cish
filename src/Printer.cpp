#include "Printer.h"
#include "Stream.h"
#include "Vector.h"

#include <clang/AST/ASTConsumer.h>
#include <clang/AST/RecursiveASTVisitor.h>

#include <llvm/Support/WithColor.h>
#include <llvm/Support/raw_ostream.h>

using namespace clang;

namespace cish {

Printer::Printer(clang::ASTContext& astContext, const FormatOptions& fmtOpts)
    : astContext(astContext), fmtOpts(fmtOpts) {
  ;
}

void Printer::run(llvm::raw_ostream& os) {
  Stream stream(astContext, fmtOpts, os);

  const TranslationUnitDecl& tu = *astContext.getTranslationUnitDecl();
  Vector<const RecordDecl*> structs;
  Vector<const VarDecl*> globals;
  Vector<const FunctionDecl*> decls;
  Vector<const FunctionDecl*> funcs;

  for(Decl* decl : tu.decls())
    if(const auto* record = dyn_cast<RecordDecl>(decl))
      structs.push_back(record);
    else if(const auto* global = dyn_cast<VarDecl>(decl))
      globals.push_back(global);
    else if(const auto* f = dyn_cast<FunctionDecl>(decl))
      if(f->hasBody())
        funcs.push_back(f);
      else
        decls.push_back(f);
    else
      llvm::WithColor::error(llvm::errs()) << "Unexpected decl found: "
                                           << decl->getDeclKindName() << "\n";

  // TODO: Could consider creating a definition order for structs so that
  // structs are defined or forward declared before use because that would be
  // nice to have in a C-ish output
  for(const RecordDecl* record : structs)
    stream << record << stream.endl();
  if(structs.size())
    stream.endl();

  // Function declarations
  for(const FunctionDecl* f : decls)
    stream << f << stream.endl();
  if(decls.size())
    stream.endl();

  // Global variables
  for(const VarDecl* g : globals)
    stream << g << stream.endl();
  if(globals.size())
    stream.endl();

  // Function definitions
  for(const FunctionDecl* f : funcs)
    stream << f << stream.endl();
}

} // namespace cish
