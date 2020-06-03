#include "Printer.h"

#include <llvm/Support/raw_ostream.h>

namespace cish {

Printer::Printer(const Context& ctxt)
    : ctxt(ctxt), ss(buf), ilevel(0), tabWidth(4) {
  ;
}

Printer& Printer::flush() {
  ss.flush();
  return *this;
}

Printer& Printer::clear() {
  buf.clear();
  return *this;
}

Printer& Printer::reposition() {
  for(unsigned i = 0; i < ilevel; i++)
    space(tabWidth);
  return *this;
}

Printer& Printer::tab() {
  ilevel += 1;
  return *this;
}

Printer& Printer::untab() {
  ilevel -= 1;
  return *this;
}

Printer& Printer::endl() {
  add("\n");
  return *this;
}

Printer& Printer::space(unsigned n) {
  for(unsigned i = 0; i < n; i++)
    add(" ");
  return *this;
}

Printer& Printer::comment(const std::string& comment) {
  add("/* ").add(comment).add(" */");
  return *this;
}

Printer& Printer::begin_func(const llvm::Function& f) {
  add(ctxt.get(f.getFunctionType()->getReturnType())).endl();
  add(ctxt.get(f)).add(" (");
  for(const llvm::Argument& arg : f.args()) {
    if(arg.getArgNo())
      ss << ", ";
    // FIXME: Correctly handle arguments passed by reference and additional
    // attributes that might have been inferred (for instance, restrict
    // and const)
    add(ctxt.get<Decl>(arg));
  }
  add(")").endl();
  begin_block();

  return *this;
}

Printer& Printer::end_func(const llvm::Function& f) {
  end_block();
  return *this;
}

Printer& Printer::begin_block(const std::string& label) {
  if(label.length())
    ss << label << ":";
  reposition().add("{").endl();
  tab();
  return *this;
}

Printer& Printer::end_block(const std::string& label) {
  untab();
  reposition().add("}");
  if(label.length())
    space().comment(label);
  endl();
  return *this;
}

Printer& Printer::label(const std::string& label) {
  return endl().add(label).add(":").endl();
}

Printer& Printer::add(const std::string& s) {
  ss << s;
  return *this;
}

Printer& Printer::add(const ASTBase& ast) {
  if(const auto* stmt = llvm::dyn_cast<Stmt>(&ast)) {
    reposition().add(stmt->str()).endl();
  } else if(const auto* iff = llvm::dyn_cast<If>(&ast)) {
    const llvm::BranchInst& br = iff->getLLVM();
    reposition().add("if (").add(ctxt.get(br.getCondition())).add(")").endl();
    tab()
        .reposition()
        .add("goto ")
        .add(ctxt.get(br.getSuccessor(0)))
        .add(";")
        .endl();
    untab().reposition().add("else").endl();
    tab()
        .reposition()
        .add("goto ")
        .add(ctxt.get(br.getSuccessor(1)))
        .add(";")
        .endl();
    untab();
  } else if(const auto* forr = llvm::dyn_cast<For>(&ast)) {
    llvm::errs() << "UNIMPLEMENTED: adding for loop\n";
  } else if(const auto* whil = llvm::dyn_cast<While>(&ast)) {
    llvm::errs() << "UNIMPLEMENTED: adding while loop\n";
  } else {
    add(ast.str());
  }

  return *this;
}

const std::string& Printer::str() {
  return ss.str();
}

const std::string& Printer::str() const {
  return buf;
}

} // namespace cish
