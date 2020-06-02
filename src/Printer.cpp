#include "Printer.h"

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
  add("}");
  if(label.length())
    space().comment(label);
  return *this;
}

Printer& Printer::add(const std::string& s) {
  ss << s;
  return *this;
}

Printer& Printer::add(const ASTBase& ast) {
  if(const auto* stmt = llvm::dyn_cast<Stmt>(&ast))
    return reposition().add(stmt->str()).endl();
  else
    return add(ast.str());
}

const std::string& Printer::str() {
  return ss.str();
}

const std::string& Printer::str() const {
  return buf;
}

} // namespace cish
