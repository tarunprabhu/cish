#include "CishLLVMContext.h"

namespace cish {

CishLLVMContext::CishLLVMContext(const llvm::Module& m)
    : CishContext(m.getTargetTriple()), llvmContext(m.getContext()) {
  irClangMap.reset(new LLVMClangMap);
  si.reset(new LLVMSourceInfo(m));
  be.reset(new LLVMBackend(*this));
}

NameGenerator& CishLLVMContext::addNameGenerator(const llvm::Function& f) {
  return CishContext::addNameGenerator(f.getName());
}

NameGenerator&
CishLLVMContext::getNameGenerator(const llvm::Function& f) const {
  return CishContext::getNameGenerator(f.getName());
}

llvm::LLVMContext& CishLLVMContext::getLLVMContext() const {
  return llvmContext;
}

LLVMBackend& CishLLVMContext::getLLVMBackend() const {
  return *be;
}

LLVMClangMap& CishLLVMContext::getLLVMClangMap() const {
  return *static_cast<LLVMClangMap*>(irClangMap.get());
}

const LLVMSourceInfo& CishLLVMContext::getLLVMSourceInfo() const {
  return *si;
}

} // namespace cish
