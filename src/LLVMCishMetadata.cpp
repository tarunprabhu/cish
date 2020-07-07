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

#include "LLVMCishMetadata.h"

using namespace llvm;

#define CISH_MD_PREFIX "cish."

#define CISH_INST_METADATA(NAME)                                             \
  bool addCishMetadata##NAME(Instruction& inst) {                            \
    if(hasCishMetadata##NAME(inst))                                          \
      return false;                                                          \
    inst.setMetadata(CISH_MD_PREFIX #NAME,                                   \
                     MDNode::get(inst.getContext(), ArrayRef<Metadata*>())); \
    return true;                                                             \
  }                                                                          \
                                                                             \
  bool addCishMetadata##NAME(Instruction* inst) {                            \
    return addCishMetadata##NAME(*inst);                                     \
  }                                                                          \
                                                                             \
  bool hasCishMetadata##NAME(const Instruction& inst) {                      \
    return inst.getMetadata(CISH_MD_PREFIX #NAME);                           \
  }                                                                          \
                                                                             \
  bool hasCishMetadata##NAME(const Instruction* inst) {                      \
    return hasCishMetadata##NAME(*inst);                                     \
  }

#define CISH_FUNC_METADATA(NAME)                   \
  bool addCishMetadata##NAME(Function& f) {        \
    if(hasCishMetadata##NAME(f))                   \
      return false;                                \
    f.addFnAttr(CISH_MD_PREFIX #NAME);             \
    return true;                                   \
  }                                                \
                                                   \
  bool addCishMetadata##NAME(Function* f) {        \
    return addCishMetadata##NAME(*f);              \
  }                                                \
                                                   \
  bool hasCishMetadata##NAME(const Function& f) {  \
    return f.hasFnAttribute(CISH_MD_PREFIX #NAME); \
  }                                                \
                                                   \
  bool hasCishMetadata##NAME(const Function* f) {  \
    return hasCishMetadata##NAME(*f);              \
  }

#define CISH_GLOBAL_METADATA(NAME)                      \
  bool addCishMetadata##NAME(GlobalVariable& g) {       \
    if(hasCishMetadata##NAME(g))                        \
      return false;                                     \
    g.addAttribute(CISH_MD_PREFIX #NAME);               \
    return true;                                        \
  }                                                     \
                                                        \
  bool addCishMetadata##NAME(GlobalVariable* g) {       \
    return addCishMetadata##NAME(*g);                   \
  }                                                     \
                                                        \
  bool hasCishMetadata##NAME(const GlobalVariable& g) { \
    return g.hasAttribute(CISH_MD_PREFIX #NAME);        \
  }                                                     \
                                                        \
  bool hasCishMetadata##NAME(const GlobalVariable* g) { \
    return hasCishMetadata##NAME(*g);                   \
  }

namespace cish {

namespace LLVM {

#include "LLVMCishMetadata.inc"

} // namespace LLVM

} // namespace cish

#undef CISH_INST_METADATA
#undef CISH_FUNC_METADATA
#undef CISH_GLOBAL_METADATA
