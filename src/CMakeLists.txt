set(SOURCES
  ASTBase.cpp
  Context.cpp
  CishPreparePass.cpp
  CishPass.cpp
  Decl.cpp
  Expr.cpp
  For.cpp
  If.cpp
  LLVMUtils.cpp
  Printer.cpp
  Stmt.cpp
  Type.cpp
  While.cpp)

set(CISH_LIBNAME Cish)

# Don't need to link any of the LLVM libraries because this is meant to be
# used with LLVM's opt tool and that will have all the libraries linked
# Linking LLVM here anyway will result in duplicate definition errors
# from LLVM's commandline libraries which has a lot of global static stuff
# in header files
add_library(${CISH_LIBNAME} SHARED ${SOURCES})
