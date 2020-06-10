set(SOURCES
  cish.cpp
  BackendBase.cpp
  CishContext.cpp
  ClangUtils.cpp
  # CishPreparePass.cpp
  Diagnostics.cpp
  DIParser.cpp
  FormatOptions.cpp
  LLVMBackend.cpp
  LLVMParser.cpp
  LLVMUtils.cpp
  Printer.cpp
  Stream.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic LLVM)
set_target_properties(${CISH}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_PROJECT_BINDIR})
