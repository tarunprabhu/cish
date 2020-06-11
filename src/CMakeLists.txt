set(SOURCES
  cish.cpp
  BackendBase.cpp
  CishContext.cpp
  CishFunctionPass.cpp
  CishModulePass.cpp
  ClangUtils.cpp
  # CishPreparePass.cpp
  Diagnostics.cpp
  SourceInfo.cpp
  FormatOptions.cpp
  LLVMBackend.cpp
  LLVMFrontend.cpp
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
