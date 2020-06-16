set(SOURCES
  cish.cpp
  BackendBase.cpp
  CishContext.cpp
  CishFunctionPass.cpp
  CishModulePass.cpp
  CishCmdLineFlags.cpp
  CishOutputPass.cpp
  CishPreparePass.cpp
  ClangUtils.cpp
  Diagnostics.cpp
  FormatOptions.cpp
  LLVMBackend.cpp
  LLVMFrontend.cpp
  LLVMUtils.cpp
  SourceInfo.cpp
  Stream.cpp
  StructureAnalysis.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic LLVM)
set_target_properties(${CISH}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_PROJECT_BINDIR})
