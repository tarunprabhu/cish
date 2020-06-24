set(SOURCES
  ASTBuilder.cpp
  ASTDeadCodeEliminationPass.cpp
  ASTDefUseInfo.cpp
  ASTExprPass.cpp
  ASTSimplifyOperatorsPass.cpp
  ASTStreamer.cpp
  ASTStripCastsPass.cpp
  cish.cpp
  BackendBase.cpp
  CishASTSimplifyPass.cpp
  CishContext.cpp
  CishFunctionPass.cpp
  CishModulePass.cpp
  CishOutputPass.cpp
  CishPreparePass.cpp
  ClangUtils.cpp
  Diagnostics.cpp
  LLVMBackend.cpp
  LLVMFrontend.cpp
  LLVMUtils.cpp
  Options.cpp
  SourceInfo.cpp
  Structure.cpp
  StructureAnalysis.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic clangAnalysis LLVM)
set_target_properties(${CISH}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_PROJECT_BINDIR})
