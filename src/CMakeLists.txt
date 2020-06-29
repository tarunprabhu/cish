set(SOURCES
  cish.cpp
  AST.cpp
  ASTBuilder.cpp
  ASTConstantFoldingPass.cpp
  ASTDeadCodeEliminationPass.cpp
  ASTDefUseCalculatorPass.cpp
  ASTFunctionPass.cpp
  ASTPropagateExprsPass.cpp
  ASTRenameVarsPass.cpp
  ASTSimplifyLoopsPass.cpp
  ASTSimplifyOperatorsPass.cpp
  ASTStreamer.cpp
  ASTStripCastsPass.cpp
  BackendBase.cpp
  CishContext.cpp
  CishASTPassesDriverPass.cpp
  CishASTWriterPass.cpp
  CishFunctionConvertPass.cpp
  CishModuleConvertPass.cpp
  ClangUtils.cpp
  Diagnostics.cpp
  LLVMBackend.cpp
  LLVMFrontend.cpp
  LLVMUtils.cpp
  IRPreparePass.cpp
  IRSourceInfo.cpp
  Options.cpp
  Structure.cpp
  StructureAnalysis.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic clangAnalysis LLVM)
set_target_properties(${CISH}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_PROJECT_BINDIR})
