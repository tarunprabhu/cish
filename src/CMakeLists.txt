set(SOURCES
  cish.cpp
  AST.cpp
  ASTBuilder.cpp
  ASTConstantFoldingPass.cpp
  ASTConvertLoopsPass.cpp
  ASTDeadCodeEliminationPass.cpp
  ASTDefUseCalculatorPass.cpp
  ASTExprNumberingPass.cpp
  ASTExprPropagationPass.cpp
  ASTFunctionPass.cpp
  ASTRenameVarsPass.cpp
  ASTSimplifyOperatorsPass.cpp
  ASTStreamer.cpp
  ASTStripCastsPass.cpp
  ASTSubexprEliminationPass.cpp
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
  NameGenerator.cpp
  Logging.cpp
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
