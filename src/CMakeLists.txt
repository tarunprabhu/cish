set(SOURCES
  cish.cpp
  AST.cpp
  ASTConstantFoldingPass.cpp
  ASTConvertLoopsPass.cpp
  ASTDeadCodeEliminationPass.cpp
  ASTExprPropagationPass.cpp
  ASTPass.cpp
  ASTPrivatizeVarsPass.cpp
  ASTRenameVarsPass.cpp
  ASTSimplifyOperatorsPass.cpp
  ASTStreamer.cpp
  ASTStripCastsPass.cpp
  ASTSubexprEliminationPass.cpp
  BackendBase.cpp
  CishContext.cpp
  CishASTPassesDriverPass.cpp
  CishASTWriterPass.cpp
  CishLLVMContext.cpp
  CishLLVMFunctionConvertPass.cpp
  CishLLVMModuleConvertPass.cpp
  ClangUtils.cpp
  Diagnostics.cpp
  ExprNumberMap.cpp
  IRClangMap.cpp
  LLVMBackend.cpp
  LLVMCishMetadata.cpp
  LLVMClangMap.cpp
  LLVMPrepareFunctionPass.cpp
  LLVMPrepareModulePass.cpp
  LLVMSourceInfo.cpp
  LLVMStructureAnalysis.cpp
  LLVMUtils.cpp
  Logging.cpp
  NameGenerator.cpp
  Operators.cpp
  Options.cpp
  ParentMap.cpp
  StructureTree.cpp
  StructureAnalysis.cpp
  UsesMap.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_options(${CISH} PUBLIC -rdynamic)
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic clangAnalysis LLVM)
set_target_properties(${CISH}
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY ${CMAKE_PROJECT_BINDIR})
