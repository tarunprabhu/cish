set(SOURCES
  cish.cpp
  CishContext.cpp
  # CishPreparePass.cpp
  DIParser.cpp
  LLVMParser.cpp
  LLVMUtils.cpp
  Printer.cpp
  Stream.cpp)

set(CISH cish)
add_executable(${CISH} ${SOURCES})
target_link_directories(${CISH} PUBLIC ${LLVM_LIBRARY_DIR} ${Clang_DIR})
target_link_libraries(${CISH} clangAST clangBasic LLVM)
