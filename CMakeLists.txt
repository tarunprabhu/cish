cmake_minimum_required(VERSION 3.13)
project(cish)
enable_language(CXX)

find_package(LLVM CONFIG REQUIRED)
find_package(Clang CONFIG REQUIRED)

message(STATUS "Got LLVM: ${LLVM_PACKAGE_VERSION}")
message(STATUS "Include dirs: " ${LLVM_INCLUDE_DIR} ${CLANG_INCLUDE_DIRS})
message(STATUS "Library dirs: " ${LLVM_LIBRARY_DIR} ${Clang_DIR})

add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})
add_compile_options(-Wall -Wextra -Wno-unused-parameter)
include_directories(${LLVM_INCLUDE_DIR} ${CLANG_INCLUDE_DIRS})
set(CMAKE_BUILD_RPATH ${LLVM_LIBRARY_DIR} ${Clang_DIR})
set(CMAKE_INSTALL_RPATH ${LLVM_LIBRARY_DIR} ${Clang_DIR})

add_subdirectory(src)
