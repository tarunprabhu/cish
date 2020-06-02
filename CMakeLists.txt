cmake_minimum_required(VERSION 3.13)
project(cish)
enable_language(CXX)

find_package(LLVM CONFIG REQUIRED)

message(STATUS "Got LLVM: ${LLVM_PACKAGE_VERSION}")
message(STATUS "LLVM definitions: ${LLVM_DEFINITIONS}")
message(STATUS "LLVM include dir: ${LLVM_INCLUDE_DIR}")

add_definitions(${LLVM_DEFINITIONS})
add_compile_options(-Wall -Wextra -Wno-unused-parameter)
include_directories(${LLVM_INCLUDE_DIR})

add_subdirectory(src)
