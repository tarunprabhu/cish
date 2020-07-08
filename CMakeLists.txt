cmake_minimum_required(VERSION 3.13)
project(cish)
enable_language(CXX)

find_package(LLVM CONFIG REQUIRED)
find_package(Clang CONFIG REQUIRED)

message(STATUS "Got LLVM: ${LLVM_PACKAGE_VERSION}")
message(STATUS "Include dirs: " ${LLVM_INCLUDE_DIR} ${CLANG_INCLUDE_DIRS})
message(STATUS "Library dirs: " ${LLVM_LIBRARY_DIR} ${Clang_DIR})

add_definitions(${LLVM_DEFINITIONS} ${CLANG_DEFINITIONS})
add_compile_options(-std=c++17 -Wall -Wextra)

# Not sure if this is a problem with Flang's LLVM or LLVM/Clang more generally
# but it produces an enormous number of warnings when compiling with -Wall
# and -Wextra. So treat all LLVM/Clang headers as system headers
# even if they are included from a non-system directory. This causes GCC/Clang
# to suppress any warnings that they generate
add_compile_options(-isystem ${LLVM_INCLUDE_DIR} -isystem ${CLANG_INCLUDE_DIRS})

set(CMAKE_BUILD_RPATH ${LLVM_LIBRARY_DIR} ${Clang_DIR})
set(CMAKE_INSTALL_RPATH ${LLVM_LIBRARY_DIR} ${Clang_DIR})

set(CMAKE_PROJECT_BINDIR "${PROJECT_BINARY_DIR}/bin")

add_subdirectory(drivers)
add_subdirectory(src)
