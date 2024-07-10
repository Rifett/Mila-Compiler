cmake_minimum_required(VERSION 3.10)

# Set the project name
project(MilaCompiler)

# Specify the C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED True)

# Add the source files
set(SOURCES
    src/main.cpp
    src/Lexer.cpp
    src/Parser.cpp
)

# Add the header files
set(HEADERS
    src/Lexer.hpp
    src/Parser.hpp
)

# Add the executable
add_executable(mila ${SOURCES} ${HEADERS})

# Link with LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Set LLVM components
set(LLVM_COMPONENTS
    core
    support
)

llvm_map_components_to_libnames(llvm_libs ${LLVM_COMPONENTS})
target_link_libraries(mila ${llvm_libs})

