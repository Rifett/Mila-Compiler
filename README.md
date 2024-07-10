# Mila Language LLVM Frontend

This project is an extension of the LLVM compiler suite, providing a frontend for the Mila programming language, which is inspired by Pascal. The compiler translates Mila source code into LLVM Intermediate Representation (IR), which then can be executed by `lli` command (more in Usage section).

Features

    Constant and Variable Declarations (integer, array and string datatypes)

    Function Deffinitions, Declarations and Calls

    Procedure Deffinitions

    Arithmetic and Logic Expressions
        +, -, *, div, mod, dec, and, or, xor, not

    Parentheses for grouping

    Assignment

    Number Constants in Hexadecimal and Octal System

    Blocks Nesting

    Control Flow Statements
        if, while, for, exit
        
    Input/Output Statements
        readln, write, writeln using scanf and printf
        
    Recursion (Direct & Indirect)

## Installation

### Prerequisites
    LLVM
    C++17 or later
    CMake

### Steps

1.) Clone the repository
```bash
git clone https://github.com/Rifett/Mila-Compiler.git
cd Mila-Compiler
```
2.) Build the project
```bash
mkdir build
cd build
cmake ..
make
```

## Usage

Given some sample Mila code: example.mila.

### Compile Mila Source Code
```bash
./mila < example.mila -o example.ll
```

### Run the LLVM IR
```bash
lli example.ll
```

Mila language code examples can be found in "samples" directory of this repository.























