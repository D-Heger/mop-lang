# My Own Programming Language

[![Changelog][changelog-badge]][changelog]
[![License][license-badge]][license]
[![Syntax][syntax-badge]][syntax]
[![Semantics][semantics-badge]][semantics]

<!-- Files -->
[changelog]: ./CHANGELOG.md
[license]: ./LICENSE
[syntax]: ./documentation/grammar.ebnf
[semantics]: ./documentation/SEMANTICS.md
<!-- Badges -->
[changelog-badge]: https://img.shields.io/badge/changelog-0.0.1-blue.svg
[license-badge]: https://img.shields.io/badge/license-DWYW--WC-green.svg
[syntax-badge]: https://img.shields.io/badge/syntax-EBNF-orange.svg
[semantics-badge]: https://img.shields.io/badge/semantics-SEMANTICS.md-yellow.svg

**MOPLang or MOPL for short.**

- Status: In Development
- Version: 0.0.2                                <!-- DON'T FORGET TO UPDATE THIS BEFORE NEW RELEASE -->
- Last Update: 2025-08-22 (YYYY-MM-DD)

The Version as well as update date stated here are also applicable to the [Syntax][syntax] and the [Semantics][semantics].

Definitions:

- CCS: Completes Current Spec
- VLSC: Version since Last Spec Completion

**Interpreters:**

| Language | Status | CCS? | VLSC  |
| -------- | ------ | ---- | ----- |
| Python   | Stable | Yes  | 0.0.2 |
| Go       | Stable | Yes  | 0.0.2 |

**Compilers:**

| Language    | Status      | CCS?        | VLSC        |
| ----------- | ----------- | ----------- | ----------- |
| Not Started | Not Started | Not Started | Not Started |

## Goals

Simple Language to learn to create Language Design, Interpreters and Compilers with. An eventual goal would also be creating interesting, fun programs with MOPlang.

## How Programming Languages Work

Programming languages serve as a bridge between human-readable instructions and machine-executable code. Understanding how they work provides valuable context in our day to day as software engineers.

### Programming Languages Overview

Programming languages, at their core, are remarkably similar to natural languages. They're essentially formal systems built upon an alphabet (the symbols you can use), a syntax (the rules for arranging those symbols), and semantics (the meaning behind those arrangements). The key distinction lies in their purpose: while human languages facilitate communication among people, programming languages enable humans to communicate instructions to / communicate with machines.

### The Translation Problem

Computers only understand machine code. Which is nothing more than sequences of binary numbers that represent specific processor instructions. However, writing directly in machine code is extremely tedious and error-prone for humans. (Almost) No one wants to endlessly type 0 and 1 a gajillion times. Programming languages solve this translation problem by providing abstractions that are more intuitive for human reasoning while still being precise enough to convert into machine instructions.

This translation process involves several key concepts. First, there's the notion of abstraction levels. Higher-level languages like Python or JavaScript are further removed from machine code than lower-level languages like C or assembly. Second, there's the parsing process, where the language's syntax rules are used to break down source code into meaningful components that can be analyzed and processed. Finally, there's the actual translation mechanism, which transforms these parsed components into executable instructions.

### Interpreters vs Compilers

The translation from high-level code to machine-executable instructions happens through two primary approaches: interpretation and compilation.

**Interpreters** work by reading and executing source code line by line (or statement by statement) at runtime. Think of an interpreter as a real-time translator who listens to a speaker and immediately translates their words to the audience. Languages like Python, JavaScript, and Ruby typically use interpreters. When you run a Python script, the Python interpreter reads your code, parses it, and executes the corresponding machine instructions on the fly. This approach offers flexibility. You can modify code and see results immediately without a separate build step. But it comes with performance overhead since translation happens during execution.

**Compilers** take a different approach by translating the entire source code into machine code before execution. This is like having a translator work on a complete document beforehand, producing a fully translated version that can be read directly by the target audience. Languages like C, C++, and Rust use compilers. The compilation process produces an executable file containing machine code that can run directly on the target processor without needing the original source code or language tools. This typically results in faster execution since no translation overhead exists at runtime, but requires a separate build step whenever code changes.

Some languages blur these boundaries. For example, Java compiles to bytecode (an intermediate representation) which is then interpreted or just-in-time compiled by the Java Virtual Machine, while languages like C# use similar hybrid approaches.

## The Language Itself

MOPLang is a stack-based programming language designed for simplicity and educational purposes. It operates on a simple abstract machine model with a stack for data storage and a linear instruction memory.

### Language Design

MOPLang follows a stack-based architecture where:

- All data manipulation happens through a LIFO (Last-In-First-Out) stack
- Programs consist of sequential instructions with support for labels and jumps
- Currently supports numeric values (floating-point numbers)
- Provides basic arithmetic operations, I/O, control flow, and variable storage
- Includes a 64-slot variable table with system variables for machine state

### Syntax

The complete syntax is formally defined in Extended Backus-Naur Form (EBNF) in [grammar.ebnf][syntax].

Key syntactic elements include:

- **Instructions**: Stack operations (`PUSH`, `POP`), arithmetic (`ADD`, `SUB`, `MUL`, `DIV`, `MOD`), I/O (`PRINT`, `READ`), control flow (`JUMP`, `HALT`), and variable operations (`STORE`, `LOAD`, `STORE_TOP`)
- **Labels**: Named positions in code for jump targets (e.g., `LOOP:`)
- **Comments**: Begin with `;` and extend to end of line
- **Numbers**: Support integers and decimals with optional signs
- **Strings**: Enclosed in double quotes with escape sequences (`\n`, `\t`, `\\`, `\"`)
- **Variable indices**: Integers from 0-63 for variable operations

### Semantics

The formal execution semantics are specified in [SEMANTICS.md][semantics].

The abstract machine model consists of:

- **Stack**: Stores numeric values
- **Program Counter**: Tracks current instruction
- **Instruction Memory**: Holds the program
- **Label Table**: Maps labels to instruction positions
- **Variable Table**: 64-slot storage (0-15 reserved for system, 16-63 for user)
- **I/O Streams**: For input/output operations

## Interpreter Implementation

MOPLang interpreters implement the abstract machine model defined in the semantics specification. Both Python and Go implementations follow the same execution model.

### Execution Process

1. **Parsing Phase**
   - Read the `.mopl` source file
   - Tokenize each line into instructions and arguments
   - Build the label table for jump targets
   - Validate syntax according to the grammar

2. **Execution Phase**
   - Initialize empty stack and set program counter to 0
   - Fetch-decode-execute cycle:
     - Fetch instruction at current program counter
     - Execute according to semantic rules
     - Update program counter (increment or jump)
   - Continue until `HALT` or error

### Error Handling

Interpreters handle runtime errors as specified in the semantics:

- Stack underflow (popping from empty stack)
- Division by zero
- Undefined labels
- Invalid numeric input
- Program counter out of bounds
- Variable index out of bounds

### Example Program

```mopl
; This program adds two numbers (5 and 3) and prints the result
PUSH 5
PUSH 3
ADD
PRINT TOP
HALT
```

Execution:

- Push 5 → stack: \[5\]
- Push 3 ⇾ stack: \[3, 5\]
- Add ⇾ pop 3 and pop 5, push 8 ⇾ stack: \[8\]
- Print top ⇾ outputs `8`
- Halt ⇾ program ends

### Variable Example

```mopl
; This program demonstrates variable operations
PUSH 42
STORE 16        ; Store 42 in user variable 16
PUSH 10
PUSH 5
ADD             ; 10 + 5 = 15
STORE_TOP 17    ; Store 15 in variable 17 without popping
LOAD 16         ; Load 42 back onto stack
MUL             ; 42 * 15 = 630
PRINT TOP       ; prints 630
LOAD 1          ; Load arithmetic result flag (1 = positive)
PRINT TOP       ; prints 1
HALT
```

## Testing Strategy

Our testing approach ensures interpreter correctness across all language features.

### Test Structure

1. Test Cases: Individual .mopl files with expected outcomes
2. Test Runner: Language-agnostic test orchestration
3. Test Harness: Per-language test execution wrapper

### Test Case Format

Each test consists of:

- `.mopl` file: The program to test
- `.expected` file: Expected stdout output
- `.input` file (optional): stdin input for READ operations
- `.error` file (optional): Expected error/exit code

#### Test Organization

Tests are organized by instruction category:

- `stack_ops/`: Stack manipulation tests
- `arithmetic_ops/`: Math operation tests  
- `io_ops/`: Input/output tests
- `control_ops/`: Control flow tests
- `variable_ops/`: Variable operation tests
- `edge_cases/`: Error handling and edge cases

Test naming convention: `<instruction_name>.<extension>` (dots in instruction names become underscores, e.g., `jump_eq_0.mopl` for `JUMP.EQ.0`)

## Known Issues

None so far :D

## TODO

- More language features
  - Heap memory management
  - Extended arithmetic operations
  - Additional comparison and logical operations
- Expanded example program collection
- CI/CD with automated test badge
- Better error handling in both interpreters
- Enhanced error reporting with line numbers
- Modular interpreter architecture
- Comprehensive edge case test suite
- Language Server Protocol (LSP) implementation
- Writing down more information and research
- Potentially more interpreter implementations in other languages
- Compiler implementations
- Virtual machine bytecode format
- Documentation improvements:
  - Tutorial for beginners
  - Language reference manual
  - Implementation guide
