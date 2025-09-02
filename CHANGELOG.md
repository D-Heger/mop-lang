# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.0.2] - 2025-09-02

### Added

- Variable table with 64 storage slots
  - Variables 0-15 reserved for system use
  - Variables 16-63 available for user programs
- New variable operations:
  - `STORE index` - Pop value from stack and store in variable
  - `LOAD index` - Load variable value and push to stack
  - `STORE_TOP index` - Store top of stack in variable without popping
- System variables for abstract machine state:
  - `VAR_0`: Program status (0=running, 1=halted, 2=error)
  - `VAR_1`: Last arithmetic result flags (0=zero, 1=positive, 2=negative)
  - `VAR_2`: Current stack size (automatically updated)
  - `VAR_3`: Program counter backup
  - `VAR_4`: Error code (0=none, 1=division by zero, 2=stack underflow, etc.)
  - `VAR_5`: Input/output status
  - `VAR_6`: Stack capacity
  - `VAR_7`: Last comparison result (-1=less, 0=equal, 1=greater)
  - `VAR_8-15`: Reserved for future system use
- Enhanced error handling for variable operations
- Comprehensive test suite for variable operations
- EBNF grammar specification for MOPLang
- Semantics documentation for MOPLang
- (Grammar) Allowed underscore (`_`) in identifiers
- Test suite for MOPLang
  - Includes test cases for all instructions in the current instruction set
  - Documentation on the testing strategy
  - Test result generation
- New jump instructions:
  - `JUMP` - Unconditional jump
  - `JUMP.NE.0` - Jump if top of stack is not equal to 0
  - `JUMP.GE.0` - Jump if top of stack is greater than or equal to 0
  - `JUMP.LT.0` - Jump if top of stack is less than 0
  - `JUMP.LE.0` - Jump if top of stack is less than or equal to 0
- Additional documentation for what programming languages are and "how they work"
- Support for escape sequences in string literals
- New arithmetic operation:
  - `MOD` - Pop two numbers and push the modulo (remainder) of the second divided by the first
- VSCode extension for MOPLang syntax highlighting and language support

### Fixed

- Typos in README.md
- Mixed usage of "-" and "_" in file and directory names, now just using "_"
- Python interpreter no longer calculates results with ".0" during division, instead now skipping the floating point

### Changed

- Added, Removed and Reworded TODO items in README.md
- When finished interpreting, both interpreters now exit with success status code (0)
- Adjusted makefile with new targets and added section separation comments
- Adjusted Goal Statement in Readme
- Replaced implementation-specific information in README.md with more generalized information
- Updated README.md with development tools table and VSCode extension documentation

### Removed

- Syntax definition from README.md

## [0.0.1] - 2025-08-09

### Added

- Initial release of MOPLang (MOPL) stack-based programming language
- Core stack operations:
  - `PUSH n` - Push rational(?) number onto stack
  - `POP` - Remove top item from stack
  - `ADD` - Pop two numbers and push their sum
  - `SUB` - Pop two numbers and push (second - first)
  - `MUL` - Pop two numbers and push their product
  - `DIV` - Pop two numbers and push (second / first)
- I/O operations:
  - `PRINT "text"` - Display text string
  - `PRINT TOP` - Display top of stack
  - `READ` - Read number from user input
- Control flow operations:
  - `JUMP.EQ.0 label` - Jump to label if top of stack equals 0
  - `JUMP.GT.0 label` - Jump to label if top of stack is greater than 0
  - `label:` - Define jump target with alphanumeric labels
  - `HALT` - Stop program execution
- Comment support using semicolon (`;`) - ignore everything after semicolon on a line
- Python interpreter (`interpreter/python/interpreter.py`) - stable implementation
- Go interpreter (`interpreter/go/interpreter.go`) - stable implementation
- Example programs demonstrating language features:
  - `compare-to-one.mopl` - Number comparison example
  - `compare-to-ten.mopl` - Number comparison example
  - `compare-to-two.mopl` - Number comparison example
  - `count_down.mopl` - Countdown loop demonstration
  - `division-example.mopl` - Division operation example
  - `is-even.mopl` - Even number check algorithm
  - `is-positive.mopl` - Positive number check algorithm
  - `multiplication-example.mopl` - Multiplication operation example
- Complete language specification and documentation
- Execution flow diagram using Mermaid
- Error handling for division by zero and invalid input
- DWYW-WC license
- Project build system with Makefile

[Unreleased]: https://github.com/d-heger/mop-lang/compare/v0.0.1...HEAD
[0.0.2]: https://github.com/d-heger/mop-lang/releases/tag/v0.0.2
[0.0.1]: https://github.com/d-heger/mop-lang/releases/tag/v0.0.1

<!--KaC syntax: Section -> Added -> Fixed -> Changed -> Removed -->