# MOPLang Semantics Specification

> **Note:** This is an early version of MOPLang. All aspects of the language and its semantics are subject to change.

- [MOPLang Semantics Specification](#moplang-semantics-specification)
  - [1. Introduction](#1-introduction)
  - [2. Abstract Machine Model](#2-abstract-machine-model)
    - [2.1 Program State](#21-program-state)
    - [2.2 Value Domain](#22-value-domain)
  - [3. Execution Model](#3-execution-model)
    - [3.1 Program Initialization](#31-program-initialization)
    - [3.2 Execution Cycle](#32-execution-cycle)
  - [4. Instruction Semantics](#4-instruction-semantics)
    - [4.1 Stack Operations](#41-stack-operations)
      - [PUSH n](#push-n)
      - [POP](#pop)
    - [4.2 Arithmetic Operations](#42-arithmetic-operations)
      - [ADD](#add)
      - [SUB](#sub)
      - [MUL](#mul)
      - [DIV](#div)
    - [4.3 I/O Operations](#43-io-operations)
      - [PRINT arg](#print-arg)
      - [READ](#read)
    - [4.4 Control Flow Operations](#44-control-flow-operations)
      - [Label Definition](#label-definition)
      - [HALT](#halt)
      - [JUMP label](#jump-label)
      - [Conditional Jumps](#conditional-jumps)
  - [5. Error Conditions](#5-error-conditions)
  - [6. Comments and Whitespace](#6-comments-and-whitespace)
  - [7. String Literals](#7-string-literals)

## 1. Introduction

This document defines the formal semantics of MOPLang (MOPL), a stack-based programming language. The semantics describe how MOPLang programs are executed and what each language construct means in terms of computational behavior.

## 2. Abstract Machine Model

MOPLang programs execute on an abstract stack machine with the following components:

### 2.1 Program State

The state of a MOPLang program consists of:

- **Stack** ($S$): A last-in-first-out (LIFO) data structure storing numeric values
- **Program Counter** ($PC$): An integer indicating the current instruction position
- **Instruction Memory** ($I$): An ordered sequence of instructions
- **Label Table** ($L$): A mapping from label identifiers to instruction positions
- **Input Stream** ($In$): A stream of input values
- **Output Stream** ($Out$): A stream of output values

Formally:
$$ State = ⟨S, PC, I, L, In, Out⟩ $$

### 2.2 Value Domain

MOPLang currently supports a single data type:

- **Number**: Floating-point numbers (implementation may use IEEE 754 double precision)

## 3. Execution Model

### 3.1 Program Initialization

1. The instruction memory I is populated by parsing the source file
2. The label table L is constructed by scanning for label definitions
3. The stack S is initialized as empty: S = []
4. The program counter is set to 0: PC = 0
5. Input and output streams are bound to standard I/O

### 3.2 Execution Cycle

Program execution follows these steps:

1. If $PC ≥ |I|$, terminate with error (PC out of bounds)
2. Fetch instruction: $inst = I(PC)$
3. Execute inst according to its semantics ([Section 4](#4-instruction-semantics))
4. If execution doesn't explicitly modify PC, increment it: $PC = PC + 1$
5. If not halted, go to step 1

## 4. Instruction Semantics

### 4.1 Stack Operations

#### PUSH n

- **Syntax**: `PUSH number`
- **Precondition**: n is a valid number
- **Effect**: $S' = n :: S$
- **Side effects**: None

#### POP

- **Syntax**: `POP`
- **Precondition**: $|S| ≥ 1$
- **Effect**: $S' = tail(S)$
- **Side effects**: None
- **Error**: Stack underflow if $|S| < 1$

### 4.2 Arithmetic Operations

For all binary arithmetic operations:

- **Precondition**: $|S| ≥ 2$
- **Stack transformation**: $S = a :: b :: S'' \to S' = result :: S''$  
  (where $a$ is the topmost value, $b$ is the next value below $a$)
- **Error**: Stack underflow if $|S| < 2$

#### ADD

- **Effect**: Pop $b$ (top of stack), then $a$; push $result = a + b$
- **Effect**: $result = a + b$

#### SUB

- **Syntax**: `SUB`
- **Effect**: $result = b - a$, where $a$ is the topmost value on the stack and $b$ is the next value below $a$

#### MUL

- **Syntax**: `MUL`
- **Effect**: Pop $b$ (top of stack), then $a$; push $result = a * b$

#### DIV

- **Syntax**: `DIV`
- **Effect**: Pop $b$ (top of stack), then $a$; push $result = a / b$  
  ($a$ is the second value from the top, $b$ is the topmost value)
- **Error**: Division by zero if $b = 0$

### 4.3 I/O Operations

#### PRINT arg

- **Syntax**: `PRINT string_literal` or `PRINT TOP`
- **Effect**:
  - If arg is string_literal: $Out' = Out ++ [string\_value]$
  - If arg is TOP:
    - **Precondition**: $|S| ≥ 1$
    - $Out' = Out ++ [toString(head(S))]$
    - Stack unchanged
- **Error**: Stack underflow if arg is TOP $|S| < 1$

#### READ

- **Syntax**: `READ`
- **Effect**:
  - Read next value $v$ from $In$
  - $S' = parseNumber(v) :: S$
- **Error**: Invalid input if $v$ cannot be parsed as a number

### 4.4 Control Flow Operations

#### Label Definition

- **Syntax**: `identifier:`
- **Effect**: No runtime effect (processed during initialization)
- **Note**: Creates entry $L[identifier] = instruction_position$

#### HALT

- **Syntax**: `HALT`
- **Effect**: Program termination with exit code 0

#### JUMP label

- **Syntax**: `JUMP identifier`
- **Precondition**: $label \in domain(L)$
- **Effect**: $PC' = L[label]$
- **Error**: Undefined label if $label \not \in domain(L)$

#### Conditional Jumps

All conditional jumps follow the pattern:

- **Syntax**: `JUMP.condition identifier`
- **Precondition**: $|S| ≥ 1$ and $label \in domain(L)$
- **Effect**:
  - Let $v = head(S)$
  - If $condition(v)$ is true: $PC' = L[label]$
  - Otherwise: $PC' = PC + 1$
  - Stack unchanged

Conditions:

- **JUMP.EQ.0**: $condition(v) ≡ (v = 0)$
- **JUMP.NE.0**: $condition(v) ≡ (v ≠ 0)$
- **JUMP.GT.0**: $condition(v) ≡ (v > 0)$
- **JUMP.GE.0**: $condition(v) ≡ (v ≥ 0)$
- **JUMP.LT.0**: $condition(v) ≡ (v < 0)$
- **JUMP.LE.0**: $condition(v) ≡ (v ≤ 0)$

## 5. Error Conditions

MOPLang programs may encounter the following runtime errors:

1. **Stack Underflow**: Attempting to pop from or inspect an empty stack
2. **Stack Overflow**: Attempting to push onto a full stack
3. **Division by Zero**: Executing DIV when the top stack value is 0
4. **Undefined Label**: Jumping to a label not defined in the program
5. **Invalid Input**: READ operation encountering non-numeric input
6. **PC Out of Bounds**: Program counter exceeding instruction memory bounds

When an error occurs, program execution terminates with a non-zero exit code.

## 6. Comments and Whitespace

- Comments (starting with `;`) have no semantic effect
- Whitespace is significant only as a token separator
- Empty lines are ignored

## 7. String Literals

String literals support the following escape sequences:

- `\n`: newline (ASCII 10)
- `\t`: horizontal tab (ASCII 9)
- `\\`: backslash
- `\"`: double quote
