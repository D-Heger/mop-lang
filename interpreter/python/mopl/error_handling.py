"""Error handling utilities for the MOPLang interpreter."""

import sys


def check_pc_bounds(pc, program_len):
    """Check if program counter is within valid bounds"""
    if pc < 0 or pc >= program_len:
        print(f"Error: Program counter out of bounds: {pc} (valid range: 0-{program_len-1})", file=sys.stderr)
        sys.exit(1)


def validate_label_exists(label, label_tracker):
    """Validate that a label exists"""
    if label not in label_tracker:
        print(f"Error: Undefined label '{label}'", file=sys.stderr)
        sys.exit(1)


def handle_stack_overflow():
    """Handle stack overflow error"""
    print("Error: Stack overflow: maximum size exceeded", file=sys.stderr)
    sys.exit(1)


def handle_stack_underflow(operation="operation"):
    """Handle stack underflow error"""
    print(f"Error: Stack underflow: cannot {operation}", file=sys.stderr)
    sys.exit(1)


def handle_stack_underflow_with_count(required_elements, available_elements):
    """Handle stack underflow with element count details"""
    print(f"Error: Stack underflow: need {required_elements} elements, have {available_elements}", file=sys.stderr)
    sys.exit(1)


def handle_division_by_zero():
    """Handle division by zero error"""
    print("Error: Division by zero", file=sys.stderr)
    sys.exit(1)


def handle_variable_out_of_bounds(index):
    """Handle variable index out of bounds error"""
    print(f"Error: Variable index out of bounds: {index} (valid range: 0-63)", file=sys.stderr)
    sys.exit(1)


def handle_invalid_variable_index(index_str):
    """Handle invalid variable index error"""
    print(f"Error: Invalid variable index: {index_str}", file=sys.stderr)
    sys.exit(1)


def handle_invalid_numeric_input():
    """Handle invalid numeric input error"""
    print("Error: Invalid numeric input", file=sys.stderr)
    sys.exit(1)


def handle_unknown_instruction(opcode):
    """Handle unknown instruction error"""
    print(f"Error: Unknown instruction '{opcode}'", file=sys.stderr)
    sys.exit(1)


def handle_missing_argument(instruction):
    """Handle missing argument error"""
    print(f"Error: Expected argument after {instruction} instruction", file=sys.stderr)
    sys.exit(1)


def handle_missing_value(instruction):
    """Handle missing value error"""
    print(f"Error: Expected value after {instruction} instruction", file=sys.stderr)
    sys.exit(1)


def handle_missing_label(instruction):
    """Handle missing label error"""
    print(f"Error: Expected label after {instruction} instruction", file=sys.stderr)
    sys.exit(1)


def handle_missing_variable_index(instruction):
    """Handle missing variable index error"""
    print(f"Error: Expected variable index after {instruction} instruction", file=sys.stderr)
    sys.exit(1)
