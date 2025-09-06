"""Instruction handlers for the MOPLang interpreter."""

import sys
from .error_handling import (
    handle_missing_value,
    handle_missing_argument,
    handle_missing_label,
    handle_missing_variable_index,
    handle_division_by_zero,
    handle_invalid_numeric_input,
    check_pc_bounds,
    validate_label_exists,
    handle_unknown_instruction,
)


class InstructionHandler:
    """Base class for instruction handlers."""

    def __init__(self, stack, variables):
        self.stack = stack
        self.variables = variables

    def execute(self, instruction, program, pc, label_tracker):
        """Execute an instruction and return the new program counter."""
        raise NotImplementedError


class PushHandler(InstructionHandler):
    """Handler for PUSH instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_value("PUSH")
        number = program[pc]
        pc += 1
        self.stack.push(number)
        self.variables.update_system_vars(self.stack)
        return pc


class PopHandler(InstructionHandler):
    """Handler for POP instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        self.stack.pop()
        self.variables.update_system_vars(self.stack)
        return pc


class ArithmeticHandler(InstructionHandler):
    """Base handler for arithmetic operations."""

    def execute(self, instruction, program, pc, label_tracker):
        self.stack.size_check(2)
        a = self.stack.pop()
        b = self.stack.pop()
        result = self.compute(a, b)
        self.stack.push(result)
        self.variables.set_arithmetic_flag(result)
        self.variables.update_system_vars(self.stack)
        return pc

    def compute(self, a, b):
        """Compute the arithmetic operation. Override in subclasses."""
        raise NotImplementedError


class AddHandler(ArithmeticHandler):
    """Handler for ADD instruction."""

    def compute(self, a, b):
        return a + b


class SubHandler(ArithmeticHandler):
    """Handler for SUB instruction."""

    def compute(self, a, b):
        return b - a


class MulHandler(ArithmeticHandler):
    """Handler for MUL instruction."""

    def compute(self, a, b):
        return a * b


class DivHandler(ArithmeticHandler):
    """Handler for DIV instruction."""

    def compute(self, a, b):
        if a == 0:
            self.variables.set_division_error_flag()
            handle_division_by_zero()
        if (b / a) % 1 == 0:
            return int(b / a)
        else:
            return b / a


class ModHandler(ArithmeticHandler):
    """Handler for MOD instruction."""

    def compute(self, a, b):
        if a == 0:
            self.variables.set_division_error_flag()
            handle_division_by_zero()
        result = b % a
        # For consistent behavior with other operations, convert to int if result is whole number
        if isinstance(result, float) and result.is_integer():
            result = int(result)
        return result


class PrintHandler(InstructionHandler):
    """Handler for PRINT instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_argument("PRINT")
        arg = program[pc]
        pc += 1
        if arg == "TOP":  # special case
            value = self.stack.top()
            # Format numbers to avoid unnecessary decimal points
            if isinstance(value, float) and value.is_integer():
                print(int(value))
            else:
                print(value)
        else:
            print(arg)
        return pc


class ReadHandler(InstructionHandler):
    """Handler for READ instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        try:
            number = int(input())
            self.stack.push(number)
            self.variables.update_system_vars(self.stack)
        except ValueError:
            handle_invalid_numeric_input()
        return pc


class JumpHandler(InstructionHandler):
    """Handler for JUMP instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_label("JUMP")
        label = program[pc]
        validate_label_exists(label, label_tracker)
        new_pc = label_tracker[label]
        check_pc_bounds(new_pc, len(program))
        return new_pc


class ConditionalJumpHandler(InstructionHandler):
    """Base handler for conditional jump instructions."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_label(instruction)
        label = program[pc]
        validate_label_exists(label, label_tracker)
        number = self.stack.top()
        if self.should_jump(number):
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            return new_pc
        else:
            return pc + 1

    def should_jump(self, number):
        """Check if jump condition is met. Override in subclasses."""
        raise NotImplementedError


class JumpEqZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.EQ.0 instruction."""

    def should_jump(self, number):
        return number == 0


class JumpNeZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.NE.0 instruction."""

    def should_jump(self, number):
        return number != 0


class JumpGtZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.GT.0 instruction."""

    def should_jump(self, number):
        return number > 0


class JumpGeZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.GE.0 instruction."""

    def should_jump(self, number):
        return number >= 0


class JumpLtZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.LT.0 instruction."""

    def should_jump(self, number):
        return number < 0


class JumpLeZeroHandler(ConditionalJumpHandler):
    """Handler for JUMP.LE.0 instruction."""

    def should_jump(self, number):
        return number <= 0


class StoreHandler(InstructionHandler):
    """Handler for STORE instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_variable_index("STORE")
        var_index = program[pc]
        pc += 1
        value = self.stack.pop()
        self.variables.store(var_index, value)
        self.variables.update_system_vars(self.stack)
        return pc


class LoadHandler(InstructionHandler):
    """Handler for LOAD instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_variable_index("LOAD")
        var_index = program[pc]
        pc += 1
        value = self.variables.load(var_index)
        self.stack.push(value)
        self.variables.update_system_vars(self.stack)
        return pc


class StoreTopHandler(InstructionHandler):
    """Handler for STORE_TOP instruction."""

    def execute(self, instruction, program, pc, label_tracker):
        if pc >= len(program):
            handle_missing_variable_index("STORE_TOP")
        var_index = program[pc]
        pc += 1
        value = self.stack.top()
        self.variables.store(var_index, value)
        self.variables.update_system_vars(self.stack)
        return pc


class InstructionRegistry:
    """Registry for mapping instructions to their handlers."""

    def __init__(self, stack, variables):
        self.handlers = {
            "PUSH": PushHandler(stack, variables),
            "POP": PopHandler(stack, variables),
            "ADD": AddHandler(stack, variables),
            "SUB": SubHandler(stack, variables),
            "MUL": MulHandler(stack, variables),
            "DIV": DivHandler(stack, variables),
            "MOD": ModHandler(stack, variables),
            "PRINT": PrintHandler(stack, variables),
            "READ": ReadHandler(stack, variables),
            "JUMP": JumpHandler(stack, variables),
            "JUMP.EQ.0": JumpEqZeroHandler(stack, variables),
            "JUMP.NE.0": JumpNeZeroHandler(stack, variables),
            "JUMP.GT.0": JumpGtZeroHandler(stack, variables),
            "JUMP.GE.0": JumpGeZeroHandler(stack, variables),
            "JUMP.LT.0": JumpLtZeroHandler(stack, variables),
            "JUMP.LE.0": JumpLeZeroHandler(stack, variables),
            "STORE": StoreHandler(stack, variables),
            "LOAD": LoadHandler(stack, variables),
            "STORE_TOP": StoreTopHandler(stack, variables),
        }

    def get_handler(self, instruction):
        """Get the handler for an instruction."""
        if instruction not in self.handlers:
            handle_unknown_instruction(instruction)
        return self.handlers[instruction]
