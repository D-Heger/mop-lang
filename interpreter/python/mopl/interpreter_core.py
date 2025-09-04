"""Core interpreter class for the MOPLang interpreter."""

import sys
from .stack import Stack
from .variables import VariableTable
from .parser import parse_program
from .instruction_handlers import InstructionRegistry
from .error_handling import check_pc_bounds


class Interpreter:
    """Main interpreter class that manages the execution state."""
    
    def __init__(self, stack_size=256):
        self.stack = Stack(stack_size)
        self.variables = VariableTable()
        self.instruction_registry = InstructionRegistry(self.stack, self.variables)
        self.program = []
        self.label_tracker = {}
        self.program_counter = 0
    
    def load_program(self, program_filepath):
        """Load and parse a program from file."""
        self.program, self.label_tracker = parse_program(program_filepath)
        self.program_counter = 0
    
    def run(self):
        """Execute the loaded program."""
        while self.program_counter < len(self.program):
            check_pc_bounds(self.program_counter, len(self.program))
            
            if self.program[self.program_counter] == "HALT":
                break
                
            opcode = self.program[self.program_counter]
            self.program_counter += 1
            
            check_pc_bounds(self.program_counter, len(self.program) + 1)  # Allow PC to equal len for end detection
            
            # Get and execute the instruction handler
            handler = self.instruction_registry.get_handler(opcode)
            self.program_counter = handler.execute(
                opcode, self.program, self.program_counter, self.label_tracker
            )

        # Check if PC went out of bounds after the loop
        if self.program_counter > len(self.program):
            print(f"Error: Program counter out of bounds: {self.program_counter} (valid range: 0-{len(self.program)})", file=sys.stderr)
            sys.exit(1)

        # Signal successful completion
        sys.exit(0)
    
    def execute_program(self, program_filepath):
        """Load and execute a program in one call."""
        self.load_program(program_filepath)
        self.run()
