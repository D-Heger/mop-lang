import sys

# read arguments
program_filepath = sys.argv[1]

# read file lines and strip comments
program_lines = []
with open(program_filepath, "r") as program_file:
    for raw_line in program_file:
        # remove anything after ';'
        line = raw_line.split(";", 1)[0].strip()
        # skip empty lines
        if not line:
            continue
        program_lines.append(line)

def parse_string_literal(s):
    """Parse escape sequences in string literals according to MOPLang semantics."""
    result = []
    i = 0
    while i < len(s):
        if s[i] == '\\' and i + 1 < len(s):
            # Handle escape sequences
            next_char = s[i + 1]
            if next_char == 'n':
                result.append('\n')
            elif next_char == 't':
                result.append('\t')
            elif next_char == '\\':
                result.append('\\')
            elif next_char == '"':
                result.append('"')
            else:
                # Unknown escape sequence, keep both characters as-is
                result.append(s[i])
                result.append(next_char)
            i += 2
        else:
            result.append(s[i])
            i += 1
    return ''.join(result)

######## tokenize input

program = []
token_counter = 0
label_tracker = {}
for line in program_lines:
    parts = line.split(" ")
    opcode = parts[0]

    # check for empty line
    if opcode == "":
        continue

    # check for label
    if opcode.endswith(":"):
        label_tracker[opcode[:-1]] = token_counter
        continue

    # store opcode token
    program.append(opcode)
    token_counter += 1

    # handle opcodes
    if opcode == "PUSH":
        # expecting a number
            # support both int and float
            number_str = parts[1]
            if "." in number_str:
                number = float(number_str)
            else:
                number = int(number_str)
            program.append(number)
            token_counter += 1

    elif opcode == "PRINT":
        # either we get `PRINT TOP` or `PRINT "some string"`
        if len(parts) == 2 and parts[1] == "TOP":
            program.append("TOP")
            token_counter += 1
        # parse string literal
        else:
            string_literal = ' '.join(parts[1:])
            # Remove whitespaces before string literal
            string_literal = string_literal.lstrip()
            # Remove surrounding quotes if present
            if string_literal.startswith('"') and string_literal.endswith('"'):
                string_literal = string_literal[1:-1]
            # Parse escape sequences
            string_literal = parse_string_literal(string_literal)
            program.append(string_literal)
            token_counter += 1

    elif opcode == "JUMP" or opcode == "JUMP.EQ.0" or opcode == "JUMP.NE.0" or opcode == "JUMP.GT.0" or opcode == "JUMP.GE.0" or opcode == "JUMP.LT.0" or opcode == "JUMP.LE.0":
        # read label
        label = parts[1]
        program.append(label)
        token_counter += 1

    elif opcode == "STORE" or opcode == "LOAD" or opcode == "STORE_TOP":
        # read variable index
        try:
            var_index = int(parts[1])
            if var_index < 0 or var_index > 63:
                print(f"Error: Variable index out of bounds: {var_index} (valid range: 0-63)", file=sys.stderr)
                sys.exit(1)
            program.append(var_index)
            token_counter += 1
        except ValueError:
            print(f"Error: Invalid variable index: {parts[1]}", file=sys.stderr)
            sys.exit(1)

######## stack implementation
class Stack:
    def __init__(self, size):
        self.buffer = [0 for _ in range(size)] # create buffer for the specified stack size and fill it with 0
        self.stack_pointer = -1 # start sp at -1 so that we can properly start with 0, as it always should be
        self.size = size

    def push(self, number):
        # Check for stack overflow
        if self.stack_pointer + 1 >= self.size:
            print("Error: Stack overflow: maximum size exceeded", file=sys.stderr)
            sys.exit(1)
        self.stack_pointer += 1
        self.buffer[self.stack_pointer] = number

    def pop(self):
        # Check for stack underflow
        if self.stack_pointer < 0:
            print("Error: Stack underflow: cannot pop from empty stack", file=sys.stderr)
            sys.exit(1)
        number = self.buffer[self.stack_pointer]
        self.stack_pointer -= 1
        return number
    
    def top(self):
        # Check for stack underflow
        if self.stack_pointer < 0:
            print("Error: Stack underflow: cannot access top of empty stack", file=sys.stderr)
            sys.exit(1)
        return self.buffer[self.stack_pointer]
    
    def size_check(self, required_elements):
        # Check if stack has enough elements
        if self.stack_pointer + 1 < required_elements:
            print(f"Error: Stack underflow: need {required_elements} elements, have {self.stack_pointer + 1}", file=sys.stderr)
            sys.exit(1)

    def current_size(self):
        return self.stack_pointer + 1

######## variable table implementation
class VariableTable:
    def __init__(self):
        self.variables = [0.0 for _ in range(64)]  # 64 variables, initialized to 0
        
    def store(self, index, value):
        if index < 0 or index > 63:
            print(f"Error: Variable index out of bounds: {index} (valid range: 0-63)", file=sys.stderr)
            sys.exit(1)
        self.variables[index] = value
        
    def load(self, index):
        if index < 0 or index > 63:
            print(f"Error: Variable index out of bounds: {index} (valid range: 0-63)", file=sys.stderr)
            sys.exit(1)
        return self.variables[index]
        
    def update_system_vars(self, stack):
        # Update system variables automatically
        self.variables[2] = stack.current_size()  # VAR_2: Current stack size
        self.variables[6] = stack.size  # VAR_6: Stack capacity

######## utility functions
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

######## interpret program
program_counter = 0
stack = Stack(256)
variables = VariableTable()  # Initialize variable table

while program_counter < len(program):
    check_pc_bounds(program_counter, len(program))
    
    if program[program_counter] == "HALT":
        break
        
    opcode = program[program_counter]
    program_counter += 1
    
    check_pc_bounds(program_counter, len(program) + 1)  # Allow PC to equal len for end detection

    if opcode == "PUSH":
        if program_counter >= len(program):
            print("Error: Expected value after PUSH instruction", file=sys.stderr)
            sys.exit(1)
        number = program[program_counter]
        program_counter += 1
        stack.push(number)
        variables.update_system_vars(stack)
    elif opcode == "POP":
        stack.pop()
        variables.update_system_vars(stack)
    elif opcode == "ADD":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        result = a + b
        stack.push(result)
        # Update arithmetic flags
        if result == 0:
            variables.store(1, 0)  # VAR_1: zero
        elif result > 0:
            variables.store(1, 1)  # VAR_1: positive
        else:
            variables.store(1, 2)  # VAR_1: negative
        variables.update_system_vars(stack)
    elif opcode == "SUB":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        result = b - a
        stack.push(result)
        # Update arithmetic flags
        if result == 0:
            variables.store(1, 0)  # VAR_1: zero
        elif result > 0:
            variables.store(1, 1)  # VAR_1: positive
        else:
            variables.store(1, 2)  # VAR_1: negative
        variables.update_system_vars(stack)
    elif opcode == "MUL":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        result = a * b
        stack.push(result)
        # Update arithmetic flags
        if result == 0:
            variables.store(1, 0)  # VAR_1: zero
        elif result > 0:
            variables.store(1, 1)  # VAR_1: positive
        else:
            variables.store(1, 2)  # VAR_1: negative
        variables.update_system_vars(stack)
    elif opcode == "DIV":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        if a == 0:
            variables.store(4, 1)  # VAR_4: division by zero error
            print("Error: Division by zero", file=sys.stderr)
            sys.exit(1)
        if (b / a) % 1 == 0:
            result = int(b / a)
        else:
            result = b / a
        stack.push(result)
        # Update arithmetic flags
        if result == 0:
            variables.store(1, 0)  # VAR_1: zero
        elif result > 0:
            variables.store(1, 1)  # VAR_1: positive
        else:
            variables.store(1, 2)  # VAR_1: negative
        variables.update_system_vars(stack)
    elif opcode == "MOD":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        if a == 0:
            variables.store(4, 1)  # VAR_4: division by zero error
            print("Error: Division by zero", file=sys.stderr)
            sys.exit(1)
        result = b % a
        # For consistent behavior with other operations, convert to int if result is whole number
        if isinstance(result, float) and result.is_integer():
            result = int(result)
        stack.push(result)
        # Update arithmetic flags
        if result == 0:
            variables.store(1, 0)  # VAR_1: zero
        elif result > 0:
            variables.store(1, 1)  # VAR_1: positive
        else:
            variables.store(1, 2)  # VAR_1: negative
        variables.update_system_vars(stack)
    elif opcode == "PRINT":
        if program_counter >= len(program):
            print("Error: Expected argument after PRINT instruction", file=sys.stderr)
            sys.exit(1)
        arg = program[program_counter]
        program_counter += 1
        if arg == "TOP":              # special case
            value = stack.top()
            # Format numbers to avoid unnecessary decimal points
            if isinstance(value, float) and value.is_integer():
                print(int(value))
            else:
                print(value)
        else:
            print(arg)
    elif opcode == "READ":
        try:
            number = int(input())
            stack.push(number)
        except ValueError:
            print("Error: Invalid numeric input", file=sys.stderr)
            sys.exit(1)
    elif opcode == "JUMP":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        new_pc = label_tracker[label]
        check_pc_bounds(new_pc, len(program))
        program_counter = new_pc
    elif opcode == "JUMP.EQ.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.EQ.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number == 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "JUMP.NE.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.NE.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number != 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "JUMP.GT.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.GT.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number > 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "JUMP.GE.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.GE.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number >= 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "JUMP.LT.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.LT.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number < 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "JUMP.LE.0":
        if program_counter >= len(program):
            print("Error: Expected label after JUMP.LE.0 instruction", file=sys.stderr)
            sys.exit(1)
        label = program[program_counter]
        validate_label_exists(label, label_tracker)
        number = stack.top()
        if number <= 0:
            new_pc = label_tracker[label]
            check_pc_bounds(new_pc, len(program))
            program_counter = new_pc
        else:
            program_counter += 1
    elif opcode == "STORE":
        if program_counter >= len(program):
            print("Error: Expected variable index after STORE instruction", file=sys.stderr)
            sys.exit(1)
        var_index = program[program_counter]
        program_counter += 1
        value = stack.pop()
        variables.store(var_index, value)
        variables.update_system_vars(stack)
    elif opcode == "LOAD":
        if program_counter >= len(program):
            print("Error: Expected variable index after LOAD instruction", file=sys.stderr)
            sys.exit(1)
        var_index = program[program_counter]
        program_counter += 1
        value = variables.load(var_index)
        stack.push(value)
        variables.update_system_vars(stack)
    elif opcode == "STORE_TOP":
        if program_counter >= len(program):
            print("Error: Expected variable index after STORE_TOP instruction", file=sys.stderr)
            sys.exit(1)
        var_index = program[program_counter]
        program_counter += 1
        value = stack.top()
        variables.store(var_index, value)
        variables.update_system_vars(stack)
    else:
        print(f"Error: Unknown instruction '{opcode}'", file=sys.stderr)
        sys.exit(1)

# Check if PC went out of bounds after the loop
if program_counter > len(program):
    print(f"Error: Program counter out of bounds: {program_counter} (valid range: 0-{len(program)})", file=sys.stderr)
    sys.exit(1)

# Signal successful completion
sys.exit(0)