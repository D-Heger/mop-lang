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
stack = Stack(1000)  # Increased stack size to 1000 for overflow testing

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
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        stack.push(a + b)
    elif opcode == "SUB":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        stack.push(b - a)
    elif opcode == "MUL":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        stack.push(a * b)
    elif opcode == "DIV":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        if a == 0:
            print("Error: Division by zero", file=sys.stderr)
            sys.exit(1)
        if (b / a) % 1 == 0:
            stack.push(int(b / a))
        else:
            stack.push(b / a)
    elif opcode == "MOD":
        stack.size_check(2)
        a = stack.pop()
        b = stack.pop()
        if a == 0:
            print("Error: Division by zero", file=sys.stderr)
            sys.exit(1)
        result = b % a
        # For consistent behavior with other operations, convert to int if result is whole number
        if isinstance(result, float) and result.is_integer():
            stack.push(int(result))
        else:
            stack.push(result)
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
    else:
        print(f"Error: Unknown instruction '{opcode}'", file=sys.stderr)
        sys.exit(1)

# Check if PC went out of bounds after the loop
if program_counter > len(program):
    print(f"Error: Program counter out of bounds: {program_counter} (valid range: 0-{len(program)})", file=sys.stderr)
    sys.exit(1)

# Signal successful completion
sys.exit(0)