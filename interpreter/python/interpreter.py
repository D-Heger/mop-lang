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
            string_literal = ' '.join(parts[1:])[1:-1]
            program.append(string_literal)
            token_counter += 1

    elif opcode == "JUMP.EQ.0":
        # read label
        label = parts[1]
        program.append(label)
        token_counter += 1

    elif opcode == "JUMP.GT.0":
        # read label
        label = parts[1]
        program.append(label)
        token_counter += 1

######## stack implementation
class Stack:
    def __init__(self, size):
        self.buffer = [0 for _ in range(size)] # create buffer for the specified stack size and fill it with 0
        self.stack_pointer = -1 # start sp at -1 so that we can properly start with 0, as it always should be

    def push(self, number):
        self.stack_pointer += 1
        self.buffer[self.stack_pointer] = number

    def pop(self):
        number = self.buffer[self.stack_pointer]
        self.stack_pointer -= 1
        return number
    
    def top(self):
        return self.buffer[self.stack_pointer]
    
######## interpret program
program_counter = 0
stack = Stack(256)

while program[program_counter] != "HALT":
    opcode = program[program_counter]
    program_counter += 1

    if opcode == "PUSH":
        number = program[program_counter]
        program_counter += 1
        stack.push(number)
    elif opcode == "POP":
        stack.pop()
    elif opcode == "ADD":
        a = stack.pop()
        b = stack.pop()
        stack.push(a + b)
    elif opcode == "SUB":
        a = stack.pop()
        b = stack.pop()
        stack.push(b - a)
    elif opcode == "MUL":
        a = stack.pop()
        b = stack.pop()
        stack.push(a * b)
    elif opcode == "DIV":
        a = stack.pop()
        b = stack.pop()
        if a == 0:
            print("Error: Division by zero")
            sys.exit(1)
        stack.push(b / a)
    elif opcode == "PRINT":
        arg = program[program_counter]
        program_counter += 1
        if arg == "TOP":              # special case
            print(stack.top())
        else:
            print(arg)
    elif opcode == "READ":
        number = int(input())
        stack.push(number)
    elif opcode == "JUMP.EQ.0":
        number = stack.top()
        if number == 0:
            program_counter = label_tracker[program[program_counter]]
        else:
            program_counter += 1
    elif opcode == "JUMP.GT.0":
        number = stack.top()
        if number > 0:
            program_counter = label_tracker[program[program_counter]]
        else:
            program_counter += 1