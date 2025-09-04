"""Parser module for the MOPLang interpreter."""

import sys
from .error_handling import handle_invalid_variable_index


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


def read_and_strip_comments(program_filepath):
    """Read file lines and strip comments."""
    program_lines = []
    with open(program_filepath, "r") as program_file:
        for raw_line in program_file:
            # remove anything after ';'
            line = raw_line.split(";", 1)[0].strip()
            # skip empty lines
            if not line:
                continue
            program_lines.append(line)
    return program_lines


def tokenize_program(program_lines):
    """Tokenize the input program."""
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

        elif opcode in ["JUMP", "JUMP.EQ.0", "JUMP.NE.0", "JUMP.GT.0", "JUMP.GE.0", "JUMP.LT.0", "JUMP.LE.0"]:
            # read label
            label = parts[1]
            program.append(label)
            token_counter += 1

        elif opcode in ["STORE", "LOAD", "STORE_TOP"]:
            # read variable index
            try:
                var_index = int(parts[1])
                if var_index < 0 or var_index > 63:
                    print(f"Error: Variable index out of bounds: {var_index} (valid range: 0-63)", file=sys.stderr)
                    sys.exit(1)
                program.append(var_index)
                token_counter += 1
            except ValueError:
                handle_invalid_variable_index(parts[1])

    return program, label_tracker


def parse_program(program_filepath):
    """Parse a MOPLang program file into tokens and labels."""
    program_lines = read_and_strip_comments(program_filepath)
    program, label_tracker = tokenize_program(program_lines)
    return program, label_tracker
