import sys
from mopl import Interpreter


def main():
    # Check for command line arguments
    if len(sys.argv) != 2:
        print("Usage: python interpreter.py <program_file>", file=sys.stderr)
        sys.exit(1)

    # Get program file path from arguments
    program_filepath = sys.argv[1]

    # Create and run interpreter
    interpreter = Interpreter()
    interpreter.execute_program(program_filepath)


if __name__ == "__main__":
    main()
