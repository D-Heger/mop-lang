package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

func parseStringLiteral(s string) string {
	// Parse escape sequences in string literals according to MOPLang semantics
	var result strings.Builder
	i := 0
	for i < len(s) {
		if s[i] == '\\' && i+1 < len(s) {
			// Handle escape sequences
			nextChar := s[i+1]
			switch nextChar {
			case 'n':
				result.WriteByte('\n')
			case 't':
				result.WriteByte('\t')
			case '\\':
				result.WriteByte('\\')
			case '"':
				result.WriteByte('"')
			default:
				// Unknown escape sequence, keep both characters as-is
				result.WriteByte(s[i])
				result.WriteByte(nextChar)
			}
			i += 2
		} else {
			result.WriteByte(s[i])
			i++
		}
	}
	return result.String()
}

func checkPCBounds(pc int, programLen int) {
	// Check if program counter is within valid bounds
	if pc < 0 || pc >= programLen {
		fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", pc, programLen-1)
		os.Exit(1)
	}
}

func validateLabelExists(label string, labelTracker map[string]int) {
	// Validate that a label exists
	if _, exists := labelTracker[label]; !exists {
		fmt.Fprintf(os.Stderr, "Error: Undefined label '%s'\n", label)
		os.Exit(1)
	}
}

func main() {
	// read arguments
	if len(os.Args) < 2 {
		os.Exit(1)
	}

	programFilepath := os.Args[1]
	file, err := os.Open(programFilepath)
	if err != nil {
		fmt.Println("Error opening file:", err)
		os.Exit(1)
	}
	defer file.Close() // ensure file is closed after function ends

	// read file lines
	var programLines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		// remove anything after ';'
		line = strings.Split(line, ";")[0]
		// skip empty lines
		if line == "" {
			continue
		}
		programLines = append(programLines, line)
	}

	if err := scanner.Err(); err != nil {
		fmt.Println("Error reading file:", err)
		os.Exit(1)
	}

	// -------- tokenize input
	var program []any
	tokenCounter := 0
	labelTracker := make(map[string]int)
	for _, line := range programLines {
		parts := strings.Split(line, " ")
		opcode := parts[0]

		// check for empty line
		if opcode == "" {
			continue
		}

		// check for label
		if strings.HasSuffix(opcode, ":") {
			labelName := opcode[:len(opcode)-1]
			labelTracker[labelName] = tokenCounter
			continue
		}

		// store opcode token
		program = append(program, opcode)
		tokenCounter++

		// handle opcodes
		switch opcode {
		case "PUSH":
			// expecting a number (support float)
			numberStr := parts[1]
			number, err := strconv.ParseFloat(numberStr, 64)
			if err != nil {
				fmt.Println("Error parsing number:", err)
				os.Exit(1)
			}
			program = append(program, number)
			tokenCounter++

		case "PRINT":
			// either we get `PRINT TOP` or `PRINT "some string"`
			if len(parts) == 2 && parts[1] == "TOP" {
				program = append(program, "TOP")
				tokenCounter++
			} else {
				// parse string literal
				stringLiteral := strings.Join(parts[1:], " ")
				stringLiteral = strings.TrimSpace(stringLiteral)
				if len(stringLiteral) >= 2 && strings.HasPrefix(stringLiteral, `"`) && strings.HasSuffix(stringLiteral, `"`) {
					stringLiteral = strings.TrimPrefix(stringLiteral, `"`)
					stringLiteral = strings.TrimSuffix(stringLiteral, `"`)
				}
				// Parse escape sequences
				stringLiteral = parseStringLiteral(stringLiteral)
				program = append(program, stringLiteral)
				tokenCounter++
			}

		case "JUMP", "JUMP.EQ.0", "JUMP.NE.0", "JUMP.GT.0", "JUMP.GE.0", "JUMP.LT.0", "JUMP.LE.0":
			// read label
			label := parts[1]
			program = append(program, label)
			tokenCounter++
		}
	}

	// -------- interpret program
	programCounter := 0
	stack := NewStack(1000) // Increased stack size to 1000 for overflow testing

	for programCounter < len(program) {
		checkPCBounds(programCounter, len(program))

		opcode := program[programCounter].(string)
		if opcode == "HALT" {
			break
		}

		programCounter++

		// Allow PC to equal len for end detection
		if programCounter > len(program) {
			fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", programCounter, len(program))
			os.Exit(1)
		}

		switch opcode {
		case "PUSH":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected value after PUSH instruction\n")
				os.Exit(1)
			}
			number := program[programCounter].(float64)
			programCounter++
			stack.push(number)
		case "POP":
			stack.pop()
		case "ADD":
			stack.sizeCheck(2)
			a := stack.pop()
			b := stack.pop()
			stack.push(a + b)
		case "SUB":
			stack.sizeCheck(2)
			a := stack.pop()
			b := stack.pop()
			stack.push(b - a)
		case "MUL":
			stack.sizeCheck(2)
			a := stack.pop()
			b := stack.pop()
			stack.push(a * b)
		case "DIV":
			stack.sizeCheck(2)
			a := stack.pop()
			b := stack.pop()
			if a == 0 {
				fmt.Fprintf(os.Stderr, "Error: Division by zero\n")
				os.Exit(1)
			}
			stack.push(b / a)
		case "PRINT":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected argument after PRINT instruction\n")
				os.Exit(1)
			}
			arg := program[programCounter].(string)
			programCounter++
			if arg == "TOP" {
				fmt.Println(stack.top())
			} else {
				fmt.Println(arg)
			}
		case "READ":
			var input string
			fmt.Scanln(&input)
			number, err := strconv.ParseFloat(input, 64)
			if err != nil {
				fmt.Fprintf(os.Stderr, "Error: Invalid numeric input\n")
				os.Exit(1)
			}
			stack.push(number)
		case "JUMP":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			newPC := labelTracker[label]
			checkPCBounds(newPC, len(program))
			programCounter = newPC
		case "JUMP.EQ.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.EQ.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number == 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		case "JUMP.NE.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.NE.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number != 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		case "JUMP.GT.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.GT.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number > 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		case "JUMP.GE.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.GE.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number >= 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		case "JUMP.LT.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.LT.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number < 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		case "JUMP.LE.0":
			if programCounter >= len(program) {
				fmt.Fprintf(os.Stderr, "Error: Expected label after JUMP.LE.0 instruction\n")
				os.Exit(1)
			}
			label := program[programCounter].(string)
			validateLabelExists(label, labelTracker)
			number := stack.pop() // Pop value for condition check
			if number <= 0 {
				newPC := labelTracker[label]
				checkPCBounds(newPC, len(program))
				programCounter = newPC
			} else {
				programCounter++
			}
		default:
			fmt.Fprintf(os.Stderr, "Error: Unknown instruction '%s'\n", opcode)
			os.Exit(1)
		}
	}

	// Check if PC went out of bounds after the loop
	if programCounter > len(program) {
		fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", programCounter, len(program))
		os.Exit(1)
	}

	// Signal successful completion
	os.Exit(0)
}

// -------- stack implementation
type Stack struct {
	buffer       []float64
	stackPointer int
	size         int
}

func NewStack(size int) *Stack {
	return &Stack{
		buffer:       make([]float64, size),
		stackPointer: -1,
		size:         size,
	}
}

func (s *Stack) push(number float64) {
	// Check for stack overflow
	if s.stackPointer+1 >= s.size {
		fmt.Fprintf(os.Stderr, "Error: Stack overflow: maximum size exceeded\n")
		os.Exit(1)
	}
	s.stackPointer++
	s.buffer[s.stackPointer] = number
}

func (s *Stack) pop() float64 {
	// Check for stack underflow
	if s.stackPointer < 0 {
		fmt.Fprintf(os.Stderr, "Error: Stack underflow: cannot pop from empty stack\n")
		os.Exit(1)
	}
	number := s.buffer[s.stackPointer]
	s.stackPointer--
	return number
}

func (s *Stack) top() float64 {
	// Check for stack underflow
	if s.stackPointer < 0 {
		fmt.Fprintf(os.Stderr, "Error: Stack underflow: cannot access top of empty stack\n")
		os.Exit(1)
	}
	return s.buffer[s.stackPointer]
}

func (s *Stack) sizeCheck(requiredElements int) {
	// Check if stack has enough elements
	if s.stackPointer+1 < requiredElements {
		fmt.Fprintf(os.Stderr, "Error: Stack underflow: need %d elements, have %d\n", requiredElements, s.stackPointer+1)
		os.Exit(1)
	}
}
