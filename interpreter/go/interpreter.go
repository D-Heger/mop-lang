package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

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
				program = append(program, stringLiteral)
				tokenCounter++
			}

		case "JUMP.EQ.0", "JUMP.GT.0":
			// read label
			label := parts[1]
			program = append(program, label)
			tokenCounter++
		}
	}

	// -------- interpret program
	programCounter := 0
	stack := NewStack(256)

	for {
		if programCounter >= len(program) {
			break
		}

		opcode := program[programCounter].(string)
		if opcode == "HALT" {
			break
		}

		programCounter++

		switch opcode {
		case "PUSH":
			number := program[programCounter].(float64)
			programCounter++
			stack.push(number)
		case "POP":
			stack.pop()
		case "ADD":
			a := stack.pop()
			b := stack.pop()
			stack.push(a + b)
		case "SUB":
			a := stack.pop()
			b := stack.pop()
			stack.push(b - a)
		case "MUL":
			a := stack.pop()
			b := stack.pop()
			stack.push(a * b)
		case "DIV":
			a := stack.pop()
			b := stack.pop()
			if a == 0 {
				fmt.Println("Error: Division by zero")
				os.Exit(1)
			}
			stack.push(b / a)
		case "PRINT":
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
				os.Exit(1)
			}
			stack.push(number)
		case "JUMP.EQ.0":
			number := stack.top()
			label := program[programCounter].(string)
			if number == 0 {
				programCounter = labelTracker[label]
			} else {
				programCounter++
			}
		case "JUMP.GT.0":
			number := stack.top()
			label := program[programCounter].(string)
			if number > 0 {
				programCounter = labelTracker[label]
			} else {
				programCounter++
			}
		}
	}
}

// -------- stack implementation
type Stack struct {
	buffer       []float64
	stackPointer int
}

func NewStack(size int) *Stack {
	return &Stack{
		buffer:       make([]float64, size),
		stackPointer: -1,
	}
}

func (s *Stack) push(number float64) {
	s.stackPointer++
	s.buffer[s.stackPointer] = number
}

func (s *Stack) pop() float64 {
	number := s.buffer[s.stackPointer]
	s.stackPointer--
	return number
}

func (s *Stack) top() float64 {
	return s.buffer[s.stackPointer]
}
