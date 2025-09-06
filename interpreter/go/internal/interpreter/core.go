package interpreter

import (
	"fmt"
	"mop-lang/interpreter/go/internal/errors"
	"mop-lang/interpreter/go/internal/instructions"
	"mop-lang/interpreter/go/internal/parser"
	"mop-lang/interpreter/go/internal/stack"
	"mop-lang/interpreter/go/internal/variables"
	"os"
)

// Interpreter represents the main interpreter state
type Interpreter struct {
	stack               *stack.Stack
	variables           *variables.VariableTable
	instructionRegistry *instructions.Registry
	program             []any
	labelTracker        map[string]int
	programCounter      int
}

// New creates a new interpreter with the specified stack size
func New(stackSize int) *Interpreter {
	return &Interpreter{
		stack:               stack.New(stackSize),
		variables:           variables.New(),
		instructionRegistry: instructions.NewRegistry(),
		program:             make([]any, 0),
		labelTracker:        make(map[string]int),
		programCounter:      0,
	}
}

// LoadProgram loads and parses a program from file
func (i *Interpreter) LoadProgram(programFilepath string) error {
	program, labelTracker, err := parser.ParseProgram(programFilepath)
	if err != nil {
		return err
	}

	i.program = program
	i.labelTracker = labelTracker
	i.programCounter = 0
	return nil
}

// Run executes the loaded program
func (i *Interpreter) Run() {
	for i.programCounter < len(i.program) {
		errors.CheckPCBounds(i.programCounter, len(i.program))

		opcode := i.program[i.programCounter].(string)
		if opcode == "HALT" {
			break
		}

		i.programCounter++

		// Allow PC to equal len for end detection
		if i.programCounter > len(i.program) {
			fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", i.programCounter, len(i.program))
			os.Exit(1)
		}

		// Get and execute the instruction handler
		handler := i.instructionRegistry.GetHandler(opcode)
		i.programCounter = handler.Execute(i.program, i.programCounter, i.labelTracker, i.stack, i.variables)
	}

	// Check if PC went out of bounds after the loop
	if i.programCounter > len(i.program) {
		fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", i.programCounter, len(i.program))
		os.Exit(1)
	}

	// Signal successful completion
	os.Exit(0)
}

// ExecuteProgram loads and executes a program in one call
func (i *Interpreter) ExecuteProgram(programFilepath string) error {
	if err := i.LoadProgram(programFilepath); err != nil {
		return err
	}
	i.Run()
	return nil
}
