package main

import (
	"fmt"
	"mop-lang/interpreter/go/internal/interpreter"
	"os"
)

func main() {
	// Check for command line arguments
	if len(os.Args) < 2 {
		fmt.Fprintf(os.Stderr, "Usage: go run interpreter.go <program_file>\n")
		os.Exit(1)
	}

	// Get program file path from arguments
	programFilepath := os.Args[1]

	// Create and run interpreter
	interp := interpreter.New(256)
	if err := interp.ExecuteProgram(programFilepath); err != nil {
		fmt.Fprintf(os.Stderr, "Error: %v\n", err)
		os.Exit(1)
	}
}
