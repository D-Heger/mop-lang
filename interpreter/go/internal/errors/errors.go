package errors

import (
	"fmt"
	"os"
)

// CheckPCBounds checks if program counter is within valid bounds
func CheckPCBounds(pc int, programLen int) {
	if pc < 0 || pc >= programLen {
		fmt.Fprintf(os.Stderr, "Error: Program counter out of bounds: %d (valid range: 0-%d)\n", pc, programLen-1)
		os.Exit(1)
	}
}

// ValidateLabelExists validates that a label exists
func ValidateLabelExists(label string, labelTracker map[string]int) {
	if _, exists := labelTracker[label]; !exists {
		fmt.Fprintf(os.Stderr, "Error: Undefined label '%s'\n", label)
		os.Exit(1)
	}
}

// HandleStackOverflow handles stack overflow error
func HandleStackOverflow() {
	fmt.Fprintf(os.Stderr, "Error: Stack overflow: maximum size exceeded\n")
	os.Exit(1)
}

// HandleStackUnderflow handles stack underflow error
func HandleStackUnderflow(operation string) {
	fmt.Fprintf(os.Stderr, "Error: Stack underflow: %s\n", operation)
	os.Exit(1)
}

// HandleStackUnderflowWithCount handles stack underflow with element count details
func HandleStackUnderflowWithCount(requiredElements int, availableElements int) {
	fmt.Fprintf(os.Stderr, "Error: Stack underflow: need %d elements, have %d\n", requiredElements, availableElements)
	os.Exit(1)
}

// HandleDivisionByZero handles division by zero error
func HandleDivisionByZero() {
	fmt.Fprintf(os.Stderr, "Error: Division by zero\n")
	os.Exit(1)
}

// HandleVariableOutOfBounds handles variable index out of bounds error
func HandleVariableOutOfBounds(index int) {
	fmt.Fprintf(os.Stderr, "Error: Variable index out of bounds: %d (valid range: 0-63)\n", index)
	os.Exit(1)
}

// HandleInvalidVariableIndex handles invalid variable index error
func HandleInvalidVariableIndex(indexStr string) {
	fmt.Fprintf(os.Stderr, "Error: Invalid variable index: %s\n", indexStr)
	os.Exit(1)
}

// HandleInvalidNumericInput handles invalid numeric input error
func HandleInvalidNumericInput() {
	fmt.Fprintf(os.Stderr, "Error: Invalid numeric input\n")
	os.Exit(1)
}

// HandleUnknownInstruction handles unknown instruction error
func HandleUnknownInstruction(opcode string) {
	fmt.Fprintf(os.Stderr, "Error: Unknown instruction '%s'\n", opcode)
	os.Exit(1)
}

// HandleMissingArgument handles missing argument error
func HandleMissingArgument(instruction string) {
	fmt.Fprintf(os.Stderr, "Error: Expected argument after %s instruction\n", instruction)
	os.Exit(1)
}

// HandleMissingValue handles missing value error
func HandleMissingValue(instruction string) {
	fmt.Fprintf(os.Stderr, "Error: Expected value after %s instruction\n", instruction)
	os.Exit(1)
}

// HandleMissingLabel handles missing label error
func HandleMissingLabel(instruction string) {
	fmt.Fprintf(os.Stderr, "Error: Expected label after %s instruction\n", instruction)
	os.Exit(1)
}

// HandleMissingVariableIndex handles missing variable index error
func HandleMissingVariableIndex(instruction string) {
	fmt.Fprintf(os.Stderr, "Error: Expected variable index after %s instruction\n", instruction)
	os.Exit(1)
}
