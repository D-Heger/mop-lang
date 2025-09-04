package variables

import "mop-lang/interpreter/go/internal/errors"

// VariableTable represents a table of 64 variables with system variable support
type VariableTable struct {
	variables []float64
}

// New creates a new variable table
func New() *VariableTable {
	return &VariableTable{
		variables: make([]float64, 64), // 64 variables, initialized to 0
	}
}

// Store stores a value at the specified variable index
func (v *VariableTable) Store(index int, value float64) {
	if index < 0 || index > 63 {
		errors.HandleVariableOutOfBounds(index)
	}
	v.variables[index] = value
}

// Load retrieves a value from the specified variable index
func (v *VariableTable) Load(index int) float64 {
	if index < 0 || index > 63 {
		errors.HandleVariableOutOfBounds(index)
	}
	return v.variables[index]
}

// UpdateSystemVars updates system variables automatically
func (v *VariableTable) UpdateSystemVars(stackSize int, stackCapacity int) {
	// Update system variables automatically
	v.variables[2] = float64(stackSize)     // VAR_2: Current stack size
	v.variables[6] = float64(stackCapacity) // VAR_6: Stack capacity
}

// SetArithmeticFlag updates arithmetic flags based on the result
func (v *VariableTable) SetArithmeticFlag(result float64) {
	if result == 0 {
		v.Store(1, 0) // VAR_1: zero
	} else if result > 0 {
		v.Store(1, 1) // VAR_1: positive
	} else {
		v.Store(1, 2) // VAR_1: negative
	}
}

// SetDivisionErrorFlag sets the division by zero error flag
func (v *VariableTable) SetDivisionErrorFlag() {
	v.Store(4, 1) // VAR_4: division by zero error
}
