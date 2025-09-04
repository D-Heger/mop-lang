package instructions

import (
	"fmt"
	"math"
	"strconv"

	"mop-lang/interpreter/go/internal/errors"
	"mop-lang/interpreter/go/internal/stack"
	"mop-lang/interpreter/go/internal/variables"
)

// Handler interface for instruction handlers
type Handler interface {
	Execute(program []any, pc int, labelTracker map[string]int, stack *stack.Stack, vars *variables.VariableTable) int
}

// PushHandler handles PUSH instruction
type PushHandler struct{}

func (h *PushHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingValue("PUSH")
	}
	number := program[pc].(float64)
	pc++
	s.Push(number)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// PopHandler handles POP instruction
type PopHandler struct{}

func (h *PopHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	s.Pop()
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// ArithmeticHandler base for arithmetic operations
type ArithmeticHandler struct {
	Operation func(a, b float64) float64
}

func (h *ArithmeticHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	s.SizeCheck(2)
	a := s.Pop()
	b := s.Pop()
	result := h.Operation(a, b)
	s.Push(result)
	vars.SetArithmeticFlag(result)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// AddHandler handles ADD instruction
type AddHandler struct {
	ArithmeticHandler
}

func NewAddHandler() *AddHandler {
	return &AddHandler{
		ArithmeticHandler{Operation: func(a, b float64) float64 { return a + b }},
	}
}

// SubHandler handles SUB instruction
type SubHandler struct {
	ArithmeticHandler
}

func NewSubHandler() *SubHandler {
	return &SubHandler{
		ArithmeticHandler{Operation: func(a, b float64) float64 { return b - a }},
	}
}

// MulHandler handles MUL instruction
type MulHandler struct {
	ArithmeticHandler
}

func NewMulHandler() *MulHandler {
	return &MulHandler{
		ArithmeticHandler{Operation: func(a, b float64) float64 { return a * b }},
	}
}

// DivHandler handles DIV instruction
type DivHandler struct{}

func (h *DivHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	s.SizeCheck(2)
	a := s.Pop()
	b := s.Pop()
	if a == 0 {
		vars.SetDivisionErrorFlag()
		errors.HandleDivisionByZero()
	}
	result := b / a
	s.Push(result)
	vars.SetArithmeticFlag(result)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// ModHandler handles MOD instruction
type ModHandler struct{}

func (h *ModHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	s.SizeCheck(2)
	a := s.Pop()
	b := s.Pop()
	if a == 0 {
		vars.SetDivisionErrorFlag()
		errors.HandleDivisionByZero()
	}
	result := math.Mod(b, a)
	s.Push(result)
	vars.SetArithmeticFlag(result)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// PrintHandler handles PRINT instruction
type PrintHandler struct{}

func (h *PrintHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingArgument("PRINT")
	}
	arg := program[pc].(string)
	pc++
	if arg == "TOP" {
		fmt.Println(s.Top())
	} else {
		fmt.Println(arg)
	}
	return pc
}

// ReadHandler handles READ instruction
type ReadHandler struct{}

func (h *ReadHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	var input string
	fmt.Scanln(&input)
	number, err := strconv.ParseFloat(input, 64)
	if err != nil {
		errors.HandleInvalidNumericInput()
	}
	s.Push(number)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// JumpHandler handles JUMP instruction
type JumpHandler struct{}

func (h *JumpHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingLabel("JUMP")
	}
	label := program[pc].(string)
	errors.ValidateLabelExists(label, labelTracker)
	newPC := labelTracker[label]
	errors.CheckPCBounds(newPC, len(program))
	return newPC
}

// ConditionalJumpHandler base for conditional jump instructions
type ConditionalJumpHandler struct {
	Instruction string
	Condition   func(float64) bool
}

func (h *ConditionalJumpHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingLabel(h.Instruction)
	}
	label := program[pc].(string)
	errors.ValidateLabelExists(label, labelTracker)
	number := s.Top()
	if h.Condition(number) {
		newPC := labelTracker[label]
		errors.CheckPCBounds(newPC, len(program))
		return newPC
	}
	return pc + 1
}

// JumpEqZeroHandler handles JUMP.EQ.0 instruction
type JumpEqZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpEqZeroHandler() *JumpEqZeroHandler {
	return &JumpEqZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.EQ.0",
			Condition:   func(n float64) bool { return n == 0 },
		},
	}
}

// JumpNeZeroHandler handles JUMP.NE.0 instruction
type JumpNeZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpNeZeroHandler() *JumpNeZeroHandler {
	return &JumpNeZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.NE.0",
			Condition:   func(n float64) bool { return n != 0 },
		},
	}
}

// JumpGtZeroHandler handles JUMP.GT.0 instruction
type JumpGtZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpGtZeroHandler() *JumpGtZeroHandler {
	return &JumpGtZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.GT.0",
			Condition:   func(n float64) bool { return n > 0 },
		},
	}
}

// JumpGeZeroHandler handles JUMP.GE.0 instruction
type JumpGeZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpGeZeroHandler() *JumpGeZeroHandler {
	return &JumpGeZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.GE.0",
			Condition:   func(n float64) bool { return n >= 0 },
		},
	}
}

// JumpLtZeroHandler handles JUMP.LT.0 instruction
type JumpLtZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpLtZeroHandler() *JumpLtZeroHandler {
	return &JumpLtZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.LT.0",
			Condition:   func(n float64) bool { return n < 0 },
		},
	}
}

// JumpLeZeroHandler handles JUMP.LE.0 instruction
type JumpLeZeroHandler struct {
	ConditionalJumpHandler
}

func NewJumpLeZeroHandler() *JumpLeZeroHandler {
	return &JumpLeZeroHandler{
		ConditionalJumpHandler{
			Instruction: "JUMP.LE.0",
			Condition:   func(n float64) bool { return n <= 0 },
		},
	}
}

// StoreHandler handles STORE instruction
type StoreHandler struct{}

func (h *StoreHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingVariableIndex("STORE")
	}
	varIndex := program[pc].(int)
	pc++
	value := s.Pop()
	vars.Store(varIndex, value)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// LoadHandler handles LOAD instruction
type LoadHandler struct{}

func (h *LoadHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingVariableIndex("LOAD")
	}
	varIndex := program[pc].(int)
	pc++
	value := vars.Load(varIndex)
	s.Push(value)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// StoreTopHandler handles STORE_TOP instruction
type StoreTopHandler struct{}

func (h *StoreTopHandler) Execute(program []any, pc int, labelTracker map[string]int, s *stack.Stack, vars *variables.VariableTable) int {
	if pc >= len(program) {
		errors.HandleMissingVariableIndex("STORE_TOP")
	}
	varIndex := program[pc].(int)
	pc++
	value := s.Top()
	vars.Store(varIndex, value)
	vars.UpdateSystemVars(s.CurrentSize(), s.Size())
	return pc
}

// Registry maps instructions to their handlers
type Registry struct {
	handlers map[string]Handler
}

// NewRegistry creates a new instruction registry
func NewRegistry() *Registry {
	registry := &Registry{
		handlers: make(map[string]Handler),
	}

	// Register all handlers
	registry.handlers["PUSH"] = &PushHandler{}
	registry.handlers["POP"] = &PopHandler{}
	registry.handlers["ADD"] = NewAddHandler()
	registry.handlers["SUB"] = NewSubHandler()
	registry.handlers["MUL"] = NewMulHandler()
	registry.handlers["DIV"] = &DivHandler{}
	registry.handlers["MOD"] = &ModHandler{}
	registry.handlers["PRINT"] = &PrintHandler{}
	registry.handlers["READ"] = &ReadHandler{}
	registry.handlers["JUMP"] = &JumpHandler{}
	registry.handlers["JUMP.EQ.0"] = NewJumpEqZeroHandler()
	registry.handlers["JUMP.NE.0"] = NewJumpNeZeroHandler()
	registry.handlers["JUMP.GT.0"] = NewJumpGtZeroHandler()
	registry.handlers["JUMP.GE.0"] = NewJumpGeZeroHandler()
	registry.handlers["JUMP.LT.0"] = NewJumpLtZeroHandler()
	registry.handlers["JUMP.LE.0"] = NewJumpLeZeroHandler()
	registry.handlers["STORE"] = &StoreHandler{}
	registry.handlers["LOAD"] = &LoadHandler{}
	registry.handlers["STORE_TOP"] = &StoreTopHandler{}

	return registry
}

// GetHandler retrieves the handler for an instruction
func (r *Registry) GetHandler(instruction string) Handler {
	if handler, exists := r.handlers[instruction]; exists {
		return handler
	}
	errors.HandleUnknownInstruction(instruction)
	return nil // This will never be reached due to os.Exit in HandleUnknownInstruction
}
