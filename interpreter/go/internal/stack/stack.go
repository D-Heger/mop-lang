package stack

import "mop-lang/interpreter/go/internal/errors"

// Stack represents a stack data structure with overflow and underflow checking
type Stack struct {
	buffer       []float64
	stackPointer int
	size         int
}

// New creates a new stack with the specified size
func New(size int) *Stack {
	return &Stack{
		buffer:       make([]float64, size),
		stackPointer: -1,
		size:         size,
	}
}

// Push adds a number to the top of the stack
func (s *Stack) Push(number float64) {
	// Check for stack overflow
	if s.stackPointer+1 >= s.size {
		errors.HandleStackOverflow()
	}
	s.stackPointer++
	s.buffer[s.stackPointer] = number
}

// Pop removes and returns the top element from the stack
func (s *Stack) Pop() float64 {
	// Check for stack underflow
	if s.stackPointer < 0 {
		errors.HandleStackUnderflow("cannot pop from empty stack")
	}
	number := s.buffer[s.stackPointer]
	s.stackPointer--
	return number
}

// Top returns the top element of the stack without removing it
func (s *Stack) Top() float64 {
	// Check for stack underflow
	if s.stackPointer < 0 {
		errors.HandleStackUnderflow("cannot access top of empty stack")
	}
	return s.buffer[s.stackPointer]
}

// SizeCheck verifies that the stack has enough elements
func (s *Stack) SizeCheck(requiredElements int) {
	// Check if stack has enough elements
	if s.stackPointer+1 < requiredElements {
		errors.HandleStackUnderflowWithCount(requiredElements, s.stackPointer+1)
	}
}

// CurrentSize returns the current number of elements in the stack
func (s *Stack) CurrentSize() int {
	return s.stackPointer + 1
}

// Size returns the maximum capacity of the stack
func (s *Stack) Size() int {
	return s.size
}
