"""Stack implementation for the MOPLang interpreter."""

from .error_handling import handle_stack_overflow, handle_stack_underflow, handle_stack_underflow_with_count


class Stack:
    """Stack implementation with overflow and underflow checking."""
    
    def __init__(self, size):
        self.buffer = [0 for _ in range(size)]  # create buffer for the specified stack size and fill it with 0
        self.stack_pointer = -1  # start sp at -1 so that we can properly start with 0, as it always should be
        self.size = size

    def push(self, number):
        """Push a number onto the stack."""
        # Check for stack overflow
        if self.stack_pointer + 1 >= self.size:
            handle_stack_overflow()
        self.stack_pointer += 1
        self.buffer[self.stack_pointer] = number

    def pop(self):
        """Pop a number from the stack."""
        # Check for stack underflow
        if self.stack_pointer < 0:
            handle_stack_underflow("pop from empty stack")
        number = self.buffer[self.stack_pointer]
        self.stack_pointer -= 1
        return number
    
    def top(self):
        """Get the top element of the stack without removing it."""
        # Check for stack underflow
        if self.stack_pointer < 0:
            handle_stack_underflow("access top of empty stack")
        return self.buffer[self.stack_pointer]
    
    def size_check(self, required_elements):
        """Check if stack has enough elements."""
        if self.stack_pointer + 1 < required_elements:
            handle_stack_underflow_with_count(required_elements, self.stack_pointer + 1)

    def current_size(self):
        """Get the current number of elements in the stack."""
        return self.stack_pointer + 1
