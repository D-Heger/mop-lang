"""Variable table implementation for the MOPLang interpreter."""

from .error_handling import handle_variable_out_of_bounds


class VariableTable:
    """Variable table implementation with system variables."""
    
    def __init__(self):
        self.variables = [0.0 for _ in range(64)]  # 64 variables, initialized to 0
        
    def store(self, index, value):
        """Store a value in the specified variable index."""
        if index < 0 or index > 63:
            handle_variable_out_of_bounds(index)
        self.variables[index] = value
        
    def load(self, index):
        """Load a value from the specified variable index."""
        if index < 0 or index > 63:
            handle_variable_out_of_bounds(index)
        return self.variables[index]
        
    def update_system_vars(self, stack):
        """Update system variables automatically."""
        # Update system variables automatically
        self.variables[2] = stack.current_size()  # VAR_2: Current stack size
        self.variables[6] = stack.size  # VAR_6: Stack capacity
    
    def set_arithmetic_flag(self, result):
        """Update arithmetic flags based on the result."""
        if result == 0:
            self.store(1, 0)  # VAR_1: zero
        elif result > 0:
            self.store(1, 1)  # VAR_1: positive
        else:
            self.store(1, 2)  # VAR_1: negative
    
    def set_division_error_flag(self):
        """Set the division by zero error flag."""
        self.store(4, 1)  # VAR_4: division by zero error
