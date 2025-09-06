"""MOPLang interpreter package."""

from .interpreter_core import Interpreter
from .stack import Stack
from .variables import VariableTable
from .parser import parse_program
from .instruction_handlers import InstructionRegistry
from .error_handling import *

__all__ = [
    "Interpreter",
    "Stack",
    "VariableTable",
    "parse_program",
    "InstructionRegistry",
]
