# Variables
SCRIPTS_PATH=mopl-examples/

INTERPRETER_PATH=interpreter/
GO_INTERPRETER=$(INTERPRETER_PATH)go/interpreter.go
PY_INTERPRETER=$(INTERPRETER_PATH)python/interpreter.py

TESTS_PATH=tests/
TEST_RUNNER=$(TESTS_PATH)test_runner.py

# Default script (can be overridden)
SCRIPT?=$(SCRIPTS_PATH)count_down.mopl

.PHONY: interpret-go interpret-python test clean-test clean

# ============================================================================
# Interpreter targets
# ============================================================================

# Run the Go interpreter with a script name argument
interpret-go:
	@if [ -z "$(script)" ] && [ -z "$(SCRIPT)" ]; then \
	  echo "Error: script argument is required."; exit 1; \
	fi
	go run $(GO_INTERPRETER) $${script:-$(SCRIPT)}

# Run the Python interpreter with a script name argument
interpret-py:
	@if [ -z "$(script)" ] && [ -z "$(SCRIPT)" ]; then \
	  echo "Error: script argument is required."; exit 1; \
	fi
	python3 $(PY_INTERPRETER) $${script:-$(SCRIPT)}

# ============================================================================
# Testing targets
# ============================================================================

# Run all tests for both interpreters
test:
	python3 $(TEST_RUNNER)

clean-test:
	rm -f tests/test_results.json

# ============================================================================
# Util targets
# ============================================================================

clean: clean-test
	@echo "Cleaning up..."