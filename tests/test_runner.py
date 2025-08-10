from dataclasses import dataclass
import difflib
from enum import Enum
import json
from pathlib import Path
import subprocess
from time import time
from typing import Dict, List, Optional
import yaml
import argparse

@dataclass
class TestCase:
    name: str
    mopl_file: str
    expected_file: str
    input_file: Optional[str] = None
    error_file: Optional[str] = None
    expected_exit_code: int = 0
    timeout: float = 5.0

class TestResult(Enum):
    PASSED = "✅"
    FAILED = "❌"
    SKIPPED = "⏭️"
    ERROR = "⚠️"

@dataclass
class TestReport:
    interpreter: str
    test_name: str
    result: TestResult
    actual_output: str = ""
    expected_output: str = ""
    actual_exit_code: int = 0
    error_message: str = ""
    execution_time: float = 0.0

class TestRunner:
    def __init__(self, manifest_path: str, test_dir: str):
        self.manifest_path = Path(manifest_path)
        self.test_dir = Path(test_dir)
        self.test_cases = []
        self.interpreters = {}

    def load_manifest(self):
        with open(self.manifest_path, "r") as file:
            manifest = yaml.safe_load(file)

        for suite_name, suite_data in manifest["test_suites"].items():
            for test_data in suite_data["tests"]:
                test_case = TestCase(
                    name = f"{suite_name}/{test_data['name']}",
                    mopl_file=self.test_dir / "test_cases" / test_data['file'],
                    expected_file=self.test_dir / "test_cases" / test_data['file'].replace('.mopl', '.expected'),
                    input_file=self.test_dir / "test_cases" / test_data.get('input_file') if test_data.get('input_file') else None,
                    error_file=self.test_dir / "test_cases" / test_data.get('error_file') if test_data.get('error_file') else None,
                    expected_exit_code=test_data.get("expected_exit_code", 0),
                    timeout=test_data.get("timeout", 5.0)
                )
                self.test_cases.append(test_case)

    def register_interpreter(self, name: str, command: List[str]):
        self.interpreters[name] = command

    def run_test(self, test_case: TestCase, interpreter_name: str, interpreter_cmd: List[str]) -> TestReport:
        report = TestReport(
            interpreter=interpreter_name,
            test_name=test_case.name,
            result = TestResult.ERROR
        )

        try:
            cmd = interpreter_cmd + [str(test_case.mopl_file)]

            stdin_data = None
            if test_case.input_file and Path(test_case.input_file).exists():
                with open(test_case.input_file, "r") as file:
                    stdin_data = file.read()
            start_time = time()
            result = subprocess.run(
                cmd,
                input=stdin_data,
                capture_output=True,
                text=True,
                timeout=test_case.timeout
            )
            report.execution_time = time() - start_time

            report.actual_output = result.stdout
            report.actual_exit_code = result.returncode

            if Path(test_case.expected_file).exists():
                with open(test_case.expected_file, "r") as file:
                    report.expected_output = file.read()

            output_matches = report.actual_output.strip() == report.expected_output.strip()
            exit_code_matches = report.actual_exit_code == test_case.expected_exit_code

            if output_matches and exit_code_matches:
                report.result = TestResult.PASSED
            else:
                report.result = TestResult.FAILED
                if not output_matches:
                    report.error_message = "Output mismatch"
                if not exit_code_matches:
                    report.error_message = f" Exit code: expected {test_case.expected_exit_code}, got {report.actual_exit_code}"
        
        except subprocess.TimeoutExpired:
            report.result = TestResult.ERROR
            report.error_message = f"Test timed out after {test_case.timeout} seconds"
        except Exception as e:
            report.result = TestResult.ERROR
            report.error_message = str(e)

        return report
    
    def run_all_tests(self) -> Dict[str, List[TestReport]]:
        results = {}

        for interpreter_name, interpreter_cmd in self.interpreters.items():
            print(f"\n{'='*60}")
            print(f"Testing {interpreter_name} interpreter")
            print(f"{'='*60}")

            interpreter_results = []

            for test_case in self.test_cases:
                report = self.run_test(test_case, interpreter_name, interpreter_cmd)
                interpreter_results.append(report)

                # Print progress
                print(f"{report.result.value} {test_case.name}", end="")
                if report.result == TestResult.FAILED:
                    print(f" - {report.error_message}", end="")
                print(f" ({report.execution_time:.3f}s)")

                # Always show diff for failures
                if report.result == TestResult.FAILED:
                    print("\n  Expected vs Actual:")
                    for line in difflib.unified_diff(
                        report.expected_output.splitlines(),
                        report.actual_output.splitlines(),
                        lineterm='',
                        fromfile='expected',
                        tofile='actual'
                    ):
                        print(f"  {line}")
            results[interpreter_name] = interpreter_results

        return results
    
    def print_summary(self, results: Dict[str, List[TestReport]]):
        print(f"\n{'='*60}")
        print("TEST SUMMARY")
        print(f"{'='*60}")
        
        for interpreter_name, reports in results.items():
            passed = sum(1 for r in reports if r.result == TestResult.PASSED)
            failed = sum(1 for r in reports if r.result == TestResult.FAILED)
            errors = sum(1 for r in reports if r.result == TestResult.ERROR)
            total = len(reports)
            total_time = sum(r.execution_time for r in reports)
            
            print(f"\n{interpreter_name}:")
            print(f"  Passed: {passed}/{total}")
            print(f"  Failed: {failed}/{total}")
            print(f"  Errors: {errors}/{total}")
            print(f"  Success Rate: {(passed/total)*100:.1f}%")
            print(f"  Total Execution Time: {total_time:.3f}s")
            
            if failed > 0:
                print(f"\n  Failed tests:")
                for report in reports:
                    if report.result == TestResult.FAILED:
                        print(f"    - {report.test_name}: {report.error_message}")

if __name__ == "__main__":
    # Setup test runner
    runner = TestRunner("tests/test_manifest.yaml", "tests")
    runner.load_manifest()

    # Register interpreters
    runner.register_interpreter("python", ["python3", "interpreter/python/interpreter.py"])
    runner.register_interpreter("go", ["go", "run", "interpreter/go/interpreter.go"])

    # Run tests
    results = runner.run_all_tests()

    # Print summary
    runner.print_summary(results)

    # Always export JSON results to file in tests directory
    json_results = {}
    for interpreter, reports in results.items():
        total_time = sum(r.execution_time for r in reports)
        json_results[interpreter] = {
            "total_execution_time": total_time,
            "tests": [
                {
                    "test": r.test_name,
                    "result": r.result.name,
                    "time": r.execution_time,
                    "error": r.error_message
                }
                for r in reports
            ]
        }
    with open("tests/test_results.json", 'w') as f:
        json.dump(json_results, f, indent=2)