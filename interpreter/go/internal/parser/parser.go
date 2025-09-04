package parser

import (
	"bufio"
	"os"
	"strconv"
	"strings"

	"mop-lang/interpreter/go/internal/errors"
)

// ParseStringLiteral parses escape sequences in string literals according to MOPLang semantics
func ParseStringLiteral(s string) string {
	var result strings.Builder
	i := 0
	for i < len(s) {
		if s[i] == '\\' && i+1 < len(s) {
			// Handle escape sequences
			nextChar := s[i+1]
			switch nextChar {
			case 'n':
				result.WriteByte('\n')
			case 't':
				result.WriteByte('\t')
			case '\\':
				result.WriteByte('\\')
			case '"':
				result.WriteByte('"')
			default:
				// Unknown escape sequence, keep both characters as-is
				result.WriteByte(s[i])
				result.WriteByte(nextChar)
			}
			i += 2
		} else {
			result.WriteByte(s[i])
			i++
		}
	}
	return result.String()
}

// ReadAndStripComments reads file lines and strips comments
func ReadAndStripComments(programFilepath string) ([]string, error) {
	file, err := os.Open(programFilepath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	var programLines []string
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		// remove anything after ';'
		line = strings.Split(line, ";")[0]
		// skip empty lines
		if line == "" {
			continue
		}
		programLines = append(programLines, line)
	}

	if err := scanner.Err(); err != nil {
		return nil, err
	}

	return programLines, nil
}

// TokenizeProgram tokenizes the input program
func TokenizeProgram(programLines []string) ([]any, map[string]int) {
	var program []any
	tokenCounter := 0
	labelTracker := make(map[string]int)

	for _, line := range programLines {
		parts := strings.Split(line, " ")
		opcode := parts[0]

		// check for empty line
		if opcode == "" {
			continue
		}

		// check for label
		if strings.HasSuffix(opcode, ":") {
			labelName := opcode[:len(opcode)-1]
			labelTracker[labelName] = tokenCounter
			continue
		}

		// store opcode token
		program = append(program, opcode)
		tokenCounter++

		// handle opcodes
		switch opcode {
		case "PUSH":
			// expecting a number (support float)
			numberStr := parts[1]
			number, err := strconv.ParseFloat(numberStr, 64)
			if err != nil {
				errors.HandleInvalidNumericInput()
			}
			program = append(program, number)
			tokenCounter++

		case "PRINT":
			// either we get `PRINT TOP` or `PRINT "some string"`
			if len(parts) == 2 && parts[1] == "TOP" {
				program = append(program, "TOP")
				tokenCounter++
			} else {
				// parse string literal
				stringLiteral := strings.Join(parts[1:], " ")
				stringLiteral = strings.TrimSpace(stringLiteral)
				if len(stringLiteral) >= 2 && strings.HasPrefix(stringLiteral, `"`) && strings.HasSuffix(stringLiteral, `"`) {
					stringLiteral = strings.TrimPrefix(stringLiteral, `"`)
					stringLiteral = strings.TrimSuffix(stringLiteral, `"`)
				}
				// Parse escape sequences
				stringLiteral = ParseStringLiteral(stringLiteral)
				program = append(program, stringLiteral)
				tokenCounter++
			}

		case "JUMP", "JUMP.EQ.0", "JUMP.NE.0", "JUMP.GT.0", "JUMP.GE.0", "JUMP.LT.0", "JUMP.LE.0":
			// read label
			label := parts[1]
			program = append(program, label)
			tokenCounter++

		case "STORE", "LOAD", "STORE_TOP":
			// read variable index
			varIndexStr := parts[1]
			varIndex, err := strconv.Atoi(varIndexStr)
			if err != nil {
				errors.HandleInvalidVariableIndex(varIndexStr)
			}
			if varIndex < 0 || varIndex > 63 {
				errors.HandleVariableOutOfBounds(varIndex)
			}
			program = append(program, varIndex)
			tokenCounter++
		}
	}

	return program, labelTracker
}

// ParseProgram parses a MOPLang program file into tokens and labels
func ParseProgram(programFilepath string) ([]any, map[string]int, error) {
	programLines, err := ReadAndStripComments(programFilepath)
	if err != nil {
		return nil, nil, err
	}

	program, labelTracker := TokenizeProgram(programLines)
	return program, labelTracker, nil
}
