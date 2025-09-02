# MOPL VSCode Extension

This extension provides syntax highlighting and language support for MOPLang (My Own Programming Language) in Visual Studio Code.

## Features

- **Syntax Highlighting**: Full syntax highlighting for all MOPL language constructs
  - Comments (`;`)
  - Labels (`label:`)
  - Stack operations (`PUSH`, `POP`)
  - Arithmetic operations (`ADD`, `SUB`, `MUL`, `DIV`, `MOD`)
  - Jump instructions (`JUMP`, `JUMP.EQ.0`, `JUMP.NE.0`, `JUMP.GT.0`, `JUMP.GE.0`, `JUMP.LT.0`, `JUMP.LE.0`)
  - I/O operations (`PRINT`, `READ`)
  - Variable operations (`STORE`, `STORE_TOP`, `LOAD`)
  - Numbers (integers and decimals with optional signs)
  - Strings with escape sequences (`\n`, `\t`, `\\`, `\"`)
  - The `TOP` keyword
  - `HALT` instruction

- **Language Configuration**:
  - Line comments with `;`
  - Auto-closing pairs for strings and brackets
  - Proper bracket matching

## Installation

### Prerequisites

- Visual Studio Code (version 1.103.0 or higher)
- Node.js and npm (for building and packaging)

### Install from Source

To build, package, and install the extension:

1. **Install vsce (VSCode Extension CLI)**:

   ```bash
   npm install -g vsce
   ```

2. **Navigate to the extension directory**:

   ```bash
   cd vscode-mopl-extension/mopl-support
   ```

3. **Package the extension**:

   ```bash
   vsce package --allow-missing-repository --baseContentUrl file://.
   ```

   This creates `mopl-support-v.v.v.vsix` in the current directory.

4. **Install the packaged extension**:

   ```bash
   code --install-extension mopl-support-v.v.v.vsix
   ```

5. **Verify Installation**:
   - Create a new file with `.mopl` extension
   - Write some MOPL code (try the examples from [`../../mopl_examples/`](../../mopl_examples/))
   - Verify that syntax highlighting appears correctly

6. **Test with Sample Files**:
   - Open [`test-syntax.mopl`](test-syntax.mopl) to see all language features highlighted
   - Try opening example programs from the main project

### Development Installation

For extension development and testing:

1. **Clone or download** the extension source code to your local machine

2. **Open the extension in VSCode**:

   ```bash
   cd vscode-mopl-extension/mopl-support
   code .
   ```

3. **Launch Extension Development Host**:
   - Press `F5` in VSCode, or
   - Go to **Run and Debug** view (Ctrl+Shift+D)
   - Select "Extension" configuration
   - Click the green "Start Debugging" button

   This will open a new VSCode window titled **"[Extension Development Host]"** with the MOPL extension loaded.

4. **Test the syntax highlighting**:
   - In the Extension Development Host window, open the test file: [`test-syntax.mopl`](test-syntax.mopl)
   - Create a new file with `.mopl` extension
   - Write MOPL code and verify syntax highlighting works correctly

5. **Test language features**:
   - Verify that comments (`;`) are properly highlighted
   - Test that strings with escape sequences display correctly
   - Check that all keywords are highlighted with appropriate colors
   - Verify that labels and numbers are properly recognized

### File Structure

```scalar
mopl-support/
├── .vscode/
│   ├── launch.json                                # Debug configuration
│   └── settings.json                              # Workspace settings
├── syntaxes/
│   └── mopl.tmLanguage.json                       # TextMate grammar definition
├── test-syntax.mopl                               # Comprehensive test file
├── .gitignore                                     # Git ignore rules
├── .vscodeignore                                  # VSCode packaging ignore rules
├── language-configuration.json                    # Language configuration
├── package.json                                   # Extension manifest
├── README.md                                      # This file
└── mopl-support-v.v.v.vsix                        # Generated package (not in git)
```

**Note**: The `.vsix` file is generated during the packaging process and should not be committed to the git repository.

### Making Changes

1. **Modify the grammar**: Edit [`syntaxes/mopl.tmLanguage.json`](syntaxes/mopl.tmLanguage.json)
2. **Update language configuration**: Edit [`language-configuration.json`](language-configuration.json)
3. **Test changes**: Press `Ctrl+R` in the Extension Development Host to reload with changes
4. **Debug**: Use the Debug Console in the main VSCode window to see any errors

### Updating Extension Versions

When updating the extension:

1. Update the version in [`package.json`](package.json)
2. Follow the installation steps above to rebuild and reinstall
3. The new `.vsix` file will reflect the updated version number

## Language Grammar Details

The extension uses a TextMate grammar defined in JSON format. Key patterns include:

- **Comments**: `^;.*$` - Lines starting with semicolon
- **Labels**: `^\\s*([a-zA-Z_][a-zA-Z0-9_]*)\\s*:` - Identifier followed by colon
- **Keywords**: Various instruction patterns with proper word boundaries
- **Numbers**: Support for integers and decimals with optional signs
- **Strings**: Double-quoted strings with escape sequence support
- **Conditional Jumps**: Proper dot notation (e.g., `JUMP.EQ.0`)

## Testing Strategy

The [`test-syntax.mopl`](test-syntax.mopl) file contains comprehensive examples of all MOPL language constructs:

- All instruction types with various argument formats
- Complex label definitions and references
- Number formats (positive, negative, integers, decimals)
- String literals with all supported escape sequences
- Complete example programs demonstrating real usage

Use this file to verify that syntax highlighting works correctly for all language features.

## Troubleshooting

### Common Issues

1. **Syntax highlighting not working**:
   - Ensure the file has `.mopl` extension
   - Check that the extension is loaded in the Extension Development Host
   - Reload the Extension Development Host with `Ctrl+R`

2. **Grammar changes not reflected**:
   - Save all files before testing
   - Reload the Extension Development Host
   - Check the Debug Console for JSON syntax errors

3. **Extension not loading**:
   - Check [`package.json`](package.json) for syntax errors
   - Verify the VSCode version requirements
   - Look for errors in the Debug Console

### Debug Console

Use **Help > Toggle Developer Tools** in the Extension Development Host to access the Debug Console for detailed error messages.

## Contributing

When modifying the extension:

1. Test thoroughly with the provided test file
2. Ensure all MOPL language constructs are properly highlighted
3. Verify that the grammar matches the EBNF specification
4. Update this documentation if adding new features

## License

This extension is part of the MOPLang project. See the main project LICENSE file for details.
