# Hot Reload Tools

Tools for modifying Lisp code without restarting the application.

## Tools Overview

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `code_compile_string` | Compile and load Lisp code | Yes (:compile-file) |
| `code_load_fasl` | Load compiled FASL file | Yes (:modify-running-code) |
| `code_reload_package` | Reload entire package | Yes (:modify-running-code) |
| `code_reload_system` | Reload ASDF system | Yes (:modify-running-code) |
| `code_replace_function` | Atomically replace function | Yes (:modify-running-code) |
| `source_location` | Get source location for symbol | No |

## Workflow Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                    Hot Reload Workflow                           │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  1. LOCATE    →  2. READ  →  3. MODIFY  →  4. COMPILE        │
│     source_location    lisp-read-file   lisp-edit-form         │
│                                                           ↑    │
│  5. LOAD      ←  4. COMPILE                                   │
│     code_load_fasl                                                 │
│          ↓                                                       │
│  6. VERIFY    →  7. PERSIST                                    │
│     repl_eval          lisp-edit-form                          │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## code_compile_string

### Overview
Compile a string of Lisp code and load it into the running image.

### Tool Definition
```json
{
  "name": "code_compile_string",
  "description": "Compile Lisp code string and load into image",
  "parameters": {
    "type": "object",
    "properties": {
      "code": {
        "type": "string",
        "description": "Lisp code to compile"
      },
      "filename": {
        "type": "string",
        "description": "Source filename for location tracking"
      },
      "position": {
        "type": "integer",
        "description": "Line number for source tracking"
      }
    },
    "required": ["code"]
  }
}
```

### Approval Required
Requires `:compile-file` approval.

### User Prompt
```
AI agent wants to compile and load code:
Filename: src/core.lisp
Code: (defun process (x) ...)

Allow? [Yes/No] Timeout: 120s
```

## code_reload_package

### Overview
Reload an entire package from its source files.

### Tool Definition
```json
{
  "name": "code_reload_package",
  "description": "Reload all source files in package",
  "parameters": {
    "type": "object",
    "properties": {
      "package": {
        "type": "string",
        "description": "Package name to reload"
      },
      "recursive": {
        "type": "boolean",
        "description": "Also reload dependent packages",
        "default": false
      }
    },
    "required": ["package"]
  }
}
```

### Usage Example
```json
{
  "tool": "code_reload_package",
  "arguments": {
    "package": "MY-APP",
    "recursive": true
  }
}
```

## code_replace_function

### Overview
Atomically replace a function definition. Safe for running threads.

### Tool Definition
```json
{
  "name": "code_replace_function",
  "description": "Atomically replace function definition",
  "parameters": {
    "type": "object",
    "properties": {
      "function": {
        "type": "string",
        "description": "Function symbol to replace"
      },
      "new_code": {
        "type": "string",
        "description": "New function definition"
      }
    },
    "required": ["function", "new_code"]
  }
}
```

### Safety Guarantees
- Atomic: New calls get new definition immediately
- Threads executing old code continue with old code
- No window of inconsistency

## Common Patterns

### Pattern 1: Safe Function Update
```json
{
  "tool": "code_replace_function",
  "arguments": {
    "function": "my-app:compute",
    "new_code": "(defun my-app:compute (x) (+ x 1))"
  }
}
```

### Pattern 2: Package Hot Reload
```json
{
  "tool": "code_reload_package",
  "arguments": {
    "package": "MY-APP"
  }
}
```

### Pattern 3: System Reload with Dependencies
```json
{
  "tool": "code_reload_system",
  "arguments": {
    "system": "my-app",
    "force": true
  }
}
```

## Error Handling

### Common Errors

**Compilation error:**
```json
{
  "error": {
    "code": -32000,
    "message": "Compilation failed",
    "data": {
      "type": "COMPILATION_ERROR",
      "message": "Unknown symbol: unknwon-var",
      "location": {
        "file": "src/core.lisp",
        "line": 42
      }
    }
  }
}
```

**Package not found:**
```json
{
  "error": {
    "code": -32000,
    "message": "Package UNKNOWN-PACKAGE not found",
    "data": {
      "type": "PACKAGE_NOT_FOUND"
    }
  }
}
```

## See Also

- @prompts/hot-reload-development.md - Complete hot reload workflows
- @agents/hot-reload-specialist.md - Live code modification expert
- docs/security.md - Approval workflow
