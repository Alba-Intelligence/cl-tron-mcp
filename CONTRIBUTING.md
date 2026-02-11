# Contributing to CL-TRON-MCP

Thank you for your interest in contributing! This guide will help you get started.

## Table of Contents

- [Quick Start](#quick-start)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [Adding New Tools](#adding-new-tools)
- [Running Tests](#running-tests)
- [Code Style](#code-style)
- [Submitting Changes](#submitting-changes)

## Quick Start

### Prerequisites

- SBCL 2.4.0 or later
- Quicklisp
- Git

### Set Up Development Environment

```bash
# Clone the repository
git clone https://github.com/anomalyco/cl-tron-mcp.git
cd cl-tron-mcp

# Ensure Quicklisp can find the project
# Option 1: Symlink to local-projects
ln -sf "$(pwd)" ~/quicklisp/local-projects/cl-tron

# Option 2: Use QL-PACKAGE-LOCAL-PROJECTS
export QL_PACKAGE_LOCAL_PROJECTS="$(pwd)"

# Verify installation
sbcl --non-interactive \
  --eval "(ql:quickload :cl-tron-mcp :silent t)" \
  --eval "(cl-tron-mcp/monitor:health-check)"
```

## Development Setup

### Running Tests

```bash
# Run all tests
sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run :cl-tron-mcp/tests)"

# Run specific test suite
sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run 'cl-tron-mcp/tests/core-test)"
```

### Running the Tutorial

```bash
sbcl --load tutorial-run.lisp
```

### Development Server

```bash
# Start with stdio transport (for MCP clients)
sbcl --non-interactive \
  --eval "(ql:quickload :cl-tron-mcp)" \
  --eval "(cl-tron-mcp/core:start-server :transport :stdio)"
```

## Project Structure

```
cl-tron-mcp/
├── src/
│   ├── core/              # Server core (start/stop, config)
│   ├── transport/         # Transport layer (stdio, http, websocket)
│   ├── protocol/          # JSON-RPC 2.0 protocol handler
│   ├── tools/             # Tool registration
│   ├── security/          # Approval workflow, audit logging
│   ├── sbcl/              # SBCL integration utilities
│   ├── inspector/         # Object/class/function introspection
│   ├── debugger/          # Debugger integration
│   ├── tracer/            # Function tracing
│   ├── xref/              # Cross-reference tools
│   ├── monitor/           # Health checks, stats
│   ├── logging/           # Logging utilities
│   ├── repl/              # REPL integration
│   ├── hot-reload/        # Code reloading
│   ├── profiler/          # Profiling tools
│   └── threads/           # Thread utilities
├── tests/                 # Rove test suites
├── tutorial/              # Tutorial materials
├── DOCKER.md            # Docker deployment guide
└── README.md             # Main documentation
```

## Adding New Tools

### 1. Create the Tool Function

Create or modify a file in the appropriate `src/<category>/` directory:

```lisp
;;;; src/mytool/mytool.lisp

(in-package :cl-tron-mcp/mytool)

(defun my-new-tool (param1 param2)
  "Short description of what the tool does."
  (handler-case
      (progn
        ;; Tool implementation
        (list :result "success"
              :data param1))
    (error (e)
      (list :error t
            :message (princ-to-string e)))))
```

### 2. Register the Tool

Add to `src/tools/register-tools.lisp`:

```lisp
(register-tool
 "my_new_tool"
 "Description of what the tool does"
 :input-schema (list :param1 "string" :param2 "integer")
 :output-schema (list :type "object")
 :requires-approval nil)
(register-tool-handler "my_new_tool" (function cl-tron-mcp/mytool:my-new-tool))
```

### 3. Export the Package

Update `src/<category>/package.lisp`:

```lisp
(defpackage :cl-tron-mcp/mytool
  (:use :cl)
  (:export
   #:my-new-tool
   ;; ... other exports
   ))
```

### 4. Add Tests

Create tests in `tests/`:

```lisp
;;;; tests/mytool-test.lisp

(defpackage :cl-tron-mcp/tests/mytool-test
  (:use :cl :rove))

(in-package :cl-tron-mcp/tests/mytool-test)

(deftest my-new-tool-test
  (testing "My new tool works correctly"
    (ok (getf (cl-tron-mcp/mytool:my-new-tool "test" 42) :result))))
```

### 5. Update ASD File

Add the new component to `cl-tron-mcp.asd`:

```lisp
(asdf:defsystem :cl-tron-mcp
  ;; ...
  :components ((:file "src/mytool/package")
               (:file "src/mytool/mytool")
               ;; ... other components
               ))
```

## Running Tests

### All Tests

```bash
sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run :cl-tron-mcp/tests)"
```

### With Coverage

```bash
sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run :cl-tron-mcp/tests :coverage t)"
```

### Specific Suite

```bash
sbcl --non-interactive \
  --eval "(ql:quickload :rove :silent t)" \
  --eval "(ql:quickload :cl-tron-mcp/tests :silent t)" \
  --eval "(rove:run 'cl-tron-mcp/tests/core-test)"
```

## Code Style

Follow the [Google Common Lisp Style Guide](https://google.github.io/styleguide/lispguide.xml) with these additions:

### Naming Conventions

| Pattern | Example | Usage |
|---------|---------|-------|
| Packages | `:cl-tron-mcp/core` | Dash-separated |
| Functions | `start-server` | Lowercase with dashes |
| Predicates | `server-running-p` | End with `-p` |
| Constants | `+max-tools+` | Surrounded by `+` |
| Special vars | `*server-state*` | Surrounded by `*` |
| Conditions | `connection-error` | Lowercase with dashes |

### Formatting

- 2-space indent
- 100-character line limit
- One blank line between top-level forms
- Docstrings for all public functions

### Example

```lisp
(defun start-server (&key (transport :stdio) (port 8080))
  "Start the MCP server with the specified transport.

   TRANSPORT can be :stdio, :http, or :websocket.
   PORT is used for HTTP/WebSocket transports.

   Returns the server state."
  (when (eq *server-state* :running)
    (format t "[MCP] Server is already running~%")
    (return-from start-server))
  ;; ... implementation
  *server-state*)
```

## Submitting Changes

### Pull Request Process

1. **Fork the repository**
2. **Create a feature branch**
   ```bash
   git checkout -b feature/my-new-feature
   ```

3. **Make changes** following the code style guide

4. **Add tests** for new functionality

5. **Run tests** to ensure nothing is broken

6. **Update documentation** as needed

7. **Submit a pull request**

### Commit Message Format

```
[component]: Brief description

- Detailed changes
- Additional notes

Refs: #issue-number
```

#### Component Prefixes

- `transport` - Transport layer changes
- `protocol` - Protocol handler changes
- `tool` - Tool implementation
- `debugger` - Debugging tools
- `profiler` - Profiling tools
- `hot-reload` - Code reloading
- `security` - Approval workflow
- `doc` - Documentation updates
- `test` - Test additions
- `refactor` - Code improvements

### Example Commit

```
tool: Add memory profiling tool

- New memory-profiler component
- profile-memory function for heap analysis
- Integration with existing profiler tools

Refs: #42
```

## Getting Help

- Check [README.md](./README.md) for documentation
- Review [AGENTS.md](./AGENTS.md) for AI agent guidelines
- Open an issue for questions or problems
- Join the community Discord

## Resources

- [SBCL Manual](http://www.sbcl.org/manual/)
- [Quicklisp Documentation](https://www.quicklisp.org/beta/)
- [CLOS MOP Specification](https://clos-mop.hexstreamsoft.com/)
- [Model Context Protocol](https://modelcontextprotocol.io/)
