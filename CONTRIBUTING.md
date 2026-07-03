# Contributing to CL-TRON-MCP

This guide is for contributors who want to set up the project, run tests, and submit changes. For the subsystem map and file-by-file orientation, read [docs/DEVELOPERS.md](docs/DEVELOPERS.md) and [docs/code-reference.md](docs/code-reference.md).

## Prerequisites

- Common Lisp implementation: **SBCL** recommended, **ECL** supported for parts of the server
- [Quicklisp](https://www.quicklisp.org/beta/)
- Git
- Optional: [devenv](https://devenv.sh) if you want the Nix-based development shell

## Development Setup

### Quicklisp-first setup

```bash
git clone https://github.com/Alba-Intelligence/cl-tron-mcp.git \
  ~/quicklisp/local-projects/cl-tron-mcp
cd ~/quicklisp/local-projects/cl-tron-mcp
```

If you keep the repository somewhere else, either symlink it into `~/quicklisp/local-projects/` or push the directory into `ql:*local-project-directories*`.

### Verify the system loads

```bash
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(ql:quickload :cl-tron-mcp :silent t)'
```

### Optional `devenv` workflow

```bash
devenv shell
./start-mcp.sh --stdio-only
```

Use this only if you want the packaged shell environment. It is not required for normal Quicklisp development.

## Running Tests

### Full test suite

```bash
cd ~/quicklisp/local-projects/cl-tron-mcp
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(pushnew (truename ".") ql:*local-project-directories* :test (function equal))' \
  --eval '(asdf:test-system :cl-tron-mcp)'
```

### Run a focused Rove test

```bash
cd ~/quicklisp/local-projects/cl-tron-mcp
sbcl --non-interactive \
  --eval '(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))' \
  --eval '(pushnew (truename ".") ql:*local-project-directories* :test (function equal))' \
  --eval '(ql:quickload :cl-tron-mcp/tests :silent t)' \
  --eval '(rove:run-test (quote cl-tron-mcp/tests::compile-and-load-test))'
```

### Swank integration tests

Some tests expect a live Swank server on `127.0.0.1:4006`:

```lisp
(ql:quickload :swank)
(swank:create-server :port 4006 :dont-close t)
```

## Local Startup Checks

### Verify stdio output

```bash
echo '{"jsonrpc":"2.0","method":"initialize","params":{},"id":1}' | \
  ./start-mcp.sh --stdio-only 2>/dev/null | head -1
```

### Verify HTTP health

```bash
./start-mcp.sh
curl http://127.0.0.1:4006/health
```

## Change Guidelines

1. Add regression tests before fixing bugs.
2. Keep the one-session Swank workflow intact; avoid changes that assume Tron owns the Lisp state.
3. Update user-facing docs when startup, transport, tool behavior, or approval flow changes.
4. Add or update tool reference pages in `docs/tools/` when the MCP surface changes.

## Where to Go Next

- [docs/DEVELOPERS.md](docs/DEVELOPERS.md) - subsystem map and extension points
- [docs/code-reference.md](docs/code-reference.md) - source-level index
- [docs/tools/index.md](docs/tools/index.md) - tool reference
