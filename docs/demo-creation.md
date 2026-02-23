# Creating Terminal Demos for Tron

## Current Setup

- **Tape files:** `demo/demo.tape`, `demo/demo-kilocode.tape`, `demo/demo-mcp-raw.tape`
- **Generate GIFs:** From project root run `./demo/generate.sh` (requires VHS; runs from `demo/`, creates `demo.gif`, `demo-kilocode.gif`, `demo-mcp-raw.gif`)
- **Demo scripts:** `demo/start-swank.lisp` (Swank on port 4005), `demo/demo-script.lisp`, `demo/demo-kilocode.lisp`, `demo/demo-mcp-raw.lisp`
- **Lisp export:** `cl-tron-mcp/demo:demo-export-vhs` can export a recorded session to a VHS tape file (see `src/demo/package.lisp`)

## Options

| Tool | Output | Pros | Cons |
|------|--------|------|------|
| **vhs** | GIF | Scripted, reproducible, beautiful | Requires Go |
| **asciinema** | Cast file, SVG | Standard, web embeddable | Needs conversion for GIF |
| **terminalizer** | GIF | Configurable styling | Node.js, heavy |
| **ttyrec + ttygif** | GIF | Lightweight | Manual recording |
| **demo-magic** | Live demo | Bash script, typed effect | Not a recording |

## Recommended: VHS

VHS lets you write a "tape" script and generates a GIF. Perfect for reproducible demos.

### Installation

```bash
# macOS/Linux
brew install vhs

# Or via Go
go install github.com/charmbracelet/vhs@latest
```

### Tape File for Tron Demo

Create `demo.tape`:

```
Output demo.gif
Set FontSize 16
Set FontFamily "JetBrains Mono"
Set Width 800
Set Height 500
Set Padding 10

Type "ai \"Debug factorial-example.lisp and fix it\""
Enter
Sleep 500ms

Type "  🔧 Connecting to Swank on port 4005... ✓"
Enter
Sleep 300ms

Type "  🔧 Running (factorial 7)..."
Enter
Sleep 400ms

Type "      ⚠️  ERROR: The value NIL is not of type NUMBER"
Enter
Type "      📍 Backtrace: (FACTORIAL 2) ← (FACTORIAL 3) ← ..."
Enter
Sleep 500ms

Type "  🔧 Inspecting frame 0..."
Enter
Type "      N = 2"
Enter
Sleep 300ms

Type "  🐛 Bug found: Line 4 has (1) called as function, returns NIL"
Enter
Type "     (if (> n 1) (* n (factorial (- n 1)) (1)))"
Enter
Type "                                          ^^^"
Enter
Sleep 400ms

Type "  🔧 Hot-reloading fixed function..."
Enter
Sleep 300ms

Type "  🔧 Verifying..."
Enter
Type "      (factorial 7)  → 5040      ✓"
Enter
Type "      (factorial 10) → 3628800   ✓"
Enter
Sleep 300ms

Type "  ✅ Done! Session preserved."
Enter

Sleep 1s
```

### Generate

```bash
vhs demo.tape
# Creates demo.gif
```

## Alternative: Asciinema + SVG

Better for web embedding (smaller, searchable):

```bash
# Install
pip install asciinema svg-term-cli

# Record (or use a script)
asciinema rec demo.cast

# Convert to SVG
svg-term --in demo.cast --out demo.svg
```

## Alternative: Live Script with demo-magic

For presentations where you want "live" typing:

```bash
# demo-magic.sh from https://github.com/darcyparker/demo-magic

#!/bin/bash
. demo-magic.sh

pei "$ ai \"Debug factorial-example.lisp\""
pei "$ 🔧 Connecting to Swank on port 4005... ✓"
pei "$ 🔧 Running (factorial 7)..."
pei "$     ⚠️  ERROR: The value NIL is not of type NUMBER"
# ... etc
```

## What Should Tron Provide?

### Option 1: Demo Tape Generator

A Lisp function that generates VHS tape files:

```lisp
(defun generate-demo-tape (output-file)
  "Generate a VHS tape file for Tron demo."
  (with-open-file (f output-file :direction :output)
    (format f "Output demo.gif~%")
    (format f "Set FontSize 16~%")
    ;; ... generate tape content
    ))
```

### Option 2: Live Demo Script

A shell script that runs the actual demo against a real Swank:

```bash
#!/bin/bash
# scripts/demo.sh - Run live demo

# Start Swank if not running
# Connect and run factorial example
# Show output in formatted way
```

### Option 3: MCP Tool for Demo

A tool that records and replays sessions:

```lisp
(register-tool "demo_record" ...)
(register-tool "demo_replay" ...)
(register-tool "demo_export" ...)
```
