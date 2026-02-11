;;;; swank-tutorial.lisp - Tutorial: Using CL-TRON-MCP with Swank

;;; This tutorial demonstrates how to connect CL-TRON-MCP to a running
;;; SBCL instance with Swank loaded, enabling full IDE-like functionality.

;;; ============================================================
;;; BEFORE STARTING
;;; ============================================================
;;;
;;; You need TWO SBCL instances:
;;;
;;; 1. SBCL with Swank (the "target" - where code runs)
;;; 2. CL-TRON-MCP (the "agent" - sends commands via MCP)
;;;
;;; The target SBCL should have Swank loaded:
;;;
;;;   (ql:quickload :swank)
;;;   (swank:create-server :port 4005)
;;;
;;; CL-TRON-MCP will connect to port 4005 and send commands.

;;; ============================================================
;;; STEP 1: Set up the target SBCL with Swank
;;; ============================================================

;; In one terminal, start SBCL with Swank:

;; sbcl --eval "(ql:quickload :swank :silent t)" --eval "(swank:create-server :port 4005)"

;; You should see:
;; ;;; Swank started at port 4005.
;; ;;; Connections will be accepted on #<UC:SOCKET 127.0.0.1:4005>

;;; ============================================================
;;; STEP 2: Start CL-TRON-MCP
;;; ============================================================

;; In another terminal, start CL-TRON-MCP:

;; sbcl --load tutorial-run.lisp

;; Or start just the server:

;; (ql:quickload :cl-tron-mcp)
;; (cl-tron-mcp/core:start-server :transport :stdio)

;;; ============================================================
;;; STEP 3: Connect to Swank from CL-TRON-MCP
;;; ============================================================

;; In the MCP session (or via the Python client):

CL-USER> (ql:quickload :cl-tron-mcp :silent t)

;; Connect to the Swank server
CL-USER> (cl-tron-mcp/swank:swank-connect :port 4005)
;; => (:SUCCESS T :HOST "127.0.0.1" :PORT 4005 :MESSAGE "Connected to Swank at 127.0.0.1:4005")

;; Check connection status
CL-USER> (cl-tron-mcp/swank:swank-status)
;; => (:CONNECTED T :HAS-CONNECTION T)

;;; ============================================================
;;; STEP 4: Evaluate code via Swank
;;; ============================================================

;; Now you can evaluate Lisp code in the TARGET SBCL!

CL-USER> (cl-tron-mcp/swank:swank-eval "(+ 10 20 30)")
;; => (:VALUE "60" :THREAD :REPL-THREAD :PACKAGE "CL-USER")

;; Define a function in the target
CL-USER> (cl-tron-mcp/swank:swank-eval "(defun fib (n) (if (< n 2) n (+ (fib (1- n)) (fib (- n 2)))))" :package "CL-USER")
;; => (:VALUE "FIB" ...)

;; Call the function
CL-USER> (cl-tron-mcp/swank:swank-eval "(fib 10)" :package "CL-USER")
;; => (:VALUE "55" ...)

;;; ============================================================
;;; STEP 5: Debugging with Swank
;;; ============================================================

;; List all threads
CL-USER> (cl-tron-mcp/swank:mcp-swank-threads)
;; => (:THREADS ((:ID "main" :NAME "main thread" :STATUS :RUNNING) ...))

;; Get the current backtrace
CL-USER> (cl-tron-mcp/swank:mcp-swank-backtrace)
;; => (:BACKTRACE (...))

;; Interrupt a running computation
CL-USER> (cl-tron-mcp/swank:mcp-swank-interrupt)
;; => (:SUCCESS T :MESSAGE "Interrupt sent")

;; Abort a specific thread
CL-USER> (cl-tron-mcp/swank:mcp-swank-abort "worker-thread")
;; => (:SUCCESS T ...)

;;; ============================================================
;;; STEP 6: Inspection and Documentation
;;; ============================================================

;; Inspect an object
CL-USER> (cl-tron-mcp/swank:mcp-swank-inspect "*package*")
;; => (:INSPECTION (...))

;; Describe a symbol
CL-USER> (cl-tron-mcp/swank:mcp-swank-describe "mapcar")
;; => (:DESCRIPTION "MAPCAR function...")

;; Get documentation
CL-USER> (cl-tron-mcp/swank:mcp-swank-autodoc "mapcar")
;; => (:AUTODOC (...))

;; Get completions
CL-USER> (cl-tron-mcp/swank:mcp-swank-completions "mak")
;; => (:COMPLETIONS ("make-array" "make-hash-table" "make-list" ...))

;;; ============================================================
;;; STEP 7: Python Client Example
;;; ============================================================

;; Using the Python client:

python3
>>> from cl_tron_client import CLTronClient
>>> 
>>> # Connect to Swank via CL-TRON-MCP's MCP tools
>>> # First, ensure CL-TRON-MCP is running as MCP server
>>> # Then use swank_eval tool to execute code

# The Python client calls CL-TRON-MCP which forwards to Swank
# JSON-RPC request:
{
    "tool": "swank_eval",
    "arguments": {
        "code": "(defun hello () \"world\")",
        "package": "CL-USER"
    }
}

# Response:
{
    "result": {
        "value": "HELLO",
        "thread": ":repl-thread",
        "package": "CL-USER"
    }
}

;;; ============================================================
;;; EXAMPLE: Debugging a broken function
;;; ============================================================

;; Target SBCL has:
;; (defun divide (a b) (/ a b))

;; Agent sends:
CL-USER> (cl-tron-mcp/swank:swank-eval "(divide 10 0)" :package "CL-USER")
;; => (:ERROR T :MESSAGE "division by zero")

;; Get the backtrace to see where the error occurred
CL-USER> (cl-tron-mcp/swank:mcp-swank-backtrace)
;; => (:BACKTRACE ((:FRAME 0 :NAME "divide") (:FRAME 1 :NAME "top level form")))

;; Inspect the local variables
CL-USER> (cl-tron-mcp/swank:mcp-swank-frame-locals 0)
;; => (:LOCALS ((:NAME "A" :VALUE "10") (:NAME "B" :VALUE "0")))

;; Fix the function
CL-USER> (cl-tron-mcp/swank:swank-eval 
           "(defun divide (a b) (if (zerop b) (error \"Cannot divide by zero\") (/ a b)))"
           :package "CL-USER")
;; => (:VALUE "DIVIDE")

;; Test the fix
CL-USER> (cl-tron-mcp/swank:swank-eval "(divide 10 2)" :package "CL-USER")
;; => (:VALUE "5")

;;; ============================================================
;;; DISCONNECTING
;;; ============================================================

CL-USER> (cl-tron-mcp/swank:swank-disconnect)
;; => (:SUCCESS T :MESSAGE "Disconnected from Swank server")

;;; ============================================================
;;; SUMMARY
;;; ============================================================

;; The workflow is:
;;
;; 1. Start SBCL with Swank: (swank:create-server :port 4005)
;; 2. Start CL-TRON-MCP as MCP server
;; 3. Connect CL-TRON-MCP to Swank: (swank-connect :port 4005)
;; 4. Send code for evaluation: (swank-eval "(+ 1 2)")
;; 5. Debugank-backtrace), errors: (sw (swank-inspect ...)
;; 6. Disconnect when done: (swank-disconnect)
;;
;; This enables the agent to:
;; - Execute code in a running Lisp image
;; - Inspect objects and their state
;; - Debug errors with full backtraces
;; - Navigate the call stack
;; - Get documentation and completions
;;
;; Just like SLIME/SLY, but via MCP for AI agents!

;;; End of tutorial
