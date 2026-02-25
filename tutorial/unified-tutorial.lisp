;;;; tutorial/unified-tutorial.lisp - Unified REPL Interface Tutorial

;; This tutorial demonstrates the unified REPL interface
;; that works with both Swank and nrepl transparently.

;; Prerequisites:
;; 1. SBCL with Quicklisp
;; 2. CL-TRON-MCP loaded: (ql:quickload :cl-tron-mcp)
;; 3. A running REPL server (Swank or nrepl)

;; ============================================================
;; Part 1: Starting a REPL Server
;; ============================================================

;; Option A: Swank (Slime, Portacle)
;; (ql:quickload :swank)
;; (swank:create-server :port 4006)

;; Option B: nrepl (Sly, CIDER)
;; (ql:quickload :sly)
;; (sly:nrepl-start :port 7888)

;; ============================================================
;; Part 2: Connecting with Unified Interface
;; ============================================================

;; Auto-detect (recommended for beginners)
;; (cl-tron-mcp/unified:repl-connect :port 4006)
;; ;; => (:SUCCESS T :TYPE :SWANK :HOST "127.0.0.1" :PORT 4006 ...)

;; Explicit type specification
;; (cl-tron-mcp/unified:repl-connect :type :swank :port 4006)
;; ;; => (:SUCCESS T :TYPE :SWANK ...)

;; (cl-tron-mcp/unified:repl-connect :type :nrepl :port 7888)
;; ;; => (:SUCCESS T :TYPE :NREPL ...)

;; Check status
;; (cl-tron-mcp/unified:repl-status)
;; ;; => (:CONNECTED T :TYPE :SWANK :HOST "127.0.0.1" :PORT 4006)

;; ============================================================
;; Part 3: Using Unified Tools
;; ============================================================

;; The same API works regardless of which REPL you're connected to!

;; Evaluate code
;; (cl-tron-mcp/unified:repl-eval :code "(+ 10 20)")
;; ;; => (:RESULT "30" :TYPE :SWANK)  or (:RESULT "30" :TYPE :NREPL)

;; Define a function
;; (cl-tron-mcp/unified:repl-eval
;;   :code "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))")
;; ;; => (:RESULT FACTORIAL ...)

;; Call the function
;; (cl-tron-mcp/unified:repl-eval :code "(factorial 5)")
;; ;; => (:RESULT 120 ...)

;; Get symbol completions
;; (cl-tron-mcp/unified:repl-completions :prefix "mak")
;; ;; => (:completions ("make-array" "make-hash-table" ...))

;; Describe a symbol
;; (cl-tron-mcp/unified:repl-describe :symbol "mapcar")
;; ;; => (:describe ...)

;; Get documentation
;; (cl-tron-mcp/unified:repl-doc :symbol "mapcar")
;; ;; => (:doc ...)

;; List threads
;; (cl-tron-mcp/unified:repl-threads)
;; ;; => (:thread-list (...))

;; Get backtrace
;; (cl-tron-mcp/unified:repl-backtrace)
;; ;; => (:backtrace (...))

;; ============================================================
;; Part 4: Disconnection
;; ============================================================

;; Disconnect when done
;; (cl-tron-mcp/unified:repl-disconnect)
;; ;; => (:SUCCESS T :MESSAGE "Disconnected from REPL")

;; ============================================================
;; MCP Agent Usage
;; ============================================================

;; MCP agents can use these unified tools:
;;
;; {
;;   "tool": "repl_connect",
;;   "arguments": {"port": 4006}
;; }
;;
;; {
;;   "tool": "repl_eval",
;;   "arguments": {"code": "(+ 1 2 3)"}
;; }
;;
;; {
;;   "tool": "repl_completions",
;;   "arguments": {"prefix": "def"}
;; }
;;
;; {
;;   "tool": "repl_describe",
;;   "arguments": {"symbol": "car"}
;; }

(print "Unified REPL tutorial loaded!")
(print "Connect with: (repl-connect :port 4006)")
(print "Evaluate with: (repl-eval :code \"(+ 1 2)\")")
