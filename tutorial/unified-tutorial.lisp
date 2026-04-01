;;;; tutorial/unified-tutorial.lisp - Unified REPL Interface Tutorial

;; This tutorial demonstrates the unified REPL interface
;; that connects to a Swank server and provides a clean API.

;; Prerequisites:
;; 1. SBCL with Quicklisp
;; 2. CL-TRON-MCP loaded: (ql:quickload :cl-tron-mcp)
;; 3. A running Swank server (see Step 0 below)

;; ============================================================
;; Part 0: Bootstrap — Launch a Swank Server
;; ============================================================

;; If you don't have a Swank server running, launch one via MCP:
;;
;;   Tool: swank_launch   Arguments: { "port": 4006 }
;;
;; Or from Lisp:
;;   (ql:quickload :cl-tron-mcp :silent t)
;;   (cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)
;;   (cl-tron-mcp/swank:wait-for-port 14006 :timeout 30)
;;
;; Or manually in another terminal:
;;   sbcl --eval "(ql:quickload :swank :silent t)" \
;;        --eval "(swank:create-server :port 4006 :dont-close t)"

;; ============================================================
;; Part 1: Connecting with the Unified Interface
;; ============================================================

;; Connect to a Swank server (auto-detects type):
;; (cl-tron-mcp/unified:repl-connect :port 4006)
;; ;; => (:SUCCESS T :TYPE :SWANK :HOST "127.0.0.1" :PORT 4006 ...)

;; With explicit type and host:
;; (cl-tron-mcp/unified:repl-connect :type :swank :host "127.0.0.1" :port 4006)

;; Check status:
;; (cl-tron-mcp/unified:repl-status)
;; ;; => (:CONNECTED T :TYPE :SWANK :HOST "127.0.0.1" :PORT 4006)

;; ============================================================
;; Part 2: Evaluation
;; ============================================================

;; Evaluate code:
;; (cl-tron-mcp/unified:repl-eval :code "(+ 10 20)")
;; ;; => (:RESULT (:OK ("" "30")) :TYPE :SWANK)

;; Define a function:
;; (cl-tron-mcp/unified:repl-eval
;;   :code "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))")
;; ;; => (:RESULT (:OK ("" "FACTORIAL")) ...)

;; Call it:
;; (cl-tron-mcp/unified:repl-eval :code "(factorial 5)")
;; ;; => (:RESULT (:OK ("" "120")) ...)

;; ============================================================
;; Part 3: Inspection and Documentation
;; ============================================================

;; Get symbol completions:
;; (cl-tron-mcp/unified:repl-completions :prefix "mak")
;; ;; => (:RESULT (:OK ("..." "NIL")) ...)

;; Describe a symbol:
;; (cl-tron-mcp/unified:repl-describe :expression "mapcar")
;; ;; => (:RESULT (:OK ("MAPCAR ..." "...")) ...)

;; List threads:
;; (cl-tron-mcp/unified:repl-threads)
;; ;; => (:THREADS (...))

;; Get backtrace:
;; (cl-tron-mcp/unified:repl-backtrace)
;; ;; => (:RESULT (:OK ("..." "NIL")) ...)

;; ============================================================
;; Part 4: Debugging
;; ============================================================

;; Get available restarts after an error:
;; (cl-tron-mcp/unified:repl-get-restarts)

;; Invoke a restart by index:
;; (cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)

;; Inspect frame locals:
;; (cl-tron-mcp/unified:repl-frame-locals :frame 0)

;; Compile code (returns compiler output + notes):
;; (cl-tron-mcp/unified:repl-compile
;;   :code "(defun f2 (x y) (+ x y))")

;; ============================================================
;; Part 5: Disconnection
;; ============================================================

;; (cl-tron-mcp/unified:repl-disconnect)
;; ;; => (:SUCCESS T :MESSAGE "Disconnected from REPL")

;; ============================================================
;; MCP Agent Usage
;; ============================================================

;; MCP agents use the equivalent JSON tools:
;;
;; { "tool": "repl_connect",   "arguments": {"port": 4006} }
;; { "tool": "repl_eval",      "arguments": {"code": "(+ 1 2 3)"} }
;; { "tool": "repl_compile",   "arguments": {"code": "(defun f2 (x y) (+ x y))"} }
;; { "tool": "repl_backtrace", "arguments": {} }
;; { "tool": "repl_completions","arguments": {"prefix": "def"} }
;; { "tool": "repl_describe",  "arguments": {"expression": "car"} }

(print "Unified REPL tutorial loaded!")
(print "Bootstrap:  (cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)")
(print "Connect:    (cl-tron-mcp/unified:repl-connect :port 4006)")
(print "Evaluate:   (cl-tron-mcp/unified:repl-eval :code \"(+ 1 2)\")")
