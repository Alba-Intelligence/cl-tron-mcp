;;;; tutorial/nrepl-tutorial.lisp - nrepl Integration Tutorial

;; This tutorial demonstrates how to use CL-TRON-MCP with nrepl (Sly, CIDER)

;; Prerequisites:
;; 1. SBCL with Quicklisp
;; 2. CL-TRON-MCP loaded: (ql:quickload :cl-tron-mcp)
;; 3. A running nrepl server (Sly or CIDER)

;; ============================================================
;; Part 1: Starting nrepl (Sly Example)
;; ============================================================

;; In Terminal 1: Start SBCL with Sly's nrepl server
;; (ql:quickload :sly)
;; (sly:nrepl-start :port 7888)
;; ;; => nrepl started on port 7888

;; ============================================================
;; Part 2: Connecting CL-TRON-MCP
;; ============================================================

;; In Terminal 2: Load CL-TRON-MCP
;; (ql:quickload :cl-tron-mcp)

;; Connect to the nrepl server
;; (cl-tron-mcp/nrepl:nrepl-connect :port 7888)
;; ;; => (:SUCCESS T :HOST "127.0.0.1" :PORT 7888 :SESSION "..." :MESSAGE "Connected to nrepl server")

;; Check connection status
;; (cl-tron-mcp/nrepl:nrepl-status)
;; ;; => (:CONNECTED T :SESSION "..." :HAS-CONNECTION T)

;; ============================================================
;; Part 3: Evaluating Code
;; ============================================================

;; Simple arithmetic
;; (cl-tron-mcp/nrepl:nrepl-eval :code "(+ 10 20)" :package "CL-USER")
;; ;; => (:VALUE "30" :RESULT "30")

;; Define a function
;; (cl-tron-mcp/nrepl:nrepl-eval
;;   :code "(defun factorial (n) (if (<= n 1) 1 (* n (factorial (1- n)))))"
;;   :package "CL-USER")
;; ;; => (:VALUE FACTORIAL ...)

;; Call the function
;; (cl-tron-mcp/nrepl:nrepl-eval :code "(factorial 5)" :package "CL-USER")
;; ;; => (:VALUE 120 ...)

;; ============================================================
;; Part 4: Thread Operations
;; ============================================================

;; List all threads
;; (cl-tron-mcp/nrepl:nrepl-threads)
;; ;; => (:thread-list (...))

;; ============================================================
;; Part 5: Debugging
;; ============================================================

;; Get a backtrace
;; (cl-tron-mcp/nrepl:nrepl-backtrace)
;; ;; => (:backtrace (...))

;; ============================================================
;; Part 6: Inspection and Documentation
;; ============================================================

;; Inspect an object
;; (cl-tron-mcp/nrepl:nrepl-inspect :expression "*package*")
;; ;; => (:inspected (...))

;; Describe a symbol
;; (cl-tron-mcp/nrepl:nrepl-describe :symbol "mapcar")
;; ;; => (:describe (...))

;; Get documentation
;; (cl-tron-mcp/nrepl:nrepl-doc :symbol "mapcar")
;; ;; => (:apropos (...))

;; Symbol completions
;; (cl-tron-mcp/nrepl:nrepl-completions :prefix "mak")
;; ;; => (:completions ("make-array" "make-hash-table" ...))

;; ============================================================
;; Part 7: Session Management
;; ============================================================

;; List all sessions
;; (cl-tron-mcp/nrepl:nrepl-sessions)
;; ;; => (:sessions (...))

;; ============================================================
;; Part 8: Disconnection
;; ============================================================

;; Disconnect when done
;; (cl-tron-mcp/nrepl:nrepl-disconnect)
;; ;; => (:SUCCESS T :MESSAGE "Disconnected from nrepl server")

;; ============================================================
;; Using with AI Agents
;; ============================================================

;; MCP clients can use these tools:
;;
;; {
;;   "tool": "nrepl_connect",
;;   "arguments": {"port": 7888}
;; }
;;
;; {
;;   "tool": "nrepl_eval",
;;   "arguments": {"code": "(+ 1 2 3)", "package": "CL-USER"}
;; }
;;
;; {
;;   "tool": "nrepl_completions",
;;   "arguments": {"prefix": "mak"}
;; }

(print "nrepl tutorial loaded! Connect with: (nrepl-connect :port 7888)")
