;;;; swank-tutorial.lisp - Tutorial: Using CL-TRON-MCP with Swank
;;;;
;;;; Demonstrates how to use cl-tron-mcp Swank tools from Lisp code.
;;;; All examples show correct keyword-argument calling conventions.
;;;;
;;;; Run with:
;;;;   sbcl --noinform --disable-debugger \
;;;;        --eval '(ql:quickload :cl-tron-mcp :silent t)' \
;;;;        --load tutorial/swank-tutorial.lisp \
;;;;        --eval '(sb-ext:quit)'

;;; ============================================================
;;; STEP 0: Bootstrap — launch a managed Swank server
;;; ============================================================

;; If you don't already have a Swank server, launch one now.
;; swank_launch starts a child SBCL process and waits for it to be ready.

;; Via MCP tool:
;; Tool: swank_launch   Arguments: { "port": 4006 }

;; Via Lisp (self-contained):
;; (ql:quickload :cl-tron-mcp :silent t)
;; (cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)
;; (cl-tron-mcp/swank:wait-for-port 14006 :timeout 30)

;;; ============================================================
;;; STEP 1: Connect to Swank
;;; ============================================================

;; (cl-tron-mcp/swank:swank-connect :port 4006)
;; => (:SUCCESS T :HOST "127.0.0.1" :PORT 4006 :MESSAGE "Connected to Swank at 127.0.0.1:4006")

;; Check connection status:
;; (cl-tron-mcp/swank:swank-status)
;; => (:CONNECTED T :HAS-CONNECTION T)

;;; ============================================================
;;; STEP 2: Evaluate code via Swank
;;; ============================================================

;; All swank-eval calls use the :code keyword argument:

;; (cl-tron-mcp/swank:swank-eval :code "(+ 10 20 30)")
;; => (:RESULT (:OK ("" "60")) ...)

;; Define a function in the target image:
;; (cl-tron-mcp/swank:swank-eval
;;   :code "(defun fib (n) (if (< n 2) n (+ (fib (1- n)) (fib (- n 2)))))"
;;   :package "CL-USER")
;; => (:RESULT (:OK ("" "FIB")) ...)

;; Call it:
;; (cl-tron-mcp/swank:swank-eval :code "(fib 10)" :package "CL-USER")
;; => (:RESULT (:OK ("" "55")) ...)

;;; ============================================================
;;; STEP 3: Debugging — threads, backtrace, interrupt
;;; ============================================================

;; List all threads in the target SBCL:
;; (cl-tron-mcp/swank:mcp-swank-threads)
;; => (:THREADS (...))

;; Get the current call stack:
;; (cl-tron-mcp/swank:mcp-swank-backtrace :start 0 :end 10)
;; => (:RESULT (:OK ("..." "NIL")) ...)

;; Interrupt a running computation:
;; (cl-tron-mcp/swank:mcp-swank-interrupt)
;; => (:SUCCESS T)

;; Abort the REPL thread (useful after a debugger event):
;; (cl-tron-mcp/swank:mcp-swank-abort)                     ; :repl-thread (default)
;; (cl-tron-mcp/swank:mcp-swank-abort :thread-id "worker") ; specific thread

;;; ============================================================
;;; STEP 4: Inspection and documentation
;;; ============================================================

;; Inspect an object by expression string:
;; (cl-tron-mcp/swank:mcp-swank-inspect :expression "*package*")
;; => (:RESULT (:OK ("..." "...")) ...)

;; Describe a symbol:
;; (cl-tron-mcp/swank:mcp-swank-describe :expression "mapcar")
;; => (:RESULT (:OK ("MAPCAR ..." "...")) ...)

;; Get autodoc (argument list) for a symbol:
;; (cl-tron-mcp/swank:mcp-swank-autodoc :symbol "mapcar")
;; => (:RESULT (:OK ("..." "...")) ...)

;; Get completions for a prefix:
;; (cl-tron-mcp/swank:mcp-swank-completions :prefix "mak" :package "CL-USER")
;; => (:RESULT (:OK ("..." "NIL")) ...)

;;; ============================================================
;;; EXAMPLE: Debug a division error
;;; ============================================================

;; 1. Define and break a function:
;; (cl-tron-mcp/swank:swank-eval
;;   :code "(defun divide (a b) (/ a b))"
;;   :package "CL-USER")

;; 2. Trigger the error:
;; (cl-tron-mcp/swank:swank-eval :code "(divide 10 0)")
;; => (:RESULT (:DEBUG T :CONDITION ("arithmetic error DIVISION-BY-ZERO ..." ...) ...))

;; 3. Inspect frame locals:
;; (cl-tron-mcp/swank:mcp-swank-frame-locals :frame 0)
;; => (:LOCALS ((:NAME "A" :VALUE "10") (:NAME "B" :VALUE "0")))

;; 4. Hot-fix the function:
;; (cl-tron-mcp/swank:swank-eval
;;   :code "(defun divide (a b)
;;             (if (zerop b) (error \"Cannot divide by zero\") (/ a b)))"
;;   :package "CL-USER")

;; 5. Verify the fix:
;; (cl-tron-mcp/swank:swank-eval :code "(divide 10 2)")
;; => (:RESULT (:OK ("" "5")) ...)

;;; ============================================================
;;; UNIFIED INTERFACE (preferred for new code)
;;; ============================================================

;; The unified repl-* API wraps the above for a cleaner interface:

;; (cl-tron-mcp/unified:repl-connect :port 4006)
;; (cl-tron-mcp/unified:repl-eval :code "(+ 1 2 3)")
;; (cl-tron-mcp/unified:repl-backtrace)
;; (cl-tron-mcp/unified:repl-get-restarts)
;; (cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)
;; (cl-tron-mcp/unified:repl-disconnect)

;;; ============================================================
;;; DISCONNECT
;;; ============================================================

;; (cl-tron-mcp/swank:swank-disconnect)
;; => (:SUCCESS T :MESSAGE "Disconnected from Swank server")

;;; ============================================================
;;; SUMMARY
;;; ============================================================

;; Workflow:
;;
;; 1. Bootstrap:   (swank_launch :port 4006)  or manual swank:create-server
;; 2. Connect:     (swank-connect :port 4006)
;; 3. Evaluate:    (swank-eval :code "(+ 1 2)")
;; 4. Debug:       (mcp-swank-backtrace), (mcp-swank-frame-locals :frame 0)
;; 5. Disconnect:  (swank-disconnect)
;;
;; Key calling conventions:
;;   - swank-eval takes :code and optional :package keywords
;;   - mcp-swank-* wrappers always use keyword arguments
;;   - Prefer the unified repl-* API for new code

;;; End of tutorial
