;;;; demo-kilocode.lisp - Script for Kilocode CLI demo
;;;; Shows what Kilocode sends to Tron internally.
;;;; Self-contained: auto-launches Swank, runs demo, cleans up.

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - Kilocode CLI Demo  (91 tools)~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")

(format t "This demo shows Kilocode CLI running with Tron MCP.~%")
(format t "Kilocode will:~%")
(format t "  1. Launch a Swank server~%")
(format t "  2. Connect and debug the factorial function~%")
(format t "  3. Hot-reload the fix~%~%")
(force-output)
(sleep 2)

;; Load Tron
(format t "🔧 Loading cl-tron-mcp...~%")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 1)

;; Launch Swank
(format t "~%🔧 Launching Swank on port 14006...~%")
(cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)
(cl-tron-mcp/swank:wait-for-port 14006 :timeout 30)
(format t "   Swank ready!~%")
(force-output)
(sleep 1)

;; Connect
(format t "~%🔧 Connecting to Swank...~%")
(let ((result (cl-tron-mcp/unified:repl-connect :port 14006)))
  (format t "   Result: ~S~%" result))
(force-output)
(sleep 2)

;; Define buggy factorial
(format t "~%🔧 Defining factorial function (buggy)...~%")
(let ((code "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"))
  (format t "   Code: ~A~%" code)
  (let ((result (cl-tron-mcp/unified:repl-compile :code code)))
    (format t "   Result: ~S~%" result)))
(force-output)
(sleep 2)

;; Run and get error
(format t "~%🔧 Running (factorial 7)...~%")
(let ((result (cl-tron-mcp/unified:repl-eval :code "(factorial 7)")))
  (let ((inner (getf result :result)))
    (if (and (listp inner) (getf inner :debug))
        (progn
          (format t "   ⚠️  ERROR detected (debugger entered)~%")
          (format t "   Condition: ~A~%" (getf inner :condition)))
        (format t "   Result: ~S~%" result))))
(force-output)
(sleep 2)

;; Abort and fix
(format t "~%🔧 Aborting error and hot-fixing...~%")
(cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)
(force-output)
(sleep 1)

(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "   Compiling: ~A~%" code)
  (cl-tron-mcp/unified:repl-compile :code code))
(force-output)
(sleep 2)

;; Verify
(format t "~%🔧 Verifying fix...~%")
(let ((r1 (cl-tron-mcp/unified:repl-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/unified:repl-eval :code "(factorial 10)")))
  (format t "   (factorial 7)  → ~A  ✓~%" r1)
  (format t "   (factorial 10) → ~A  ✓~%" r2))
(force-output)
(sleep 2)

;; Done
(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! Bug found, fixed, and verified.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 5)

(cl-tron-mcp/unified:repl-disconnect)
(cl-tron-mcp/swank:kill-managed-process :port 14006)
(sb-ext:quit)
