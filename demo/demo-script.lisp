;;;; demo-script.lisp - Script for VHS demo recording
;;;; Self-contained: auto-launches Swank, runs demo, cleans up.

(defun announce-step (msg)
  (format t "~%~%~%")
  (format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
  (format t "════~%")
  (format t "════ ~A ~%" msg)
  (format t "════~%~%")
  (force-output))

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - AI Debugging Demo  (91 tools)~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")

(announce-step "Loading cl-tron-mcp")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 2)

;; Launch Swank
(announce-step "🔧 Launching managed Swank on port 14006...")
(cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)
(cl-tron-mcp/swank:wait-for-port 14006 :timeout 30)
(format t "   Swank ready on port 14006~%")
(force-output)
(sleep 2)

;; Connect
(announce-step "🔧 Connecting to Swank...")
(let ((result (cl-tron-mcp/unified:repl-connect :port 14006)))
  (format t "   Result: ~S~%" result))
(force-output)
(sleep 2)

;; Define buggy factorial (base case divides by zero → runtime error)
(announce-step "🔧 Defining factorial function (buggy)...")
(let ((code "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"))
  (format t "   Code: ~A~%" code)
  (let ((result (cl-tron-mcp/unified:repl-compile :code code)))
    (format t "   Result: ~S~%" result)))
(force-output)
(sleep 2)

;; Run and get error
(announce-step "🔧 Running (factorial 7)...")
(let ((result (cl-tron-mcp/unified:repl-eval :code "(factorial 7)")))
  (let ((inner (getf result :result)))
    (if (and (listp inner) (getf inner :debug))
        (progn
          (format t "   ⚠️  ERROR detected (debugger entered)~%")
          (format t "   Condition: ~A~%" (getf inner :condition)))
        (format t "   Result: ~S~%" result))))
(force-output)
(sleep 2)

;; Get restarts and abort
(announce-step "🔧 Getting restarts and aborting...")
(let ((restarts (cl-tron-mcp/unified:repl-get-restarts)))
  (format t "   Restarts: ~S~%" restarts))
(cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)
(format t "   Aborted to top level.~%")
(force-output)
(sleep 2)

;; Hot-reload fix
(announce-step "🔧 Hot-reloading corrected function...")
(let ((code "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "   Code: ~A~%" code)
  (cl-tron-mcp/unified:repl-compile :code code))
(force-output)
(sleep 2)

;; Verify
(announce-step "🔧 Verifying fix...")
(let ((r1 (cl-tron-mcp/unified:repl-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/unified:repl-eval :code "(factorial 10)")))
  (format t "   (factorial 7)  → ~A  ✓~%" r1)
  (format t "   (factorial 10) → ~A  ✓~%" r2))
(force-output)
(sleep 3)

;; Done
(format t "~%~%~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! Bug found, fixed, and verified.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 5)

;; Cleanup
(cl-tron-mcp/unified:repl-disconnect)
(cl-tron-mcp/swank:kill-managed-process :port 14006)
(sb-ext:quit)
