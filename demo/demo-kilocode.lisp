;;;; demo-kilocode.lisp - Script for Kilocode CLI demo
;;;; Shows what Kilocode sends to Tron internally

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - Kilocode CLI Demo~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")

(format t "This demo shows Kilocode CLI running with Tron MCP.~%")
(format t "Kilocode will:~%")
(format t "  1. Connect to Swank via Tron MCP~%")
(format t "  2. Debug the factorial function~%")
(format t "  3. Hot-reload the fix~%~%")
(force-output)
(sleep 2)

;; Load Tron
(format t "🔧 Loading cl-tron-mcp...~%")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 1)

;; Connect
(format t "~%🔧 Connecting to Swank on port 4005...~%")
(let ((result (cl-tron-mcp/swank:swank-connect :port 4005)))
  (format t "   Result: ~S~%" result))
(force-output)
(sleep 2)

;; Define buggy factorial
(format t "~%🔧 Defining factorial function (buggy)...~%")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1)) (1))))"))
  (format t "   Code: ~A~%" code)
  (let ((result (cl-tron-mcp/swank:swank-eval :code code)))
    (format t "   Result: ~S~%" result)))
(force-output)
(sleep 2)

;; Run and get error
(format t "~%🔧 Running (factorial 7)...~%")
(let ((result (cl-tron-mcp/swank:swank-eval :code "(factorial 7)")))
  (if (getf result :result)
      (let ((res (getf result :result)))
        (when (getf res :debug)
          (format t "   ⚠️  ERROR: ~A~%" (getf res :condition))
          (format t "   📍 Backtrace:~%")
          (dolist (frame (subseq (getf res :frames) 0 (min 5 (length (getf res :frames)))))
            (format t "      ~A~%" frame))))
      (format t "   Result: ~S~%" result)))
(force-output)
(sleep 2)

;; Abort and fix
(format t "~%🔧 Aborting error...~%")
(cl-tron-mcp/swank:swank-invoke-restart :restart_index 2)
(force-output)
(sleep 1)

(format t "~%🔧 Hot-reloading corrected function...~%")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "   Code: ~A~%" code)
  (cl-tron-mcp/swank:swank-eval :code code))
(force-output)
(sleep 2)

;; Verify
(format t "~%🔧 Verifying...~%")
(let ((r1 (cl-tron-mcp/swank:swank-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/swank:swank-eval :code "(factorial 10)")))
  (format t "   (factorial 7)  → ~A  ✓~%"
          (let ((res (getf r1 :result)))
            (if (listp res) (second res) res)))
  (format t "   (factorial 10) → ~A  ✓~%"
          (let ((res (getf r2 :result)))
            (if (listp res) (second res) res))))
(force-output)
(sleep 2)

;; Done
(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! Session preserved.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 100)

(cl-tron-mcp/swank:swank-disconnect)
(sb-ext:quit)