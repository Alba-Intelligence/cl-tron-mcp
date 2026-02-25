;;;; demo-script.lisp - Script for VHS demo recording
;;;; Run by the tape file

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - AI Debugging Demo~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")


(defun announce-step (msg)
  (format t "~%~%~%")
  (format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
  (format t "════~%")
  (format t "════ ~S ~%" msg)
  (format t "════~%~%")
  (force-output)
  )

(announce-step "Load the Quicklisp package")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 3)

;; Connect
(announce-step "🔧 Connecting to Swank on port 4006...")
(let ((result (cl-tron-mcp/swank:swank-connect :port 4006)))
  (format t "   Result: ~S~%" result))
(force-output)
(sleep 3)

;; Define buggy factorial
(announce-step "🔧 Defining factorial function (buggy)...")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1)) (1))))"))
  (format t "   Code: ~A~%" code)
  (let ((result (cl-tron-mcp/swank:swank-eval :code code)))
    (format t "   Result: ~S~%" result)))
(force-output)
(sleep 3)

;; Run and get error
(announce-step "🔧 Running (factorial 7)...")
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
(sleep 3)

;; Abort and fix
(announce-step "🔧 Aborting error...")
(force-output)
(cl-tron-mcp/swank:swank-invoke-restart :restart_index 2)
(force-output)
(sleep 3)

(announce-step "🔧 Hot-reloading corrected function...")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "   Code: ~A~%" code)
  (cl-tron-mcp/swank:swank-eval :code code))
(force-output)
(sleep 3)

;; Verify
(announce-step "🔧 Verifying...")
(let ((r1 (cl-tron-mcp/swank:swank-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/swank:swank-eval :code "(factorial 10)")))
  (format t "   (factorial 7)  → ~A  ✓~%"
          (let ((res (getf r1 :result)))
            (if (listp res) (second res) res)))
  (format t "   (factorial 10) → ~A  ✓~%"
          (let ((res (getf r2 :result)))
            (if (listp res) (second res) res))))
(force-output)
(sleep 3)

;; Done
(format t "~%")
(format t "~%")
(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! Session preserved.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 100)

(cl-tron-mcp/swank:swank-disconnect)
(sb-ext:quit)
