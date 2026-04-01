;;;; demo-f1-f2.lisp - The canonical f1/f2 hot-reload demo
;;;; 
;;;; Workflow:
;;;;   1. Launch Swank on port 14006
;;;;   2. Define f1 that calls undefined f2
;;;;   3. Call f1 → error (f2 not defined)
;;;;   4. Hot-compile f2 into the running image
;;;;   5. Call f1 again → success (f2 now exists)
;;;;
;;;; Self-contained: no pre-existing Swank needed.

(defvar *demo-port* 14006)

(defun announce (msg &rest args)
  (format t "~%~%")
  (format t "════════════════════════════════════════════════════════════════════════════~%")
  (format t "  ~?~%" msg args)
  (format t "════════════════════════════════════════════════════════════════════════════~%~%")
  (force-output))

(format t "~%")
(format t "════════════════════════════════════════════════════════════════════════════~%")
(format t "  TRON MCP — f1/f2 Hot-Reload Demo~%")
(format t "  Demonstrates live code injection into a running Lisp image~%")
(format t "════════════════════════════════════════════════════════════════════════════~%~%")
(force-output)
(sleep 2)

;;; ===== Step 1: Load =====
(announce "Step 1: Load cl-tron-mcp")
(ql:quickload :cl-tron-mcp :silent t)
(format t "  ✓ Loaded (91 tools, 14 categories)~%")
(force-output)
(sleep 2)

;;; ===== From here on, packages are available =====

(defun demo-cleanup ()
  (ignore-errors (cl-tron-mcp/unified:repl-disconnect))
  (ignore-errors (cl-tron-mcp/swank:kill-managed-process :port *demo-port*)))

(defun demo-fail (msg &rest args)
  (format t "  ✗ ~?~%" msg args)
  (force-output)
  (demo-cleanup)
  (sb-ext:quit :unix-status 1))

;;; ===== Step 2: Launch Swank =====
(announce "Step 2: Launch Swank server on port ~D" *demo-port*)
(let ((result (cl-tron-mcp/swank:launch-sbcl-with-swank :port *demo-port*)))
  (unless (getf result :success)
    (demo-fail "Failed to launch: ~S" result)))
(unless (cl-tron-mcp/swank:wait-for-port *demo-port* :timeout 30)
  (demo-fail "Swank not ready in 30s"))
(format t "  ✓ Swank ready on port ~D~%" *demo-port*)
(force-output)
(sleep 2)

;;; ===== Step 3: Connect =====
(announce "Step 3: Connect to Swank")
(let ((result (cl-tron-mcp/unified:repl-connect :port *demo-port*)))
  (let ((inner (if (consp (car result)) (car result) result)))
    (if (getf inner :success)
        (format t "  ✓ Connected~%")
        (demo-fail "Connect failed: ~S" result))))
(force-output)
(sleep 2)

;;; ===== Step 4: Define f1 =====
(announce "Step 4: Define f1 (calls undefined f2)")
(format t "  Code: (defun f1 (a b) (f2 a b))~%")
(let ((result (cl-tron-mcp/unified:repl-compile :code "(defun f1 (a b) (f2 a b))")))
  (format t "  Compile: ~S~%" result))
(format t "  ✓ f1 compiled — but f2 doesn't exist yet!~%")
(force-output)
(sleep 3)

;;; ===== Step 5: Call f1 → error =====
(announce "Step 5: Call (f1 1 2) — expect error")
(let ((result (cl-tron-mcp/unified:repl-eval :code "(f1 1 2)")))
  (format t "  Raw: ~S~%" result)
  (let ((inner (getf result :result)))
    (cond
      ((and (listp inner) (getf inner :debug))
       (format t "  ⚠️  Debugger activated: ~A~%" (getf inner :condition)))
      (t
       (format t "  Result: ~S~%" result)))))
(force-output)
(sleep 3)

;;; ===== Step 5b: Abort debugger =====
(format t "  Aborting debugger...~%")
(let ((result (cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)))
  (format t "  Restart result: ~S~%" result))
(format t "  ✓ Back to top level~%")
(force-output)
(sleep 2)

;;; ===== Step 6: Hot-compile f2 =====
(announce "Step 6: Hot-compile f2 into running image")
(format t "  Code: (defun f2 (x y) (+ x y))~%")
(let ((result (cl-tron-mcp/unified:repl-compile :code "(defun f2 (x y) (+ x y))")))
  (format t "  Compile: ~S~%" result))
(format t "  ✓ f2 compiled — injected into live image!~%")
(force-output)
(sleep 3)

;;; ===== Step 7: Call f1 again → success =====
(announce "Step 7: Call (f1 1 2) again — should work now")
(let ((result (cl-tron-mcp/unified:repl-eval :code "(f1 1 2)")))
  (format t "  Result: ~S~%" result)
  (let ((inner (getf result :result)))
    (if (and (listp inner) (eq (car inner) :ok))
        (let ((value (second (second inner))))
          (format t "  ✓ f1 → f2 → (+ 1 2) = ~A~%" value))
        (format t "  (result format differs from expected)~%"))))
(force-output)
(sleep 3)

;;; ===== Done =====
(format t "~%~%")
(format t "════════════════════════════════════════════════════════════════════════════~%")
(format t "  ✅ Hot-reload demo complete!~%")
(format t "  f2 was compiled into the live image without restarting anything.~%")
(format t "════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 5)

;;; ===== Cleanup =====
(demo-cleanup)
(sb-ext:quit)
