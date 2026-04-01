;;;; tutorial/f1-f2-tutorial.lisp
;;;;
;;;; Canonical interactive debugging tutorial: the f1/f2 workflow.
;;;;
;;;; This file walks through the complete agent-assisted Lisp debugging loop:
;;;;   - Bootstrap a fresh SBCL+Swank process (no manual setup needed)
;;;;   - Write a function that calls an undefined helper
;;;;   - Detect the warning at compile time
;;;;   - Enter the debugger when the function is called
;;;;   - Hot-compile the missing helper into the running image
;;;;   - Resume execution via restart — no session restart needed
;;;;
;;;; Prerequisites:
;;;;   - cl-tron-mcp loaded (Swank is bootstrapped automatically)

;;; ===========================================================================
;;; STEP 0: Bootstrap a Swank server (automated — no manual setup needed)
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   swank_launch {:port 4006}
;;
;; What happens:
;;   - A fresh SBCL subprocess is spawned with Swank listening on port 4006
;;   - The MCP polls until Swank is ready (up to 60s)
;;   - The process is registered in the MCP's process registry
;;
;; Expected result:
;;   {:success true, :port 4006, :pid <n>, :message "SBCL+Swank running on port 4006 ..."}
;;
;; Note: swank_launch requires user approval (it spawns a subprocess).
;; If you already have a Swank server running, skip this step.
;;
;; Alternatively, from Lisp directly:
;;   (cl-tron-mcp/swank:launch-sbcl-with-swank :port 4006 :timeout 60)

;;; ===========================================================================
;;; STEP 1: Connect the MCP to the live image
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_connect {:port 4006}
;;
;; Expected result:
;;   {:connected true, :port 4006, :host "127.0.0.1"}

;;; ===========================================================================
;;; STEP 2: Write and compile f1 — a function that calls undefined f2
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_compile {:code "(defun f1 (a b) (f2 a b))", :package "CL-USER"}
;;
;; What happens:
;;   - SBCL compiles the function and emits a WARNING:
;;       "undefined function: F2"
;;   - The function is defined anyway — the warning is informational
;;
;; Expected result (contains compilation output with the warning):
;;   {:success true, :output "...undefined function F2...", :compiled true}
;;
;; Key learning for the agent:
;;   The WARNING at compile time tells you f2 is not yet defined.
;;   This is your first signal that the helper is missing.

;;; ===========================================================================
;;; STEP 3: Call f1 — trigger the debugger
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_eval {:code "(f1 1 2)", :package "CL-USER"}
;;
;; What happens:
;;   - SBCL executes f1(1, 2), which calls f2(1, 2)
;;   - f2 does not exist → UNDEFINED-FUNCTION condition is signalled
;;   - The SBCL debugger is invoked
;;
;; Expected result (error/debugger entry):
;;   {:error true, :condition "UNDEFINED-FUNCTION", :function "F2"}
;;   OR the eval blocks/returns with a debugger description.

;;; ===========================================================================
;;; STEP 4: Inspect the debugger state
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_backtrace {:start 0, :end 10}
;;
;; Expected result (stack frames):
;;   {:frames [{:index 0, :description "...F2..."}, ...]}
;;
;; Agent action (MCP tool call):
;;   repl_get_restarts {}
;;
;; Expected result (available restarts):
;;   {:restarts [{:index 0, :name "RETRY", :description "..."},
;;               {:index 1, :name "RETURN-VALUE", ...},
;;               {:index 2, :name "ABORT", ...}]}
;;
;; Key learning:
;;   The RETRY restart will re-try calling f2 after you've defined it.
;;   Save the index of RETRY for Step 6.

;;; ===========================================================================
;;; STEP 5: Hot-compile f2 into the running image
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_compile {:code "(defun f2 (x y) (+ x y))", :package "CL-USER"}
;;
;; What happens:
;;   - f2 is compiled and loaded INTO THE SAME IMAGE
;;   - No restart needed — the running image is updated in place
;;   - The debugger session for f1 is still active
;;
;; Expected result:
;;   {:success true, :compiled true}

;;; ===========================================================================
;;; STEP 6: Resume execution via the RETRY restart
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_invoke_restart {:restart_index <index-of-RETRY>}
;;
;; What happens:
;;   - SBCL retries the failed call to f2 — which now exists
;;   - f2(1, 2) → (+ 1 2) → 3
;;   - f1(1, 2) returns 3
;;   - The debugger exits cleanly
;;
;; Expected result:
;;   {:value "3"} or {:output "3"}

;;; ===========================================================================
;;; STEP 7: Verify — call f1 again in a fresh context
;;; ===========================================================================

;; Agent action (MCP tool call):
;;   repl_eval {:code "(f1 1 2)", :package "CL-USER"}
;;
;; Expected result:
;;   {:value "3"}
;;
;; This confirms both f1 and f2 are now defined and working correctly.

;;; ===========================================================================
;;; SUMMARY: The complete workflow in one view
;;; ===========================================================================

;;  [Bootstrap]    swank_launch :port 4006           ; spawns fresh SBCL+Swank
;;       ↓
;;  [Connect]      repl_connect :port 4006
;;       ↓
;;  [Compile f1]   repl_compile "(defun f1 (a b) (f2 a b))"
;;                   → WARNING: undefined function F2
;;       ↓
;;  [Call f1]      repl_eval "(f1 1 2)"
;;                   → DEBUGGER: UNDEFINED-FUNCTION F2
;;       ↓
;;  [Inspect]      repl_backtrace / repl_get_restarts
;;                   → See frames, note RETRY restart index
;;       ↓
;;  [Hot-compile]  repl_compile "(defun f2 (x y) (+ x y))"
;;                   → f2 now exists in the running image
;;       ↓
;;  [Resume]       repl_invoke_restart :restart_index N   ; RETRY
;;                   → f2(1,2) = 3, f1(1,2) = 3
;;       ↓
;;  [Verify]       repl_eval "(f1 1 2)"
;;                   → "3" ✓
;;       ↓
;;  [Cleanup]      swank_kill :port 4006             ; optional — kills managed process
;;
;;  Total: 7 MCP tool calls, 0 session restarts, 0 manual setup steps, full state preserved.

;;; ===========================================================================
;;; RUNNING AS A SCRIPT (automated verification)
;;; ===========================================================================

;; (ql:quickload :cl-tron-mcp)
;; (ql:quickload :rove)
;; (ql:quickload :cl-tron-mcp/tests)
;;
;; ;; The integration test runs the full workflow automatically:
;; (rove:run :cl-tron-mcp/tests/integration)
