;;;; tutorial/f1-f2-tutorial.lisp
;;;;
;;;; Canonical interactive debugging tutorial: the f1/f2 workflow.
;;;;
;;;; This file walks through the complete agent-assisted Lisp debugging loop:
;;;;   - Write a function that calls an undefined helper
;;;;   - Detect the warning at compile time
;;;;   - Enter the debugger when the function is called
;;;;   - Hot-compile the missing helper into the running image
;;;;   - Resume execution via restart — no session restart needed
;;;;
;;;; Prerequisites:
;;;;   - A running SBCL image with Swank loaded on port 4006
;;;;     e.g.: (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t)
;;;;   - cl-tron-mcp loaded and connected to that image

;;; ===========================================================================
;;; STEP 0: Start a Swank server in the target image (one-time setup)
;;; ===========================================================================

;; In your SBCL REPL (or start-mcp.sh target), run:
;;
;;   (ql:quickload :swank)
;;   (swank:create-server :port 4006 :dont-close t)
;;
;; Now the MCP can connect to it. Never restart this session — state lives here.

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
;;
;;  Total: 6 MCP tool calls, 0 session restarts, full state preserved.

;;; ===========================================================================
;;; RUNNING AS A SCRIPT (automated verification)
;;; ===========================================================================

;; (ql:quickload :cl-tron-mcp)
;; (ql:quickload :rove)
;; (ql:quickload :cl-tron-mcp/tests)
;; 
;; ;; The integration test runs the full workflow automatically:
;; (rove:run :cl-tron-mcp/tests/integration)
