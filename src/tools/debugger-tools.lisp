;;;; src/tools/debugger-tools.lisp
;;;; Debugger tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "debugger_frames"
  "Get debugger stack frames when an error has occurred. Use after swank_eval returns an error to see the call stack. Shows function names and source locations."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/debugger:get-debugger-frames)

(define-simple-tool "debugger_restarts"
  "List available debugger restarts. Use to see recovery options after an error. Common restarts: ABORT, RETRY, USE-VALUE, CONTINUE."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/debugger:list-restarts)

(define-validated-tool "breakpoint_set"
  "Set a breakpoint on a function. Execution will pause when the function is called. Requires approval. Use for proactive debugging."
  :input-schema (list :functionName "string" :condition "string" :hitCount "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :validation ((validate-symbol-name "function_name" function_name :required t)
               (when condition (validate-string "condition" condition))
               (when hit_count (validate-integer "hit_count" hit_count :min 0)))
  :body (cl-tron-mcp/debugger:set-breakpoint :function-name function_name :condition condition :hit-count hit_count))

(define-validated-tool "breakpoint_remove"
  "Remove a breakpoint by its ID. Use to clear breakpoints after debugging is complete."
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/debugger:remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "breakpoint_list"
  "List all active breakpoints. Use to see what breakpoints are currently set."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :function cl-tron-mcp/debugger:list-breakpoints)

(define-validated-tool "step_frame"
  "Step execution in a debugger frame. Modes: into (step into calls), over (step over calls), out (step out of current function). Use after hitting a breakpoint."
  :input-schema (list :frame "integer" :mode "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when mode (validate-choice "mode" mode '("into" "over" "out"))))
  :body (cl-tron-mcp/debugger:step-frame :frame frame :mode mode))