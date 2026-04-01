;;;; src/tools/debugger-tools.lisp
;;;; Debugger tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "debugger_frames"
  "Get current debugger stack frames with source locations"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/debugger-frames.md"
  :function cl-tron-mcp/debugger:get-debugger-frames)

(define-simple-tool "debugger_restarts"
  "List available restarts in the current debugger context"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/debugger-restarts.md"
  :function cl-tron-mcp/debugger:list-restarts)

(define-validated-tool "breakpoint_set"
  "Set a breakpoint on a named function with optional condition"
  :input-schema (list :functionName "string" :condition "string" :hitCount "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/breakpoint-set.md"
  :validation ((validate-symbol-name "function_name" function_name :required t)
               (when condition (validate-string "condition" condition))
               (when hit_count (validate-integer "hit_count" hit_count :min 0)))
  :body (cl-tron-mcp/debugger:set-breakpoint :function-name function_name :condition condition :hit-count hit_count))

(define-validated-tool "breakpoint_remove"
  "Remove a breakpoint by ID"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/breakpoint-remove.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/debugger:remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "breakpoint_list"
  "List all active breakpoints with ID, location, and condition"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/breakpoint-list.md"
  :function cl-tron-mcp/debugger:list-breakpoints)

(define-validated-tool "breakpoint_toggle"
  "Enable or disable a breakpoint by ID without removing it"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/breakpoint-toggle.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/debugger:toggle-breakpoint breakpoint_id))

(define-validated-tool "step_frame"
  "Step through execution in a stack frame (into, over, or out)"
  :input-schema (list :frame "integer" :mode "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/step-frame.md"
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when mode (validate-choice "mode" mode '("into" "over" "out"))))
  :body (cl-tron-mcp/debugger:step-frame :frame frame :mode mode))