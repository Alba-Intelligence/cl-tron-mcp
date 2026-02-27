;;;; src/tools/debugger-tools.lisp
;;;; Debugger tool registrations

(in-package :cl-tron-mcp/tools)

(define-simple-tool "debugger_frames"
  "Get debugger stack frames"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/debugger-frames.md"
  :function cl-tron-mcp/debugger:get-debugger-frames)

(define-simple-tool "debugger_restarts"
  "List debugger restarts"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/debugger-restarts.md"
  :function cl-tron-mcp/debugger:list-restarts)

(define-validated-tool "breakpoint_set"
  "Set a breakpoint"
  :input-schema (list :functionName "string" :condition "string" :hitCount "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/breakpoint-set.md"
  :validation ((validate-symbol-name "function_name" function_name :required t)
               (when condition (validate-string "condition" condition))
               (when hit_count (validate-integer "hit_count" hit_count :min 0)))
  :body (cl-tron-mcp/debugger:set-breakpoint :function-name function_name :condition condition :hit-count hit_count))

(define-validated-tool "breakpoint_remove"
  "Remove a breakpoint"
  :input-schema (list :breakpointId "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/breakpoint-remove.md"
  :validation ((validate-integer "breakpoint_id" breakpoint_id :required t :min 0))
  :body (cl-tron-mcp/debugger:remove-breakpoint :breakpoint-id breakpoint_id))

(define-simple-tool "breakpoint_list"
  "List all breakpoints"
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/breakpoint-list.md"
  :function cl-tron-mcp/debugger:list-breakpoints)

(define-validated-tool "step_frame"
  "Step execution in frame"
  :input-schema (list :frame "integer" :mode "string")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/step-frame.md"
  :validation ((when frame (validate-integer "frame" frame :min 0))
               (when mode (validate-choice "mode" mode '("into" "over" "out"))))
  :body (cl-tron-mcp/debugger:step-frame :frame frame :mode mode))