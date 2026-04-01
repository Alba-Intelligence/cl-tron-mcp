;;;; src/tools/process-tools.lisp
;;;; MCP tool registrations for Swank process lifecycle management.

(in-package :cl-tron-mcp/tools)

(define-validated-tool "swank_launch"
  "Launch a new SBCL image with Swank listening on a given port.
The process is managed by the MCP and can be killed with swank_kill.
Use swank_connect / repl_connect afterwards to connect to it."
  :input-schema (list :port "integer"
                      :timeout "integer"
                      :communication_style "string"
                      :sbcl_binary "string"
                      :extra_eval "string")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-launch.md"
  :validation
  ((when port (validate-integer "port" port :min 1024 :max 65535))
   (when timeout (validate-integer "timeout" timeout :min 5 :max 120))
   (when communication_style
     (validate-string "communication_style" communication_style
                      :allowed-values '("spawn" "fd-handler" "sigio"))))
  :body
  (cl-tron-mcp/swank:launch-sbcl-with-swank
   :port    (or port 4006)
   :timeout (or timeout 30)
   :communication-style (if communication_style
                             (intern (string-upcase communication_style) :keyword)
                             :spawn)
   :sbcl-binary (or sbcl_binary "sbcl")
   :extra-eval  (when extra_eval (list extra_eval))))

(define-validated-tool "swank_kill"
  "Terminate a managed SBCL+Swank process by port number.
Only processes launched via swank_launch can be killed this way."
  :input-schema (list :port "integer")
  :output-schema (list :type "object")
  :requires-approval t
  :documentation-uri "file://docs/tools/swank-kill.md"
  :validation
  ((validate-required "port" port)
   (validate-integer "port" port :min 1024 :max 65535))
  :body
  (cl-tron-mcp/swank:kill-managed-process :port port))

(define-simple-tool "swank_process_list"
  "List all SBCL+Swank processes currently managed by this MCP instance."
  :input-schema nil
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-process-list.md"
  :function cl-tron-mcp/swank:list-managed-processes)

(define-validated-tool "swank_process_status"
  "Get the status of a specific managed SBCL+Swank process by port."
  :input-schema (list :port "integer")
  :output-schema (list :type "object")
  :requires-approval nil
  :documentation-uri "file://docs/tools/swank-process-status.md"
  :validation
  ((validate-required "port" port)
   (validate-integer "port" port :min 1024 :max 65535))
  :body
  (cl-tron-mcp/swank:managed-process-status :port port))
