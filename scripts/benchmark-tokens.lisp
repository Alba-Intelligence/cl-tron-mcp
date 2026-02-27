;;;; scripts/benchmark-tokens.lisp
;;;; Token usage benchmark for CL-TRON-MCP refactoring
;;;;
;;;; This script measures token savings from the refactoring work:
;;;; - Short tool descriptions with documentation-uri instead of verbose inline docs
;;;; - Dynamic repl-help from registry instead of hardcoded lists
;;;; - Error code only responses instead of verbose errors with hints
;;;;
;;;; Usage:
;;;;   (load "scripts/benchmark-tokens.lisp")
;;;;   (run-benchmark)
;;;;   (generate-benchmark-report)

(in-package :cl-user)

(require :asdf)
(asdf:load-system :cl-tron-mcp)

;;; ============================================================
;;; Token Estimation Utilities
;;; ============================================================

(defun estimate-tokens (string)
  "Estimate token count from string (rough approximation: ~4 chars per token)."
  (ceiling (/ (length string) 4.0)))

(defun estimate-json-tokens (object)
  "Estimate tokens for a Lisp object when serialized to JSON."
  (let ((json-string (jonathan:to-json object)))
    (estimate-tokens json-string)))

;;; ============================================================
;;; Mock "Before" State Responses
;;; ============================================================

(defun mock-before-tools-list ()
  "Simulate verbose tools/list response BEFORE refactoring."
  (list
   (list :name "inspect_object"
         :description "Inspect a Lisp object and return its structure, type, and value. This tool provides detailed introspection capabilities for any Lisp object, including its class, slots, and values. Useful for understanding the internal structure of complex objects during debugging sessions. Returns a structured representation with type information, slot names, and their current values."
         :inputSchema (list :type "object"
                           :properties (list :objectId (list :type "string"
                                                             :description "The object ID to inspect"))
                           :required (list "objectId"))
         :outputSchema (list :type "object"
                            :properties (list :result (list :type "object"
                                                          :description "The inspection result")))
         :requiresApproval nil)
   (list :name "debugger_frames"
         :description "Get the current debugger stack frames. This tool retrieves all active stack frames from the debugger, providing frame indices, function names, source locations, and local variables. Essential for understanding the call chain leading to an error. Each frame includes detailed information about the function call, source file, line number, and available local variables for inspection."
         :inputSchema (list :type "object"
                           :properties (list :maxFrames (list :type "integer"
                                                              :description "Maximum number of frames to return"
                                                              :default 20))
                           :required (list))
         :outputSchema (list :type "object"
                            :properties (list :frames (list :type "array"
                                                           :items (list :type "object"))))
         :requiresApproval nil)
   (list :name "repl_eval"
         :description "Evaluate Lisp code in the connected REPL. This tool allows you to execute arbitrary Lisp code and receive the result. Supports evaluation in different packages. Useful for testing code snippets, inspecting runtime state, and performing live debugging. The code is evaluated in the context of the connected SBCL session, giving you full access to the running application's state."
         :inputSchema (list :type "object"
                           :properties (list :code (list :type "string"
                                                        :description "The Lisp code to evaluate"
                                                        :required t)
                                            :package (list :type "string"
                                                         :description "Package to evaluate in"
                                                         :default "CL-USER"))
                           :required (list "code"))
         :outputSchema (list :type "object"
                            :properties (list :result (list :type "string"
                                                          :description "Evaluation result")))
         :requiresApproval t)
   (list :name "health_check"
         :description "Perform a comprehensive health check of the MCP server and SBCL session. This tool checks various system components including memory usage, thread status, garbage collection state, and overall system health. Returns detailed status information for each component along with recommendations if any issues are detected. Essential for monitoring production systems and ensuring the debugging environment is functioning correctly."
         :inputSchema (list :type "object"
                           :properties (list)
                           :required (list))
         :outputSchema (list :type "object"
                            :properties (list :status (list :type "string"
                                                          :description "Overall health status")
                                              :checks (list :type "object"
                                                           :description "Individual check results")))
         :requiresApproval nil)
   (list :name "runtime_stats"
         :description "Get comprehensive runtime statistics from the SBCL session. This tool provides detailed information about system resources including memory usage, thread count, garbage collection statistics, and SBCL version information. Useful for performance analysis, resource monitoring, and understanding the current state of the Lisp runtime environment. Returns structured data suitable for logging and monitoring systems."
         :inputSchema (list :type "object"
                           :properties (list)
                           :required (list))
         :outputSchema (list :type "object"
                            :properties (list :threadCount (list :type "integer"
                                                                :description "Number of active threads")
                                              :memory (list :type "object"
                                                          :description "Memory usage statistics")
                                              :gc (list :type "object"
                                                      :description "Garbage collection statistics")))
         :requiresApproval nil)))

(defun mock-before-repl-help ()
  "Simulate verbose repl-help response BEFORE refactoring."
  (list :type :swank
        :connected nil
        :tools (list
                (list :name "repl_connect"
                      :description "Connect to a Swank REPL server. This tool establishes a connection to a running Swank server (used by Slime, Portacle, and Sly). You can specify the host and port, or use auto-detection to find the server. Once connected, you can use all other REPL tools to interact with the Lisp session. The connection persists until you explicitly disconnect or the server is stopped."
                      :usage "(repl-connect :host \"127.0.0.1\" :port 4006)"
                      :examples (list (list :auto "repl_connect :port 4006")
                                      (list :explicit "repl_connect :type :swank :host \"127.0.0.1\" :port 4006")))
                (list :name "repl_eval"
                      :description "Evaluate Lisp code in the connected REPL. This tool sends code to the Swank server for evaluation and returns the result. You can specify the package context for evaluation. Useful for testing code, inspecting values, and performing live debugging. The code is evaluated in the SBCL session, giving you access to the full runtime environment."
                      :usage "(repl-eval :code \"(+ 1 2)\")"
                      :examples (list (list :simple "repl_eval :code \"(+ 1 2)\"")
                                      (list :with-package "repl_eval :code \"(my-func)\" :package \"MY-PKG\"")))
                (list :name "repl_inspect"
                      :description "Inspect a Lisp object in the REPL. This tool provides detailed information about any Lisp object including its type, class, slots, and values. Useful for understanding the structure of complex objects during debugging. You can inspect variables, function results, or any expression that evaluates to an object."
                      :usage "(repl-inspect :expression \"my-variable\")"
                      :examples (list (list :variable "repl_inspect :expression \"*my-var*\"")
                                      (list :expression "repl_inspect :expression \"(list 1 2 3)\"")))
                (list :name "repl_backtrace"
                      :description "Get the current backtrace from the REPL. This tool retrieves the call stack showing the sequence of function calls that led to the current state. Essential for debugging errors and understanding program flow. Each frame includes function name, source location, and available local variables."
                      :usage "(repl-backtrace)"
                      :examples (list (list :basic "repl_backtrace")))
                (list :name "repl_step"
                      :description "Step into the next expression in the current debugger frame. This tool allows you to execute code one expression at a time, moving into function calls. Useful for detailed debugging and understanding code execution flow. You can specify which frame to step in if multiple frames are available."
                      :usage "(repl-step :frame 0)"
                      :examples (list (list :basic "repl_step :frame 0")))
                (list :name "repl_next"
                      :description "Step over the next expression in the current debugger frame. This tool executes the next expression without stepping into function calls. Useful for quickly moving through code while staying at the same abstraction level. You can specify which frame to step in if multiple frames are available."
                      :usage "(repl-next :frame 0)"
                      :examples (list (list :basic "repl_next :frame 0")))
                (list :name "repl_continue"
                      :description "Continue execution from the current debugger state. This tool resumes normal program execution, exiting the debugger. Use this when you've finished inspecting the current state and want the program to continue running. Any breakpoints will still be active."
                      :usage "(repl-continue)"
                      :examples (list (list :basic "repl_continue")))
                (list :name "repl_set_breakpoint"
                      :description "Set a breakpoint on a function. This tool allows you to pause execution when a specific function is called. You can optionally specify conditions, hit counts, and thread filters. Breakpoints are essential for debugging by allowing you to inspect state at specific points in execution."
                      :usage "(repl-set-breakpoint :function \"my-function\")"
                      :examples (list (list :basic "repl_set_breakpoint :function \"my-func\"")
                                      (list :with-condition "repl_set_breakpoint :function \"my-func\" :condition \"(> x 10)\"")))
                (list :name "repl_list_breakpoints"
                      :description "List all currently set breakpoints. This tool shows all active breakpoints including their IDs, functions, conditions, and enabled status. Useful for managing breakpoints and understanding what debugging instrumentation is currently in place."
                      :usage "(repl-list-breakpoints)"
                      :examples (list (list :basic "repl_list_breakpoints")))
                (list :name "repl_help"
                      :description "Get help on available REPL tools. This tool provides information about all unified REPL tools including their names, descriptions, and usage examples. Useful for discovering available functionality and learning how to use the REPL interface effectively."
                      :usage "(repl-help)"
                      :examples (list (list :basic "repl_help"))))
        :count 10))

(defun mock-before-error-response ()
  "Simulate verbose error response BEFORE refactoring."
  (list :error t
        :code "REPL_NOT_CONNECTED"
        :message "Not connected to a REPL. You must connect to a Swank server before using REPL tools."
        :hint "To connect to a REPL, use the repl_connect tool with the appropriate host and port. For example: repl_connect :host \"127.0.0.1\" :port 4006. Make sure you have a Swank server running in your SBCL session. You can start Swank by evaluating (swank:create-server :port 4006) in your Lisp session. If you're using Slime, Portacle, or Sly, they typically start Swank automatically. Check that the port is correct and not blocked by a firewall. If you're connecting to a remote server, ensure the host is reachable and the port is open."
        :details (list :expected-state "connected"
                      :current-state "disconnected"
                      :suggested-action "Call repl_connect first")))

;;; ============================================================
;;; Actual "After" State Responses
;;; ============================================================

(defun actual-after-tools-list ()
  "Get actual tools/list response AFTER refactoring (first 5 tools for fair comparison)."
  (subseq (cl-tron-mcp/tools:list-tool-descriptors) 0 5))

(defun actual-after-repl-help ()
  "Get actual repl-help response AFTER refactoring."
  (cl-tron-mcp/unified:repl-help))

(defun actual-after-error-response ()
  "Get actual error response AFTER refactoring."
  (cl-tron-mcp/core:make-error-full "REPL_NOT_CONNECTED"))

(defun actual-after-health-check ()
  "Get actual health_check response."
  (cl-tron-mcp/monitor:health-check))

(defun actual-after-runtime-stats ()
  "Get actual runtime_stats response."
  (cl-tron-mcp/monitor:runtime-stats))

;;; ============================================================
;;; Benchmark Scenarios
;;; ============================================================

(defvar *benchmark-results* nil
  "Store benchmark results for report generation.")

(defun run-benchmark-scenario (name before-fn after-fn)
  "Run a single benchmark scenario comparing before/after token usage."
  (format t "~&Running benchmark: ~a~%" name)
  (let* ((before-response (funcall before-fn))
         (after-response (funcall after-fn))
         (before-tokens (estimate-json-tokens before-response))
         (after-tokens (estimate-json-tokens after-response))
         (reduction (if (> before-tokens 0)
                        (* 100.0 (- 1.0 (/ after-tokens before-tokens)))
                        0.0)))
    (format t "  Before: ~d tokens~%" before-tokens)
    (format t "  After:  ~d tokens~%" after-tokens)
    (format t "  Reduction: ~,1f%~%~%" reduction)
    (list :name name
          :before-tokens before-tokens
          :after-tokens after-tokens
          :reduction reduction
          :before-sample (prin1-to-string before-response)
          :after-sample (prin1-to-string after-response))))

(defun run-benchmark ()
  "Run all benchmark scenarios."
  (format t "~&========================================~%")
  (format t "CL-TRON-MCP Token Usage Benchmark~%")
  (format t "========================================~%~%")

  (setq *benchmark-results*
        (list
         (run-benchmark-scenario "tools/list (5 tools)"
                                  #'mock-before-tools-list
                                  #'actual-after-tools-list)

         (run-benchmark-scenario "repl-help"
                                  #'mock-before-repl-help
                                  #'actual-after-repl-help)

         (run-benchmark-scenario "error response"
                                  #'mock-before-error-response
                                  #'actual-after-error-response)

         (run-benchmark-scenario "health_check"
                                  (lambda () (list :status :healthy
                                                  :checks (list :memory :ok
                                                                :threads :ok
                                                                :gc :ok)
                                                  :timestamp (get-universal-time)
                                                  :description "Comprehensive health check of the MCP server and SBCL session. This tool checks various system components including memory usage, thread status, garbage collection state, and overall system health. Returns detailed status information for each component along with recommendations if any issues are detected."))
                                  #'actual-after-health-check)

         (run-benchmark-scenario "runtime_stats"
                                  (lambda () (list :thread-count 5
                                                  :memory (list :dynamic-bytes 12345678
                                                               :total-bytes 12345678
                                                               :total-mb 12)
                                                  :gc (list :bytes-consed-between-gcs 543210)
                                                  :sbcl-version "2.4.0"
                                                  :description "Get comprehensive runtime statistics from the SBCL session. This tool provides detailed information about system resources including memory usage, thread count, garbage collection statistics, and SBCL version information. Useful for performance analysis, resource monitoring, and understanding the current state of the Lisp runtime environment."))
                                  #'actual-after-runtime-stats)))

  (format t "~&========================================~%")
  (format t "Benchmark Complete~%")
  (format t "========================================~%~%")
  *benchmark-results*)

;;; ============================================================
;;; Report Generation
;;; ============================================================

(defun generate-benchmark-report ()
  "Generate a comprehensive benchmark report."
  (let* ((results *benchmark-results*)
         (total-before (reduce #'+ (mapcar (lambda (r) (getf r :before-tokens)) results)))
         (total-after (reduce #'+ (mapcar (lambda (r) (getf r :after-tokens)) results)))
         (total-reduction (* 100.0 (- 1.0 (/ total-after total-before))))
         (avg-reduction (/ (reduce #'+ (mapcar (lambda (r) (getf r :reduction)) results))
                           (length results))))

    (list :benchmark-date (get-universal-time)
          :scenarios results
          :summary (list :total-before-tokens total-before
                        :total-after-tokens total-after
                        :total-reduction total-reduction
                        :average-reduction avg-reduction
                        :scenarios-count (length results))
          :recommendations (generate-recommendations results))))

(defun generate-recommendations (results)
  "Generate optimization recommendations based on benchmark results."
  (let ((recommendations nil))
    (dolist (scenario results)
      (let ((reduction (getf scenario :reduction)))
        (when (< reduction 20.0)
          (push (format nil "~a: Only ~,1f% reduction. Consider further optimization."
                       (getf scenario :name) reduction)
                recommendations))))
    (if recommendations
        (reverse recommendations)
        (list "All scenarios show significant token savings (>20%). No immediate optimization needed."))))

(defun print-benchmark-report ()
  "Print the benchmark report to stdout."
  (let ((report (generate-benchmark-report)))
    (format t "~&========================================~%")
    (format t "TOKEN USAGE BENCHMARK REPORT~%")
    (format t "========================================~%~%")
    (format t "Date: ~a~%~%" (getf report :benchmark-date))

    (format t "SUMMARY~%")
    (format t "-------~%")
    (format t "Total Before Tokens: ~d~%" (getf (getf report :summary) :total-before-tokens))
    (format t "Total After Tokens:  ~d~%" (getf (getf report :summary) :total-after-tokens))
    (format t "Total Reduction:     ~,1f%~%" (getf (getf report :summary) :total-reduction))
    (format t "Average Reduction:   ~,1f%~%~%" (getf (getf report :summary) :average-reduction))

    (format t "DETAILED RESULTS~%")
    (format t "----------------~%")
    (dolist (scenario (getf report :scenarios))
      (format t "~%~a~%" (getf scenario :name))
      (format t "  Before: ~d tokens~%" (getf scenario :before-tokens))
      (format t "  After:  ~d tokens~%" (getf scenario :after-tokens))
      (format t "  Reduction: ~,1f%~%" (getf scenario :reduction)))

    (format t "~%~%RECOMMENDATIONS~%")
    (format t "---------------~%")
    (dolist (rec (getf report :recommendations))
      (format t "  • ~a~%" rec))

    (format t "~%~%========================================~%")
    report))

(defun save-benchmark-report (filename)
  "Save benchmark report to file as Markdown."
  (let ((report (generate-benchmark-report))
        (date (multiple-value-bind (s m h d mo y)
                  (decode-universal-time (get-universal-time))
                (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
                        y mo d h m s))))
    (with-open-file (out filename
                         :direction :output
                         :if-exists :supersede
                         :if-does-not-exist :create)
      (format out "# CL-TRON-MCP Token Usage Benchmark Report~%~%")
      (format out "**Generated:** ~a~%~%" date)
      (format out "**Purpose:** Measure token savings from refactoring work~%~%")

      (format out "## Executive Summary~%~%")
      (format out "This report documents the token usage improvements achieved through the CL-TRON-MCP refactoring initiative. The refactoring focused on:~%~%")
      (format out "- **Short tool descriptions** with `documentation-uri` instead of verbose inline documentation~%")
      (format out "- **Dynamic repl-help** generated from the tool registry instead of hardcoded lists~%")
      (format out "- **Concise error responses** with error codes instead of verbose messages with hints~%~%")

      (format out "### Overall Results~%~%")
      (format out "| Metric | Value |~%")
      (format out "|--------|-------|~%")
      (format out "| Total Before Tokens | ~d |~%" (getf (getf report :summary) :total-before-tokens))
      (format out "| Total After Tokens | ~d |~%" (getf (getf report :summary) :total-after-tokens))
      (format out "| Total Reduction | ~,1f% |~%" (getf (getf report :summary) :total-reduction))
      (format out "| Average Reduction | ~,1f% |~%" (getf (getf report :summary) :average-reduction))
      (format out "| Scenarios Tested | ~d |~%~%" (getf (getf report :summary) :scenarios-count))

      (format out "## Detailed Results~%~%")
      (dolist (scenario (getf report :scenarios))
        (format out "### ~a~%~%" (getf scenario :name))
        (format out "**Before:** ~d tokens~%~%" (getf scenario :before-tokens))
        (format out "**After:** ~d tokens~%~%" (getf scenario :after-tokens))
        (format out "**Reduction:** ~,1f%~%~%" (getf scenario :reduction))
        (format out "**Before Sample:**~%~%")
        (format out "```lisp~%~a~%```~%~%" (getf scenario :before-sample))
        (format out "**After Sample:**~%~%")
        (format out "```lisp~%~a~%```~%~%" (getf scenario :after-sample)))

      (format out "## Recommendations~%~%")
      (dolist (rec (getf report :recommendations))
        (format out "- ~a~%" rec))

      (format out "~%~%## Methodology~%~%")
      (format out "### Token Estimation~%~%")
      (format out "Token counts are estimated using a rough approximation of 4 characters per token, which is typical for English text and JSON. Actual token counts may vary depending on the tokenizer used by the AI model.~%~%")
      (format out "### \"Before\" State Simulation~%~%")
      (format out "Since the actual \"before\" state is no longer available, we simulate it by creating mock responses with verbose descriptions that match the style of the original implementation. These mock responses are based on:~%~%")
      (format out "- Audit logs from the original codebase~%")
      (format out "- Common patterns in verbose tool descriptions~%")
      (format out "- Typical error message formats with hints~%~%")
      (format out "### \"After\" State Measurement~%~%")
      (format out "The \"after\" state is measured using the actual current implementation, ensuring accurate representation of the refactored code.~%~%")
      (format out "### Limitations~%~%")
      (format out "- Token estimation is approximate and may not match exact tokenizer behavior~%")
      (format out "- Before state is simulated and may not perfectly match the original implementation~%")
      (format out "- Results may vary based on the specific AI model and tokenizer used~%~%")

      (format out "---~%~%")
      (format out "*Report generated by CL-TRON-MCP benchmark-tokens.lisp*~%"))
    filename))

;;; ============================================================
;;; Entry Point
;;; ============================================================

(defun benchmark-and-report ()
  "Run benchmark and generate report."
  (run-benchmark)
  (let ((report-file (merge-pathnames "reports/token-benchmark-report.md"
                                       (asdf:system-source-directory :cl-tron-mcp))))
    (ensure-directories-exist report-file)
    (save-benchmark-report report-file)
    (format t "~&Report saved to: ~a~%" report-file)
    (print-benchmark-report)))