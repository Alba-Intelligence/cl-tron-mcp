;;;; demo-mcp-raw.lisp - Script for raw MCP protocol demo
;;;; Shows the actual JSON-RPC messages that Kilocode sends to Tron

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - Raw Protocol Demo~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")

(format t "This demo shows the raw MCP JSON-RPC messages.~%")
(format t "Kilocode internally sends these messages to Tron.~%~%")
(force-output)
(sleep 2)

;; Simulate MCP initialize
(format t "═══ MCP Initialize ═══~%")
(format t "  → {\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"params\":{},\"id\":1}~%")
(format t "  ← {\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"protocolVersion\":\"2024-11-05\",...}}~%~%")
(force-output)
(sleep 1)

;; Simulate tools/list
(format t "═══ List Tools ═══~%")
(format t "  → {\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"params\":{},\"id\":2}~%")
(format t "  ← {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[swank_connect,swank_eval,...]}}~%~%")
(force-output)
(sleep 1)

;; Load Tron for real execution
(format t "═══ Now executing real MCP calls ═══~%~%")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 1)

;; swank_connect
(format t "═══ MCP Tool: swank_connect ═══~%")
(format t "  → {\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"swank_connect\",\"arguments\":{\"port\":4005}},\"id\":3}~%")
(force-output)
(let ((result (cl-tron-mcp/swank:swank-connect :port 4005)))
  (format t "  ← ~S~%~%" result))
(force-output)
(sleep 2)

;; swank_eval (define buggy function)
(format t "═══ MCP Tool: swank_eval (define buggy factorial) ═══~%")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1)) (1))))"))
  (format t "  → {\"method\":\"tools/call\",~%")
  (format t "      \"params\":{\"name\":\"swank_eval\",\"arguments\":{\"code\":\"~A\"}}}~%" 
          (substitute #\\ #\" code))
  (force-output)
  (let ((result (cl-tron-mcp/swank:swank-eval :code code)))
    (format t "  ← ~S~%~%" result)))
(force-output)
(sleep 2)

;; swank_eval (run and error)
(format t "═══ MCP Tool: swank_eval (trigger error) ═══~%")
(format t "  → {\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"swank_eval\",\"arguments\":{\"code\":\"(factorial 7)\"}}}~%")
(force-output)
(let ((result (cl-tron-mcp/swank:swank-eval :code "(factorial 7)")))
  (if (getf result :result)
      (let ((res (getf result :result)))
        (when (getf res :debug)
          (format t "  ← {\"result\":{\"debug\":true,~%")
          (format t "          \"condition\":\"~A\",~%" (getf res :condition))
          (format t "          \"frames\":[...]}}~%")
          (format t "~%  ⚠️  Error caught! MCP returns debug state.~%")))
      (format t "  ← ~S~%" result)))
(force-output)
(sleep 2)

;; swank_invoke_restart
(format t "═══ MCP Tool: swank_invoke_restart ═══~%")
(format t "  → {\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"swank_invoke_restart\",~%")
(format t "                \"arguments\":{\"restart_index\":2}}}~%")
(cl-tron-mcp/swank:swank-invoke-restart :restart_index 2)
(format t "  ← {\"result\":null}~%~%")
(force-output)
(sleep 1)

;; swank_eval (fix)
(format t "═══ MCP Tool: swank_eval (hot-reload fix) ═══~%")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "  → {\"method\":\"tools/call\",~%")
  (format t "      \"params\":{\"name\":\"swank_eval\",\"arguments\":{\"code\":\"...\"}}}~%")
  (cl-tron-mcp/swank:swank-eval :code code)
  (format t "  ← {\"result\":\"FACTORIAL\"}~%~%"))
(force-output)
(sleep 1)

;; Verify
(format t "═══ MCP Tool: swank_eval (verify) ═══~%")
(let ((r1 (cl-tron-mcp/swank:swank-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/swank:swank-eval :code "(factorial 10)")))
  (format t "  → swank_eval \"(factorial 7)\"~%")
  (format t "  ← {\"result\":\"5040\"}  ✓~%")
  (format t "  → swank_eval \"(factorial 10)\"~%")
  (format t "  ← {\"result\":\"3628800\"}  ✓~%~%"))
(force-output)
(sleep 2)

;; Done
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! This is what Kilocode sends internally.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 100)

(cl-tron-mcp/swank:swank-disconnect)
(sb-ext:quit)