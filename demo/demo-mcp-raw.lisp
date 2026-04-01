;;;; demo-mcp-raw.lisp - Script for raw MCP protocol demo
;;;; Shows the actual JSON-RPC messages that MCP clients send to Tron.
;;;; Self-contained: auto-launches Swank, runs demo, cleans up.

(format t "~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════~%")
(format t "════  TRON MCP - Raw Protocol Demo  (91 tools, 14 categories)~%")
(format t "════~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%~%")

(format t "This demo shows the raw MCP JSON-RPC messages.~%")
(format t "Cursor / Kilocode / OpenCode internally send these to Tron.~%~%")
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
(format t "  ← {\"jsonrpc\":\"2.0\",\"id\":2,\"result\":{\"tools\":[91 tools...]}}~%~%")
(force-output)
(sleep 1)

;; Load Tron for real execution
(format t "═══ Now executing real MCP calls ═══~%~%")
(ql:quickload :cl-tron-mcp :silent t)
(force-output)
(sleep 1)

;; swank_launch
(format t "═══ MCP Tool: swank_launch ═══~%")
(format t "  → {\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"swank_launch\",\"arguments\":{\"port\":14006}},\"id\":3}~%")
(force-output)
(cl-tron-mcp/swank:launch-sbcl-with-swank :port 14006)
(cl-tron-mcp/swank:wait-for-port 14006 :timeout 30)
(format t "  ← {\"result\":{\"success\":true,\"port\":14006}}~%~%")
(force-output)
(sleep 2)

;; repl_connect
(format t "═══ MCP Tool: repl_connect ═══~%")
(format t "  → {\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"repl_connect\",\"arguments\":{\"port\":14006}},\"id\":4}~%")
(force-output)
(let ((result (cl-tron-mcp/unified:repl-connect :port 14006)))
  (format t "  ← ~S~%~%" result))
(force-output)
(sleep 2)

;; repl_compile (define buggy function)
(format t "═══ MCP Tool: repl_compile (define buggy factorial) ═══~%")
(let ((code "(defun factorial (n) (if (> n 1) (* n (factorial (- n 1))) (/ 1 0)))"))
  (format t "  → {\"method\":\"tools/call\",~%")
  (format t "      \"params\":{\"name\":\"repl_compile\",\"arguments\":{\"code\":\"...\"}}}~%")
  (force-output)
  (let ((result (cl-tron-mcp/unified:repl-compile :code code)))
    (format t "  ← ~S~%~%" result)))
(force-output)
(sleep 2)

;; repl_eval (run and error)
(format t "═══ MCP Tool: repl_eval (trigger error) ═══~%")
(format t "  → {\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"repl_eval\",\"arguments\":{\"code\":\"(factorial 7)\"}}}~%")
(force-output)
(let ((result (cl-tron-mcp/unified:repl-eval :code "(factorial 7)")))
  (let ((inner (getf result :result)))
    (if (and (listp inner) (getf inner :debug))
        (progn
          (format t "  ← {\"result\":{\"debug\":true,\"condition\":...}}~%")
          (format t "~%  ⚠️  Error caught! MCP returns debug state.~%~%"))
        (format t "  ← ~S~%~%" result))))
(force-output)
(sleep 2)

;; repl_invoke_restart
(format t "═══ MCP Tool: repl_invoke_restart ═══~%")
(format t "  → {\"method\":\"tools/call\",~%")
(format t "      \"params\":{\"name\":\"repl_invoke_restart\",\"arguments\":{\"restart_index\":0}}}~%")
(cl-tron-mcp/unified:repl-invoke-restart :restart_index 0)
(format t "  ← {\"result\":\"aborted\"}~%~%")
(force-output)
(sleep 1)

;; repl_compile (fix)
(format t "═══ MCP Tool: repl_compile (hot-reload fix) ═══~%")
(let ((code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (format t "  → {\"method\":\"tools/call\",~%")
  (format t "      \"params\":{\"name\":\"repl_compile\",\"arguments\":{\"code\":\"...\"}}}~%")
  (cl-tron-mcp/unified:repl-compile :code code)
  (format t "  ← {\"result\":\"FACTORIAL\"}~%~%"))
(force-output)
(sleep 1)

;; Verify
(format t "═══ MCP Tool: repl_eval (verify) ═══~%")
(let ((r1 (cl-tron-mcp/unified:repl-eval :code "(factorial 7)"))
      (r2 (cl-tron-mcp/unified:repl-eval :code "(factorial 10)")))
  (format t "  → repl_eval \"(factorial 7)\"~%")
  (format t "  ← ~S  ✓~%" r1)
  (format t "  → repl_eval \"(factorial 10)\"~%")
  (format t "  ← ~S  ✓~%~%" r2))
(force-output)
(sleep 2)

;; Done
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(format t "════  ✅ Done! This is what MCP clients send internally.~%")
(format t "══════════════════════════════════════════════════════════════════════════════════════════════════════~%")
(force-output)
(sleep 5)

(cl-tron-mcp/unified:repl-disconnect)
(cl-tron-mcp/swank:kill-managed-process :port 14006)
(sb-ext:quit)