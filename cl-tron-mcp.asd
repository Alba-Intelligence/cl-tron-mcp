;;;; cl-tron-mcp.asd

(asdf:defsystem :cl-tron-mcp
  :name "SBCL Debugging MCP"
  :description "Model Context Protocol server for SBCL debugging, introspection, and hot code reloading"
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (:jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre)
:components ((:file "src/core/package")
                 (:file "src/core/version")
                 (:file "src/logging/package")
                 (:file "src/logging/core")
                 (:file "src/core/config")
                 (:file "src/core/utils")
                (:file "src/sbcl/package")
                (:file "src/sbcl/eval")
                (:file "src/sbcl/compile")
                (:file "src/sbcl/threads")
                (:file "src/sbcl/debug-internals")
                (:file "src/security/package")
                (:file "src/security/approval")
                (:file "src/security/audit")
               (:file "src/xref/package")
               (:file "src/xref/core")
               (:file "src/swank/package")
               (:file "src/swank/protocol")
               (:file "src/swank/client")
               (:file "src/unified/package")
               (:file "src/unified/client")
               (:file "src/resources/package")
               (:file "src/resources/handler")
               (:file "src/prompts/package")
               (:file "src/prompts/handler")
(:file "src/tools/package")
                (:file "src/tools/registry")
                (:file "src/tools/define-tool")
                (:file "src/tools/validation")
                (:file "src/tools/all")
                (:file "src/protocol/package")
               (:file "src/protocol/messages")
               (:file "src/protocol/handlers")
               (:file "src/transport/package")
               (:file "src/transport/stdio")
               (:file "src/transport/http")
               (:file "src/transport/http-hunchentoot")
               (:file "src/transport/websocket")
               (:file "src/core/server")
               (:file "src/debugger/package")
               (:file "src/debugger/frames")
               (:file "src/debugger/restarts")
               (:file "src/debugger/breakpoints")
               (:file "src/debugger/stepping")
               (:file "src/inspector/package")
               (:file "src/inspector/core")
               (:file "src/repl/package")
               (:file "src/repl/core")
               (:file "src/hot-reload/package")
               (:file "src/hot-reload/core")
               (:file "src/profiler/package")
               (:file "src/profiler/core")
               (:file "src/tracer/package")
               (:file "src/tracer/core")
               (:file "src/monitor/package")
               (:file "src/monitor/core")
               (:file "src/tools/register-tools")))

(asdf:defsystem :cl-tron-mcp/tests
  :name "cl-tron-mcp Tests"
  :description "Test suite for cl-tron-mcp"
  :author "Emmanuel"
  :licence "Apache"
  :depends-on (:cl-tron-mcp :rove)
  :serial t
:components ((:file "tests/package")
                (:file "tests/core-test")
                (:file "tests/protocol-test")
                (:file "tests/security-test")
                (:file "tests/transport-test")
                (:file "tests/validation-test")
                (:file "tests/swank-test")
                (:file "tests/swank-integration-test")
                (:file "tests/mcp-e2e-test")))
