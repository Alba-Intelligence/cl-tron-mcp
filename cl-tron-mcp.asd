;;;; cl-tron-mcp.asd

(asdf:defsystem :cl-tron-mcp
  :name "SBCL Debugging MCP"
  :description "Model Context Protocol server for SBCL debugging, introspection, and hot code reloading"
  :version "0.1.0"
  :author "Emmanuel[emms@anoma.ai]"
  :licence "MIT"
  :serial t
  :depends-on (:jonathan :alexandria :local-time :bordeaux-threads)
  :components ((:file "src/core/package")
               (:file "src/core/version")
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
               (:file "src/tools/package")
               (:file "src/tools/registry")
               (:file "src/tools/define-tool")
               (:file "src/protocol/package")
               (:file "src/protocol/messages")
               (:file "src/protocol/handlers")
               (:file "src/transport/package")
               (:file "src/transport/stdio")
               (:file "src/transport/http")
               (:file "src/transport/websocket")
               (:file "src/debugger/package")
               (:file "src/debugger/frames")
               (:file "src/debugger/restarts")
               (:file "src/debugger/breakpoints")
               (:file "src/debugger/stepping")))

(asdf:defsystem :cl-tron-mcp/tests
  :name "cl-tron-mcp Tests"
  :description "Test suite for cl-tron-mcp"
  :author "Emmanuel"
  :licence "MIT"
  :depends-on (:cl-tron-mcp :rove)
  :serial t
  :components ((:file "tests/package")
               (:file "tests/core-test")
               (:file "tests/protocol-test")
               (:file "tests/security-test")))
