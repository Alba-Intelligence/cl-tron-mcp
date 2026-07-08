;;;; cl-tron-mcp.asd

(asdf:defsystem :cl-tron-mcp
  :name "Tron Debugging MCP"
  :description "Model Context Protocol server for developing, debugging, introspection, and hot code reloading wrapping Swank"
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging
    :cl-tron-mcp/repl
    :cl-tron-mcp/sbcl
    :cl-tron-mcp/monitor
    :cl-tron-mcp/resources
    :cl-tron-mcp/tracer
    :cl-tron-mcp/profiler
    :cl-tron-mcp/hot-reload
    :cl-tron-mcp/config
    :cl-tron-mcp/security
    :cl-tron-mcp/prompts
    :cl-tron-mcp/inspector
    :cl-tron-mcp/swank
    :cl-tron-mcp/xref
    :cl-tron-mcp/debugger
    :cl-tron-mcp/protocol
    :cl-tron-mcp/tools
    :cl-tron-mcp/installation
    :cl-tron-mcp/transport
    :cl-tron-mcp/core))

(asdf:defsystem :cl-tron-mcp/core
  :name "core"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging  
    :cl-tron-mcp/config 
    :cl-tron-mcp/transport)
  :components (
    ;; Core
    (:file "src/core/package")
    (:file "src/core/server")
    (:file "src/core/token-tracker")
    (:file "src/core/utils")))

(asdf:defsystem :cl-tron-mcp/transport
  :name "Logging"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket  :cl-ppcre :flexi-streams
    :hunchentoot
    :cl-tron-mcp/config 
    :cl-tron-mcp/protocol 
    :cl-tron-mcp/tools)
  :components (
    ;; Transports
    (:file "src/transport/package")
    (:file "src/transport/stdio")
    (:file "src/transport/http")
    (:file "src/transport/http-hunchentoot")
    (:file "src/transport/websocket")))

(asdf:defsystem :cl-tron-mcp/installation
  :name "Installation"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/config
    :cl-tron-mcp/tools)
  :components (
    (:file "src/installation/package")
    (:file "src/installation/installation")))

(asdf:defsystem :cl-tron-mcp/protocol 
  :name "Protocol"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :jonathan 
    :cl-tron-mcp/logging
    :cl-tron-mcp/resources
    :cl-tron-mcp/prompts 
    :cl-tron-mcp/config    
    :cl-tron-mcp/swank 
    :cl-tron-mcp/monitor 
    :cl-tron-mcp/tools)
  :components (
    ;; Protocols
    (:file "src/protocol/package")
    (:file "src/protocol/messages")
    (:file "src/protocol/handlers-utils")
    (:file "src/protocol/handlers-initialize")
    (:file "src/protocol/handlers-resources")
    (:file "src/protocol/handlers-prompts")
    (:file "src/protocol/handlers-ping")
    (:file "src/protocol/handlers")))

(asdf:defsystem :cl-tron-mcp/tools 
  :name "Tools"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging
    :cl-tron-mcp/config
    :cl-tron-mcp/repl
    :cl-tron-mcp/sbcl
    :cl-tron-mcp/monitor
    :cl-tron-mcp/tracer
    :cl-tron-mcp/profiler
    :cl-tron-mcp/hot-reload
    :cl-tron-mcp/monitor
    :cl-tron-mcp/security
    :cl-tron-mcp/inspector
    :cl-tron-mcp/swank
    :cl-tron-mcp/xref
    :cl-tron-mcp/debugger)    
  :components (
    (:file "src/tools/package")
    (:file "src/tools/utils")
    (:file "src/tools/registry")
    (:file "src/tools/help")
    (:file "src/tools/define-tool")
    (:file "src/tools/validation")
    (:file "src/tools/macros")
    (:file "src/tools/inspector-tools")
    (:file "src/tools/debugger-tools")
    (:file "src/tools/handlers-tools")
    (:file "src/tools/repl-tools")
    (:file "src/tools/hot-reload-tools")
    (:file "src/tools/profiler-tools")
    (:file "src/tools/tracer-tools")
    (:file "src/tools/thread-tools")
    (:file "src/tools/monitor-tools")
    (:file "src/tools/logging-tools")
    (:file "src/tools/xref-tools")
    (:file "src/tools/security-tools")
    (:file "src/tools/swank-tools")
    (:file "src/tools/process-tools")
    (:file "src/tools/unified-tools")
    (:file "src/tools/all")))

(asdf:defsystem :cl-tron-mcp/debugger 
  :name "Debugger"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on ( 
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/swank)
  :components (
    (:file "src/debugger/package")
    (:file "src/debugger/frames")
    (:file "src/debugger/restarts")
    (:file "src/debugger/breakpoints")
    (:file "src/debugger/stepping")))

(asdf:defsystem :cl-tron-mcp/xref
  :name "xref"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    :cl-tron-mcp/swank 
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/xref/package")
    (:file "src/xref/core")))

(asdf:defsystem :cl-tron-mcp/swank 
  :name "Swank"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging
    :cl-tron-mcp/resources
    :cl-tron-mcp/config)
  :components (
    (:file "src/swank/package")
    (:file "src/swank/protocol")
    (:file "src/swank/swank-connection")
    (:file "src/swank/swank-rpc")
    (:file "src/swank/swank-events")
    (:file "src/swank/swank-api")
    (:file "src/swank/process-manager")))

(asdf:defsystem :cl-tron-mcp/inspector
  :name "Inspector"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :closer-mop
    :cl-tron-mcp/sbcl)
  :components (
    ;; Inspector
    (:file "src/inspector/package")
    (:file "src/inspector/core")))


(asdf:defsystem :cl-tron-mcp/security
  :name "Security"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging )
  :components (
    (:file "src/security/package")
    (:file "src/security/approval")
    (:file "src/security/audit")))

(asdf:defsystem :cl-tron-mcp/config
  :name "Config"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :cl-tron-mcp/logging)
  :components (
    ;; Global config
    (:file "src/config/package")
    (:file "src/config/config")
    (:file "src/config/version")))

(asdf:defsystem :cl-tron-mcp/hot-reload
  :name "Hot Reload"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/hot-reload/package")
    (:file "src/hot-reload/core")))

(asdf:defsystem :cl-tron-mcp/profiler
  :name "Profiler"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :local-time
    )
  :components (
    (:file "src/profiler/package")
    (:file "src/profiler/core")))


(asdf:defsystem :cl-tron-mcp/tracer
  :name "Tracer"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/tracer/package")
    (:file "src/tracer/core")
    (:file "src/tracer/metrics")))
 

(asdf:defsystem :cl-tron-mcp/prompts 
  :name "Prompts"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/prompts/package")
    (:file "src/prompts/handler")
    (:file "src/prompts/individual/discover-mcp")
    (:file "src/prompts/individual/getting-started")
    (:file "src/prompts/individual/debugging-workflow")
    (:file "src/prompts/individual/hot-reload-workflow")
    (:file "src/prompts/individual/profiling-workflow")
    (:file "src/prompts/individual/token-optimization")
    (:file "src/prompts/individual/sbcl-debugging-expert")
    (:file "src/prompts/individual/hot-reload-specialist")
    (:file "src/prompts/individual/performance-engineer")))

(asdf:defsystem :cl-tron-mcp/resources 
  :name "Resources"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/resources/package")
    (:file "src/resources/handler")
    (:file "src/resources/error-codes")))

(asdf:defsystem :cl-tron-mcp/monitor 
  :name "Monitor"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    :local-time)
  :components (
    ;; Logging
    (:file "src/monitor/package")
    (:file "src/monitor/core")    
    (:file "src/monitor/request-tracing")))



(asdf:defsystem :cl-tron-mcp/sbcl
  :name "SBCL"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/sbcl/package")
    (:file "src/sbcl/eval")
    (:file "src/sbcl/compile")
    (:file "src/sbcl/threads")
    (:file "src/sbcl/debug-internals")))

(asdf:defsystem :cl-tron-mcp/repl
  :name "REPL"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/repl/package")
    (:file "src/repl/core")))

(asdf:defsystem :cl-tron-mcp/logging
  :name "Logging"
  :description ""
  :version "0.1.0"
  :author "Emmanuel Rialland [alba.intelligence@gmail.com]"
  :licence "Apache"
  :serial t
  :depends-on (
    :log4cl
    ; :jonathan :alexandria :local-time :bordeaux-threads :closer-mop :log4cl :usocket :hunchentoot :cl-ppcre :flexi-streams
    )
  :components (
    (:file "src/logging/package")
    (:file "src/logging/core")))


(asdf:defsystem :cl-tron-mcp/tests
  :name "cl-tron-mcp Tests"
  :description "Test suite for cl-tron-mcp"
  :author "Emmanuel"
  :licence "Apache"
  :depends-on (:cl-tron-mcp :rove)
  :serial t
 :components (
    (:file "tests/package")
    (:file "tests/core-test")
    (:file "tests/protocol-test")
    (:file "tests/security-test")
    (:file "tests/transport-test")
    (:file "tests/validation-test")
    (:file "tests/swank-test")
    (:file "tests/swank-integration-test")
    (:file "tests/mcp-e2e-test")
    (:file "tests/token-tracker-test")
    (:file "tests/monitor-test")
    (:file "tests/inspector-test")
    (:file "tests/hot-reload-test")
    (:file "tests/profiler-test")
    (:file "tests/xref-test")
    (:file "tests/logging-test")
    (:file "tests/process-manager-test")))

(asdf:defsystem :cl-tron-mcp/tests/integration
  :name "cl-tron-mcp Integration Tests"
  :description "Integration tests requiring a live Swank server on localhost:4006"
  :author "Emmanuel"
  :licence "Apache"
  :depends-on (:cl-tron-mcp :rove :usocket)
  :serial t
  :components ((:file "tests/integration/f1-f2-workflow-test")))