(push #p"/home/emmanuel/quicklisp/local-projects/cl-tron-mcp/" ql:*local-project-directories*)
(asdf:load-system :cl-tron-mcp)
(cl-tron-mcp/core:start-server :transport :stdio)
