(push (uiop:pathname-directory-pathname (or (ignore-errors (asdf:system-source-file (asdf:find-system :cl-tron-mcp))) (truename *default-pathname-defaults*))) ql:*local-project-directories*)
(asdf:load-system :cl-tron-mcp)
(cl-tron-mcp/core:start-server :transport :stdio-only)
