;;;; src/protocol/handlers-utils.lisp
;;;;
;;;; The JSON-RPC helpers used by the protocol handlers (VALIDATE-*-PARAM, MAKE-ERROR-RESPONSE)
;;;; are defined once, in src/tools/handlers-support.lisp, and imported by the protocol package
;;;; (see package.lisp). Protocol depends on tools, not the reverse, so there is nothing to duplicate
;;;; here. This file is kept so existing load order and `provide` names stay valid.

(in-package :cl-tron-mcp/protocol)

(provide :cl-tron-mcp/protocol-handlers-utils)
