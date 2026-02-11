;;;; src/tools/all.lisp
;;;;
;;;; Tool loader module for MCP server.
;;;; This module exists solely to trigger loading of all tool modules,
;;;; which register themselves with the tool registry at load time.

(defpackage #:cl-tron-mcp/tools/all
  (:use #:cl)
  (:documentation "Loader module that imports all tool modules for registration side effects."))

(in-package #:cl-tron-mcp/tools/all)

;; Load all tool modules to trigger their register-tool calls.
;; The :import-from approach doesn't work with ASDF's serial loading
;; because defpackage is evaluated at compile-time before dependencies.
;; Instead, we load the files after all packages are defined.

;; Tool modules - these are already loaded by ASDF via cl-tron-mcp.asd
;; The tools register themselves with the registry at load time.
;; This file exists as a convenient entry point if needed.

;; No code needed here - ASDF's serial loading ensures all tool modules
;; are loaded before this file, and they register themselves automatically.
