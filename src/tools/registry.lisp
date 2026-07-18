;;;; src/tools/registry.lisp

(in-package :cl-tron-mcp/tools)

(defvar *tool-registry* (make-hash-table :test 'equal))

(defun normalize-argument-key (key)
  "Convert an incoming MCP argument key to the snake_case keyword expected by handlers."
  (let* ((raw (string key))
         (normalized
           (with-output-to-string (out)
             (loop for i from 0 below (length raw)
                   for ch = (char raw i)
                   for prev = (and (> i 0) (char raw (1- i)))
                   do (cond
                        ((char= ch #\-)
                         (write-char #\_ out))
                        ((and (alpha-char-p ch)
                              (char= ch (char-upcase ch))
                              (char/= ch (char-downcase ch))
                              prev
                              (or (lower-case-p prev)
                                  (digit-char-p prev)))
                         (write-char #\_ out)
                         (write-char ch out))
                        (t
                         (write-char (char-upcase ch) out)))))))
    (intern normalized :keyword)))

(defstruct tool-entry
  name
  descriptor
  handler)

;;; ------------------------------------------------------------------
;;; Schema normalization (cl-tron-mcp#2, "bug #7")
;;;
;;; ~90 tool registrations across src/tools/*.lisp author :input-schema
;;; and :output-schema as flat plists, e.g.
;;;   (list :objectId "string" :maxDepth "integer")
;;; or the bare-type shorthand
;;;   (list :type "object")
;;; Neither shape is valid JSON Schema: MCP requires
;;;   {"type":"object","properties":{...}}
;;; and every key in that authored plist is an UNESCAPED keyword, which
;;; SBCL's standard readtable upcases at read time (before any of this code
;;; runs) -- :objectId is already, irrecoverably, the symbol OBJECTID by the
;;; time register-tool sees it. The functions below normalize whatever shape
;;; a call site authored into a spec-valid JSON Schema hash-table, at this
;;; single chokepoint, without touching any of the ~90 call sites.
;;;
;;; Property-name casing choice: DOWNCASE (e.g. "objectid"), not an attempt
;;; to reconstruct "objectId". Reconstruction is impossible from a symbol
;;; whose original case is already gone. Downcasing is safe because
;;; NORMALIZE-ARGUMENT-KEY (below) upcases every letter of an incoming
;;; wire-argument key before it ever checks for a camelCase word-boundary,
;;; so "OBJECTID" (today's served spelling) and "objectid" (this fix's
;;; spelling) normalize identically -- both to :OBJECTID. Downcasing the
;;; served property name is therefore a pure cosmetic/shape fix: it changes
;;; no argument-binding behavior relative to the pre-fix baseline, so
;;; NORMALIZE-ARGUMENT-KEY needed no change. (Whether that baseline
;;; argument-binding is itself fully correct for every camelCase-vs-
;;; snake_case handler parameter is a separate, deeper, pre-existing
;;; question this fix does not touch -- see task-14 report.)

(defparameter *json-schema-primitive-types*
  '("object" "array" "string" "number" "integer" "boolean" "null")
  "The JSON Schema §6.1.1 \"type\" keyword's valid primitive values.
Claude Code's MCP client validates every property's \"type\" against
exactly this set and refuses the whole tools/list response (\"Connected
tools fetch failed\", reason \"type must be JSONType or JSONType[]\") if
any single one doesn't match -- confirmed via `claude mcp get cl-tron`
against src/installation/installation.lisp's skill_discover tool, authored
with :skill_registry \"map\" (a non-standard, made-up type name).")

(defun canonical-json-schema-type (type-name)
  "Coerce an authored type-name string to a valid JSON Schema primitive
type. Falls back to \"object\" for anything not in
*JSON-SCHEMA-PRIMITIVE-TYPES* (case-insensitively) -- e.g. \"map\", used at
one call site to mean a key/value dictionary, which IS a JSON object."
  (if (member type-name *json-schema-primitive-types* :test #'string-equal)
      (string-downcase type-name)
      "object"))

(defun schema-keyword-plist-p (value)
  "T if VALUE looks like a nested schema-keyword plist as authored at a
tool-registration call site, e.g. (:enum (\"a\" \"b\")): a non-empty list of
even length whose even-position elements are all keywords."
  (and (consp value)
       (evenp (length value))
       (loop for (k nil) on value by #'cddr always (keywordp k))))

(defun schema-keyword-plist->hash (plist)
  "Convert a nested schema-keyword plist (keys are JSON Schema keywords such
as :enum or :description -- single lowercase words, so downcasing recovers
their exact spelling losslessly) into a hash-table with lowercase string
keys. A value that is itself a schema-keyword plist recurses; anything else
(e.g. an :enum's list of allowed literal values) passes through unchanged so
it serializes as a plain JSON array/scalar, not a nested schema."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on plist by #'cddr
          do (setf (gethash (string-downcase (string k)) h)
                   (if (schema-keyword-plist-p v)
                       (schema-keyword-plist->hash v)
                       v)))
    h))

(defun schema-property-fragment (value)
  "Convert one authored property VALUE into a JSON Schema fragment:
a bare type-name string (\"string\", \"integer\", ...) becomes {\"type\":
value}; a nested schema-keyword plist (e.g. (:enum (...))) recurses via
SCHEMA-KEYWORD-PLIST->HASH; anything else passes through unchanged."
  (cond
    ((stringp value)
     (let ((h (make-hash-table :test 'equal)))
       (setf (gethash "type" h) (canonical-json-schema-type value))
       h))
    ((schema-keyword-plist-p value)
     (schema-keyword-plist->hash value))
    (t value)))

(defun normalize-tool-schema (schema)
  "Convert an authored :input-schema/:output-schema plist into a spec-valid
JSON Schema hash-table: {\"type\":\"object\",\"properties\":{...}}.

SCHEMA is one of:
  NIL                                     -- a no-argument tool
  (:type \"sometype\")                     -- bare-type shorthand, used
                                             throughout as a schema-less
                                             \"result is a JSON object\"
                                             output annotation (never used
                                             for a real, multi-property
                                             schema, so a single (:type ...)
                                             pair is unambiguous)
  (:propName \"type\" :propName2 ... )     -- flat property-name -> type
                                             (or nested descriptor) map

NIL always becomes {\"type\":\"object\",\"properties\":{}} -- an empty
HASH-TABLE, not an empty list, because json-compat's encode-tree serializes
NIL as \"[]\"; \"properties\" must serialize as \"{}\" when empty."
  (let ((h (make-hash-table :test 'equal)))
    (cond
      ((null schema)
       (setf (gethash "type" h) "object")
       (setf (gethash "properties" h) (make-hash-table :test 'equal)))
      ((and (= (length schema) 2)
            (keywordp (first schema))
            (string-equal (string (first schema)) "type")
            (stringp (second schema)))
       (setf (gethash "type" h) (canonical-json-schema-type (second schema))))
      (t
       (setf (gethash "type" h) "object")
       (let ((props (make-hash-table :test 'equal)))
         (loop for (k v) on schema by #'cddr
               do (setf (gethash (string-downcase (string k)) props)
                        (schema-property-fragment v)))
         (setf (gethash "properties" h) props))))
    h))

(defun register-tool (name description &key input-schema output-schema requires-approval documentation-uri concurrency)
  "Register a tool with descriptor and handler.
   requires-approval t means approvalLevel \"user\" (human must approve); nil means \"none\" (auto-run)."
  (let ((descriptor (make-hash-table :test 'equal)))
    ;; NOTE: keys are pipe-escaped (:|name|, not :name) so the reader preserves
    ;; their lowercase/camelCase spelling. An unescaped :name reads as the
    ;; symbol NAME (all upper), which json-compat's encode-tree serializes via
    ;; (string key) -- producing "NAME" in the wire JSON instead of "name".
    ;; This is the same convention handlers.lisp/handlers-initialize.lisp
    ;; already use for outgoing JSON-RPC keys; see cl-tron-mcp#2.
    (setf (gethash :|name| descriptor) name)
    (setf (gethash :|description| descriptor) description)
    ;; inputSchema/outputSchema: normalized to spec-valid JSON Schema shape
    ;; (see NORMALIZE-TOOL-SCHEMA above) -- this is "bug #7", the layer below
    ;; the top-level descriptor-key fix already applied to this file.
    (setf (gethash :|inputSchema| descriptor) (normalize-tool-schema input-schema))
    (setf (gethash :|outputSchema| descriptor) (normalize-tool-schema output-schema))
    (setf (gethash :|requiresApproval| descriptor) (or requires-approval nil))
    (setf (gethash :|approvalLevel| descriptor) (if requires-approval "user" "none"))
    (setf (gethash :|documentationUri| descriptor) documentation-uri)
    (setf (gethash :|concurrency| descriptor) (or concurrency "sequential"))
    (setf (gethash name *tool-registry*)
          (make-tool-entry :name name
                           :descriptor descriptor
                           :handler nil))))

(defun register-tool-handler (name handler)
  "Register handler function for already registered tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (setf (tool-entry-handler entry) handler))))

(defun list-tool-descriptors ()
  "Get list of all tool descriptors."
  (let ((descriptors (make-array 0 :adjustable t :fill-pointer 0)))
    (maphash (lambda (name entry)
               (declare (ignore name))
               (vector-push-extend (tool-entry-descriptor entry) descriptors))
             *tool-registry*)
    (coerce descriptors 'list)))

(defun get-tool-handler (name)
  "Get handler function for tool."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-handler entry))))

(defun get-tool-descriptor (name)
  "Get tool descriptor (hash table) for tool NAME, or nil if unknown."
  (let ((entry (gethash name *tool-registry*)))
    (when entry
      (tool-entry-descriptor entry))))

(defun tool-requires-user-approval-p (name)
  "Return t if tool NAME has approval level user (requires human approval)."
  (let ((desc (get-tool-descriptor name)))
    (and desc (gethash :|requiresApproval| desc))))

(defun call-tool (name arguments)
  "Call tool by name with arguments plist.
The plist keys are JSON-style (e.g., :|port|) and converted to proper keywords (:PORT)."
  (let ((handler (get-tool-handler name)))
    (unless handler
      (error "Unknown tool: ~a" name))
    (let ((args-list (loop for (key value) on arguments by #'cddr
                           for keyword = (normalize-argument-key key)
                           append (list keyword value))))
      (apply handler args-list))))

(defun load-all-tools ()
  "No-op: tool files are loaded by ASDF in dependency order as declared in cl-tron-mcp.asd.
This function is kept for backward compatibility but performs no action."
  nil)
