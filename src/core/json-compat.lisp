;;;; src/core/json-compat.lisp
;;;;
;;;; JSON compatibility shim: provides jonathan-style TO-JSON / PARSE
;;;; semantics (keyword plists as objects; :false / :null / :empty
;;;; markers) implemented over com.inuoe.jzon, whose native convention
;;;; uses hash-tables for objects and CL NIL / the symbol NULL for
;;;; false / null.
;;;;
;;;; Rationale: jonathan hard-depends on :cl-syntax / :cl-syntax-annot
;;;; / :cl-annot (2015-era reader-macro machinery, jonathan.asd) which
;;;; breaks under SBCL 2.6.4 + current ASDF when loaded transitively
;;;; (see .superpowers/sdd/task-7-report.md, Failure #2: "Nihil ex
;;;; nihil. (can't set SYMBOL-VALUE of NIL)"). jzon has no such
;;;; dependency. This shim exists so every call site that used to read
;;;; "jonathan:to-json" / "jonathan:parse" can instead read
;;;; "cl-tron-mcp/json-compat:to-json" / "...:parse" with zero change
;;;; to the surrounding code's data shapes.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :com.inuoe.jzon :silent t))

(defpackage :cl-tron-mcp/json-compat
  (:use :cl)
  (:export :to-json :parse))

(in-package :cl-tron-mcp/json-compat)

(defun jonathan-plist-p (list)
  "T if LIST is object-shaped in jonathan's convention: non-empty,
every even-position element is a keyword, even total length.
Mirrors jonathan.util:my-plist-p exactly (jonathan-20200925-git/src/util.lisp)."
  (and (consp list)
       (loop for (key val next) on list by #'cddr
             if (not (keywordp key)) return nil
             else unless next return t
             finally (return t))))

(defun encode-tree (obj)
  "Convert a jonathan-convention value tree into a jzon-convention tree."
  (cond
    ((eq obj :false) nil)
    ((eq obj :null) 'cl:null)
    ((eq obj :empty) (make-hash-table :test 'equal))
    ((eq obj t) t)
    ((null obj) #())                    ; jonathan: NIL encodes as "[]"
    ((stringp obj) obj)                 ; strings are vectors -- must precede vectorp
    ((hash-table-p obj)
     (let ((out (make-hash-table :test 'equal)))
       (maphash (lambda (k v)
                  (setf (gethash (string k) out) (encode-tree v)))
                obj)
       out))
    ((and (consp obj) (jonathan-plist-p obj))
     (let ((out (make-hash-table :test 'equal)))
       (loop for (k v) on obj by #'cddr
             do (setf (gethash (string k) out) (encode-tree v)))
       out))
    ((vectorp obj) (map 'vector #'encode-tree obj))
    ((consp obj) (mapcar #'encode-tree obj))
    ((symbolp obj) (symbol-name obj))
    (t obj)))

(defun to-json (obj)
  "Jonathan-compatible TO-JSON, implemented over com.inuoe.jzon:stringify."
  (com.inuoe.jzon:stringify (encode-tree obj)))

(defun decode-tree (obj)
  "Convert a jzon-parsed value back into a jonathan-convention value tree."
  (cond
    ((eq obj 'cl:null) :null)
    ((eq obj t) t)
    ((null obj) :false)                 ; jzon parse: JSON false => NIL
    ((stringp obj) obj)                 ; strings are vectors -- must precede vectorp
    ((hash-table-p obj)
     (let (out)
       (maphash (lambda (k v)
                  (setf out (list* (intern k :keyword) (decode-tree v) out)))
                obj)
       out))
    ((vectorp obj) (map 'list #'decode-tree obj))
    (t obj)))

(defun parse (json-string)
  "Jonathan-compatible PARSE, implemented over com.inuoe.jzon:parse."
  (decode-tree (com.inuoe.jzon:parse json-string)))
