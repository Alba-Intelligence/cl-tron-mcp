;;;; src/swank/protocol.lisp - Swank wire protocol implementation

(defpackage #:cl-tron-mcp/swank-protocol
  (:use :cl)
  (:export
   #:read-packet
   #:write-message
   #:read-message
   #:read-form
   #:utf8-to-string
   #:string-to-utf8
   #:prin1-to-string-for-emacs
   #:swank-protocol-error
   #:swank-read-error
   #:swank-write-error))

(in-package #:cl-tron-mcp/swank-protocol)

;;; ============================================================
;;; Swank Wire Protocol
;;; ============================================================
;;;
;;; The Swank protocol uses length-prefixed S-expressions over a
;;; bidirectional stream. Each message is preceded by a 6-character
;;; hexadecimal string indicating the payload length in bytes.
;;;
;;; Example:
;;;   "000039(:emacs-rex (swank:connection-info) nil t 1)"
;;;
;;; References:
;;; - git_examples/slime/swank/rpc.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; UTF-8 Encoding/Decoding
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun string-to-utf8 (string)
  "Convert STRING to UTF-8 byte vector."
  #+sbcl
  (sb-ext:string-to-octets string :external-format :utf-8)
  #+ccl
  (ccl:encode-string-to-octets string :external-format :utf-8)
  #+(or ecl abcl)
  (let ((stream (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop for code across string
          do (cond ((< code 128)
                    (vector-push-extend code stream))
                   (t
                    (let ((bytes (char-code code)))
                      (when (>= bytes #x1000)
                        (vector-push-extend (ldb (byte 3 12) bytes) stream))
                      (vector-push-extend (ldb (byte 6 6) bytes) stream)
                      (vector-push-extend (ldb (byte 6 0) bytes) stream)))))
    stream)
  #-(or sbcl ccl ecl abcl)
  (error "UTF-8 encoding not implemented for this Lisp"))

(defun utf8-to-string (octets)
  "Convert UTF-8 byte vector to STRING."
  #+sbcl
  (sb-ext:octets-to-string octets :external-format :utf-8)
  #+ccl
  (ccl:decode-string-from-octets octets :external-format :utf-8)
  #+ecl
  (ext:octets-to-string octets :external-format :utf-8)
  #+abcl
  (error "UTF-8 decoding not implemented for this Lisp")
  #-(or sbcl ccl ecl abcl)
  (error "UTF-8 decoding not supported"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Framing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-chunk (stream length)
  "Read exactly LENGTH bytes from STREAM into a vector."
  (let* ((buffer (make-array length :element-type '(unsigned-byte 8)))
         (count (read-sequence buffer stream)))
    (cond ((= count length)
           buffer)
          ((zerop count)
           (error 'end-of-file :stream stream))
          (t
           (error "Short read: expected ~D bytes, got ~D" length count)))))

(defun parse-header (stream)
  "Read 6 characters from STREAM and parse as hexadecimal integer."
  (let ((hex-chars (read-chunk stream 6)))
    (parse-integer (map 'string #'code-char hex-chars) :radix 16)))

(defun read-packet (stream)
  "Read a single Swank message packet from STREAM.
Returns the decoded S-expression string."
  (handler-case
      (let* ((length (parse-header stream))
             (octets (read-chunk stream length))
             (string (utf8-to-string octets)))
        string)
    (condition (c)
      (error 'swank-read-error :condition c :stream stream))))

(defun write-header (stream length)
  "Write LENGTH as 6-digit hexadecimal to STREAM."
  (declare (type (unsigned-byte 24) length))
  (let ((header (format nil "~6,'0x" length)))
    (loop for c across header
          do (write-byte (char-code c) stream))))

(defun write-message (message package stream)
  "Write MESSAGE as Swank protocol packet to STREAM.
MESSAGE is an S-expression that will be printed with *PRINT-CASE* = :downcase."
  (handler-case
      (let* ((string (prin1-to-string-for-emacs message package))
             (octets (string-to-utf8 string))
             (length (length octets)))
        (write-header stream length)
        (write-sequence octets stream)
        (finish-output stream))
    (condition (c)
      (error 'swank-write-error :condition c :message message))))

(defun prin1-to-string-for-emacs (object package)
  "Print OBJECT to string using Emacs-compatible format.
This mirrors Swank's prin1-to-string-for-emacs:
- *print-case* = :downcase
- *print-readably* = nil
- *print-pretty* = nil"
  (with-standard-io-syntax
    (let ((*print-case* :downcase)
          (*print-readably* nil)
          (*print-pretty* nil)
          (*package* package)
          (*read-default-float-format* 'double-float))
      (prin1-to-string object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Message Parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-form (string package)
  "Read S-expression from STRING in PACKAGE."
  (with-standard-io-syntax
    (let ((*package* package))
      (read-from-string string))))

(defun read-message (stream package)
  "Read and parse a complete Swank message from STREAM.
Returns the parsed S-expression in PACKAGE."
  (handler-case
      (let ((packet (read-packet stream)))
        (read-form packet package))
    (swank-read-error (c)
      (values nil c))
    (reader-error (c)
      (values nil (make-swank-reader-error :packet (when (boundp 'packet) packet) :cause c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-condition swank-protocol-error (error)
  ((stream :initarg :stream :reader swank-protocol-error-stream))
  (:report (lambda (c s)
             (format s "Swank protocol error on stream ~a" (swank-protocol-error-stream c)))))

(define-condition swank-read-error (swank-protocol-error)
  ((condition :initarg :condition :reader swank-read-error-condition))
  (:report (lambda (c s)
             (format s "Swank read error: ~a" (swank-read-error-condition c)))))

(define-condition swank-write-error (swank-protocol-error)
  ((message :initarg :message :reader swank-write-error-message))
  (:report (lambda (c s)
             (format s "Swank write error while sending ~s: ~a"
                     (swank-write-error-message c)
                     (swank-write-error-condition c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun skip-whitespace (stream)
  "Skip whitespace characters on STREAM."
  (loop for c = (peek-char t stream nil)
        while (and c (member c '(#\Space #\Tab #\Newline #\Return)))
        do (read-char stream)))

(defun read-token (stream)
  "Read a token (symbol or number) from STREAM."
  (skip-whitespace stream)
  (let ((c (peek-char t stream nil)))
    (cond
      ((null c) nil)
      ((digit-char-p c)
       (read stream))
      ((alpha-char-p c)
       (read stream))
      (t
       (read-char stream)
       nil))))

(provide :cl-tron-mcp/swank-protocol)
