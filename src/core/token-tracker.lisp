;;;; src/core/token-tracker.lisp

(in-package :cl-tron-mcp/core)

(defvar *token-stats* (make-hash-table :test 'equal)
  "Hash table tracking token usage per tool. Keys are tool names, values are lists of (total-tokens call-count).")

(defvar *token-tracking-enabled* t
  "Global flag to enable/disable token tracking. Set to NIL to disable tracking.")

(defvar *token-stats-lock* (bordeaux-threads:make-lock "token-stats-lock")
  "Lock for thread-safe access to *token-stats*.")

(defun count-tokens (string)
  "Estimate token count for a string using ~4 characters per token heuristic."
  (unless (stringp string)
    (error "count-tokens requires a string, got: ~a" (type-of string)))
  (max 1 (ceiling (/ (length string) 4))))

(defun count-response-tokens (response)
  "Count tokens in a response. Response can be a string, hash table, or list."
  (cond
    ((stringp response)
     (count-tokens response))
    ((or (numberp response) (booleanp response))
     1)
    ((hash-table-p response)
     (let ((total 0))
       (maphash (lambda (key value)
                  (incf total (count-response-tokens key))
                  (incf total (count-response-tokens value)))
                response)
       total))
    ((listp response)
     (let ((total 0))
       (dolist (item response)
         (incf total (count-response-tokens item)))
       total))
    (t
     (count-tokens (princ-to-string response)))))

(defun track-response (tool-name response)
  "Record token usage for a tool response."
  (unless *token-tracking-enabled*
    (return-from track-response))
  
  (let ((tokens (count-response-tokens response)))
    (bordeaux-threads:with-lock-held (*token-stats-lock*)
      (multiple-value-bind (stats exists-p)
          (gethash tool-name *token-stats*)
        (if exists-p
            (setf (gethash tool-name *token-stats*)
                  (list (+ (first stats) tokens)
                        (+ (second stats) 1)))
            (setf (gethash tool-name *token-stats*)
                  (list tokens 1)))))))

(defun get-token-stats ()
  "Return current token statistics as a copy of the stats hash table."
  (bordeaux-threads:with-lock-held (*token-stats-lock*)
    (let ((copy (make-hash-table :test 'equal)))
      (maphash (lambda (key value)
                 (setf (gethash key copy) (copy-list value)))
               *token-stats*)
      copy)))

(defun reset-token-stats ()
  "Clear all token statistics."
  (bordeaux-threads:with-lock-held (*token-stats-lock*)
    (clrhash *token-stats*)))

(defun generate-token-report ()
  "Generate a report showing token usage statistics."
  (let ((stats (get-token-stats))
        (total-tokens 0)
        (total-calls 0)
        tool-list)
    
    (maphash (lambda (tool-name value)
               (destructuring-bind (tokens calls) value
                 (incf total-tokens tokens)
                 (incf total-calls calls)
                 (push (list tool-name tokens calls) tool-list)))
             stats)
    
    (setf tool-list (sort tool-list #'> :key #'second))
    
    (let ((report (make-hash-table :test 'equal)))
      (setf (gethash "total-tokens" report) total-tokens)
      (setf (gethash "total-calls" report) total-calls)
      (setf (gethash "average-tokens-per-call" report)
            (if (> total-calls 0)
                (float (/ total-tokens total-calls))
                0.0))
      (setf (gethash "tokens-per-tool" report) tool-list)
      (setf (gethash "top-10-tools" report)
            (subseq tool-list 0 (min 10 (length tool-list))))
      report)))

(defmacro with-token-tracking (&body body)
  "Execute body and return token usage report."
  `(let ((start-stats (get-token-stats)))
     (let ((result (progn ,@body)))
       (let ((end-stats (get-token-stats))
             (report (make-hash-table :test 'equal)))
         (setf (gethash "tokens-used" report)
               (hash-table-token-diff end-stats start-stats))
         (values result report)))))

(defun hash-table-token-diff (new-stats old-stats)
  "Calculate token difference between two stats snapshots."
  (let ((diff (make-hash-table :test 'equal))
        (total 0))
    (maphash (lambda (tool-name new-value)
               (destructuring-bind (new-tokens new-calls) new-value
                 (let ((old-value (gethash tool-name old-stats)))
                   (if old-value
                       (destructuring-bind (old-tokens old-calls) old-value
                         (let ((token-diff (- new-tokens old-tokens))
                               (call-diff (- new-calls old-calls)))
                           (when (> token-diff 0)
                             (setf (gethash tool-name diff)
                                   (list token-diff call-diff))
                             (incf total token-diff))))
                       (when (> new-tokens 0)
                         (setf (gethash tool-name diff) new-value)
                         (incf total new-tokens))))))
             new-stats)
    (setf (gethash "total" diff) total)
    diff))

(defun benchmark-tool (tool-name &rest args)
  "Benchmark a single tool call and return the result as a string."
  (let ((start-stats (get-token-stats))
        (start-time (get-internal-real-time))
        result)
    (unwind-protect
         (progn
           (setf result (apply tool-name args))
           (when (and (boundp '*token-tracking-enabled*)
                      *token-tracking-enabled*)
             (track-response (princ-to-string tool-name) result))
           (format nil "Result: ~a" result))
      (declare (ignore start-stats start-time)))))