;;;; src/tools/help.lisp
;;;; Unified REPL tool registrations

(in-package :cl-tron-mcp/tools)

;;; ============================================================
;;; Help
;;; ============================================================

; (defun repl-help ()
;   "Get help on available unified REPL tools."
;   (let ((repl-tools (handler-case
;     (let ((tools-package (find-package :cl-tron-mcp/tools)))
;       (when tools-package
;         (let ((registry-symbol (find-symbol "*TOOL-REGISTRY*" tools-package))
;               (entry-descriptor-symbol (find-symbol "TOOL-ENTRY-DESCRIPTOR" tools-package)))
;           (when (and registry-symbol entry-descriptor-symbol)
;             (let ((registry (symbol-value registry-symbol))
;                   (entry-descriptor-fn (symbol-function entry-descriptor-symbol)))
;               (when (and registry entry-descriptor-fn)
;                 (loop for tool-name being the hash-keys of registry
;                       using (hash-value tool-entry)
;                       when (and (stringp tool-name)
;                                 (string= (subseq tool-name 0 (min 5 (length tool-name))) "repl_"))
;                       collect (let ((descriptor (funcall entry-descriptor-fn tool-entry)))
;                                 (list :name tool-name
;                                       :description (gethash :description descriptor)
;                                       :documentation-uri (gethash :documentationUri descriptor))))))))))
;          (package-error nil)
;          (unbound-variable nil))))
;     (list :type *repl-type*
;           :connected *repl-connected*
;           :tools (sort repl-tools #'string< :key (lambda (x) (getf x :name)))
;           :count (length repl-tools)
;           :examples (list
;                       (list :auto-detect "repl_connect" :port 4005)
;                       (list :explicit-swank "repl_connect" :type :swank :port 4005)))))

(defun repl-help ()
  "Get help on available unified REPL tools."
  (let ((repl-tools (handler-case 
                (loop for tool-name being the hash-keys of *tool-registry*
                      using (hash-value tool-entry)
                      when (and (stringp tool-name)
                                (string= (subseq tool-name 0 (min 5 (length tool-name))) "repl_"))
                      collect (let ((descriptor (tool-entry-descriptor tool-entry)))
                                (list :name tool-name
                                      :description (gethash :|description| descriptor)
                                      :documentation-uri (gethash :|documentationUri| descriptor)))))))
    (list :type *repl-type*
          :connected *repl-connected*
          :tools (sort repl-tools #'string< :key (lambda (x) (getf x :name)))
          :count (length repl-tools)
          :examples (list (list :auto-detect "repl_connect" :port 4005)
                          (list :explicit-swank "repl_connect" :type :swank :port 4005)))))
