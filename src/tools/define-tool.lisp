;;;; src/tools/define-tool.lisp

(in-package :cl-tron-mcp/tools)

(defmacro define-tool (name &key
                              description
                              parameters
                              returns
                              requires-approval
                              (concurrency :sequential))
  "Define a new tool with automatic registration."
  (let* ((param-names (mapcar (lambda (p)
                                (getf p :name))
                              parameters))
         (param-str (format nil "~{~a~^, ~}" param-names))
         (handler-sym (intern (format nil "~a-HANDLER" name))))
    `(progn
       (defun ,handler-sym (arguments)
         (declare (ignorable arguments))
         ;; Placeholder - actual implementation in separate file
         (list :result ,name :called t))
       (register-tool
        ,name
        (alexandria:plist-hash-table
         :name ,name
         :description ,description
         :inputSchema (alexandria:plist-hash-table
                       :type "object"
                       :properties (alexandria:plist-hash-table
                                    ,@(apply #'append
                                             (mapcan (lambda (p)
                                                       (list (getf p :name)
                                                             (alexandria:plist-hash-table
                                                              :type (string-downcase (symbol-name (getf p :type)))
                                                              :description (getf p :description))))
                                                     parameters)))
                       :required (quote ,(mapcar (lambda (p)
                                                  (getf p :name))
                                                 parameters)))
         :outputSchema (alexandria:plist-hash-table
                        :type "object"
                        :properties (alexandria:plist-hash-table
                                     :content (alexandria:plist-hash-table
                                              :type "text"
                                              :text "string")))
         :requiresApproval ,requires-approval
         :concurrency ,concurrency)
        #',handler-sym))))
