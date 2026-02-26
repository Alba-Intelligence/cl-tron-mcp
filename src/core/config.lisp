;;;; src/core/config.lisp

(in-package :cl-tron-mcp/core)

(defparameter *config* (make-hash-table))

(defvar *config-file-paths*
  '("~/.config/cl-tron-mcp/config.lisp"
    "~/.cl-tron-mcp.lisp"
    "./config.lisp"
    "./.cl-tron-mcp.lisp")
  "List of configuration file paths to search, in order of priority.")

(defvar *config-loaded* nil
  "Flag indicating if configuration has been loaded.")

(defvar *env-var-prefix* "CL_TRON_MCP_"
  "Prefix for environment variables (e.g., CL_TRON_MCP_PORT).")

(defun get-config (key &optional default)
  (gethash key *config* default))

(defun set-config (key value)
  (setf (gethash key *config*) value))

(defsetf get-config (key) (new-value)
  `(setf (gethash ,key *config*) ,new-value))

(defun load-config-file (path)
  "Load configuration from a Lisp file at PATH.
The file should contain setf forms like:
  (setf (get-config :port) 4006)
  (setf (get-config :transport) :http)
Returns T if loaded successfully, NIL otherwise."
  (handler-case
      (let* ((expanded-path (if (and (> (length path) 0) (char= (char path 0) #\~))
                                (merge-pathnames (subseq path 1) (user-homedir-pathname))
                                (parse-namestring path)))
             (resolved-path (uiop:resolve-symlinks expanded-path)))
        (when (probe-file resolved-path)
          (load resolved-path)
          (cl-tron-mcp/logging:log-info (format nil "Loaded configuration from ~a" resolved-path))
          t))
    (error (e)
      (cl-tron-mcp/logging:log-warn (format nil "Failed to load config from ~a: ~a" path e))
      nil)))

(defun load-config-from-env ()
  "Load configuration from environment variables.
Variables should be prefixed with *ENV-VAR-PREFIX* (e.g., CL_TRON_MCP_PORT).
Supported variables:
  CL_TRON_MCP_PORT - HTTP port (integer)
  CL_TRON_MCP_TRANSPORT - Transport mode (stdio, http, combined, websocket)
  CL_TRON_MCP_APPROVAL_TIMEOUT - Approval timeout in seconds
  CL_TRON_MCP_DEBUG - Enable debug mode (true/false)
  CL_TRON_MCP_SWANK_HOST - Swank host (default: 127.0.0.1)
  CL_TRON_MCP_SWANK_PORT - Swank port (default: 4006)
  CL_TRON_MCP_LOG_LEVEL - Log level (debug, info, warn, error)
Returns number of variables loaded."
  (let ((loaded 0))
    (let ((port (uiop:getenv (concatenate 'string *env-var-prefix* "PORT"))))
      (when port
        (handler-case
            (set-config :port (parse-integer port))
          (error (e)
            (cl-tron-mcp/logging:log-warn (format nil "Invalid CL_TRON_MCP_PORT: ~a" e))))
        (incf loaded)))
    
    (let ((transport (uiop:getenv (concatenate 'string *env-var-prefix* "TRANSPORT"))))
      (when transport
        (let ((transport-key (intern (string-upcase transport) :keyword)))
          (when (member transport-key '(:stdio :http :combined :websocket))
            (set-config :transport transport-key)
            (incf loaded)))))
    
    (let ((timeout (uiop:getenv (concatenate 'string *env-var-prefix* "APPROVAL_TIMEOUT"))))
      (when timeout
        (handler-case
            (set-config :approval-timeout (parse-integer timeout))
          (error (e)
            (cl-tron-mcp/logging:log-warn (format nil "Invalid CL_TRON_MCP_APPROVAL_TIMEOUT: ~a" e))))
        (incf loaded)))
    
    (let ((debug (uiop:getenv (concatenate 'string *env-var-prefix* "DEBUG"))))
      (when debug
        (set-config :debug (or (string= debug "true") (string= debug "1")))
        (incf loaded)))
    
    (let ((host (uiop:getenv (concatenate 'string *env-var-prefix* "SWANK_HOST"))))
      (when host
        (set-config :swank-host host)
        (incf loaded)))
    
    (let ((port (uiop:getenv (concatenate 'string *env-var-prefix* "SWANK_PORT"))))
      (when port
        (handler-case
            (set-config :swank-port (parse-integer port))
          (error (e)
            (cl-tron-mcp/logging:log-warn (format nil "Invalid CL_TRON_MCP_SWANK_PORT: ~a" e))))
        (incf loaded)))
    
    (let ((level (uiop:getenv (concatenate 'string *env-var-prefix* "LOG_LEVEL"))))
      (when level
        (let ((level-key (intern (string-upcase level) :keyword)))
          (when (member level-key '(:debug :info :warn :error))
            (set-config :log-level level-key)
            (incf loaded)))))
    
    loaded))

(defun load-configuration ()
  "Load configuration from all sources in priority order:
1. Default values (already set)
2. Configuration file (first found in *CONFIG-FILE-PATHS*)
3. Environment variables (override file settings)
Returns T if configuration was loaded successfully."
  (when *config-loaded*
    (cl-tron-mcp/logging:log-info "Configuration already loaded")
    (return-from load-configuration t))
  
  (cl-tron-mcp/logging:log-info "Loading configuration...")
  
  (dolist (path *config-file-paths*)
    (when (load-config-file path)
      (return)))
  
  (let ((env-count (load-config-from-env)))
    (when (> env-count 0)
      (cl-tron-mcp/logging:log-info (format nil "Loaded ~d configuration values from environment" env-count))))
  
  (setf *config-loaded* t)
  (cl-tron-mcp/logging:log-info "Configuration loaded successfully")
  t)

(defun get-config-value (key &optional default)
  "Get configuration value for KEY with DEFAULT fallback.
Also checks environment variables for the key."
  (let ((value (gethash key *config*)))
    (if value
        value
        (let ((env-var-name (concatenate 'string 
                                        *env-var-prefix*
                                        (substitute #\_ #\- (string-upcase key)))))
          (let ((env-value (uiop:getenv env-var-name)))
            (if env-value
                env-value
                default))))))

(defun print-configuration ()
  "Print current configuration values."
  (format t "~&Current Configuration:~%")
  (maphash (lambda (key value)
             (format t "  ~a: ~a~%" key value))
           *config*))

(set-config :transport :stdio)
(set-config :port 4006)
(set-config :approval-timeout 300)
(set-config :debug nil)
(set-config :swank-host "127.0.0.1")
(set-config :swank-port 4006)
(set-config :log-level :info)
