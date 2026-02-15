;;;; src/demo/package.lisp
;;;;
;;;; Demo recording and playback tools for creating terminal demos.

(defpackage :cl-tron-mcp/demo
  (:use :cl)
  (:export
   ;; Recording
   #:demo-record-start
   #:demo-record-stop
   #:demo-record-add-event
   
   ;; Playback
   #:demo-replay
   
   ;; Export
   #:demo-export-vhs
   #:demo-export-transcript
   
   ;; State
   *demo-events*
   *demo-recording*))

(in-package :cl-tron-mcp/demo)

;;; ============================================================
;;; Recording State
;;; ============================================================

(defvar *demo-recording* nil
  "Whether we're currently recording a demo.")

(defvar *demo-events* nil
  "List of recorded events (tool calls, responses, messages).")

(defvar *demo-start-time* nil
  "Start time of recording.")

;;; ============================================================
;;; Recording Functions
;;; ============================================================

(defun demo-record-start ()
  "Start recording a demo session."
  (setq *demo-recording* t
        *demo-events* nil
        *demo-start-time* (get-internal-real-time))
  (list :success t :message "Demo recording started"))

(defun demo-record-stop ()
  "Stop recording and return event count."
  (setq *demo-recording* nil)
  (let ((count (length *demo-events*)))
    (list :success t 
          :events count
          :message (format nil "Recorded ~a events" count))))

(defun demo-record-add-event (type &rest args)
  "Add an event to the recording."
  (when *demo-recording*
    (let ((elapsed (/ (- (get-internal-real-time) *demo-start-time*)
                      internal-time-units-per-second)))
      (push (list* :time elapsed :type type args) *demo-events*))))

(defun demo-record-tool-call (tool-name arguments)
  "Record a tool call."
  (demo-record-add-event :tool-call :tool tool-name :args arguments))

(defun demo-record-tool-response (tool-name response)
  "Record a tool response."
  (demo-record-add-event :tool-response :tool tool-name :response response))

(defun demo-record-message (message &key (style :info))
  "Record a narrative message."
  (demo-record-add-event :message :text message :style style))

;;; ============================================================
;;; Export Functions
;;; ============================================================

(defun demo-export-vhs (&key (output "demo.tape") (title "Tron Demo"))
  "Export recorded demo to VHS tape file."
  (let ((events (nreverse *demo-events*)))
    (with-open-file (f output :direction :output :if-exists :supersede)
      ;; VHS header
      (format f "Output demo.gif~%")
      (format f "Set FontSize 14~%")
      (format f "Set FontFamily \"JetBrains Mono\"~%")
      (format f "Set Width 900~%")
      (format f "Set Height 600~%")
      (format f "Set Padding 15~%")
      (format f "~%")
      
      ;; Title
      (format f "Type \"~a\"" title)
      (format f "Enter~%")
      (format f "Sleep 500ms~%")
      (format f "~%")
      
      ;; Events
      (dolist (event events)
        (let ((type (getf event :type)))
          (case type
            (:message
             (let ((text (getf event :text))
                   (style (getf event :style)))
               (format f "Type \"~a\"~%" (escape-vhs-string text))
               (format f "Enter~%")
               (format f "Sleep ~ams~%" (case style
                                         (:error 800)
                                         (:success 300)
                                         (t 400)))))
            (:tool-call
             (let ((tool (getf event :tool))
                   (args (getf event :args)))
               (format f "Type \"  🔧 ~a\"~%" tool)
               (format f "Enter~%")
               (format f "Sleep 300ms~%")))
            (:tool-response
             (let ((response (getf event :response)))
               (when (getf response :error)
                 (format f "Type \"      ⚠️  ~a\"~%" 
                         (escape-vhs-string (getf response :message))))
               (when (getf response :result)
                 (format f "Type \"      → ~a\"~%"
                         (escape-vhs-string (format nil "~a" (getf response :result)))))
               (format f "Enter~%")
               (format f "Sleep 200ms~%")))))))
    (list :success t :file output :events (length events))))

(defun demo-export-transcript (&key (output "demo-transcript.txt"))
  "Export recorded demo as plain text transcript."
  (let ((events (nreverse *demo-events*)))
    (with-open-file (f output :direction :output :if-exists :supersede)
      (dolist (event events)
        (let ((type (getf event :type)))
          (case type
            (:message
             (format f "~a~%" (getf event :text)))
            (:tool-call
             (format f "  🔧 TOOL: ~a~%" (getf event :tool)))
            (:tool-response
             (let ((response (getf event :response)))
               (format f "     ← ~s~%" response)))))))
    (list :success t :file output)))

(defun escape-vhs-string (s)
  "Escape string for VHS tape file."
  (let ((s (string s)))
    (substitute #\Space #\" 
                (substitute #\\ #\\ s))))

;;; ============================================================
;;; Demo Scripts
;;; ============================================================

(defun run-factorial-demo ()
  "Run the factorial debugging demo and export to VHS."
  (demo-record-start)
  
  ;; Connect
  (demo-record-message "🔧 Connecting to Swank on port 4005...")
  (demo-record-tool-call "swank_connect" '(:port 4005))
  (demo-record-tool-response "swank_connect" '(:success t))
  (demo-record-message "✓ Connected" :style :success)
  
  ;; Define buggy function
  (demo-record-message "🔧 Defining factorial function...")
  (demo-record-tool-call "swank_eval" '(:code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1)) (1))))"))
  (demo-record-tool-response "swank_eval" '(:result "FACTORIAL"))
  
  ;; Run and get error
  (demo-record-message "🔧 Running (factorial 7)...")
  (demo-record-tool-call "swank_eval" '(:code "(factorial 7)"))
  (demo-record-tool-response "swank_eval" '(:error t :condition "The value NIL is not of type NUMBER"))
  (demo-record-message "⚠️  ERROR: The value NIL is not of type NUMBER" :style :error)
  (demo-record-message "📍 Backtrace: (FACTORIAL 2) ← (FACTORIAL 3) ← (FACTORIAL 4)...")
  
  ;; Inspect
  (demo-record-message "🔧 Inspecting frame 0...")
  (demo-record-tool-call "swank_frame_locals" '(:frame 0))
  (demo-record-tool-response "swank_frame_locals" '(:locals ((n . 2))))
  (demo-record-message "   N = 2")
  
  ;; Explain bug
  (demo-record-message "🐛 Bug found: Line 4 has (1) called as function, returns NIL")
  (demo-record-message "   (if (> n 1) (* n (factorial (- n 1)) (1)))")
  (demo-record-message "                                        ^^^")
  (demo-record-message "   Should be: 1 (base case)")
  
  ;; Abort and fix
  (demo-record-message "🔧 Aborting error...")
  (demo-record-tool-call "swank_invoke_restart" '(:restart_index 2))
  (demo-record-tool-response "swank_invoke_restart" '(:result nil))
  
  (demo-record-message "🔧 Hot-reloading fixed function...")
  (demo-record-tool-call "swank_eval" '(:code "(defun factorial(n) (if (> n 1) (* n (factorial (- n 1))) 1))"))
  (demo-record-tool-response "swank_eval" '(:result "FACTORIAL"))
  
  ;; Verify
  (demo-record-message "🔧 Verifying...")
  (demo-record-tool-call "swank_eval" '(:code "(factorial 7)"))
  (demo-record-tool-response "swank_eval" '(:result "5040"))
  (demo-record-message "   (factorial 7)  → 5040      ✓" :style :success)
  
  (demo-record-tool-call "swank_eval" '(:code "(factorial 10)"))
  (demo-record-tool-response "swank_eval" '(:result "3628800"))
  (demo-record-message "   (factorial 10) → 3628800   ✓" :style :success)
  
  ;; Done
  (demo-record-message "✅ Done! Session preserved. Update your source file!" :style :success)
  
  (demo-record-stop)
  (demo-export-vhs :output "demo/factorial-demo.tape" :title "Tron: AI Debugging Demo"))