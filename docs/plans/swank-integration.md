# Swank Integration Implementation Plan

## Goal

Enable the MCP to interact with a running SBCL+Swank session exactly like Slime does: debugger interaction (frames, locals, restarts, eval-in-frame, stepping), REPL evaluation, inspector, thread operations.

## Current Gap

The existing `src/swank/client.lisp` is skeletal and non-functional:

- Uses line-based protocol instead of length-prefixed
- No dedicated reader thread for async messages
- No request/response correlation
- Debugger tools use direct SBCL introspection (wrong process)

## Strategy

Implement proper Swank protocol client with threading and event handling, then migrate debugger tools to use Swank RPC instead of local SBCL internals.

---

## Phase 1: Core Swank Client Foundation (BLOCKING)

### 1.1 Message Framing (`src/swank/protocol.lisp` - NEW)

Implement the wire protocol utilities:

```lisp
(defun read-packet (stream)
  "Read 6-char hex length prefix, then that many bytes as UTF-8."
  (let ((length (parse-header stream))
        (octets (read-chunk stream length)))
    (utf8-to-string octets)))

(defun write-message (message package stream)
  "Write message as UTF-8, prefixed with 6-char hex length."
  (let* ((string (prin1-to-string-for-emacs message package))
         (octets (string-to-utf8 string))
         (length (length octets)))
    (write-header stream length)      ; 6 hex chars
    (write-sequence octets stream)
    (finish-output stream)))

(defun parse-header (stream)
  "Read 6 bytes and parse as hex integer."
  (parse-integer (map 'string #'code-char (read-chunk stream 6)) :radix 16))

(defun read-chunk (stream length)
  "Read exactly LENGTH bytes from STREAM."
  (let ((buffer (make-array length :element-type '(unsigned-byte 8)))
        (count (read-sequence buffer stream)))
    (cond ((= count length) buffer)
          ((zerop count) (error 'end-of-file :stream stream))
          (t (error "Short read: length=~D count=~D" length count)))))
```

**Why separate file?** Clean separation for the Swank client.

### 1.2 Connection State & Management (`src/swank/client.lisp` - REWRITE)

State variables:

```lisp
(defvar *swank-connection* nil)
(defvar *swank-socket* nil)
(defvar *swank-io* nil)          ; bidirectional stream
(defvar *swank-reader-thread* nil)
(defvar *swank-running* nil)
```

Request tracking:

```lisp
(defvar *pending-requests* (make-hash-table :test 'equal))
(defvar *request-lock* (bt:make-lock "swank-requests"))
(defvar *next-request-id* 1)
```

Event queue:

```lisp
(defvar *event-queue* (queue:make-queue))
(defvar *event-condition* (bt:make-condition-variable))
(defvar *event-mutex* (bt:make-lock))
```

**Core operations**:

```lisp
(defun swank-connect (&key (host "127.0.0.1") (port 4006) (timeout 10))
  "Connect to Swank server. Returns connection info or error."
  (setf *swank-socket* (usocket:socket-connect host port :timeout (* timeout 1000)))
  (setf *swank-io* (usocket:socket-stream *swank-socket*))
  ;; Start reader thread
  (setf *swank-reader-thread*
        (bt:make-thread
         #'swank-reader-loop
         :name "swank-reader"))
  (setf *swank-running* t)
  (list :success t :host host :port port))

(defun swank-disconnect ()
  "Disconnect and stop reader thread."
  (setf *swank-running* nil)
  (when *swank-reader-thread*
    (bt:interrupt-thread *swank-reader-thread* #'bt:condition-notify)
    (bt:join-thread *swank-reader-thread*))
  (when *swank-io* (close *swank-io*))
  (when *swank-socket* (usocket:socket-close *swank-socket*))
  (setf *swank-connection* nil
        *swank-io* nil
        *swank-socket* nil
        *swank-reader-thread* nil))

(defun swank-reader-loop ()
  "Dedicated thread: read incoming messages and dispatch."
  (loop while *swank-running*
        do (handler-case
               (let* ((packet (read-packet *swank-io*))
                      (message (read-from-string packet)))
                 (dispatch-incoming-message message))
             (error (e)
               (log-error "Swank reader error: ~a" e)
               (return)))))
```

### 1.3 Request-Response Correlation

```lisp
(defun make-request-id ()
  (bt:with-lock-held (*request-lock*)
    (prog1 *next-request-id*
      (incf *next-request-id*))))

(defun send-request (form &key (package "CL-USER") (thread t))
  "Send :emacs-rex and wait for :return with matching ID."
  (let* ((id (make-request-id))
         (request `(:emacs-rex ,form ,package ,thread ,id))
         (condition (bt:make-condition-variable))
         (response nil)
         (received nil))
    ;; Track pending request
    (bt:with-lock-held (*request-lock*)
      (setf (gethash id *pending-requests*)
            (list :condition condition
                  :response nil
                  :received nil)))
    ;; Send
    (write-message request *swank-io-package* *swank-io*)
    ;; Wait for response (with timeout)
    (bt:with-lock-held (*request-lock*)
      (loop while (not received)
            do (bt:condition-wait condition *request-lock*)
            do (when (gethash id *pending-requests*)
                 (let ((entry (gethash id *pending-requests*)))
                   (when (third entry)
                     (setf response (second entry)
                           received t)
                     (remhash id *pending-requests*))
                   (return))))
      (if received
          response
          (list :error t :message "Request timeout")))))
```

**Dispatcher**:

```lisp
(defun dispatch-incoming-message (message)
  "Route incoming Swank message."
  (destructuring-bind (tag &rest args) message
    (ecase tag
      (:return
       (destructuring-bind (thread result id) args
         (fulfill-request id result)))
      (:debug
       (destructuring-bind (thread level condition restarts frames) args
         (enqueue-debugger-event condition restarts frames)))
      (:write-string
       (destructuring-bind (string &optional target thread-id) args
         (handle-output string)))
      ;; ... other tags
      )))

(defun fulfill-request (id result)
  (bt:with-lock-held (*request-lock*)
    (let ((entry (gethash id *pending-requests*)))
      (when entry
        (setf (second entry) result
              (third entry) t)
        (bt:condition-notify (first entry))))))
```

### 1.4 Event Queue for Async Messages

```lisp
(defun enqueue-debugger-event (condition restarts frames)
  "Queue debugger event for processing by tools."
  (bt:with-lock-held (*event-mutex*)
    (queue:queue-add *event-queue*
                     (list :type :debug
                           :condition condition
                           :restarts restarts
                           :frames frames
                           :timestamp (get-unix-time)))
    (bt:condition-notify *event-condition*)))

(defun dequeue-debugger-event (&optional (timeout 0.1))
  "Wait for and return next debugger event."
  (bt:with-lock-held (*event-mutex*)
    (if (queue:queue-empty-p *event-queue*)
        (bt:condition-wait *event-condition* *event-mutex* timeout))
    (queue:queue-remove *event-queue*)))
```

---

## Phase 2: Replace Debugger Tools with Swank RPC

**Modify `src/debugger/frames.lisp`**:

```lisp
(defun get-debugger-frames (&key (thread nil) (start 0) (end 20))
  "Get frames via Swank backtrace."
  (unless (cl-tron-mcp/swank:swank-connected-p)
    (return-from get-debugger-frames
      (list :error t :message "Not connected to Swank")))
  (cl-tron-mcp/swank:swank-backtrace start end))

(defun list-restarts ()
  "Get restarts from current debugger event queue."
  (multiple-value-bind (condition restarts frames)
      (cl-tron-mcp/swank:pop-debugger-event)
    (if condition
        (list :condition condition :restarts restarts)
        (list :restarts nil :message "No active debugger event"))))

(defun invoke-restart (index)
  "Invoke nth restart."
  (cl-tron-mcp/swank:swank-invoke-nth-restart index))

(defun eval-in-frame (frame-index code &key (package "CL-USER"))
  "Evaluate code in frame context via Swank."
  (cl-tron-mcp/swank:swank-eval-in-frame code frame-index package))

(defun step-frame (frame-index mode)
  "Step execution: :into, :over, :out."
  (ecase mode
    (:into (cl-tron-mcp/swank:swank-sldb-step frame-index))
    (:over (cl-tron-mcp/swank:swank-sldb-next frame-index))
    (:out (cl-tron-mcp/swank:swank-sldb-out frame-index))))
```

**Key Swank RPC mappings**:

| Operation        | Swank call                                                         |
| ---------------- | ------------------------------------------------------------------ |
| Backtrace        | `(swank:backtrace start end)`                                      |
| Frame locals     | `(swank:frame-locals-and-catch-tags frame-index)`                  |
| Eval in frame    | `(swank:eval-string-in-frame code frame-index package)`            |
| Invoke restart   | `(swank:invoke-nth-restart restart-index)`                         |
| Step             | `(swank:sldb-step frame)`, `swank:sldb-next`, `swank:sldb-out`     |
| Thread list      | `(swank:thread-list)`                                              |
| Thread backtrace | `(swank:thread-backtrace thread)`                                  |
| Interrupt thread | `(:emacs-interrupt thread-id)`                                     |
| Inspector        | `(swank:init-inspector expression)`, `(swank:inspect-nth-part id)` |

---

## Phase 3: Unified REPL Integration (`src/unified/client.lisp`)

Extend `repl-connect` to use Swank when type is `:swank` or auto-detected:

```lisp
(defun repl-connect (&key (type :auto) (host "127.0.0.1") (port 4006))
  (let ((actual-type (if (eq type :auto)
                        (auto-detect-repl host port)
                        type)))
    (cond ((eq actual-type :swank)
           (let ((result (cl-tron-mcp/swank:swank-connect :host host :port port)))
             (when (getf result :success)
               (setq *repl-connected* t
                     *repl-type* :swank
                     *repl-port* port
                     *repl-host* host))
             (list* result (list :type :swank))))
          ...)))

(defun repl-eval (code &key (package "CL-USER"))
  (unless *repl-connected*
    (return-from repl-eval (list :error t :message "Not connected")))
  (cl-tron-mcp/swank:swank-eval code :package package))
```

Add debugger operations to `repl_backtrace`, `repl_inspect`, etc.

---

## Phase 4: Optional Process Spawning (`src/process/spawn.lisp`)

**Goal**: MCP can start Lisp+Swank itself.

```lisp
(defun spawn-sbcl-with-swank (&key (port 4006) (swank-args nil))
  "Start SBCL with Swank server on given port."
  (let* ((command (list "sbcl"
                        "--noinform"
                        "--disable-debugger"
                        "--eval" (format nil "(ql:quickload :swank :silent t)")
                        "--eval" (format nil "(swank:create-server :port ~d :style :spawn)" port)
                        "--eval" "(loop (sleep 3600))")) ; keep alive
         (process (uiop:launch-program command
                                       :output :stream
                                       :error :stream
                                       :input nil)))
    (list :success t
          :process process
          :port port
          :message (format nil "Spawned SBCL+Swank on port ~d" port))))

(defun ensure-swank-process (&key (port 4006) (auto-start t))
  "Ensure we have a Swank connection, optionally spawning one."
  (if (cl-tron-mcp/swank:swank-connected-p)
      (list :success t :message "Already connected")
      (when auto-start
        (multiple-value-bind (spawned error)
            (spawn-sbcl-with-swank :port port)
          (if (getf spawned :success)
              (progn
                (sleep 2) ; wait for Swank to start
                (cl-tron-mcp/swank:swank-connect :port port))
              error)))))
```

---

## Technical Details from Swank Reference

### Message Format

```
[6-digit hex length][UTF-8 S-expression]
Example: "000039(:emacs-rex (swank:connection-info) nil t 1)"
```

### Essential RPC Calls

```lisp
;; Connection info
(:emacs-rex (swank:connection-info) nil t 1)

;; Evaluation
(:emacs-rex (swank:interactive-eval "(+ 1 2)") "CL-USER" t 2)

;; Backtrace (when debugger active)
(:emacs-rex (swank:backtrace 0 20) "CL-USER" t 3)

;; Frame locals
(:emacs-rex (swank:frame-locals-and-catch-tags 2) "CL-USER" t 4)

;; Eval in frame
(:emacs-rex (swank:eval-string-in-frame "(+ x 1)" 2 "CL-USER") "CL-USER" t 5)

;; Invoke restart
(:emacs-rex (swank:invoke-nth-restart 1) "CL-USER" t 6)

;; Step
(:emacs-rex (swank:sldb-step 2) "CL-USER" t 7)
(:emacs-rex (swank:sldb-next 2) "CL-USER" t 8)
(:emacs-rex (swank:sldb-out 2) "CL-USER" t 9)
```

### Async Debugger Entry

When error occurs in evaluated code, Swank sends:

```
(:debug thread-id level condition restarts frames)
```

The MCP must capture this and make it available to `debugger_frames`, `debugger_restarts`, etc.

---

## Testing Strategy (Fastest Path)

### Option 1: Manual SBCL+Swank (Fastest initially)

1. Start SBCL manually: `(ql:quickload :swank) (swank:create-server :port 4006)`
2. Start MCP with stdio transport
3. Use MCP tools: `swank_connect`, `repl_eval`, `debugger_frames`, etc.
4. Induce errors to test debugger

### Option 2: Automated Test Swank Server

Create test helper that spawns SBCL+Swank, but simpler to start manually first.

**Quick verification**:

```bash
# Terminal 1
sbcl --eval "(ql:quickload :swank)" --eval "(swank:create-server :port 4006)"

# Terminal 2
echo '{"jsonrpc":"2.0","method":"tools/call","params":{"name":"swank_connect","arguments":{"port":4006}},"id":1}' | sbcl --noinform --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :stdio)"
```

---

## Implementation Order (by dependency)

1. **`src/swank/protocol.lisp`** - NEW: Message framing (read-packet, write-message)
2. **`src/swank/client.lisp`** - REWRITE: Connection, reader thread, request routing, event queue
3. **`src/debugger/frames.lisp`** - MODIFY: Use Swank RPC instead of local SBCL
4. **`src/debugger/breakpoints.lisp`** - MODIFY: Use `swank:set-breakpoint`
5. **`src/debugger/stepping.lisp`** - MODIFY: Use Swank step operations
6. **`src/debugger/restarts.lisp`** - MODIFY: Use `swank:invoke-nth-restart`
7. **`src/unified/client.lisp`** - MODIFY: Add Swank connection type
8. **`src/process/spawn.lisp`** - OPTIONAL: Process launching
9. **`start-mcp.sh`** - OPTIONAL: Add flags for auto-spawn

---

## Acceptance Criteria

- `swank_connect` successfully connects to manual SBCL+Swank
- `repl_eval` executes code and returns result
- Inducing error (e.g., `(/ 1 0)`) makes Swank send `:debug` event
- `debugger_frames` returns stack frames
- `debugger_restarts` lists available restarts
- `invoke-nth-restart` (via tool) continues execution
- `eval-in-frame` executes code in frame context
- Stepping tools work (`step_frame` with modes)
- All tools work through `repl_*` unified interface
- Optional: MCP can spawn SBCL+Swank itself

---

## Risks & Mitigations

| Risk                               | Mitigation                                                     |
| ---------------------------------- | -------------------------------------------------------------- |
| Reader thread deadlock             | Use separate mutexes for request table vs event queue          |
| Async events missed during request | Event queue is independent; reader always enqueues             |
| Memory leak (pending requests)     | Timeout cleanup and disconnection purge                        |
| Swank protocol version differences | Stick to stable operations (all listed are in Swank for years) |
| UTF-8 encoding issues              | Use existing `swank/backend:string-to-utf8` from git_examples  |

---

## Success Criteria

Full Slime-like debugger interaction through MCP tools. AI agent can:

1. Connect to Swank session
2. Evaluate code and see output
3. Trigger error, inspect frames
4. Examine locals, invoke restarts
5. Eval in frame context, step, continue

All while MCP server runs stdio-only JSON-RPC to client.
