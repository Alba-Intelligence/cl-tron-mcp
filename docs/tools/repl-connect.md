# repl_connect

**Short Description:** Connect to Swank REPL

**Full Description:** Connect to Swank REPL. PREREQUISITE: Start Swank in SBCL first: (ql:quickload :swank) (swank:create-server :port 4006). Default port 4006.

**Parameters:**
- `type`: Connection type (currently only "swank" supported)
- `host`: Host address (optional, default: localhost)
- `port`: Port number (optional, default: 4006)

**Returns:** Connection status object with `:connected`, `:type`, `:host`, and `:port` fields

**Example Usage:**
```lisp
(repl_connect :type "swank" :host "localhost" :port 4006)
```

**Notes:** Must be called before any other REPL tools. The SBCL session must have Swank loaded and listening on the specified port.