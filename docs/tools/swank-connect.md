# swank_connect

**Short Description:** Connect to Swank server

**Full Description:** Connect to a running SBCL with Swank loaded. PREREQUISITE: Start Swank in SBCL first with (ql:quickload :swank) (swank:create-server :port 4006 :dont-close t). Default port is 4006. Returns connection status.

**Parameters:**
- `host`: Host address (optional, default: localhost)
- `port`: Port number (optional, default: 4006)

**Returns:** Connection status object

**Example Usage:**
```lisp
(swank_connect :host "localhost" :port 4006)
```

**Notes:** Must be called before any other Swank tools. The SBCL session must have Swank loaded and listening.