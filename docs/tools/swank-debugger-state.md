# swank_debugger_state

**Short Description:** Get debugger state

**Full Description:** Get the current debugger state: which thread is in the debugger, the debugger level, and whether debugging is active. REQUIRES: swank_connect first.

**Parameters:** None

**Returns:** Debugger state information

**Example Usage:**
```lisp
(swank_debugger_state)
```

**Notes:** Useful for determining if the debugger is active and which thread is being debugged.