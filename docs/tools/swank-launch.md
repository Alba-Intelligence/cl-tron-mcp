# swank_launch

**Short Description:** Launch a managed SBCL process with Swank enabled

**Full Description:** Start a fresh SBCL image, quickload Swank, open a Swank server on the requested port, and keep that process registered inside Tron so it can be inspected or terminated later. After launch, connect to it with `swank_connect` or `repl_connect`.

**Parameters:**

- `port`: Swank port to open (optional, defaults to `4006`)
- `timeout`: Seconds to wait for the port to accept connections (optional, defaults to `30`)
- `communication_style`: Swank communication style: `spawn`, `fd-handler`, or `sigio` (optional, defaults to `spawn`)
- `sbcl_binary`: SBCL executable to launch (optional, defaults to `sbcl`)
- `extra_eval`: Additional Lisp form to evaluate after Swank starts (optional)

**Returns:** Success status with port, PID, and communication style, or an error describing why launch failed

**Example Usage:**

```lisp
(swank_launch :port 4010 :communication_style "spawn")
```

**Notes:** Requires approval. This launcher is SBCL-specific and manages only processes started by Tron itself.
