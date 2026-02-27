# reload_system

**Short Description:** Reload ASDF system

**Full Description:** Reload an ASDF system with dependencies. Use to pick up source file changes. Force option reloads even if not changed. Requires approval.

**Parameters:**
- `systemName`: ASDF system name to reload (required)
- `force`: Force reload even if not changed (optional)

**Returns:** Reload status and any errors

**Example Usage:**
```lisp
(reload_system :systemName "my-system")
(reload_system :systemName "my-system" :force true)
```

**Notes:** Requires user approval. Useful for picking up source file changes during development without restarting the entire Lisp image.