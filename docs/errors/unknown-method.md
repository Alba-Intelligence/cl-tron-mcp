# UNKNOWN_METHOD

**Error Code:** `UNKNOWN_METHOD`

**Message:** Unknown method: {method_name}

## Description

This error occurs when attempting to call an MCP method that does not exist or is not registered in the tool registry.

## Common Causes

- Typo in method name
- Method has been removed or renamed
- Tool is not loaded or registered
- Using deprecated method name

## Resolution

1. Verify the method name is correct
2. Check available methods using `tools/list`
3. Ensure the tool is loaded
4. Check documentation for current method names

## Example

```lisp
;; List available tools
(tools/list)

;; Use correct method name
;; Correct: (repl_eval :code "(+ 1 2)")
;; Incorrect: (repl_evaluate :code "(+ 1 2)")
```

## Related Tools

- `tools/list` - List available tools
- `tools/call` - Call a tool
- All MCP tools