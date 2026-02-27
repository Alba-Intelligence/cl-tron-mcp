# Troubleshooting

## Common Issues

### Symbol Not Found

**Symptom**: `Package not found` or `Symbol not found` errors

**Cause**: Package not loaded

**Solution**:
```lisp
(ql:quickload :cl-tron-mcp)
```

### Approval Timeout

**Symptom**: `Approval timeout` error

**Cause**: User not responding within 300 seconds

**Solution**:
- Increase timeout in security configuration
- Proceed without approval (if whitelisted)
- Use `whitelist_add` to auto-approve operations

### Transport Bind Failed

**Symptom**: `Address already in use` or `bind failed`

**Cause**: Port already in use

**Solution**:
```lisp
;; Use different port
(cl-tron-mcp/core:start-server :transport :http-only :port 4007)

;; Or kill conflicting process
lsof -ti:4006 | xargs kill -9
```

### Tests Failing

**Symptom**: Test failures after code changes

**Cause**: Stale FASL files

**Solution**:
```lisp
(asdf:compile-system :cl-tron-mcp :force t)
(asdf:test-system :cl-tron-mcp)
```

### HTTP Server Won't Stop

**Symptom**: Server continues running after Ctrl+C

**Cause**: Hunchentoot runs until explicitly stopped

**Solution**:
```lisp
(cl-tron-mcp/core:stop-server)
```

Or kill the process in the terminal where the server is running.

### Debugger Features Unavailable

**Symptom**: `Feature not present` errors for debugger tools

**Cause**: SBCL compiled without `:sb-dbg` feature

**Solution**:
- Rebuild SBCL with debugging support
- Or use default fallbacks where available

### MCP Client Shows "Failed"

**Symptom**: Client reports connection failure

**Cause**: Wrong JSON key case in responses

**Solution**:
- Ensure responses use lowercase keys (`jsonrpc`, `id`, `result`)
- Check that Jonathan uses `:|key|` syntax for lowercase output

### No Response to Requests

**Symptom**: Client sends request but gets no response

**Cause**: Thread crash or output buffering

**Solution**:
- Use stdio transport for reliability
- Ensure `force-output` is called after writing
- Check stderr logs for errors

### Package Not Found on Startup

**Symptom**: `Package not found` error when starting server

**Cause**: Quicklisp not loaded

**Solution**:
```lisp
;; Add before starting server
(ql:quickload :cl-tron-mcp)
(cl-tron-mcp/core:start-server)
```

### "Not a Function" Error

**Symptom**: `is not a function` when calling a tool

**Cause**: Case sensitivity in symbol names

**Solution**:
- Use uppercase symbols: `CL:CAR` not `cl:car`
- Check package prefixes are correct

## Swank Connection Issues

### Connection Refused

**Symptom**: `Connection refused` when connecting to Swank

**Cause**: Swank server not running or wrong port

**Solution**:
```lisp
;; Start Swank in your Lisp session
(swank:create-server :port 4006 :dont-close t)

;; Then connect from MCP
(cl-tron-mcp/unified:repl-connect :host "127.0.0.1" :port 4006)
```

### Swank Evaluation Hangs

**Symptom**: `repl_eval` hangs indefinitely

**Cause**: Debugger waiting for input

**Solution**:
- Use `repl_get_restarts` to see options
- Use `repl_invoke_restart` to abort
- Or interrupt with `repl_abort`

## Performance Issues

### Slow Evaluation

**Symptom**: Code evaluation takes too long

**Cause**: Large compilation or infinite loop

**Solution**:
- Use `profile_start` and `profile_stop` to measure
- Check for infinite loops with `trace_function`
- Use `thread_list` to see if threads are stuck

### High Memory Usage

**Symptom**: Memory grows continuously

**Cause**: Memory leak or large allocations

**Solution**:
```lisp
;; Force garbage collection
(cl-tron-mcp/monitor:gc-run)

;; Check memory stats
(cl-tron-mcp/monitor:runtime-stats)
```

## Tool-Specific Issues

### Inspector Returns "Object Not Found"

**Symptom**: `inspect_object` fails with object ID

**Cause**: Object was garbage collected

**Solution**:
- Re-evaluate code to recreate object
- Use `inspect_package` or `inspect_function` for persistent objects

### Profiler Returns No Data

**Symptom**: `profile_report` shows empty results

**Cause**: Code didn't run during profiling

**Solution**:
```lisp
(cl-tron-mcp/profiler:profile_start)
;; Run your code here
(cl-tron-mcp/profiler:profile_stop)
(cl-tron-mcp/profiler:profile_report)
```

### Tracer Not Working

**Symptom**: `trace_function` doesn't show output

**Cause**: Function not called or wrong package

**Solution**:
- Ensure function is actually called
- Use full package name: `cl-tron-mcp/tools:get-tool`
- Check with `trace_list` to see active traces

## Getting Help

### Check Documentation

1. [Getting Started](getting-started.md) - Connection setup
2. [Workflows](workflows.md) - Usage patterns
3. [Tool Reference](tool-reference.md) - Tool details
4. [Conventions](conventions.md) - Coding standards

### Debug Mode

Enable debug logging:

```lisp
(cl-tron-mcp/logging:log-configure :cl-tron-mcp :debug)
```

### Manual Verification

Test server manually:

```bash
echo '{"jsonrpc": "2.0", "method": "initialize", "params": {}, "id": 1}' | \
  sbcl --non-interactive --noinform \
    --eval '(ql:quickload :cl-tron-mcp :silent t)' \
    --eval '(cl-tron-mcp/core:start-server :transport :stdio-only)'
```

Expected response:

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "protocolVersion": "2024-11-05",
    "capabilities": [],
    "serverInfo": {
      "name": "cl-tron-mcp",
      "version": "0.1.0"
    }
  }
}
```

### Report Issues

If you encounter issues not covered here:

1. Check stderr logs for error details
2. Enable debug logging
3. Verify with manual test
4. Report with:
   - SBCL version
   - Transport mode used
   - Full error message
   - Steps to reproduce

## See Also

- [Transport & Logging](transport-logging.md) - Transport configuration
- [Getting Started](getting-started.md) - Setup guide
- [Workflows](workflows.md) - Common workflows