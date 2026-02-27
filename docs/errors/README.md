# Error Codes Reference

This directory contains documentation for all error codes used by the Tron MCP server.

## Error Codes

| Error Code | Description | Documentation |
|------------|-------------|---------------|
| `REPL_NOT_CONNECTED` | REPL is not connected | [repl-not-connected.md](repl-not-connected.md) |
| `REPL_ALREADY_CONNECTED` | REPL is already connected | [repl-already-connected.md](repl-already-connected.md) |
| `NREPL_NOT_SUPPORTED` | NREPL is not supported | [nrepl-not-supported.md](nrepl-not-supported.md) |
| `REPL_DETECTION_FAILED` | Failed to detect REPL type | [repl-detection-failed.md](repl-detection-failed.md) |
| `MISSING_REQUIRED_PARAMETER` | Missing required parameter | [missing-required-parameter.md](missing-required-parameter.md) |
| `INVALID_STRING_PARAMETER` | Invalid string parameter | [invalid-string-parameter.md](invalid-string-parameter.md) |
| `INVALID_LIST_PARAMETER` | Invalid list parameter | [invalid-list-parameter.md](invalid-list-parameter.md) |
| `INVALID_INTEGER_PARAMETER` | Invalid integer parameter | [invalid-integer-parameter.md](invalid-integer-parameter.md) |
| `PARAMETER_TOO_SHORT` | Parameter is too short | [parameter-too-short.md](parameter-too-short.md) |
| `PARAMETER_TOO_SMALL` | Parameter is too small | [parameter-too-small.md](parameter-too-small.md) |
| `SWANK_ALREADY_CONNECTED` | Swank is already connected | [swank-already-connected.md](swank-already-connected.md) |
| `SWANK_CONNECTION_FAILED` | Swank connection failed | [swank-connection-failed.md](swank-connection-failed.md) |
| `SWANK_NOT_CONNECTED` | Swank is not connected | [swank-not-connected.md](swank-not-connected.md) |
| `REQUEST_NOT_FOUND` | Request not found | [request-not-found.md](request-not-found.md) |
| `REQUEST_TIMEOUT` | Request timed out | [request-timeout.md](request-timeout.md) |
| `RECONNECTION_DISABLED` | Reconnection is disabled | [reconnection-disabled.md](reconnection-disabled.md) |
| `MAX_RECONNECTION_ATTEMPTS` | Maximum reconnection attempts reached | [max-reconnection-attempts.md](max-reconnection-attempts.md) |
| `RECONNECTION_ERROR` | Reconnection error | [reconnection-error.md](reconnection-error.md) |
| `INVALID_CODE_PARAMETER` | Invalid code parameter | [invalid-code-parameter.md](invalid-code-parameter.md) |
| `INVALID_PACKAGE_PARAMETER` | Invalid package parameter | [invalid-package-parameter.md](invalid-package-parameter.md) |
| `INVALID_FILENAME_PARAMETER` | Invalid filename parameter | [invalid-filename-parameter.md](invalid-filename-parameter.md) |
| `INVALID_FRAME_INDEX` | Invalid frame index | [invalid-frame-index.md](invalid-frame-index.md) |
| `NO_DEBUGGER_EVENT` | No debugger event is active | [no-debugger-event.md](no-debugger-event.md) |
| `NOT_IN_STEPPER` | Not currently in stepper mode | [not-in-stepper.md](not-in-stepper.md) |
| `INTERRUPT_ERROR` | Interrupt error | [interrupt-error.md](interrupt-error.md) |
| `INVALID_EXPRESSION_PARAMETER` | Invalid expression parameter | [invalid-expression-parameter.md](invalid-expression-parameter.md) |
| `INVALID_SYMBOL_PARAMETER` | Invalid symbol parameter | [invalid-symbol-parameter.md](invalid-symbol-parameter.md) |
| `INVALID_PREFIX_PARAMETER` | Invalid prefix parameter | [invalid-prefix-parameter.md](invalid-prefix-parameter.md) |
| `NOT_A_FUNCTION` | Not a function | [not-a-function.md](not-a-function.md) |
| `INVALID_BOOLEAN_PARAMETER` | Invalid boolean parameter | [invalid-boolean-parameter.md](invalid-boolean-parameter.md) |
| `APPROVAL_EXPIRED` | Approval request has expired | [approval-expired.md](approval-expired.md) |
| `TOOL_EXECUTION_TIMEOUT` | Tool execution timed out | [tool-execution-timeout.md](tool-execution-timeout.md) |
| `UNKNOWN_METHOD` | Unknown method | [unknown-method.md](unknown-method.md) |
| `JSON_PARSE_ERROR` | JSON parse error | [json-parse-error.md](json-parse-error.md) |
| `INTERNAL_ERROR` | Internal error | [internal-error.md](internal-error.md) |
| `OBJECT_NOT_FOUND` | Object not found | [object-not-found.md](object-not-found.md) |
| `CLASS_NOT_FOUND` | Class not found | [class-not-found.md](class-not-found.md) |
| `PACKAGE_NOT_FOUND` | Package not found | [package-not-found.md](package-not-found.md) |

## Error Response Format

All errors follow this format:

```json
{
  "error": {
    "code": "ERROR_CODE",
    "message": "Error message",
    "hint": "Optional hint for resolution",
    "setup": "Optional setup instructions",
    "documentationUri": "Optional URI to full documentation"
  }
}
```

## Error Categories

### Connection Errors
- `REPL_NOT_CONNECTED`
- `REPL_ALREADY_CONNECTED`
- `NREPL_NOT_SUPPORTED`
- `REPL_DETECTION_FAILED`
- `SWANK_ALREADY_CONNECTED`
- `SWANK_CONNECTION_FAILED`
- `SWANK_NOT_CONNECTED`
- `RECONNECTION_DISABLED`
- `MAX_RECONNECTION_ATTEMPTS`
- `RECONNECTION_ERROR`

### Parameter Validation Errors
- `MISSING_REQUIRED_PARAMETER`
- `INVALID_STRING_PARAMETER`
- `INVALID_LIST_PARAMETER`
- `INVALID_INTEGER_PARAMETER`
- `PARAMETER_TOO_SHORT`
- `PARAMETER_TOO_SMALL`
- `INVALID_CODE_PARAMETER`
- `INVALID_PACKAGE_PARAMETER`
- `INVALID_FILENAME_PARAMETER`
- `INVALID_FRAME_INDEX`
- `INVALID_EXPRESSION_PARAMETER`
- `INVALID_SYMBOL_PARAMETER`
- `INVALID_PREFIX_PARAMETER`
- `INVALID_BOOLEAN_PARAMETER`

### Debugger Errors
- `NO_DEBUGGER_EVENT`
- `NOT_IN_STEPPER`
- `INTERRUPT_ERROR`

### Request Errors
- `REQUEST_NOT_FOUND`
- `REQUEST_TIMEOUT`
- `APPROVAL_EXPIRED`
- `TOOL_EXECUTION_TIMEOUT`

### Protocol Errors
- `UNKNOWN_METHOD`
- `JSON_PARSE_ERROR`
- `INTERNAL_ERROR`

### Object/Type Errors
- `NOT_A_FUNCTION`
- `OBJECT_NOT_FOUND`
- `CLASS_NOT_FOUND`
- `PACKAGE_NOT_FOUND`

## Usage

When an error occurs, the error code can be used to:

1. **Quickly identify the issue** - The error code provides a unique identifier
2. **Get hints** - The `hint` field provides immediate guidance
3. **Access full documentation** - The `documentationUri` field links to detailed documentation
4. **Programmatic handling** - Error codes can be used for automated error handling

## Token Optimization

Error codes are designed to reduce token usage:

- **Default response**: Only includes `code` and `message` (minimal tokens)
- **With hints**: Add `:include-hint t` to get resolution hints
- **Full details**: Add `:include-setup t` and `:include-docs t` for complete information

Example:

```lisp
;; Minimal response (default)
(make-error :code "REPL_NOT_CONNECTED")
;; => {:code "REPL_NOT_CONNECTED" :message "REPL is not connected"}

;; With hint
(make-error-with-hint :code "REPL_NOT_CONNECTED")
;; => {:code "REPL_NOT_CONNECTED" :message "REPL is not connected"
;;     :hint "Connect to REPL using repl_connect"}

;; Full details
(make-error-full :code "REPL_NOT_CONNECTED")
;; => {:code "REPL_NOT_CONNECTED" :message "REPL is not connected"
;;     :hint "Connect to REPL using repl_connect"
;;     :setup "1. Start Swank server in SBCL\n2. Connect using repl_connect"
;;     :documentationUri "docs/errors/repl-not-connected.md"}
```

## Contributing

When adding new error codes:

1. Add the error code definition to `src/core/error-codes.lisp`
2. Create a documentation file in this directory
3. Update this index with the new error code
4. Test the error response format

## See Also

- [Error Codes Implementation](../../src/core/error-codes.lisp)
- [MCP Protocol Documentation](../../docs/agents/tool-reference.md)
- [Troubleshooting Guide](../../docs/agents/troubleshooting.md)