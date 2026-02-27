# JSON_PARSE_ERROR

**Error Code:** `JSON_PARSE_ERROR`

**Message:** JSON parse error: {error_message}

## Description

This error occurs when the MCP server receives invalid JSON that cannot be parsed. This typically indicates a client-side issue with request formatting.

## Common Causes

- Malformed JSON in request
- Missing quotes or brackets
- Invalid JSON syntax
- Encoding issues

## Resolution

1. Verify JSON syntax is correct
2. Use a JSON validator to check the request
3. Ensure proper encoding
4. Check for missing or extra commas

## Example

```json
// Correct JSON
{
  "jsonrpc": "2.0",
  "method": "repl_eval",
  "params": {
    "code": "(+ 1 2)"
  },
  "id": 1
}

// Incorrect - missing quote
{
  "jsonrpc": "2.0",
  "method": repl_eval,
  "params": {
    "code": "(+ 1 2)"
  },
  "id": 1
}
```

## Related Tools

- All MCP tools (via JSON-RPC)
- Protocol handler