# INVALID_CODE_PARAMETER

## Error Code
`INVALID_CODE_PARAMETER`

## Message
code is required and must be a non-empty string

## Hint
Provide a Lisp expression as a string, e.g., :code "(+ 1 2)"

## Common Causes
- The `code` parameter was omitted
- The `code` parameter is not a string
- The `code` parameter is an empty string

## Resolution Steps
1. Ensure the `code` parameter is provided
2. Make sure `code` is a string value
3. Provide a non-empty Lisp expression

## Example
**Incorrect:**
```json
{
  "name": "repl_eval",
  "arguments": {
    "code": ""
  }
}
```

**Correct:**
```json
{
  "name": "repl_eval",
  "arguments": {
    "code": "(+ 1 2)"
  }
}
```

## Related Tools
- `repl_eval` - Evaluate Lisp code
- `repl_compile` - Compile Lisp code
- `swank_eval` - Evaluate via Swank
- `swank_compile` - Compile via Swank