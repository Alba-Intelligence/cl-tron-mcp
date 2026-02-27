# MISSING_REQUIRED_PARAMETER

## Error Code
`MISSING_REQUIRED_PARAMETER`

## Message
Missing required parameter

## Hint
Provide a value for this parameter

## Common Causes
- A required parameter was omitted from the tool call
- The parameter name was misspelled
- The parameter was passed as nil instead of a value

## Resolution Steps
1. Check the tool's schema for required parameters
2. Ensure all required parameters are included in the call
3. Verify parameter names match exactly (case-sensitive)

## Example
**Incorrect:**
```json
{
  "name": "repl_eval",
  "arguments": {}
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

## Related Error Codes
- `INVALID_STRING_PARAMETER` - Parameter has wrong type
- `INVALID_INTEGER_PARAMETER` - Parameter has wrong type
- `INVALID_LIST_PARAMETER` - Parameter has wrong type