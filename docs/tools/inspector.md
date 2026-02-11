# Inspector Tools

Tools for introspecting Lisp objects at runtime.

## Tools Overview

| Tool | Purpose | Approval Required |
|------|---------|------------------|
| `inspect_object` | Inspect any Lisp object | No |
| `inspect_slot` | Get/set slot values on CLOS objects | No |
| `inspect_class` | Inspect class hierarchy and slots | No |
| `inspect_function` | Inspect function definition | No |
| `inspect_package` | Inspect package contents | No |

## inspect_object

### Overview
Introspect any Lisp object and return structured information.

### Tool Definition
```json
{
  "name": "inspect_object",
  "description": "Introspect any Lisp object",
  "parameters": {
    "type": "object",
    "properties": {
      "id": {
        "type": "integer",
        "description": "Object ID from previous result"
      },
      "max_depth": {
        "type": "integer",
        "description": "Maximum inspection depth",
        "default": 3
      },
      "max_elements": {
        "type": "integer",
        "description": "Maximum elements for collections",
        "default": 100
      }
    },
    "required": ["id"]
  }
}
```

### Return Value
```json
{
  "type": "OBJECT",
  "kind": "string",        // e.g., "cons", "array", "hash-table", "standard-object"
  "description": "string",
  "elements": [
    {
      "name": "string",
      "value": "string",
      "object_id": 42       // For nested objects
    }
  ],
  "total_elements": 10
}
```

### Usage Examples

**Inspect a list:**
```json
{
  "tool": "inspect_object",
  "arguments": {
    "id": 42,
    "max_depth": 2
  }
}
```

**Inspect a hash-table:**
```json
{
  "tool": "inspect_object",
  "arguments": {
    "id": 43,
    "max_elements": 50
  }
}
```

## inspect_slot

### Overview
Get or set slot values on CLOS instances.

### Tool Definition
```json
{
  "name": "inspect_slot",
  "description": "Get/set slot values on CLOS instances",
  "parameters": {
    "type": "object",
    "properties": {
      "object_id": {
        "type": "integer",
        "description": "Object ID of CLOS instance"
      },
      "slot_name": {
        "type": "string",
        "description": "Name of slot to inspect"
      },
      "value": {
        "type": "any",
        "description": "New value to set (optional)"
      }
    },
    "required": ["object_id", "slot_name"]
  }
}
```

### Usage Examples

**Get slot value:**
```json
{
  "tool": "inspect_slot",
  "arguments": {
    "object_id": 42,
    "slot_name": "cache"
  }
}
```

**Set slot value:**
```json
{
  "tool": "inspect_slot",
  "arguments": {
    "object_id": 42,
    "slot_name": "cache",
    "value": "#<HASH-TABLE ...>"
  }
}
```

## inspect_class

### Overview
Inspect class hierarchy, slots, and methods.

### Tool Definition
```json
{
  "name": "inspect_class",
  "description": "Inspect class hierarchy and slots",
  "parameters": {
    "type": "object",
    "properties": {
      "class_name": {
        "type": "string",
        "description": "Class name to inspect"
      }
    },
    "required": ["class_name"]
  }
}
```

## inspect_function

### Overview
Inspect function definition, lambda list, and documentation.

### Tool Definition
```json
{
  "name": "inspect_function",
  "description": "Inspect function definition",
  "parameters": {
    "type": "object",
    "properties": {
      "symbol": {
        "type": "string",
        "description": "Function symbol (package:format)"
      }
    },
    "required": ["symbol"]
  }
}
```

## Error Handling

### Common Errors

**Object ID not found:**
```json
{
  "error": {
    "code": -32000,
    "message": "Object ID 999 not found",
    "data": {
      "type": "OBJECT_NOT_FOUND"
    }
  }
}
```

**Slot not found:**
```json
{
  "error": {
    "code": -32000,
    "message": "Slot 'nonexistent' not found",
    "data": {
      "type": "SLOT_NOT_FOUND"
    }
  }
}
```

## See Also

- @prompts/debugging-workflows.md - Using inspector in debugging
- docs/tools/debugger.md - Debugger tools for runtime inspection
