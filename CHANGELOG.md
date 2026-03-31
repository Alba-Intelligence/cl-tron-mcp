# Changelog

All notable changes to cl-tron-mcp will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

#### Security Hardening
- **Whitelist pattern matching**: Fixed `whitelist-check` to actually match stored patterns. Supports string substring, function predicate, and wildcard (`t`) patterns via new `whitelist-match-p`
- **Log sanitization**: Added `sanitize-arguments` and `*sensitive-parameter-names*` to redact secrets (`:password`, `:token`, `:secret`, `:api-key`) from audit log entries
- **Audit log bounds**: Replaced unbounded audit log array with a circular buffer capped at `*audit-log-max-size*` (default 5000); trims oldest 25% when limit is reached
- **Approval actor tracking**: Added `:actor` and `:approved-at` fields to `approval-request` struct for enterprise compliance

#### Reliability & Consistency
- **Eval timeout**: `safe-eval` now spawns a thread and enforces the `timeout` parameter using `bt:destroy-thread` on expiry
- **Object registry eviction**: Added TTL-based (`*object-registry-max-age*` = 3600s) and LRU eviction (`*object-registry-max-size*` = 1000) to the debug-internals object registry, preventing memory leaks in long sessions
- **Thread state model**: `thread-state` now returns `:blocked` (mutex wait), `:waiting` (condition variable), or `:sleeping` in addition to `:running`/`:dead` on SBCL
- **Breakpoint approval**: `breakpoint_remove` now requires user approval, consistent with `breakpoint_set`
- **GC validation**: `gc_run` converted to `define-validated-tool` with integer validation (0–7) for the `:generation` parameter
- **Tool registry**: `load-all-tools` made a documented no-op; tool files are already loaded by ASDF in the correct order
- **HTTP stop signal**: Replaced `sleep 1` busy-wait loop in HTTP watchdog with a condition variable (`*http-stop-condition*`), eliminating unnecessary CPU wake-ups

#### Swank Client Refactoring
- **Module split**: Monolithic `src/swank/client.lisp` (1152 LOC) split into four focused files:
  - `swank-connection.lisp` — connection state, connect/disconnect, protocol I/O, `get-unix-time`
  - `swank-rpc.lisp` — request-response correlation, reader loop, heartbeat, message dispatch
  - `swank-events.lisp` — event queue, reconnection with exponential backoff, event processor
  - `swank-api.lisp` — high-level RPC operations (eval, compile, backtrace, stepping, etc.), MCP wrappers
- **Input handling**: Implemented `:read-string` handler in `dispatch-incoming-message`. Pending requests are stored in `*pending-input-requests*`; the new `swank_send_input` tool / `swank-provide-input` function sends `(:emacs-return-string ...)` back to Swank

#### Validation
- Added `validate-url`, `validate-uri`, and `validate-list` validators to `src/tools/validation.lisp`

#### New Tool
- **`swank_send_input`**: Respond to a pending Swank `:read-string` request with a user-supplied string

#### Tool Descriptions
- Expanded terse 1–3 word descriptions to actionable 5–10 word descriptions across inspector, debugger, monitor, thread, tracer, and xref tool categories

#### WebSocket Transport
- `start-websocket-transport` now raises a clear error ("WebSocket transport not implemented. Use :stdio-only or :http-only.") instead of silently doing nothing

#### Test Coverage
- Added `tests/security-test.lisp` tests: audit log bounds, log sanitization, whitelist add/remove/pattern/wildcard/disable
- Added `tests/monitor-test.lisp`: health-check, runtime-stats, system-info, gc-run (valid + invalid generation)
- Added `tests/inspector-test.lisp`: inspect-class, inspect-function, inspect-package, inspect-object
- Added `tests/hot-reload-test.lisp`: compile-and-load (valid + invalid), reload-system (unknown system), get-source-location
- Added `tests/profiler-test.lisp`: profile-start, profile-stop, profile-report, profile-reset
- Added `tests/xref-test.lisp`: who-calls, who-references, list-callees, who-binds, who-sets
- Added `tests/logging-test.lisp`: log-configure, log-info/debug/warn/error, log-level

### Fixed
- `src/security/approval.lisp`: whitelist patterns were stored but never matched — `whitelist-check` always returned the global enable flag
- `cl-tron-mcp.asd`: added missing `:flexi-streams` dependency
- `src/sbcl/eval.lisp`: `timeout` parameter was accepted but silently ignored

- **Timeout Support**: Added configurable timeout for Swank evaluations (default 30 seconds)
- **Async Evaluation**: Implemented asynchronous evaluation support with callback-based results
- **Heartbeat/Keepalive**: Added heartbeat mechanism to detect and recover from stale connections
- **Debugger State Tracking**: Added tracking of debugger state in Swank client
- **Output Streaming**: Implemented output streaming for long-running evaluations
- **Event Queue Cleanup**: Added automatic cleanup of event queue to prevent memory leaks
- **Reconnection Logic**: Implemented reconnection logic with exponential backoff (max 5 attempts, 30s max delay)

#### Swank Protocol Improvements
- **UTF-8 Encoding**: Fixed UTF-8 encoding bug in protocol message handling
- **Missing Condition**: Added `swank-reader-error` condition type for proper error handling
- **Socket Timeout**: Added socket read timeout support (default 60 seconds)

#### Protocol Handler Improvements
- **Error Recovery**: Added comprehensive error recovery and cleanup to protocol handlers
- **Input Validation**: Added input validation to all tool handlers
- **Timeout Handling**: Added timeout handling for long-running operations
- **Approval Respond**: Added `handle-approval-respond` function for approval workflow

#### HTTP Transport Improvements
- **Connection Limits**: Added configurable connection limits (default 100 concurrent connections)
- **Rate Limiting**: Added rate limiting (default 100 requests per minute)
- **Request Size Limits**: Added request size limits (default 10MB)
- **Timeouts**: Added configurable timeouts for requests (default 30 seconds)

#### Security Improvements
- **Race Condition Fix**: Fixed approval system race condition using atomic counter
- **Cleanup Functions**: Added cleanup functions for approval requests

#### Configuration Improvements
- **Config File Support**: Added configuration file support (JSON format)
- **Environment Variables**: Added environment variable support for configuration
- **Priority System**: Implemented configuration priority system (env vars > config file > defaults)

#### Validation Module
- **New Module**: Created `src/tools/validation.lisp` with validation functions
- **Validation Tests**: Added comprehensive validation tests in `tests/validation-test.lisp`
- **Tool Validation**: Added validation wrappers to 40+ tool handlers

### Changed

#### Swank Client
- Refactored client state management for better thread safety
- Improved error handling and recovery mechanisms
- Enhanced connection management with automatic reconnection

#### Protocol Handlers
- Improved error handling with proper cleanup on errors
- Added validation to all tool parameters
- Enhanced timeout handling for long-running operations

#### HTTP Transport
- Enhanced security with connection and rate limiting
- Improved resource management with configurable limits
- Better error handling and logging

#### Security
- Improved approval workflow with atomic operations
- Enhanced audit logging with better error tracking

### Fixed

#### Swank Protocol
- Fixed UTF-8 encoding bug that could cause character corruption
- Fixed missing `swank-reader-error` condition type
- Fixed socket read operations that could block indefinitely

#### Swank Client
- Fixed thread safety issues in global state access
- Fixed memory leak in event queue
- Fixed connection issues with no timeout support
- Fixed blocking operations in event queue

#### Protocol Handlers
- Fixed undefined variable `e` at line 357 in handlers.lisp
- Fixed missing export of `handle-approval-respond` function
- Fixed compilation warnings for undefined variables

#### HTTP Transport
- Fixed potential resource exhaustion with no connection limits
- Fixed potential DoS vulnerability with no rate limiting
- Fixed potential memory issues with no request size limits

#### Security
- Fixed race condition in approval system that could cause duplicate approvals
- Fixed cleanup issues in approval workflow

#### Tests
- Fixed tool count expectation in mcp-e2e-test.lisp (changed from 86 to 85)
- Fixed validation test expectations

### Security

- Added input validation to all tool handlers to prevent injection attacks
- Added rate limiting to prevent DoS attacks
- Added connection limits to prevent resource exhaustion
- Fixed race condition in approval system
- Enhanced audit logging for security events

### Performance

- Improved Swank client performance with async evaluation
- Reduced memory usage with event queue cleanup
- Improved connection management with heartbeat mechanism
- Better resource management with configurable limits

### Documentation

- Added comprehensive code review report in `reports/code-review.md`
- Added Swank client fixes documentation in `reports/swank-client-fixes.md`
- Added HTTP transport improvements documentation in `reports/http-transport-improvements.md`
- Updated AGENTS.md with new features and improvements
- Updated README.md with new features and improvements

## [0.1.0] - 2024-XX-XX

### Added
- Initial release of cl-tron-mcp
- 85 tools across 14 categories
- Swank integration for Slime/Portacle
- Debugger, inspector, profiler, hot-reload capabilities
- MCP protocol support (stdio, HTTP, WebSocket)
- Approval workflow for dangerous operations
- Comprehensive test suite
