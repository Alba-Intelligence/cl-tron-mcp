# Changelog

All notable changes to cl-tron-mcp will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

#### Swank Client Improvements
- **Thread Safety**: Added thread-safe access to Swank client global state using locks
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
