# syntax=docker/dockerfile:1
# CL-TRON-MCP Dockerfile

# Build stage
FROM ghcr.io/sbcl/sbcl:2.6.0 AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    git \
    && rm -rf /var/lib/apt/lists/*

# Install Quicklisp
RUN curl -o /quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
    && sbcl --load /quicklisp.lisp \
           --eval '(quicklisp-quickstart:install :path "/opt/quicklisp")' \
           --eval '(ql:add-to-init-file "/root/.sbclrc")' \
    && rm /quicklisp.lisp

# Set up working directory
WORKDIR /app

# Copy project files
COPY . .

# Download dependencies (cached if no changes)
RUN sbcl --non-interactive \
    --eval "(push \"/app\" ql:*local-project-directories*)" \
    --eval "(ql:quickload :cl-tron-mcp :silent t)"

# Final stage
FROM ghcr.io/sbcl/sbcl:2.6.0-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Copy Quicklisp from builder
COPY --from=builder /opt/quicklisp /opt/quicklisp
COPY --from=builder /root/.sbclrc /root/.sbclrc

# Set up working directory
WORKDIR /app

# Copy project (dependencies are in the image)
COPY --from=builder /app .

# Environment variables
ENV PATH="/opt/quicklisp/bin:$PATH"
ENV HOME="/root"
ENV SBCL_HOME="/usr/local/lib/sbcl"

# Expose HTTP port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Default command - HTTP transport
CMD ["sbcl", "--non-interactive", \
     "--eval", "(ql:quickload :cl-tron-mcp :silent t)", \
     "--eval", "(cl-tron-mcp/core:start-server :transport :http :port 8080)"]

# Alternative commands:
# - Stdio transport: sbcl --non-interactive --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :stdio)"
# - Custom port: sbcl --non-interactive --eval "(ql:quickload :cl-tron-mcp)" --eval "(cl-tron-mcp/core:start-server :transport :http :port 9090)"
