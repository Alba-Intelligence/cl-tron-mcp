#!/usr/bin/env bash
# create_configs.sh - Generate MCP client configuration files locally
#
# This script creates MCP client configuration files with absolute paths
# (since JSON files cannot use variable expansion like $HOME or ~).
#
# Usage:
#   ./create_configs.sh              # Interactive menu
#   ./create_configs.sh --all        # Generate all configs
#   ./create_configs.sh --client <name>  # Generate specific client config
#
# Supported clients:
#   - cursor: Cursor IDE
#   - kilocode: Kilocode IDE
#   - vscode: VS Code
#   - opencode: OpenCode IDE
#   - claude: Claude Desktop

set -e

# ============================================================================
# Configuration
# ============================================================================

PROOT="$(cd "$(dirname "$0")" && pwd)"
SCRIPT_PATH="$PROOT/start-mcp.sh"

# Colors for menu
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# ============================================================================
# Helper Functions
# ============================================================================

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1" >&2
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1" >&2
}

# Check if script exists
check_script() {
    if [[ ! -f "$SCRIPT_PATH" ]]; then
        log_error "start-mcp.sh not found at: $SCRIPT_PATH"
        exit 1
    fi
    if [[ ! -x "$SCRIPT_PATH" ]]; then
        log_error "start-mcp.sh is not executable. Run: chmod +x $SCRIPT_PATH"
        exit 1
    fi
}

# ============================================================================
# Config Generation Functions
# ============================================================================

generate_cursor_config() {
    local config_dir="$HOME/.cursor"
    local config_file="$config_dir/mcp.json"
    
    log_info "Generating Cursor MCP config..."
    
    mkdir -p "$config_dir"
    
    cat > "$config_file" << CURSORJSON
{
    "mcpServers": {
        "cl-tron-mcp": {
            "command": "$SCRIPT_PATH",
            "args": ["--stdio-only"],
            "disabled": false,
            "env": {}
        }
    }
}
CURSORJSON
    
    log_info "Created: $config_file"
    log_info "  - Command: $SCRIPT_PATH --stdio-only"
}

generate_kilocode_config() {
    local config_dir="$HOME/.kilocode"
    local config_file="$config_dir/mcp.json"
    
    log_info "Generating Kilocode MCP config..."
    
    mkdir -p "$config_dir"
    
    cat > "$config_file" << KILOJSON
{
    "mcpServers": {
        "cl-tron-mcp-stdio": {
            "command": "$SCRIPT_PATH",
            "args": ["--stdio-only"],
            "env": {},
            "alwaysAllow": [],
            "disabled": false
        },
        "cl-tron-mcp": {
            "command": "$SCRIPT_PATH",
            "args": ["--port", "4006"],
            "env": {},
            "alwaysAllow": [],
            "disabled": true
        }
    }
}
KILOJSON
    
    log_info "Created: $config_file"
    log_info "  - stdio: $SCRIPT_PATH --stdio-only"
    log_info "  - http: $SCRIPT_PATH --port 4006 (disabled by default)"
}

generate_vscode_config() {
    local config_dir="$HOME/.vscode"
    local config_file="$config_dir/mcp.json"
    
    log_info "Generating VS Code MCP config..."
    
    mkdir -p "$config_dir"
    
    cat > "$config_file" << VSCODEJSON
{
    "mcpServers": {
        "cl-tron-mcp": {
            "command": "bash",
            "args": ["-c", "cd $PROOT && ./start-mcp.sh --stdio-only"],
            "disabled": false
        }
    }
}
VSCODEJSON
    
    log_info "Created: $config_file"
    log_info "  - Command: cd $PROOT && ./start-mcp.sh --stdio-only"
}

generate_opencode_config() {
    local config_dir="$HOME/.config/opencode"
    local config_file="$config_dir/opencode.json"
    
    log_info "Generating OpenCode MCP config..."
    
    mkdir -p "$config_dir"
    
    # Check if config already exists
    if [[ -f "$config_file" ]]; then
        log_warn "OpenCode config already exists at: $config_file"
        log_warn "Please manually add the MCP configuration to your existing config."
        log_info ""
        log_info "Add this to your opencode.json:"
        echo '{'
        echo '    "$schema": "https://opencode.ai/config.json",'
        echo '    "mcp": {'
        echo '        "cl-tron-mcp": {'
        echo '            "type": "local",'
        echo "            \"command\": \"$SCRIPT_PATH\","
        echo '            "enabled": true'
        echo '        }'
        echo '    }'
        echo '}'
        return
    fi
    
    cat > "$config_file" << OPENCODEJSON
{
    "\$schema": "https://opencode.ai/config.json",
    "mcp": {
        "cl-tron-mcp": {
            "type": "local",
            "command": "$SCRIPT_PATH",
            "enabled": true
        }
    }
}
OPENCODEJSON
    
    log_info "Created: $config_file"
    log_info "  - Command: $SCRIPT_PATH"
}

generate_claude_config() {
    local config_dir="$HOME/.config"
    local config_file="$config_dir/claude_desktop_config.json"
    
    log_info "Generating Claude Desktop MCP config..."
    
    mkdir -p "$config_dir"
    
    # Check if config already exists
    if [[ -f "$config_file" ]]; then
        log_warn "Claude Desktop config already exists at: $config_file"
        log_warn "Please manually add the MCP configuration to your existing config."
        log_info ""
        log_info "Add this to your claude_desktop_config.json:"
        echo '{'
        echo '    "mcpServers": {'
        echo '        "cl-tron-mcp": {'
        echo "            \"command\": \"$SCRIPT_PATH\","
        echo '            "args": ["--stdio-only"]'
        echo '        }'
        echo '    }'
        echo '}'
        return
    fi
    
    cat > "$config_file" << CLAUDEJSON
{
    "mcpServers": {
        "cl-tron-mcp": {
            "command": "$SCRIPT_PATH",
            "args": ["--stdio-only"]
        }
    }
}
CLAUDEJSON
    
    log_info "Created: $config_file"
    log_info "  - Command: $SCRIPT_PATH --stdio-only"
}

# ============================================================================
# Menu Functions
# ============================================================================

show_menu() {
    echo ""
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}  MCP Client Configuration Generator${NC}"
    echo -e "${BLUE}========================================${NC}"
    echo ""
    echo "Select MCP client(s) to generate config for:"
    echo ""
    echo "  1) Cursor IDE"
    echo "  2) Kilocode IDE"
    echo "  3) VS Code"
    echo "  4) OpenCode IDE"
    echo "  5) Claude Desktop"
    echo "  6) All of the above"
    echo "  7) Exit"
    echo ""
    echo -n "Enter choice (1-7): "
}

generate_all() {
    log_info "Generating all MCP client configurations..."
    echo ""
    
    generate_cursor_config
    echo ""
    
    generate_kilocode_config
    echo ""
    
    generate_vscode_config
    echo ""
    
    generate_opencode_config
    echo ""
    
    generate_claude_config
    echo ""
    
    log_info "All configurations generated successfully!"
    log_info "Restart your MCP client(s) to pick up the new configuration."
}

# ============================================================================
# Main
# ============================================================================

main() {
    check_script
    
    # Parse command line arguments
    if [[ $# -gt 0 ]]; then
        case "$1" in
            --all)
                generate_all
                exit 0
                ;;
            --client)
                if [[ -z "$2" ]]; then
                    log_error "Usage: $0 --client <name>"
                    exit 1
                fi
                case "$2" in
                    cursor)
                        generate_cursor_config
                        ;;
                    kilocode)
                        generate_kilocode_config
                        ;;
                    vscode)
                        generate_vscode_config
                        ;;
                    opencode)
                        generate_opencode_config
                        ;;
                    claude)
                        generate_claude_config
                        ;;
                    *)
                        log_error "Unknown client: $2"
                        log_info "Supported clients: cursor, kilocode, vscode, opencode, claude"
                        exit 1
                        ;;
                esac
                echo ""
                log_info "Configuration generated successfully!"
                log_info "Restart your MCP client to pick up the new configuration."
                exit 0
                ;;
            --help|-h)
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --all              Generate all MCP client configurations"
                echo "  --client <name>    Generate config for specific client"
                echo "                     (cursor, kilocode, vscode, opencode, claude)"
                echo "  --help, -h         Show this help message"
                echo ""
                echo "Without arguments, shows an interactive menu."
                exit 0
                ;;
            *)
                log_error "Unknown option: $1"
                echo "Use --help for usage information."
                exit 1
                ;;
        esac
    fi
    
    # Interactive menu
    while true; do
        show_menu
        read -r choice
        
        case "$choice" in
            1)
                generate_cursor_config
                echo ""
                log_info "Restart Cursor to pick up the new configuration."
                ;;
            2)
                generate_kilocode_config
                echo ""
                log_info "Restart Kilocode to pick up the new configuration."
                ;;
            3)
                generate_vscode_config
                echo ""
                log_info "Restart VS Code to pick up the new configuration."
                ;;
            4)
                generate_opencode_config
                echo ""
                log_info "Restart OpenCode to pick up the new configuration."
                ;;
            5)
                generate_claude_config
                echo ""
                log_info "Restart Claude Desktop to pick up the new configuration."
                ;;
            6)
                generate_all
                ;;
            7)
                log_info "Exiting."
                exit 0
                ;;
            *)
                log_error "Invalid choice. Please enter 1-7."
                ;;
        esac
        
        echo ""
        echo -n "Generate another config? (y/n): "
        read -r again
        if [[ "$again" != "y" && "$again" != "Y" ]]; then
            log_info "Exiting."
            exit 0
        fi
    done
}

main "$@"
