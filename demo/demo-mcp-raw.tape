Output demo/demo-mcp-raw.gif
Set Shell "bash"
Set FontSize 12
Set FontFamily "JetBrains Mono"
Set Width 1200
Set Height 800
Set Padding 15

# NixOS: set LD_LIBRARY_PATH for libcrypto (hidden from recording)
Hide
Type@0 "export LD_LIBRARY_PATH=$(find /nix/store -maxdepth 2 -name 'libcrypto.so.3' -printf '%h' -quit 2>/dev/null):$LD_LIBRARY_PATH"
Enter
Sleep 1s
Show

# Explain
Type "# Raw MCP JSON-RPC messages that clients send to Tron"
Enter
Sleep 2s

# Run the self-contained demo (launches its own Swank)
Type "sbcl --noinform --disable-debugger --load demo/demo-mcp-raw.lisp"
Enter
Sleep 40s