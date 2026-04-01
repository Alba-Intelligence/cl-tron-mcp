Output demo/demo-kilocode.gif
Set Shell "bash"
Set FontSize 14
Set FontFamily "JetBrains Mono"
Set Width 1200
Set Height 700
Set Padding 15

# NixOS: set LD_LIBRARY_PATH for libcrypto (hidden from recording)
Hide
Type@0 "export LD_LIBRARY_PATH=$(find /nix/store -maxdepth 2 -name 'libcrypto.so.3' -printf '%h' -quit 2>/dev/null):$LD_LIBRARY_PATH"
Enter
Sleep 1s
Show

# Show what we're doing
Type "# Kilocode CLI with Tron MCP — debugging factorial"
Enter
Sleep 2s

# Run the self-contained demo (launches its own Swank)
Type "sbcl --noinform --disable-debugger --load demo/demo-kilocode.lisp"
Enter
Sleep 35s