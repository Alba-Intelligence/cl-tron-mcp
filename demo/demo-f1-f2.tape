Output demo/demo-f1-f2.gif
Set Shell "bash"
Set FontSize 14
Set FontFamily "JetBrains Mono"
Set Width 1000
Set Height 600
Set Padding 15

# NixOS: set LD_LIBRARY_PATH for libcrypto (hidden from recording)
Hide
Type@0 "export LD_LIBRARY_PATH=$(find /nix/store -maxdepth 2 -name 'libcrypto.so.3' -printf '%h' -quit 2>/dev/null):$LD_LIBRARY_PATH"
Enter
Sleep 1s
Show

# The canonical hot-reload demo
Type "# f1/f2 Hot-Reload Demo — live code injection"
Enter
Sleep 2s

# Run the self-contained demo (launches its own Swank)
Type "sbcl --noinform --disable-debugger --load demo/demo-f1-f2.lisp"
Enter
Sleep 45s
