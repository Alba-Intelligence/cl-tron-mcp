{
  description = "The CL-Tron MCP";

  inputs = {
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-25.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    llms.url = "github:numtide/llm-agents.nix";
  };

  outputs =
    {
      self,
      flake-utils,
      nixpkgs-stable,
      nixpkgs-unstable,
      llms,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs-unstable = import nixpkgs-unstable {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        pkgs-stable = import nixpkgs-stable {
          inherit system;
          config = {
            allowUnfree = true;
          };
        };
        llmsPkgs = llms.packages.${pkgs-unstable.stdenv.hostPlatform.system};
      in
      {
        devShells.default = pkgs-unstable.mkShell {
          # Use stdenv to get proper C compilation environment
          # This ensures standard C library headers (ctype.h, errno.h, etc.) are available
          stdenv = pkgs-unstable.stdenv;

          # Native build inputs (tools needed during build)
          nativeBuildInputs = with pkgs-stable; [
          ];

          # Build inputs (libraries and runtime dependencies)
          buildInputs =
            (with pkgs-stable; [
              roswell
            ])
            ++ (with pkgs-unstable; [
              openssl

              # Basic utilities
              ripgrep # Text search utility
              hexdump # Hexadecimal dump utility
              jq # JSON processor
              xxd # Hexadecimal editor

              sbcl
              code-cursor
              vscode
            ])
            ++ (with llmsPkgs; [
              cursor-agent
              amp
              kilocode-cli
              opencode
              openskills
              openspec
              spec-kit
            ]);

          # Set up environment variables. Fix for 'missing libcrypto'.
          shellHook = ''
            export LD_LIBRARY_PATH="${pkgs-unstable.openssl.out}/lib:${
              pkgs-unstable.lib.makeLibraryPath (
                with pkgs-unstable;
                [
                  stdenv.cc.cc
                  openssl
                  libGL
                  libuuid
                  curlFull
                ]
              )
            }:$LD_LIBRARY_PATH"
          '';
        };
      }
    );
}
