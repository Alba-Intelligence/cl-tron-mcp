{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  llmsPkgs = inputs.llms.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages =
    (with pkgs; [
      git
      openssl_oqs
      openssl_3_5
      openssl_3_6
      openssl_legacy

      # Basic utilities
      ripgrep # Text search utility
      hexdump # Hexadecimal dump utility
      jq # JSON processor
      xxd # Hexadecimal editor

      # Create ASCII GIFs
      vhs
      asciinema_3
      asciinema-agg

      sbcl
      ecl
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

  # https://devenv.sh/languages/
  # languages.rust.enable = true;

  # https://devenv.sh/processes/
  # Start the Tron MCP HTTP server for development. Run with: devenv up
  processes.tron-mcp = {
    exec = "$DEVENV_ROOT/start-mcp.sh --http-only";
    ready.http.get = {
      port = 4006;
      path = "/health";
    };
  };

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  scripts.hello.exec = ''
    echo hello from $GREET
  '';

  # tron-mcp: stdio wrapper for use in MCP client configs.
  # AI clients (Cursor, Copilot CLI, etc.) can use `tron-mcp` as the command
  # when the devenv shell is active, or use `devenv shell -- tron-mcp` from outside.
  scripts.tron-mcp.exec = ''
    exec "$DEVENV_ROOT/start-mcp.sh" --stdio-only
  '';

  # https://devenv.sh/basics/
  enterShell = ''
    export LD_LIBRARY_PATH=${
      pkgs.lib.makeLibraryPath (
        with pkgs;
        [
          openssl_legacy
          openssl_oqs
          openssl_3_5
          openssl_3_6
        ]
      )
    }:$LD_LIBRARY_PATH

    hello         # Run scripts directly
    git --version # Use packages
    echo
  '';

  # https://devenv.sh/tasks/
  # Precompile Tron's Lisp source to .fasl cache on shell entry.
  # This ensures the first MCP startup is fast (~2s) rather than recompiling from scratch.
  tasks."tron-mcp:precompile" = {
    exec = ''
      echo "Precompiling cl-tron-mcp fasls..."
      sbcl --noinform --non-interactive \
        --eval "(push #p\"$DEVENV_ROOT/\" ql:*local-project-directories*)" \
        --eval "(ql:quickload :cl-tron-mcp :silent t)" \
        --eval "#+sbcl (sb-ext:exit :code 0)"
      echo "cl-tron-mcp fasls ready."
    '';
    execIfModified = [
      "src"
      "cl-tron-mcp.asd"
    ];
    before = [ "devenv:enterShell" ];
  };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
    echo
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;

  # See full reference at https://devenv.sh/reference/options/
}
