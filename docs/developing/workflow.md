# Development workflow

For people and agents **changing** Tron. (To *use* Tron, see [../using/quickstart.md](../using/quickstart.md).)

## 1. Environment

- Preferred: `devenv shell` (SBCL, ECL, OpenSSL on `LD_LIBRARY_PATH`).
- Without devenv: SBCL + Quicklisp, and an OpenSSL 3 library directory on `LD_LIBRARY_PATH`
  (`cl+ssl` loads `libcrypto.so` through Hunchentoot). On NixOS use the store path of `openssl`.
- `~/quicklisp/local-projects` must be a real directory (or a symlink to one); a dangling link makes
  Quicklisp's `setup.lisp` fail with "Can't create directory".

## 2. Branch

One task, one branch. Use a git worktree under `.claude/worktrees/<name>` based on `origin/main`.
Never push to `main`; never force-push.

## 3. Test

```bash
sbcl --noinform --non-interactive --load scripts/run-tests.lisp   # exit code = verdict
sbcl --noinform --non-interactive --load scripts/gen-docs.lisp    # regenerate generated docs
```

`scripts/run-tests.lisp` tests the checkout it lives in, not a same-named copy under Quicklisp. Tests
that start ECL or `start-mcp.sh` subprocesses need ECL/devenv on `PATH` and may fail elsewhere; report
them separately from real failures.
Bug fixes are **test-first**: write the failing regression test, then fix. A task is done only when its
tests have actually run and passed. Otherwise say UNVERIFIED in the commit message and the report.

## 4. Documentation rules

- One source of truth per fact. Tool lists, counts and approval levels come from the registry and are
  **generated** (`docs/using/tools.md`, `docs/using/errors.md`). Never type a tool count by hand.
- Workflow and persona text lives in `src/prompts/individual/*.lisp`; docs link to it.
- Every backticked tool name in docs must be a registered tool (`tests/docs-test.lisp` enforces this),
  and relative links must resolve. Planned-but-missing tools belong in `docs/plans/roadmap.md`, not in
  user-facing docs.
- Changing a tool's description, schema or approval level: rerun `gen-docs.lisp` and commit the result.
  CI fails if generated files are stale.
- Changing the agent flow (connect, approve, debug): update `docs/using/quickstart.md`.

## 5. Reports

Findings from reviews, benchmarks or diagnostics go in `reports/YYYY-MM-DD-<slug>.md` with a status
header (see [../../reports/README.md](../../reports/README.md)). Give findings IDs (`S1`, `B3`...),
reference them from commit messages, and update the report's status table in the same change.
Superseded reports move to `reports/archive/`. Logs are not committed. Do not publish a report that
describes an unfixed vulnerability before the fix is merged.

## 6. Commits

Small and single-purpose. Message explains why. End with the project trailer
(`Co-Authored-By: ...`). Run the suite before committing.
