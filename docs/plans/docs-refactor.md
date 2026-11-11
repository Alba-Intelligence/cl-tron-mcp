# Plan: one coherent documentation system

Status: **EXECUTED 2026-11-10** (all phases D0-D6). Maintainer decisions: delete what is redundant;
remove phantom tools from user docs after recording missing capabilities in `roadmap.md`; commit generated docs.

Deviations from the plan as written:

- D0/D6 were merged: the doc checks were written as failing tests (`tests/docs-test.lisp`) and the docs were
  fixed until they passed, instead of a report-only phase.
- `docs/architecture.md`, `starting-the-mcp.md`, `swank-integration.md`, `DEVELOPERS.md`, `code-reference.md`
  stayed at `docs/` (moving them would only have churned links). `docs/developing/` holds the new files plus
  `conventions.md` and `transports.md`.
- Per-tool pages `docs/tools/<tool>.md` and `docs/errors/*.md` were kept (error codes and tools reference
  them), fixed, and are now checked; the 8 category pages and `docs/tools/index.md` were deleted.
- `tools.md` lists name, approval level and one-line description only; parameters come from `tools/list`
  (schema property names are lowercased, so a generated parameter list would be misleading).
- Found while writing: the docs gave 4006 as the Swank port, but Swank defaults to 4005 (4006 is the MCP HTTP
  port). Corrected everywhere.
- Not verified: the CI workflow changes (cannot run GitHub Actions locally).
Scope: `AGENTS.md`, `CLAUDE.md`, `agents/`, `prompts/*.md`, `docs/`, `reports/`, `tutorial/`, and the
development workflow they describe.

## 1. Problem (measured, 2026-11-10, 94 tools registered)

| Symptom | Evidence |
|---|---|
| Two audiences mixed in one file | `AGENTS.md` (587 lines) tells *users of the MCP* how to debug SBCL **and** tells *developers of the MCP* how to build/test it. |
| Heavy always-on context | `CLAUDE.md` → `AGENTS.md` → 9 `@`-includes (`prompts/*.md`, `agents/*.md`) ≈ 3.6k lines loaded every session, mostly generic Lisp advice. |
| Docs name tools that do not exist | 30 tool-like names in prose are not registered, e.g. `code_replace_function`, `code_reload_package`, `debugger_step`, `debugger_eval_in_frame`, `memory_stats`, `metrics_export`, `profile_flamegraph`, `thread_kill`, `source_location` (mostly `prompts/*.md`, `agents/*.md`, `docs/errors/*.md`). An agent following them fails. |
| Counts and structure drift | "91 tools / 14 categories" vs 94 registered; 3 tools lack a page in `docs/tools/`; 8 pages in `docs/tools/` match no tool (`debugger.md`, `inspector.md`, … are category pages mixed with per-tool pages). |
| Same content in 3–4 places | Workflows exist as `src/prompts/individual/*.lisp` (what MCP clients actually receive), `prompts/*.md`, `agents/*.md`, `docs/agents/workflows.md`. They disagree. |
| Reports have no lifecycle | `reports/` mixes dated reviews, logs (`*.log`), benchmark output, and a 5-line README. `code-review.md` (Feb) lists blockers with no status; some are fixed, some not. |
| Stale facts | `conventions.md` says Jonathan (code moved to jzon); `unified` package references; security doc claims approval is a hard guarantee (see review S4). |
| No doc checks | Nothing fails CI when docs and code diverge. |

## 2. Target structure

Two audiences, two entry points, one source of truth per fact.

```
AGENTS.md                  <= 120 lines. DEVELOPER entry point: what this repo is, how to build/test,
                           workflow, rules, links. No tool catalogue, no Lisp tutorials.
CLAUDE.md                  one line: "Read AGENTS.md".
docs/
  using/                   FOR AGENTS USING THE MCP  (served via resources/read)
    quickstart.md          <= 80 lines: start Swank, repl_connect, eval, debug, hot-reload. The only
                           file an agent must read; also returned by prompt `discover-mcp`.
    tools.md               GENERATED from the registry (name, params, approval level, one line).
    approval.md            approval flow + the real trust boundary (agent can self-approve, see S4).
    troubleshooting.md     error code -> cause -> fix (merge docs/errors/*.md into one table, generated
                           from src/resources/error-codes.lisp).
  developing/              FOR PEOPLE/AGENTS CHANGING THE MCP
    architecture.md        (existing, kept; add package/dependency diagram)
    workflow.md            branch/worktree, test, report, PR rules (section 4)
    conventions.md         (existing, trimmed; fix Jonathan -> jzon)
    adding-a-tool.md       define-validated-tool, schema, approval, test, doc generation
    transports.md          stdio/http/combined, security (Origin check, no eval endpoint)
  plans/                   roadmap.md + design plans (this file)
reports/
  README.md                index table: date | title | status | owner finding IDs
  YYYY-MM-DD-<slug>.md     dated, each starts with a status header (open/fixed/superseded)
  archive/                 superseded reports, benchmark output
  (logs are gitignored)
agents/                    DELETE after merging: personas already exist as MCP prompts
prompts/*.md               DELETE after merging: canonical text lives in src/prompts/individual/*.lisp
tutorial/                  keep ONE tutorial (e2e-mcp-workflow.md); CURSOR tutorial -> docs/using/clients.md
```

Decisions behind this layout:

1. **Single source of truth.** Workflow/persona content lives only in `src/prompts/individual/*.lisp`
   (what clients get). `docs/using/*` is generated from, or links to, those and the tool registry.
   The three agent personas become prompts only; `agents/*.md` and `prompts/*.md` are removed rather than
   kept in sync by hand.
2. **Generated, not hand-written, where facts are enumerable:** tool list, tool count, categories,
   approval levels, error-code table. Produced by `scripts/gen-docs.lisp` from `*tool-registry*` and the
   error-code table; committed output; CI fails if regenerating changes the tree.
3. **Short always-on context.** `AGENTS.md` carries rules and pointers only. Everything else is read on
   demand (`resources/read` for MCP users; normal file reads for developers). Drop the `@`-includes.
4. **Honesty about security.** `docs/using/approval.md` states the actual trust boundary instead of
   presenting approval as a guarantee.
5. **Every claim checkable.** Tool names in prose must exist; counts come from the registry.

## 3. Doc checks (put in the Rove suite, so CI enforces them)

- `doc-tool-names-test`: every `` `snake_case` `` token that matches a tool-name prefix in
  `docs/using/**`, `AGENTS.md`, `src/prompts/**` is a registered tool.
- `doc-tool-count-test`: counts quoted in `AGENTS.md`/README equal `(hash-table-count *tool-registry*)`
  (or the doc contains no count at all — preferred).
- `doc-resource-whitelist-test`: every path in `*resource-whitelist*` exists on disk.
- `doc-links-test`: relative markdown links resolve.
- `doc-generated-fresh-test`: running `gen-docs` produces no diff.
- Fix CI exit status (wrap `rove:run`; see review T2) and use `scripts/run-tests.lisp`.

## 4. Development workflow to document (`docs/developing/workflow.md`)

1. **Environment:** `devenv shell` (provides OpenSSL via `LD_LIBRARY_PATH`, SBCL, ECL). Outside devenv,
   set `LD_LIBRARY_PATH` to an OpenSSL 3 lib dir. Quicklisp `local-projects` must point at a real directory.
2. **Branch:** one worktree/branch per task (`.claude/worktrees/<name>`), based on `origin/main`.
3. **Test-first for bugs:** write the failing regression test, then fix. Run
   `sbcl --noinform --non-interactive --load scripts/run-tests.lisp` (exit code is the verdict).
   Environmental tests (ECL/devenv subprocess tests) are tagged and reported separately.
4. **Verify before moving on:** a task is done only when its tests ran and passed; unverified work is
   labelled UNVERIFIED in the commit message and the report.
5. **Reports:** findings get IDs (`S1`, `B3`, …) in a dated report; fixes reference the ID in the commit
   message; the report's status table is updated in the same PR. Superseded reports move to `archive/`.
6. **Commits/PRs:** small, one concern each; commit trailer per project convention; never push to main;
   security-sensitive reports are not pushed until the fix lands.
7. **Docs:** a PR that adds or changes a tool must update the tool's schema/description only
   (docs regenerate); behaviour changes update `docs/using/quickstart.md` if the agent flow changes.

## 5. Phases (each ends with a verification gate)

| Phase | Work | Gate |
|---|---|---|
| D0 | Add the doc-check tests (section 3) in *report-only* mode; record baseline failures. | Tests run; baseline list committed. |
| D1 | `scripts/gen-docs.lisp`: generate `docs/using/tools.md`, error table. | Regeneration is a no-op; D0 tool-name/count checks pass for generated files. |
| D2 | Write `docs/using/quickstart.md`, `approval.md`; point prompt `discover-mcp` and the resource whitelist at them. | `resources/read` returns them; doc tests green. |
| D3 | Move developer content to `docs/developing/`; shrink `AGENTS.md` to <=120 lines; remove `@`-includes. | A fresh session with only `CLAUDE.md`→`AGENTS.md` can build and run tests following the file alone (dry-run check). |
| D4 | Delete `agents/`, `prompts/*.md`, duplicated `docs/agents/*`, per-tool pages superseded by `tools.md`; fix remaining phantom tool names (either implement the tool or remove the mention). | Doc tests green; link check green. |
| D5 | Reports: index, status headers, archive, `.gitignore` logs; add status column to `code-review.md`. | `reports/README.md` lists every report with status. |
| D6 | Switch doc tests from report-only to failing; fix CI exit code. | CI red on a deliberately broken doc, green on main. |

Open questions for the maintainer (only decisions that are theirs):

- Delete `agents/*.md` and `prompts/*.md` outright, or keep as redirects for external links?
- Should phantom tools in the docs (`memory_stats`, `metrics_export`, `thread_kill`, `profile_flamegraph`,
  `code_replace_function`, `debugger_step`) be **implemented** (roadmap items) or **removed** from docs?
  Default proposed here: remove from docs now; implement via roadmap if wanted.
- Is publishing generated docs to the repo acceptable (committed output) versus build-time only?
