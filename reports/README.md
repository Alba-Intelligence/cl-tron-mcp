# reports/

Dated results of reviews, benchmarks and diagnostics. Not for scratch files (use a temp dir) and not for logs
(`*.log` is gitignored).

## Conventions

- File name: `YYYY-MM-DD-<slug>.md`.
- Start with the title, then a **Status** line (`open`, `partly fixed`, `fixed`, `superseded`, `archived`) and a
  table of findings with IDs (`S1`, `B3`...) and their status. Update the table in the same change that fixes a
  finding, and reference the ID in the commit message.
- Superseded or historical reports move to `archive/` (keep the date in the name).
- Do not publish a report that describes an unfixed vulnerability before the fix is merged.
- Findings that need work become items in [`../docs/plans/roadmap.md`](../docs/plans/roadmap.md) or are fixed.

## Index

| Report | Status | Summary |
|---|---|---|
| [2026-11-10-code-quality-review.md](2026-11-10-code-quality-review.md) | partly fixed (see its section 0) | Static review: security, correctness, maintainability; fix order |
| [archive/2026-02-27-token-benchmark-report.md](archive/2026-02-27-token-benchmark-report.md) | archived | Token-usage benchmark of the Feb 2026 refactor |
| [archive/2026-02-26-code-review.md](archive/2026-02-26-code-review.md) | archived, superseded | Earlier review of long-running session behaviour |
| [archive/2026-02-26-http-transport-improvements.md](archive/2026-02-26-http-transport-improvements.md) | archived | HTTP transport implementation summary |
| [archive/2026-02-26-swank-client-fixes.md](archive/2026-02-26-swank-client-fixes.md) | archived | Swank client fixes summary |
| [archive/2026-02-14-status.md](archive/2026-02-14-status.md) | archived | Status snapshot |
| [archive/2026-02-13-mcp-stdio-diagnostic-results.md](archive/2026-02-13-mcp-stdio-diagnostic-results.md) | archived | Stdio diagnostic run |
