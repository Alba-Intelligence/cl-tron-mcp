# skill_store_installation_guide

**Short Description:** Return the installation guide for agent skills.

**Parameters:** `target_platform` (for example `claude`, `codex`, `opencode`, `kilocode`, `pi`,
`copilot`) and `format` (`json`, `markdown` or `yaml`); see `tools/list` for the current schema.

**Returns:** Platform-specific installation text for connecting that agent to Tron through the MCP
port (4006), not directly to Swank.

**Notes:** No approval required. Source: `src/installation/installation.lisp`.
