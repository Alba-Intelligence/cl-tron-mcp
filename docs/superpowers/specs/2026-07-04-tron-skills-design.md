# Tron Skills and Skill-Discovery Tool Design

## Goal

Add a repo-local skill library that teaches agents how to use Tron well, and add one MCP tool that explains how to discover those skills and how to install them across a broad set of agent ecosystems.

This design is intentionally **repository-local first**:

- skills live inside this repository under `agents/skills/`
- the MCP tool explains how to install those repo-local skills elsewhere
- the install guidance targets a ranked set of widely used coding agents, with exact instructions where known and a generic fallback where exact instructions are not stable

## Constraints

- Do not assume `devenv`; the design must work for ordinary Common Lisp users.
- The skills are **workflow-specific**, not one giant catch-all file.
- The MCP surface for discovery/install guidance is a **callable tool**, not only a prompt.
- The install guidance must support a **ranked matrix for roughly 20 major agent tools**, not just Copilot.
- Exact install steps are required where known; otherwise the tool must return a generic/manual fallback instead of pretending full support exists.

## Desired Outcomes

1. An agent can discover that Tron ships installable skills.
2. An agent can see which skills exist and what each one is for.
3. An agent can ask for install guidance for a named ecosystem such as Copilot, Claude Code, or Cursor.
4. The repository can expand or revise supported install targets without rewriting large amounts of Lisp code.

## Proposed Approach

### 1. Repo-local skill library

Create a new repository subtree:

```text
agents/
  skills/
    tron-discover-mcp/
      SKILL.md
    tron-debugging/
      SKILL.md
    tron-hot-reload/
      SKILL.md
    tron-profiling/
      SKILL.md
    tron-agent-install/
      SKILL.md
```

The initial skills should be:

1. `tron-discover-mcp` — how to use `resources/list`, `resources/read`, `prompts/list`, `prompts/get`, and `tools/list` to orient a new agent
2. `tron-debugging` — how to drive debugger, restart, frame, and condition workflows safely
3. `tron-hot-reload` — how to patch running code and verify fixes without restarting the Lisp image
4. `tron-profiling` — how to profile, inspect hot spots, and validate performance changes
5. `tron-agent-install` — what the skill library is, how the tool works, and when to install versus just read in-place

These skills should not duplicate the full documentation tree. They should instead:

- point agents to the best MCP entrypoints
- explain recommended workflow order
- call out failure modes and recovery paths
- keep the guidance targeted and operational

### 2. Agent install registry

Add a structured install registry that the MCP tool can read. The preferred shape is a data file in the same subtree, for example:

```text
agents/
  skills/
    install-targets.json
```

Each entry should include:

- `name` — display name of the agent ecosystem
- `slug` — stable lookup key
- `rank` — relative priority in the “top 20” list
- `support_level` — one of `exact`, `partial`, `generic`
- `install_style` — for example `copy`, `symlink`, `manual`, `config-reference`
- `skills_root` — known install location, if stable
- `copy_example` — exact command where known
- `symlink_example` — exact command where known
- `notes` — caveats, restart requirements, config nuances

This registry should cover roughly 20 commonly used coding-agent ecosystems. The important design point is not the exact list in this document; it is that the tool logic stays data-driven and the support level is explicit.

### 3. New MCP tool

Add a new MCP tool named `skills_help`.

Its purpose is to explain:

- what repo-local Tron skills exist
- where they live in the repository
- which agent ecosystems have known install flows
- how to install skills for a named agent, if requested
- how to fall back to manual copy/symlink instructions when no exact path is known

#### Input shape

Recommended arguments:

- `agent_name` (optional string)
- `skill_name` (optional string)
- `include_all_targets` (optional boolean, default `false`)

#### Output shape

The response should be structured text or a plist/object that includes:

- skill inventory
- short purpose for each skill
- top install targets
- targeted install instructions for `agent_name`, when supplied
- a generic fallback if `agent_name` is unknown or only partially documented

#### Tool behavior

1. **Overview mode**
   - no `agent_name`
   - returns skill list, recommended workflows, and the highest-priority install targets

2. **Targeted mode**
   - `agent_name` given
   - returns the best known install instructions for that ecosystem
   - if support is partial or generic, say so explicitly

3. **Skill-focused mode**
   - `skill_name` given
   - returns that skill’s purpose plus installation guidance

## Why this approach

This design is preferred over one giant skill because:

- agents work better with focused workflow instructions
- maintenance stays local to the workflow that changed
- the install problem stays separate from the usage problem
- the MCP tool becomes the single stable entrypoint for discovery

It is preferred over embedding all install prose directly in Lisp because:

- the list of supported ecosystems will drift
- support quality differs by ecosystem
- adding or updating targets should mostly be a data edit, not a code edit

## Internal Responsibilities

### Skill files

Each skill file is responsible for:

- when to use that skill
- the recommended MCP workflow order
- the minimum safe sequence of tools/prompts/resources
- common mistakes and recovery advice

### Install registry

The registry is responsible for:

- target ranking
- support-level truthfulness
- agent-specific install location knowledge
- example commands

### `skills_help` tool

The tool is responsible for:

- enumerating repo-local skills
- reading install-target metadata
- presenting a coherent answer for overview, targeted, and fallback cases

## Error Handling

- Unknown `agent_name` should **not** hard-fail. Return:
  - known skills
  - generic installation layout
  - advice to copy or symlink `agents/skills/<skill>/` into the target agent’s skill directory
- Unknown `skill_name` should return:
  - valid skill names
  - close matches if easy to compute
- Missing or malformed install registry should return an explicit internal error
- A target marked `partial` or `generic` must never be presented as fully supported

## Testing Strategy

Add tests for:

1. skill inventory discovery from `agents/skills/`
2. install registry parsing
3. `skills_help` overview behavior
4. `skills_help` targeted known-agent behavior
5. `skills_help` unknown-agent fallback behavior
6. `skills_help` unknown-skill behavior
7. at least one MCP-dispatch-level integration test that calls the new tool through the registry

The tests should focus on:

- truthful support-level reporting
- deterministic formatting of install guidance
- robust fallback behavior

## Non-Goals

- solving every external agent ecosystem perfectly in version 1
- inventing a universal external skill standard
- duplicating all of `README.md`, `AGENTS.md`, and `docs/agents/*` inside each skill

## Implementation Notes

- Reuse the existing MCP tool registration pattern in `src/tools/`
- Prefer a small helper module for skill discovery/registry loading if the tool body would otherwise become large
- Add the new skill docs to any relevant resource whitelist only if they should also be exposed via MCP `resources/list`
- Keep the first version markdown-only for the skills unless a specific skill needs assets

## Open Decisions Settled in This Design

- **Scope:** repository-local skills only
- **Organization:** multiple specialized workflow skills
- **MCP surface:** callable tool
- **Install audience:** ranked matrix for roughly 20 major agent ecosystems
- **Support policy:** exact instructions where known, generic/manual fallback where not

## Deliverables

1. `agents/skills/` subtree with the initial Tron skills
2. install-target registry data file
3. `skills_help` MCP tool
4. tests for inventory, registry parsing, and tool behavior
5. docs updates pointing agents toward the new skills and tool
