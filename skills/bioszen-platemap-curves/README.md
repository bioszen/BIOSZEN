# BIOSZEN Platemap and Curves Skill

This folder contains a reusable AI-agent skill for preparing BIOSZEN-compatible
input workbooks. It is meant to be used by Codex, Claude, Antigravity, and other
agents that can follow Markdown-based skill instructions.

## What It Helps With

- Create or repair platemap workbooks with `Datos` and `PlotSettings`.
- Generate a platemap from any readable source file with data.
- Create or repair separate curves workbooks with `Sheet1` and `Sheet2`.
- Correct parameter-name typing mistakes that make BIOSZEN reject or misread a
  platemap.
- Validate that `Datos$Well` and curve column headers match exactly.
- Convert arbitrary tabular numeric datasets into BIOSZEN-compatible plotting
  inputs.
- Detect and report mismatches before files are uploaded to BIOSZEN.

The skill is intentionally generic. It treats plotting parameters as arbitrary
numeric variables supplied by the user or source files. It does not assume fixed
parameter names, fixed experiment labels, or one specific measurement type.

## How To Use

Point the agent to this folder or copy the folder into the agent's skill system,
then ask it to use `bioszen-platemap-curves`.

If the agent can read web links, give it the GitHub folder URL:
<https://github.com/bioszen/BIOSZEN/tree/main/skills/bioszen-platemap-curves>.
If it needs local files, download the repository ZIP from
<https://github.com/bioszen/BIOSZEN/archive/refs/heads/main.zip> and copy this
folder into the agent's skill system.

Example prompt:

```text
Use the bioszen-platemap-curves skill to build BIOSZEN-compatible platemap and
curves workbooks from these files. Validate that the Well values and curve
headers match before giving me the result.
```

## Files

- `SKILL.md` - main reusable skill instructions.
- `references/bioszen_io_reference.md` - detailed BIOSZEN workbook schema,
  validation rules, and failure checks.
- `agents/openai.yaml` - optional interface metadata for OpenAI/Codex-style
  skill surfaces.

This is an extra repository resource only. It does not modify or run the BIOSZEN
Shiny application.
