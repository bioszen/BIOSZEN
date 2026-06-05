---
name: bioszen-platemap-curves
summary: Build, repair, and validate BIOSZEN-compatible platemap and curves workbooks from arbitrary readable data files and numeric plotting datasets.
description: This skill should be used when the user asks to "create BIOSZEN inputs", "create a platemap from data", "generate a platemap from a file", "prepare platemap and curves", "make a curves file", "repair PlotSettings", "fix parameter typos", "fix platemap column names", "align curves with a platemap", "validate Well mapping", "convert numeric data for plotting", or needs Excel files whose metadata rows, numeric parameters, and optional curve columns agree before upload to BIOSZEN.
version: 0.3.0
---

# BIOSZEN Platemap and Curves Builder

## Purpose

Generate, repair, and validate BIOSZEN-compatible Excel inputs from arbitrary
readable data files and numeric datasets. Support platemap-only work,
curves-only work, and paired platemap plus curves work.

Keep the workflow generic. Do not hardcode parameter names, source file names,
group labels, condition labels, row counts, curve lengths, plate dimensions, or
measurement semantics unless the user explicitly supplies them.

When curves are needed, produce two distinct BIOSZEN inputs:

- A platemap workbook loaded as the main BIOSZEN data file.
- A curves workbook loaded separately as the BIOSZEN curves file.

## Core Compatibility Contract

Treat `Well` as the shared BIOSZEN compatibility key. When both platemap and
curves are produced, every curve column after `Time` must represent the same
observation as the matching `Datos$Well` row.

Default to strict pairing:

```text
list(curves Sheet1 columns except Time) == list(platemap Datos$Well)
```

Require the same count, same IDs, same order, and no duplicates. Use relaxed
subset pairing only when the user requests it, and report kept, excluded,
reordered, or unmatched observations.

Never place curve time-series data inside `Datos`. Never place platemap
metadata, parameter columns, or `PlotSettings` content inside the curves
workbook.

## Input Triage

Classify each supplied file before building outputs:

- Existing platemap-like input: contains `Datos`, optionally `PlotSettings`.
  Repair or standardize those sheets, including safe parameter-name typo fixes,
  and validate against any curves file.
- Existing curves-like input: contains `Sheet1` whose first column is `Time`.
  Keep it separate, repair or generate `Sheet2`, and validate against any
  platemap.
- Row-level numeric input: rows represent observations, samples, objects,
  regions, records, or repeated measurements. Preserve row-level granularity
  unless aggregation is requested.
- Grouped numeric input: tables contain numeric parameters by group,
  condition, replicate, or another user-defined category. Convert to a platemap
  workbook and do not invent curves.
- Summary input: contains already summarized numeric parameters. Preserve that
  mode only when requested; otherwise create standard platemap inputs.

If the user supplies only parameters or metadata, create a platemap only. If the
user supplies only curves, create or repair a curves workbook only unless enough
metadata exists to create a meaningful platemap. If both data types exist, build
both and validate the shared `Well` mapping.

## Platemap From Any Data File

When the user provides a readable source file with data, inspect its sheets,
tables, headers, and row structure before deciding the final mapping. Accept any
parseable spreadsheet or delimited table format available in the working
environment. Convert the source into `Datos` by mapping one source row or one
requested aggregate row to one final observation.

Preserve source labels where possible. If required BIOSZEN fields are missing,
infer neutral grouping, condition, replicate, and observation IDs only when the
mapping is obvious or explicitly requested. Ask for mapping guidance when
several interpretations could change the plotted result.

## Platemap Workbook

Create a workbook with at least:

- `Datos`
- `PlotSettings`

In `Datos`, write one row per plotted or curve-paired observation. Put required
BIOSZEN compatibility columns first:

1. `Well`
2. `Strain`
3. `Media`
4. `Orden`
5. `Replicate`
6. `BiologicalReplicate`
7. `TechnicalReplicate`

Use `Strain` as the primary grouping field and `Media` as the within-group
condition field, regardless of the source column names. Preserve cleaned source
labels in output. Set `Replicate = BiologicalReplicate` by default.

After the required columns, include numeric or safely numeric-convertible
parameters intended for plotting. Do not limit detection to a fixed parameter
list. Store source IDs, join keys, filenames, paths, notes, and transformation
details in an optional `SourceMapping` sheet unless the user explicitly asks to
plot or keep them in `Datos`.

Generate or repair `PlotSettings` with:

1. `Parameter`
2. `Y_Max`
3. `Interval`
4. `Y_Title`

Ensure every `PlotSettings$Parameter` exists in `Datos`, and every valid
numeric parameter in `Datos` has a `PlotSettings` row. Remove or report stale
settings. Estimate readable axis maxima and intervals from observed numeric
values, ignoring blanks, missing values, and non-finite values.

## Typo and Parameter-Name Repair

Repair platemaps that fail in BIOSZEN because a parameter name was typed
differently in one place than another. Compare `Datos` column names and
`PlotSettings$Parameter` values using safe internal normalization: trim leading
and trailing spaces, collapse repeated spaces, compare case-insensitively, and
ignore harmless punctuation or accent differences only for matching.

Apply automatic repairs only when a mismatch maps one-to-one. Prefer updating
`PlotSettings$Parameter` to the exact `Datos` column name, because `Datos`
contains the actual data column BIOSZEN must read. Rename `Datos` columns only
when the user asks for cleaned headers or when the intended correction is
unambiguous and does not collide with another column.

Detect and report:

- `PlotSettings` parameters missing from `Datos`;
- numeric `Datos` columns missing from `PlotSettings`;
- duplicate or near-duplicate parameter names after normalization;
- leading/trailing whitespace in headers;
- stale `PlotSettings` rows;
- repairs applied and repairs that need user confirmation.

Do not guess between multiple plausible parameter matches. Stop and ask when
two or more columns could be the intended target, or when changing a name could
change the scientific or analytical meaning.

## Curves Workbook

Create a separate workbook with:

- `Sheet1`
- `Sheet2`

In `Sheet1`, use exactly one `Time` column followed by one column per `Well`:

```text
Time | A1 | A2 | A3 | ...
```

Keep all curve columns numeric or safely convertible to numeric. Support any
curve length. Do not add metadata columns or multiple time axes to `Sheet1`.

In `Sheet2`, include:

1. `X_Max`
2. `Interval_X`
3. `Y_Max`
4. `Interval_Y`
5. `X_Title`
6. `Y_Title`

Generate `Sheet2` when missing. Set axis values from `Time` and curve values
unless the user provides explicit axis limits or titles.

## Metadata Resolution

Use flexible alias matching for detection, while preserving cleaned source
values in output. Normalize case, accents, whitespace, underscores, hyphens,
and punctuation internally.

Resolve:

- `Well`: explicit observation, position, sample, record, or curve ID columns
  when present. Generate deterministic synthetic wells only when no source key
  exists.
- `Strain`: the main group, category, class, type, source group, or
  user-specified primary grouping.
- `Media`: the main condition, subgroup, state, level, phase, or
  user-specified within-group category.
- `BiologicalReplicate`: sample, replicate, batch, source unit, or another
  stable independent replicate.
- `TechnicalReplicate`: object ID, region ID, row ID, repeated measurement ID,
  or another stable within-replicate observation.

Use `TechnicalReplicate = "A"` only when no meaningful technical replicate
exists or there is one technical observation per biological row.

## Generic Build Workflow

1. Identify required outputs: platemap only, curves only, or both.
2. Inspect workbook sheets, headers, row counts, and likely metadata or
   parameter columns.
3. Ask only for missing mapping information that cannot be inferred safely.
4. Build or repair `Datos` using one row per final observation.
5. Assign one unique `Well` per final row. Reuse provided IDs when safe;
   otherwise generate deterministic compatibility IDs.
6. Detect numeric parameters from the supplied data and honor requested
   inclusions or exclusions.
7. Build or repair `PlotSettings` from the final parameter set, including
   one-to-one parameter-name typo repairs.
8. Build or repair curves `Sheet1` and `Sheet2` when curve data exists or
   curves are requested.
9. When pairing files, order `Datos$Well` and curve columns together. Never sort
   or regenerate wells independently.
10. Validate structure, pairing, parameter settings, and assumptions before
    delivery.

Stop and ask for mapping instructions if curves and metadata cannot be joined
safely by `Well`, source ID, sample ID, object ID, row ID, or another explicit
key. Do not generate paired files from ambiguous positional guesses.

## Validation Requirements

Before calling files ready for BIOSZEN, verify:

- The platemap and curves are separate workbooks when both are produced.
- `Datos` and `PlotSettings` exist in the platemap workbook.
- Required metadata columns exist and `Well` is non-empty.
- `Well` is unique when curves are paired.
- At least one valid numeric parameter exists when creating a platemap.
- `Orden` is constant per `Strain + Media` combination unless the user
  specifies another ordering scheme.
- `PlotSettings` contains only parameters present in `Datos`.
- One-to-one parameter-name typing mistakes are repaired or reported.
- `Sheet1` exists in the curves workbook, starts with `Time`, and has unique
  non-empty well headers.
- Curve values are numeric or safely convertible.
- `Sheet2` exists and has the required axis columns.
- Strict paired mode has exact `curves wells == Datos$Well` order.
- Dropped parameters, generated wells, inferred groupings, repaired settings,
  or unmatched curves are reported.

Write a compact validation report every time, such as
`BIOSZEN_validation_report.txt` or `BIOSZEN_validation_report.xlsx`. Include at
minimum:

- number of `Datos` rows;
- number of curve columns after `Time`, when curves exist;
- whether `Datos$Well` exactly matches curve headers;
- missing or stale `PlotSettings` parameters;
- parameter-name typos repaired or requiring confirmation;
- generated wells;
- assumptions used for grouping, conditions, replicates, and parameters.

If validation fails, do not describe the output as ready to upload. Report the
exact mismatch and repair only when the mapping is unambiguous.

## Delivery Format

Report the output directory, generated filenames, validation report filename,
whether curves were produced, row counts, curve-column counts, parameter counts,
validation status, exact paired-well status, assumptions made, dropped or
excluded parameters, repaired `PlotSettings`, and backup paths when overwriting
files.

## Additional Resources

Load `references/bioszen_io_reference.md` when detailed schemas, alias rules,
axis rules, well generation, paired validation, failure modes, or reusable
script-interface guidance are needed.
