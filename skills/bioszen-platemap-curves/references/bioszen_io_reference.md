# BIOSZEN I/O Reference

Use this reference for detailed BIOSZEN platemap and curves behavior. Keep every
source-specific decision driven by the user's files and instructions, not by
examples from earlier datasets.

## Workbook Separation

BIOSZEN standard input uses two workbook types:

- Platemap workbook: `Datos` plus `PlotSettings`.
- Curves workbook: `Sheet1` plus `Sheet2`.

Keep these files separate by default. Compatibility depends on sheet names,
column names, and identical `Well` pairing, not on filenames. Suggested default
filenames:

- `platemap_BIOSZEN.xlsx`
- `Curvas_BIOSZEN.xlsx`

Create different filenames when the user gives a naming convention or when
avoiding overwrite.

## Source File Handling

Use any readable source file with data as a possible platemap source. Inspect
available sheets or tables, headers, row counts, and parseability before
mapping. For delimited files, detect separator and decimal style when possible.
For spreadsheets, inspect each relevant sheet before choosing a data table.

Create a platemap from source data by mapping each final observation to one
`Datos` row. Preserve source labels and put source tracking fields in
`SourceMapping` when they are not intended for plotting.

Ask for guidance when the file contains multiple plausible data tables, when
the final observation grain is unclear, or when grouping and condition fields
cannot be inferred safely.

## Platemap Schema

### `Datos`

Required metadata columns, in order:

1. `Well`
2. `Strain`
3. `Media`
4. `Orden`
5. `Replicate`
6. `BiologicalReplicate`
7. `TechnicalReplicate`

Rules:

- Keep one row per observation to be plotted or paired with one curve column.
- Require unique `Well` values when pairing with curves.
- Preserve user/source labels after trimming whitespace.
- Avoid formulas, merged cells, blank header cells, duplicate column names, and
  multi-row headers.
- Store source tracking fields in `SourceMapping` unless explicitly requested in
  `Datos`.
- Allow missing values for parameters that apply only to subsets of rows.

### `PlotSettings`

Required columns:

1. `Parameter`
2. `Y_Max`
3. `Interval`
4. `Y_Title`

Rules:

- Every `Parameter` must exist as a `Datos` column.
- Every valid numeric parameter in `Datos` should have one row.
- Remove or repair stale rows that refer to missing columns.
- Default `Y_Title` to the parameter name unless a clearer label is supplied.
- Use readable axis values based on observed numeric values.

Recommended axis generation:

- Convert numeric-like strings safely, including decimal comma and decimal
  point.
- Ignore blanks, missing values, and non-finite values.
- Set `Y_Max` above the observed maximum, commonly max times 1.05 to 1.15
  rounded to a readable value.
- Set `Interval` to `Y_Max / 4` or `Y_Max / 5`, rounded readably.
- If values are all zero or unavailable, use a safe fallback such as
  `Y_Max = 1` and `Interval = 0.2`, then report the assumption.

### Parameter-name typo repair

Repair parameter-name typing mistakes when `Datos` and `PlotSettings` disagree.
Use safe internal normalization for comparison:

- trim leading and trailing spaces;
- collapse repeated spaces;
- compare case-insensitively;
- ignore harmless punctuation differences for matching;
- compare accent-insensitive forms for matching.

Apply automatic repair only for one-to-one matches. Prefer changing
`PlotSettings$Parameter` to the exact `Datos` column name. Rename a `Datos`
column only when the user requests cleaned headers or when the intended fix is
unambiguous and does not create a duplicate.

Block and ask for confirmation when:

- one `PlotSettings` value could match multiple `Datos` columns;
- multiple `PlotSettings` values normalize to the same `Datos` column;
- a possible repair would merge two distinct parameters;
- a name change could alter the analytical meaning.

Always report the original name, repaired name, repaired location, and whether
the repair was automatic or requires confirmation.

## Curves Schema

### `Sheet1`

Required structure:

```text
Time | A1 | A2 | A3 | ...
```

Rules:

- Name the first column exactly `Time`.
- Use `Well` IDs for all remaining columns.
- Keep headers unique and non-empty.
- Support any number of time rows.
- Keep all curve values numeric or safely convertible.
- Do not add metadata columns.
- Do not place multiple time axes in the same sheet.

### `Sheet2`

Required columns:

1. `X_Max`
2. `Interval_X`
3. `Y_Max`
4. `Interval_Y`
5. `X_Title`
6. `Y_Title`

Rules:

- Generate `Sheet2` if missing.
- Set `X_Max` from the maximum `Time` value.
- Set `Interval_X` to a readable tick interval, commonly `X_Max / 4` or
  `X_Max / 5`.
- Set `Y_Max` from the maximum curve value unless the user provides another
  limit.
- Use generic titles such as `Time` and `Value` unless the user provides
  clearer axis labels or units.

## Metadata Alias Guidance

Use aliases only to detect likely roles. Preserve cleaned original values in
output.

Normalize internally:

- case
- accents
- leading and trailing whitespace
- repeated whitespace
- underscores
- hyphens
- punctuation

### `Well` aliases

Examples: `well`, `position`, `pos`, `id`, `observation_id`, `sample_id`,
`record_id`, `curve_id`.

Preserve source wells or source IDs when they are unique. If a dataset has
repeated physical positions or repeated IDs across files, create unique BIOSZEN
compatibility wells and store original IDs in `SourceMapping`.

### `Strain` aliases

Examples: `strain`, `group`, `category`, `class`, `type`, `primary_group`,
`series`, `label`.

Choose the column that best represents the main grouping for plotting. When
several plausible columns exist, follow explicit user instructions or use a
neutral documented grouping.

### `Media` aliases

Examples: `media`, `condition`, `subgroup`, `state`, `level`, `phase`,
`secondary_group`, `setting`.

Choose the column that best represents the condition or category within each
`Strain`.

### `BiologicalReplicate` aliases

Examples: `biologicalreplicate`, `biological_replicate`, `replicate`, `rep`,
`sample`, `source_unit`, `batch`, `independent_unit`.

Preserve meaningful replicate IDs. Create a stable value, usually `1`, only
when no independent replicate is present.

### `TechnicalReplicate` aliases

Examples: `technicalreplicate`, `technical_replicate`, `object_id`,
`region_id`, `row_id`, `measurement_id`, `repeat_id`.

For row-level data, use the most stable within-replicate ID or a documented row
index.

## Parameter Detection

Include as parameters:

- numeric columns after metadata;
- numeric-like columns that convert safely;
- user-requested parameters even with partial missing values;
- transformed values when both source and transformed versions are meaningful.

Exclude from the parameter area when possible:

- file paths;
- file names;
- join keys;
- source row IDs;
- QC flags not intended for plotting;
- free-text notes;
- duplicate identifiers;
- curve time-series columns.

Place excluded tracking fields in `SourceMapping`. Report dropped parameters and
reasons.

Never limit parameter detection to a fixed list. Never assume labels, group
names, conditions, ordering, or units unless present in the source or requested
by the user.

Avoid placing non-numeric metadata columns after the required metadata columns in
`Datos`, because BIOSZEN may treat later columns as candidate plotting
parameters. Move ambiguous metadata to `SourceMapping` unless the user
explicitly requests otherwise.

## Synthetic Well Generation

Generate deterministic synthetic `Well` IDs only when source wells or source IDs
are absent or not unique enough for BIOSZEN pairing.

Default pattern with plate width 12:

```text
A1, A2, ..., A12,
B1, B2, ..., B12,
...
Z1, ..., Z12,
AA1, AA2, ...
```

Rules:

- Support any number of rows, not just a fixed plate size.
- Treat generated wells as compatibility keys, not necessarily physical
  coordinates.
- Reuse exactly the same generated wells in `Datos$Well` and curve headers.
- Store original source identifiers in `SourceMapping` when present.

## `Orden` Construction

Default dynamic rule:

1. Preserve existing `Orden` if valid and constant per `Strain + Media` group.
2. Otherwise, determine stable first-appearance order of unique `Strain` values.
3. Determine stable first-appearance order of unique `Media` values.
4. Assign one integer `Orden` per unique `Strain + Media` combination.

Rules:

- Keep `Orden` constant within each `Strain + Media` group.
- Apply custom ordering when the user supplies it.
- Do not hardcode ordering from previous datasets.

## Paired Build Workflow

Use this workflow when both platemap and curves are supplied or requested:

1. Read or build the platemap table.
2. Read or build the curve matrix.
3. Establish a source-level join key when available, such as existing `Well`,
   source ID, sample ID, object ID, row ID, or another explicit ID.
4. Stop and ask for a mapping if no safe join key exists.
5. Preserve or assign one unique `Well` per final platemap row.
6. Assign curve columns using exactly the same `Well` IDs.
7. Sort `Datos` and curve columns with the same final order.
8. Write two separate files.
9. Validate strict paired compatibility.

Never assign wells independently in each file. Never sort one file without
applying the same `Well` order to the other. If curves and metadata cannot be
joined safely by an explicit key, stop and ask for mapping instructions. Do not
produce paired BIOSZEN files from ambiguous positional guesses.

## Validation Details

### Platemap validation

- Workbook is separate from curves workbook.
- `Datos` exists.
- `PlotSettings` exists.
- Required metadata columns exist.
- `Well` is non-empty.
- `Well` is unique when paired with curves.
- At least one valid parameter exists.
- `Replicate` equals `BiologicalReplicate` unless intentionally different.
- `Orden` is constant per `Strain + Media`.
- Every `PlotSettings$Parameter` exists in `Datos`.
- Every valid parameter in `Datos` has a `PlotSettings` row.
- Empty or stale parameters are removed, repaired, or reported.
- One-to-one parameter-name typing mistakes are repaired or reported.

### Curves validation

- Workbook is separate from platemap workbook.
- `Sheet1` exists.
- First column is `Time`.
- Curve headers after `Time` are non-empty.
- Curve headers after `Time` are unique.
- Curve values are numeric or safely convertible.
- `Sheet2` exists.
- `Sheet2` has all required columns.
- Axis values are numeric and positive when applicable.

### Paired validation

In strict mode, check exact order:

```text
identical(names(curves Sheet1)[-1], platemap Datos$Well)
```

Report:

- missing curve columns;
- extra curve columns;
- duplicated wells;
- order differences;
- rows removed or columns excluded;
- generated wells;
- source mapping used.

## Validation Report

Write a compact report with:

- input files inspected;
- output files generated;
- workbook sheets present;
- `Datos` row count;
- numeric parameter count;
- curve column count after `Time`;
- strict paired-well status;
- missing, extra, duplicated, or reordered wells;
- `PlotSettings` repairs;
- parameter-name typo repairs;
- generated wells;
- assumptions and exclusions.

If validation fails, mark status as failed or blocked. Do not call the files
ready to upload until the mismatch is repaired or the user accepts the
limitation.

## Failure Modes To Prevent

- Curves embedded in the platemap workbook when the standard separate curves
  mode is requested.
- `Sheet1` curves with a first column not named `Time`.
- Curve headers that do not exactly match `Datos$Well`.
- `PlotSettings$Parameter` values that are missing from `Datos`.
- Parameter-name typing mistakes between `Datos` and `PlotSettings` that remain
  unrepaired.
- Parameter columns treated as metadata because they were placed before the
  required BIOSZEN columns.
- Metadata columns treated as parameters because they were left after the
  required BIOSZEN columns.
- Generated wells created separately in platemap and curves files.
- Silent row drops, silent column drops, or silent ordering changes.
