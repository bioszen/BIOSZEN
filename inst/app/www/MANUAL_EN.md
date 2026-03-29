# BIOSZEN User Manual (English)

This manual explains how to run BIOSZEN workflows from data import to reproducible export.

## 1. Prerequisites

- R >= 4.1 installed.
- BIOSZEN app launched from `App.R` or `BIOSZEN::run_app()`.
- Input files for **Load Data** can be `Excel` (`.xlsx`, `.xls`) or `CSV` (`.csv`).
- Curve files for **Load Curves** can be `Excel` (`.xlsx`, `.xls`) or `CSV` (`.csv`).

Reference templates are available in the app under **Reference input files (download)**:

- `Ejemplo_platemap_parametros.xlsx`
- `Ejemplo_curvas.xlsx`
- `Ejemplo_parametros_agrupados.xlsx`
- `Ejemplo_input_summary_mean_sd.xlsx`

## 2. Input Modes

### 2.1 Recommended mode: Platemap + Curves

Use this mode for full functionality and strongest statistical support.

1. Upload a platemap file in **Load Data**.
2. Upload a curves file in **Load Curves** (`.xlsx`, `.xls`, or `.csv`).

Platemap requirements:

- Sheet `Datos`: `Well`, `Strain`, `Media`, `BiologicalReplicate`, `TechnicalReplicate`, `Orden`, plus one or more parameter columns.
- Sheet `PlotSettings`: `Parameter`, `Y_Max`, `Interval`, `Y_Title`.

Curves requirements:

- In `Excel` (`.xlsx`, `.xls`):
  `Sheet1`: first column `Time`, additional columns by well (`A1`, `A2`, etc.).
  `Sheet2`: `X_Max`, `Interval_X`, `Y_Max`, `Interval_Y`, `X_Title`, `Y_Title`.
- In `CSV` (`.csv`):
  first column `Time`, additional columns by well (`A1`, `A2`, etc.).
  This is a single-table format (no extra sheets).
  Axis settings are auto-generated:
  `X_Max` and `Y_Max` use observed maxima, `Interval_X` and `Interval_Y` use `max/4`.
  `X_Title` and `Y_Title` are blank by default.

### 2.2 Grouped parameters mode (parameters only)

- Upload grouped-parameter workbook in **Load Data**.
- Use when you only need parameter charts and stats.
- Not valid as a curves input.

### 2.3 Single-file summary mode (Mean/SD/N)

- Upload summary workbook in **Load Data**.
- Parameters and curves can be detected from dedicated summary sheets.
- Useful when raw replicate rows are unavailable.

### 2.4 CSV mode (high-volume datasets)

- **Load Data** supports `.csv` files.
- Recommended for larger datasets because it is a lighter format.
- BIOSZEN auto-detects common delimiters (`,`, `;`, tab, or `|`).
- If CSV structure is not already platemap-ready, BIOSZEN attempts conversion to a compatible working profile.
- **Load Curves** also supports `.csv` for well-based trajectories (`Time` + well columns).
- Metadata files remain `.xlsx`.

## 3. Standard Workflow

1. Load primary data file.
2. Optionally load/merge curves.
3. Optionally load metadata file.
4. Choose scope (`By Strain` or `Combined`).
5. Choose plot type.
6. Apply filters and replicate selections.
7. Optionally normalize by control.
8. Run statistical analysis.
9. Add significance annotations.
10. Export plots, tables, metadata, and bundle.

## 4. Plot Types and How to Use Them

### Boxplot

- Best for raw replicate distributions.
- Control spread with jitter, box width, and point size.
- Add significance bars or labels after stats.

### Barplot

- Best for summarized comparison by group.
- Supports mean with error bars and optional raw points (raw mode).
- Supports manual and automatic significance annotations.

### Violin

- Best for distribution shape with replicate overlays.
- Supports same annotation workflow as boxplot/barplot.

### Stacked

- Use parameter selector and parameter order controls.
- Configure deviation bars and parameter color behavior.
- Supports label or bracket significance mode.

### Correlation

- Select X and Y parameters.
- Choose Pearson, Spearman, or Kendall.
- Optional overlays: regression line, r, p, R2, equation.
- Advanced panel supports anchor-based one-vs-all screening and Excel export.

### Heatmap

- Select parameter subset.
- Scale mode: none, by row, or by column.
- Optional row/column clustering and dendrograms.
- Optional in-cell value labels.
- Cluster exports available when row clustering is enabled.

### Correlation Matrix

- Multi-select parameters.
- Choose correlation method.
- Apply multiple-testing correction (Holm, FDR, Bonferroni, none).
- Optionally display only significant labels.

### Curves

- Configure axis limits, labels, and line width.
- Choose line geometry and confidence interval style.
- Optional replicate trajectory display in raw mode.

### Composition Panel (multi-plot layouts)

Recommended flow:

1. From each individual chart, click **Add to panel**.
2. Open the **Composition Panel** tab.
3. In **1) Plot Selection**, choose plots to combine and adjust order (move up/down, move to top/bottom, or remove).
4. In **2) Layout & Canvas**, define rows/columns, optional custom grid, column/row sizes, and final output size in px.
5. In **3) Visual Style**, configure legend mode/side, fonts, global text sizes, and palette.
6. Optional: add rich text boxes and per-plot overrides.
7. Preview and export to `PNG`, `PPTX`, or `PDF`.

Composition metadata:

- **Download metadata** stores panel layout and styling.
- **Upload composition metadata (.xlsx)** restores a composition in a new session.

## 5. Normalization

Enable **Normalize by control** and choose a control medium.

- BIOSZEN computes normalized columns with `_Norm` suffix.
- Correlation supports axis-specific normalization (`both`, `X only`, `Y only`).
- If strict control pairing is not possible, BIOSZEN applies fallback logic.

## 6. Statistics

### Normality tests

- Shapiro-Wilk
- Kolmogorov-Smirnov
- Anderson-Darling

### Significance tests

- ANOVA
- Kruskal-Wallis
- t-test
- Wilcoxon

Comparison modes:

- All vs all
- Control vs all
- Pair

Multiple-testing correction:

- Holm
- FDR
- Bonferroni
- None

Summary mode notes:

- Normality may be unavailable (`NA`).
- Some non-parametric paths requiring raw observations are disabled.

## 7. Significance Annotation Workflow

### Manual

1. Select Group 1 and Group 2.
2. Enter annotation label (for example `*`, `**`, `***`, `ns`).
3. Add, reorder, edit, or remove annotations.

### Automatic

1. Run significance tests.
2. Open auto-annotation options.
3. Choose inclusion (`significant only` or `all`).
4. Choose label mode (`stars` or `p-value`).
5. Replace or append existing annotations.

## 8. QC and Replicate Management

Use QC panels to review:

- Missing values.
- Outliers by group.
- Sample size and replicate coverage.

### Biological replicates

- Manual include/exclude controls.
- Automatic IQR-based filtering.
- Keep-N reproducibility-oriented selection.

Keep-N automatic selection:

- Ranks replicates by reproducibility (distance to the group median across parameters).
- Keeps the closest replicates to that center (lowest score).
- Combine it with IQR outlier filtering when you want stricter replicate selection.

### Technical replicates (inside each biological replicate)

- Dedicated **Technical replicate QC** tab (shown when valid technical replicates exist).
- Group and biological-replicate selectors to view and manually deselect technical replicates.
- Global buttons to select all or deselect all technical replicates.
- Automatic technical outlier detection with IQR.
- Technical Keep-N to keep the N most reproducible technical replicates per subgroup.

## 9. Metadata, Versioning, and Reproducibility

Metadata workflow:

- Export current UI state with **Download metadata**.
- Re-import metadata to restore compatible settings.

Versioning and bundle workflow:

- Save plot versions in-session.
- Build reproducibility ZIP with plot assets and metadata.
- Reopen work with consistent configuration.

## 10. Downloads

Main outputs include:

- Plot image (`PNG`, `PDF` depending on plot type).
- Data export.
- Metadata export.
- Statistics export.
- Bundle ZIP.
- Advanced correlation table export.
- Merged platemap/curves exports (when merge tools are used).

## 11. Growth Module

The growth tab extracts growth parameters from uploaded files:

- `uMax`
- `max_percap_time`
- `doub_time`
- `lag_time`
- `ODmax`
- `max_time`
- `AUC`

Typical use:

1. Upload one or more growth files.
2. Set max time and interval.
3. Run extraction.
4. Download ZIP output.
5. Import extracted outputs into plotting workflow when needed.

## 12. Troubleshooting

- **Upload error**: verify required sheets and column names.
- **No plot generated**: confirm selected strain/parameter is present after filters.
- **Normalization unavailable**: verify control medium exists in current scope.
- **Stats disabled**: check whether your input mode supports that test path.
- **Curve merge issues**: ensure platemap merge is loaded first for well remapping.
- **CSV not recognized**: validate required headers and a consistent delimiter (`data .csv`: `Strain`/`Media`; `curves .csv`: `Time` + well columns).
- **Slow performance**: reduce selected parameters, disable heavy overlays, or filter groups.

## 13. Support

For support or bug reports: `bioszenf@gmail.com`
