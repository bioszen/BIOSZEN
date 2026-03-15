# BIOSZEN Manual

BIOSZEN generates graphs, filtering views, normalization workflows, statistical analyses, and export bundles from Excel inputs.

## 1. What You Can Do in BIOSZEN

- Generate graphs from one or more Excel inputs.
- Filter by strain, medium/group, and biological replicate.
- Normalize data to a control medium.
- Run normality and significance tests.
- Add significance bars/labels manually or auto-generate them from significance results.
- Analyze curves, correlations, heatmaps, and correlation matrices.
- Save plot versions and export reproducibility bundles.

## 2. Prepare Input Files

Use **Reference input files (download)** to get templates:

- `Ejemplo_platemap_parametros.xlsx`
- `Ejemplo_curvas.xlsx`
- `Ejemplo_parametros_agrupados.xlsx`
- `Ejemplo_input_summary_mean_sd.xlsx`

### 2.1 Recommended Pack: Platemap + Curves (Best Control)

This is the recommended setup for the highest control and the broadest analysis options inside BIOSZEN.
Use this two-file pack as the default workflow whenever possible.

Use these two files together:

- Main data workbook in **Load Data**
- Curves workbook in **Load Curves**

#### Main data workbook (platemap)

Workbook structure: sheets `Datos` and `PlotSettings`.

`Datos` required columns:

- `Well`: unique well ID (must match well IDs used in curves when curves are loaded).
- `Strain`: biological strain/group identity (alias accepted: `Sample`; localized legacy aliases are also accepted).
- `Media`: condition/treatment identity (aliases accepted: `Condition`, `Treatment`; localized legacy aliases are also accepted).
- `BiologicalReplicate`: biological replicate identifier used for filtering and statistics.
- `TechnicalReplicate`: technical replicate identifier inside each biological replicate.
- `Orden`: ordering index used for group display on categorical axes.
- One column per parameter (for example `Parameter 1`, `Viability`, `ROS`): raw value used for plotting/statistics.

Do not add `SD_<ParameterName>` or `N_<ParameterName>` in platemap mode; BIOSZEN derives variability from replicate-level data.

`PlotSettings` required columns:

- `Parameter`: parameter name exactly as written in `Datos`.
- `Y_Max`: y-axis upper bound for that parameter.
- `Interval`: y-axis tick spacing for that parameter.
- `Y_Title`: y-axis title text for that parameter.

These values define y-axis limits, spacing, and label text; they can be adjusted in the app.

#### Curves workbook (2-sheet format)

Workbook structure: `Sheet1` and `Sheet2`.

`Sheet1` columns:

- First column `Time`: x-axis time coordinate for each measurement row.
- Remaining columns are curve series by `Well` (A1, A2, ...): y-values over time for each well.

`Sheet2` columns:

- `X_Max`: x-axis upper bound for curve plots.
- `Interval_X`: spacing between x-axis ticks.
- `Y_Max`: y-axis upper bound for curve plots.
- `Interval_Y`: spacing between y-axis ticks.
- `X_Title`: x-axis title.
- `Y_Title`: y-axis title.

These values define curve-axis limits, spacing, and labels; they can be modified in the app.

### 2.2 Grouped-Parameter Workbook (Parameters Only)

This format is for parameter values only. It is not a curves input format.

- Use this workbook in **Load Data**.
- Do not use it as a curves file in **Load Curves**.
- If you need curve plots, load a proper curves workbook (`Sheet1` + `Sheet2`) or use the summary file mode described below.

Structure:

- One sheet per parameter.
- Each sheet contains grouped values by strain/group and replicate.

BIOSZEN converts this into internal plotting structures for parameter plots and statistics.

### 2.3 Single-File Summary Mode (Mean/SD/N in Different Sheets)

In this mode, Mean/SD/N summaries for parameters and curves are uploaded in one workbook, using different sheets.

- Upload this workbook in **Load Data**.
- If it also includes a curves-summary sheet, BIOSZEN can detect curves from the same file.
- You may also upload the same workbook in **Load Curves** if you want to force the curves import path.

#### Parameter summary sheet

Accepted sheet names: `Parameters_Summary` or `Summary_Parameters`.
Localized legacy summary sheet names are also accepted.

Required columns:

- `Strain`: group identity.
- `Media`: condition identity.
- `Parameter`: parameter name to be plotted/analyzed.
- `Mean`: summarized central value used to draw the bar/line point.

Optional columns:

- `SD`: summarized variability used to compute error representation and tests in summary mode.
- `N`: sample size used in summary-mode uncertainty/statistics.
- `Orden`: group ordering index.

#### Curves summary sheet

Accepted sheet names: `Curves_Summary` or `Summary_Curves`.
Localized legacy summary sheet names are also accepted.

Required columns:

- `Time`: x-axis time coordinate.
- `Strain`: group identity.
- `Media`: condition identity.
- `Mean`: summarized curve value at each timepoint.

Optional columns:

- `SD`: summarized variability at each timepoint.
- `N`: sample size at each timepoint.
- `Orden`: ordering index for grouped displays.

## 3. App Workflow

1. Load your main data workbook.
2. Optionally load curves workbook (or use single-file summary mode).
3. Optionally load metadata workbooks.
4. Choose scope (`By Strain` or `Combined`).
5. Choose graph type and settings.
6. Apply filters and normalization.
7. Run statistics.
8. Add significance annotations.
9. Export outputs.

## 4. Graph Types and Availability

| Graph type | Platemap / Grouped | Summary Mean/SD/N |
|---|---|---|
| Boxplot | Yes | No |
| Barplot | Yes | Yes |
| Violin | Yes | No |
| Curves | Yes | Yes |
| Stacked | Yes | Yes |
| Correlation | Yes | Yes |
| Heatmap | Yes | Yes |
| Correlation Matrix | Yes | Yes |

Notes:

- Boxplot and Violin are disabled in summary mode.
- Translation keys exist for `Raincloud` and `Estimation`, but these graph types are not currently exposed in the graph selector.

## 5. Global Controls

- Graph size: `Width px`, `Height px`
- Typography: `Base size`, `Title size`, `Axis size`, `Legend size`
- Axis styling: `Axis line thickness`
- X-axis readability: `X label angle`, `Wrap X labels`, `Max lines`
- Color palette + advanced palette controls
- Custom color overrides per group

## 6. Graph-Specific Settings

### 6.1 Boxplot

- Box width
- Point jitter and point size
- Significance bars/labels
- Legend-right option (non B/W mode)

### 6.2 Barplot

- Mean bar + error bars
- Raw point overlay (raw mode)
- In summary mode, bars/errors are rendered without raw replicate dots
- Significance bars/labels

### 6.3 Violin

- Violin body + point overlay
- Shared axis/point controls
- Significance bars

### 6.4 Stacked

- Included parameters selector
- Parameter order (CSV, bottom-to-top)
- Deviation bars on/off
- Error-bar color by parameter
- Black outline only for total bar
- Significance mode: bracket bars or labels
- Label mode supports parameter targeting and parameter-color labels

### 6.5 Correlation

- X/Y parameter selection
- Methods: Pearson, Spearman, Kendall
- Optional overlays: regression line, r, p, R^2, equation, labels
- Confidence interval style:
  - Shaded band
  - Dashed bounds
- Adjustable confidence level
- Axis min/max/interval controls
- Custom axis labels
- Correlation normalization target:
  - both axes
  - X only
  - Y only

### 6.6 Heatmap

- Parameter subset selection
- Scale mode:
  - None
  - By row (parameter)
  - By column (group)
- Clustering method: `ward.D2`, `ward.D`, `complete`, `average`, `single`, `mcquitty`, `median`, `centroid`
- Row/column dendrogram toggles
- Cell-value labels toggle

### 6.7 Correlation Matrix

- Multi-parameter selection
- Methods: Pearson, Spearman, Kendall
- Multiple-testing correction: Holm, FDR, Bonferroni, None
- Option to show only significant correlations in labels

### 6.8 Curves

- Axis limits and intervals (`X_Max`, `Y_Max`, breaks)
- Custom axis labels
- Curve line width
- Geometry:
  - Line + points
  - Line only
- Color mode:
  - By group palette
  - Single color
- Confidence interval style:
  - Ribbon
  - Error bars
- Replicate trajectories toggle (raw-curve mode only) + transparency control

## 7. Filters and Replicate Selection

### By Strain

- Strain selector
- Media filter (toggle all)
- Replicate filters per medium
- Global replicate exclusion

### Combined

- Visible group filter (`Strain-Media`)
- Replicate filters per group
- Optional strain-only labels
- Global replicate exclusion

## 8. Normalize by Control

Enable normalization and choose a control medium.

- Replicate-aware normalization is used whenever possible.
- If strict control matching is unavailable, BIOSZEN applies fallback logic and reports it.
- Normalized columns are stored internally with `_Norm` suffix.
- Correlation can normalize both axes or only one axis.

## 9. Statistical Analysis

### 9.1 Normality

- Shapiro-Wilk (`stats::shapiro.test`)
- Kolmogorov-Smirnov (`stats::ks.test`)
- Anderson-Darling (`nortest::ad.test`)

### 9.2 Significance

Main tests:

- ANOVA (`stats::aov`; post-hoc workflows include `rstatix` and `DescTools`)
- Kruskal-Wallis (`stats::kruskal.test`)
- Independent t-test (`rstatix::t_test`)
- Independent Wilcoxon (`rstatix::wilcox_test`)

Comparison modes:

- All vs All
- Control vs All
- Pair

Multiple-testing correction:

- Holm (`stats::p.adjust`, method `"holm"`)
- FDR (Benjamini-Hochberg; `stats::p.adjust`, method `"fdr"`)
- Bonferroni (`stats::p.adjust`, method `"bonferroni"`)
- None

Post-hoc options are dynamic by main test.

### 9.3 Summary Mode Behavior (Mean/SD/N)

- Normality is not computable from summary-only input; normality values are `NA`.
- Kruskal-Wallis and Wilcoxon are disabled (raw observations required).
- Significance uses Welch-style pairwise comparisons derived from Mean/SD/N.

## 10. Significance Annotations

Annotation modes for Boxplot/Barplot/Violin/Stacked:

- Bracket bars
- Direct labels

### Manual flow

1. Choose Group 1 and Group 2.
2. Enter label text (`*`, `**`, `***`, `ns`, or custom).
3. Add item.
4. Reorder items.
5. Edit selected item.
6. Remove selected items or clear all.

### Auto-generate from significance tests

1. Open auto-generation from the significance panel.
2. Choose inclusion mode:
   - only significant
   - all comparisons
3. Choose label mode:
   - stars
   - p-value (3 decimals)
4. Choose replace or append behavior.
5. Apply auto-generation.

After auto-generation, you can still edit, reorder, and remove individual items.

If bars/labels already exist:

- If `Replace current bars` is enabled, existing items are replaced by the new auto-generated set.
- If `Replace current bars` is disabled, existing items are kept and new items are appended.
- In append mode, duplicate comparisons are skipped automatically (same group pair; and same parameter context for stacked-label mode).

### Annotation layout controls

- Line width
- Offset from data
- Vertical spacing between bars
- Text padding
- Text size
- Hide caps

## 11. Curve Statistics Panel

Curve comparisons are available through these statistical procedures:
Primary R packages in this panel: `stats`, `splines`, and `gcplyr`.

- **Global curve-shape model comparison:** weighted linear models with natural splines (`stats::lm`, `splines::ns`) compared with `stats::anova`.
- **Point-by-point curve differences:** per-time z-style comparisons built from SD/N uncertainty, then combined with Fisher's method (`stats::pnorm`, `stats::pchisq`).
- **End-point difference test:** z-style comparison at the last shared timepoint using propagated SE (`stats::pnorm`).
- **Area under the curve (AUC):**
  - AUC is computed with `gcplyr::auc`.
  - Group normality screening uses Shapiro-Wilk (`stats::shapiro.test`).
  - Significance tests are selected by structure:
    - 2 groups: Welch t-test (`stats::t.test`) or Wilcoxon rank-sum (`stats::wilcox.test`).
    - 3+ groups: ANOVA (`stats::aov`) or Kruskal-Wallis (`stats::kruskal.test`).
  - Pairwise AUC comparisons are also reported.
- **Multiple-testing correction in the curve table:** Holm (`stats::p.adjust`).

Output fields include `Estimate`, `P_value`, `P_adjusted`, and `Stars`.

## 12. Data Quality Panel

QC tables for current filters:

- Missing values by parameter
- Outliers by parameter/group (IQR rule)
- Sample size and biological replicate coverage by group

Automatic biological-replicate deselection tools:

- **IQR-based outlier deselection:** detects outlier replicates per group using the selected IQR multiplier, then deselects flagged biological replicates.
- **Keep-N replicate limit:** keeps only the N most reproducible biological replicates per group (ranked by distance to the group median across parameters).
- **Manual keep/deselect control:** replicate selectors remain editable, so you can keep exactly the biological replicates you want after automatic steps.

## 13. Composition Panel

- Add plots to the panel
- Reorder included plots
- Configure rows/columns and canvas size
- Apply style overrides globally or by graph type
- Export as PNG, PPTX, or PDF
- Download/upload composition metadata

## 14. Downloads and Reproducibility

Exports available from the main plotting tab:

- Plot PNG/PDF
- Data workbook
- Plot metadata workbook
- Statistical results workbook
- Full ZIP bundle with saved versions and reproducibility assets

## 15. Growth Parameter Extraction

In the growth module, outputs are intended for parameter extraction from microbial growth curves. You can:

- Load one or more growth workbooks
- Set max time and interval
- Compute:
  - `uMax`: maximum specific growth rate (per time unit).
  - `max_percap_time`: time at which the per-capita growth rate reaches its maximum.
  - `doub_time`: estimated doubling time (typically `ln(2) / uMax`).
  - `lag_time`: estimated lag-phase duration before sustained exponential growth.
  - `ODmax`: maximum signal/optical density reached in the analyzed window.
  - `max_time`: time at which `ODmax` is reached.
  - `AUC`: area under the growth curve across the analyzed time range.
- Download ZIP outputs
- Import outputs into Plots and Stats when one file is loaded

## Contact

For comments or support: `bioszenf@gmail.com`

© BioSzen. All rights reserved.
