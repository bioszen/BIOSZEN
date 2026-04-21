# Reusable Script Index

This folder contains reusable utilities created for BIOSZEN data preparation workflows.

## Scripts

### `build_grouped_params_platemap.py`
- Purpose: Converts a grouped-parameters workbook (one sheet per parameter, with `Strain` + `RepBiol` + media columns) into a BIOSZEN-compatible platemap workbook.
- What it generates:
  - `Datos`:
    - required BIOSZEN metadata columns first (`Well`, `Strain`, `Media`, `Orden`, `Replicate`, `BiologicalReplicate`, `TechnicalReplicate`),
    - then one parameter column per input sheet (sheet name preserved as parameter name).
  - `PlotSettings`:
    - one row per parameter (`Parameter`, `Y_Max`, `Interval`, `Y_Title`).
  - `SourceMapping`:
    - row-level key traceability (`Well`, `Strain`, `Media`, `BiologicalReplicate`, `SourceKey`).
- Merge behavior:
  - `intersection` mode (default): keeps only keys present in all parameter sheets to avoid missing values in the final platemap.
  - `union` mode: keeps all keys from all sheets and allows missing values when a parameter is absent for some groups.
- Additional behavior:
  - Deterministic ordering by media, then strain, then biological replicate.
  - Deterministic `Orden` assignment by strain/media grid.
  - 96-well style sequential wells (`A1..A12`, `B1..B12`, ...).
  - Optional `Control` relabeling (for example to `Mock`) via CLI argument.
- Reuse without asking again when:
  - You receive grouped parameter files (multi-sheet by parameter) and need an app-ready `platemap` file.
  - You need to concatenate all parameters into a single `Datos` sheet while preserving BIOSZEN metadata columns first.
  - You need a deterministic and repeatable conversion workflow for RNA-seq or phenotype parameter matrices.

### `build_ergosterol_platemap_with_replicates.py`
- Purpose: Builds a new ergosterol platemap workbook by taking a base file with biological replicate 1 and adding replicate 2 and 3 from two Results workbooks distributed by metric sheet.
- Mapping rules implemented:
  - `raw data` sheet -> raw sterol columns (no prefix in `Datos`).
  - `normalized` sheet -> `NORM ...` columns.
  - `µg per mg` sheet -> `μg/mg ...` columns.
  - `%` sheet -> `% ...` columns.
  - `Total sterols` is populated only for raw/NORM/μg/mg groups, never for `%`.
- Key matching behavior:
  - Matches rows by normalized `Media + Strain`.
  - Treatment labels are normalized as:
    - `mock -> Mock`
    - `U18666A -> U18`
    - `Rapamycin -> Rapa`
    - `Rapamycin + U18666A (co-treatment) -> Rapa-U18`
  - Strain labels are normalized so:
    - `NA` maps to blank/NA strain rows in the base platemap.
    - `BY4742 Δ...` maps to `Δ...`.
    - `BY4742` (with/without trailing spaces) maps to `BY4742`.
- Output behavior:
  - Replicate 1 rows come from the base platemap values.
  - Replicate 2/3 rows are added when the key exists in each Results source.
  - If a sample exists only in two biological replicates, the missing replicate is skipped automatically.
  - Wells are reassigned sequentially (`A1`, `A2`, ...), and `PlotSettings` is copied from the base file.
- Reuse without asking again when:
  - You need to extend an existing sterol platemap from one biological replicate to two or three replicates using Results files with the same sheet/column layout.
  - You need the same BIOSZEN-style metadata structure (`Well`, `Strain`, `Media`, `Orden`, `Replicate`, `BiologicalReplicate`, `TechnicalReplicate`).
  - You need safe handling of incomplete replicate coverage (for example, some keys only present in replicate 1+2).

### `build_tools2_cell_and_n_from_comparative.py`
- Purpose: Builds one shared per-cell `Curvas_Int.xlsx` and two matching platemaps (cell-level and n-level) by combining:
  - `informe_comparativo_tools2_selector.xlsx` (`*_celula` sheets, including cumulative per-cell histogram bins),
  - `organized_results_advanced.xlsx` (all cell-level advanced sheets: intensity, background-corrected, spatial, mito, coloc where available).
- Main outputs:
  - `Curvas_Int.xlsx`:
    - `Sheet1`: cumulative frequency percentage (0-100) by intensity bin (`Time` 0..255), one column per cell/well.
    - `Sheet2`: fixed axis settings for cumulative percentage curves.
  - `platemap_Int_cell.xlsx`:
    - `Datos`: one row per cell (biological replicate indexed per `Strain + Media`, one technical replicate per row).
    - `PlotSettings`: one row per numeric parameter.
  - `platemap_Int_n.xlsx`:
    - `Datos`: same wells/rows as curves, but biological replicate is `n` and technical replicate indexes cells within each `Strain + Media + n`.
    - `PlotSettings`: one row per numeric parameter.
  - App-ready bundles:
    - `cell_level_bundle/Curvas_Int.xlsx` + `cell_level_bundle/platemap_Int.xlsx`
    - `n_level_bundle/Curvas_Int.xlsx` + `n_level_bundle/platemap_Int.xlsx`
- n-mapping rules implemented:
  - Uses `cell_intensity` (`n_numeric`) from `organized_results_advanced.xlsx` as primary source (exact per-cell join).
  - Uses advanced file-level mode (`stain + condition + rep + archivo`) as secondary source when exact per-cell n is missing.
  - If advanced n is still missing, uses fallback from comparative filename + rep:
    - explicit `nX` token when present,
    - `rep1 -> n6`
    - `rep2 -> n7`
  - This fills n values even when some rows are partially missing in one source workbook.
- Parameter coverage:
  - Comparative per-cell metrics (`intensidad_celula`, `histograma_celula_metricas`).
  - Advanced per-cell intensity (raw and corrected mean/integrated), spatial distribution (including radial bins), mito fragmentation metrics, and available coloc metrics.
- Reuse without asking again when:
  - You need one shared curves file plus both cell-level and n-level platemaps from tools2 comparative + advanced workbooks.
  - You need strict well concordance (`A1` curve corresponds to `A1` platemap row in both outputs).
  - You need n-level app inputs where each `n` is biological replicate and each cell is technical replicate.

### `build_tools2_nlevel_bioszen_inputs.py`
- Purpose: Builds a BIOSZEN-compatible n-level `Curvas` + `Platemap` pair from tools2 advanced outputs.
- Main input:
  - `organized_results_advanced.xlsx`
- Main outputs:
  - `Curvas_Int_n.xlsx`:
    - `Sheet1`: cumulative percentage curves per `stain + condition + n`.
    - `Sheet2`: axis settings.
  - `platemap_Int_n.xlsx`:
    - `Datos`: one row per `stain + condition + n` with n-level averaged parameters.
    - `PlotSettings`: one row per numeric parameter.
- Aggregation behavior:
  - Operates at n-level only (`n_label`, `n_numeric`), no per-cell rows.
  - Uses `histogram_n` counts to reconstruct cumulative percentage curves (0-100).
  - Averages metrics across all cells that belong to each n group (not mean-of-photo-means).
- Parameter coverage:
  - Intensity raw/corrected mean + integrated.
  - Spatial distribution metrics (including radial bins and edge enrichment).
  - Mito metrics (including fragmentation, components, skeleton, branch points; mito rows only).
- Reuse without asking again when:
  - You need n-level BIOSZEN inputs (instead of cell-level) from tools2 advanced outputs.
  - You need strict well concordance between curves and platemap at n-level.
  - You need fixed condition ordering and BIOSZEN-ready `Datos` + `PlotSettings`.

### `build_tools2_celllevel_bioszen_inputs.py`
- Purpose: Builds a new BIOSZEN-compatible per-cell `Curvas_Int.xlsx` + `platemap_Int.xlsx` pair from:
  - `organized_results_advanced.xlsx` (advanced per-cell parameters),
  - `informe_comparativo_tools2_selector.xlsx` (per-cell cumulative histogram bins).
- What it generates:
  - Curves workbook:
    - `Sheet1`: cumulative frequency percentage (0-100) by intensity bin (`Time` 0..255), one column per well/cell.
    - `Sheet2`: curve axis settings.
  - Platemap workbook:
    - `Datos`: metadata + per-cell parameters (raw/corrected mean + integrated intensity, spatial metrics, mito metrics).
    - `PlotSettings`: one row per numeric parameter.
- Replicate rules implemented:
  - `rep1 -> BiologicalReplicate = 6`
  - `rep2 -> BiologicalReplicate = 7`
  - `TechnicalReplicate`: sequential index per `Strain + Media + BiologicalReplicate`.
- Group/order rules implemented:
  - Cell-level only (no aggregation by rep/photo/n).
  - Media labels preserved as `-`, `siNEG`, `siSOR`.
  - Fixed `Orden` by condition block:
    - Filipin (1-3), Mitotracker (4-6), Lysotracker Red (7-9), Lysotracker Green (10-12).
- Reuse without asking again when:
  - You need a tools2-based per-cell BIOSZEN input pair with strict well alignment (`A1 curve == A1 parameters`).
  - You need to regenerate outputs from updated tools2 advanced/comparative workbooks with the same schema.
  - You need cumulative percent curves from per-cell cumulative count bins (`I000..I255`).
- Notes:
  - Creates timestamped backups before overwriting existing output files.
  - Validates well order, media labels, and biological replicate mapping (`{6, 7}`).

### `build_per_cell_intensity_inputs.py`
- Purpose: Converts averaged intensity inputs into per-cell inputs for BIOSZEN by rebuilding:
  - `Curvas_Int.xlsx` with one cumulative curve per cell/well.
  - `platemap_Int.xlsx` with one mean intensity per same well plus `PlotSettings`.
- Main inputs:
  - `resumen_hist_cum_sin_rep_usado_por_celula_seleccionada_por_tincion.xlsx`
  - `informe_comparativo_por_celula_rep1_rep2.xlsx`
- Main outputs:
  - Curves workbook (`Sheet1`, `Sheet2`)
  - Platemap workbook (`Datos`, `PlotSettings`)
- Reuse without asking again when:
  - You need per-cell curves and per-cell mean intensity values instead of group averages.
  - Source workbook structures are unchanged (same sheet names and key columns).
  - You need strict one-to-one alignment between curve wells and platemap wells.
- Notes:
  - Automatically filters to selected photos (`photo_include == 1`) from the comparative report.
  - Creates timestamped backups before overwriting outputs.
  - Validates alignment, labels, and cumulative curve integrity.
  - Uses fixed original `Orden` by condition (Filipin: 1–3, Mitotracker: 4–6, Lysotracker Red: 7–9, Lysotracker Green: 10–12).
  - Assigns one biological replicate per row within each condition and sets one technical replicate (`A`) per row.

### `build_counttmm_platemap_from_reference.py`
- Purpose: Builds a BIOSZEN-ready platemap from `CountTMM.csv` by using a reference platemap layout (`Datos` + `PlotSettings`) and preserving `μMax`.
- What it generates:
  - `Datos` sheet with:
    - metadata columns (`Well`, `Strain`, `Media`, `Orden`, `Replicate`, `BiologicalReplicate`, `TechnicalReplicate`),
    - one parameter column per gene from `CountTMM.csv`,
    - `μMax` as the final parameter column.
  - `PlotSettings` sheet with one row per gene parameter plus `μMax`.
  - `SourceMapping` sheet that documents which CountTMM sample column was mapped to each well.
- Main inputs:
  - `CountTMM.csv` (gene x sample matrix with sample names like `WA.C`, `WA.RU.2`, etc.)
  - Reference platemap workbook containing `Datos` and `PlotSettings`.
- Condition mapping used:
  - `C -> Mock`
  - `R -> Rapa`
  - `U -> U18`
  - `RU -> Rapa-U18`
- Reuse without asking again when:
  - You need to regenerate a complete gene-parameter platemap from an updated CountTMM matrix.
  - You want to keep the same well structure and `μMax` values from a known reference workbook.
  - You need explicit strain-condition ordering for BIOSZEN plotting and correlation workflows.
- Notes:
  - Uses 3 biological replicates per condition and a single technical replicate label (`A`).
  - Orders conditions per strain as `Mock`, `Rapa`, `U18`, `Rapa-U18`.
  - Creates a timestamped backup if the output workbook already exists.

### `shiny_startup_smoke_test.py`
- Purpose: Runs a startup and responsiveness smoke test for the BIOSZEN Shiny app.
- What it checks:
  - The app process starts and serves the main HTML on localhost within a timeout.
  - The first response includes expected UI markers (for example `plotInteractivo`/BIOSZEN title text).
  - A second request succeeds shortly after, confirming the app remains responsive and does not hang immediately after startup.
- Main inputs:
  - App directory path (`--app-dir`, default current directory)
  - Rscript path (`--rscript`)
  - Optional host/port/timeouts
- Main output:
  - JSON report with `success`, startup timing, marker hits, and process stderr/stdout tails for debugging.
- Reuse without asking again when:
  - You need a quick operability check after UI/server changes that may affect startup.
  - You need to confirm the app is serving and not freezing before manual interactive testing.
  - You need a reproducible local health check in troubleshooting sessions.

### `generate_markdown_pdf.ps1`
- Purpose: Generates a polished PDF from a Markdown file using the same mechanism as the external reference script (`pandoc` + headless Microsoft Edge print-to-PDF).
- Main inputs:
  - `-InputMarkdown` (required): Markdown file path.
  - `-OutputPdf` (optional): target PDF path; if omitted, the script writes next to the Markdown file with `.pdf` extension.
  - `-Title` (optional): document title metadata.
  - `-WorkingDirectory` (optional): base folder for relative paths.
- Main output:
  - A PDF rendered with GitHub-style Markdown CSS and print-friendly pagination rules.
- Notes:
  - Waits for Edge completion before cleanup, preventing transient `ERR_FILE_NOT_FOUND` PDFs.
  - Uses print CSS rules to keep headings/subheadings together with the first content block and reduce orphaned subtitle page breaks.
- Reuse without asking again when:
  - You need to convert BIOSZEN documentation (`.md`) into distributable PDFs.
  - You want consistent A4 print styling and page-break behavior across docs.
  - You need reproducible local doc exports without opening a browser manually.

### `fix_buttons.py`
- Purpose: Normalizes selected Shiny `actionButton(...)` definitions in an R UI file by ensuring full-width button styling (`w-100`) and wrapped labels (`white-space: normal;`).
- Main inputs:
  - `--file` (optional): path to the target R UI file (default `inst/app/ui/ui_main.R`).
  - `--dry-run` (optional): report potential edits without writing changes.
- Main output:
  - In-place update of target `actionButton` calls for:
    - `runNorm`
    - `runSig`
    - `runAdvancedStats`
    - `sig_auto_apply`
    - `sig_update_label`
- Notes:
  - Uses a parenthesis-aware parser to patch full `actionButton` calls safely, including multiline calls.
  - Adds missing `class`/`style` arguments when absent.
- Reuse without asking again when:
  - You need consistent width and text wrapping for key action buttons after UI label or layout changes.
  - You are validating button accessibility/responsiveness in `ui_main.R` or equivalent UI files.
  - You want a repeatable, script-based fix instead of manual edits.

### `capture_manual_section_images.py`
- Purpose: Captures high-resolution, section-focused screenshots from the live BIOSZEN app and updates manual image assets in place.
- What it generates:
  - `01_app_home_overview.png`
  - `02_plot_setup_layers.png`
  - `03_filter_media_conditions.png`
  - `04_filter_biological_replicates.png`
  - `10_significance_annotations.png`
  - `13_growth_parameters_workflow.png`
- Main inputs:
  - `--app-dir`: BIOSZEN app root path.
  - `--output-dir`: target image folder (default `inst/app/www/manual_images`).
  - `--rscript`: path to `Rscript.exe`.
  - `--data-file`: reference platemap workbook used to initialize UI state.
  - `--curve-file`: reference curves workbook used for curve/growth-enabled states.
  - `--window-size`: headless browser viewport (for final image resolution).
- Main behavior:
  - Starts the app on a random localhost port.
  - Loads reference files to unlock data-dependent UI sections.
  - Captures section crops from stable UI regions (not full-window screenshots).
  - Overwrites the six manual images with refreshed captures.
- Reuse without asking again when:
  - You need to refresh manual screenshots after UI/layout changes.
  - You need consistent screenshots between English and Spanish manuals.
  - You need higher-resolution section captures while preserving existing file names and references.
