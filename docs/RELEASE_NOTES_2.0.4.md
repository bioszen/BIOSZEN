# BIOSZEN 2.0.4 Release Notes

Release candidate updated: 2026-07-11

## Highlights

- Hardened generated downloads across workbooks, metadata, statistics, graphics, and bundle exports.
- Improved standalone startup so `App.R` can launch BIOSZEN from versioned source archives and extracted source folders.
- Added stronger metadata reproducibility for typography, per-axis text styling, data-label styling, composition settings, error-bar choices, and legend placement/style.
- Fixed axis interval inputs so typed values are accepted directly and no longer get overwritten while editing.
- Fixed Plotly text styling so style changes apply to both X/Y axis titles, axis tick labels, and data labels where those texts are visible.
- Added optional independent styling for visible group labels on plot axes, with metadata export/import support.
- Renamed axis text styling controls to axis tick labels so categorical and numeric axes are described correctly.
- Stabilized parameter switching while normalization is active so the selected raw parameter, normalized axis controls, and metadata remain synchronized without restarting the app.
- Metadata now round-trips the selected parameter, normalization flag, and control medium with validation on import.
- Stabilized filtered exports when biological or technical replicates are deselected.
- Strengthened statistical-analysis and normalization guards so valid plotted data remains available without restarting the app.
- Changed the default raster export resolution to **300 DPI** while preserving the editable 72-600 DPI range.
- Centralized DPI validation and kept browser/CSS geometry separate, so changing DPI changes raster pixel density without moving or resizing graphs.
- Added safe 300 DPI fallback for legacy metadata without DPI and for missing, non-numeric, zero, negative, or unsupported DPI values.
- Applied the selected DPI to individual and composition PNG output, Plotly raster downloads, clipboard raster output, saved plot versions, and PowerPoint raster fallback. PDF and normal editable PowerPoint content remain vector-based.
- Added publication-style defaults, internal violin boxes, proportional composition scaling, and configurable single-slide PowerPoint size/orientation.
- Added universal composition typography controls for tick labels, legends, font family/styles, axis-specific sizes, angles, and alignment.

## Release Hygiene

- Version metadata is set to `2.0.4` in `DESCRIPTION`, `.zenodo.json`, and `CITATION.cff`; README and startup citation text use the approved Zenodo citation and DOI.
- Local-only files such as R history, launch logs, Codex/session attachments, helper-tool caches, local libraries, and generated archives are excluded from source builds.
- Repository-only documentation, screenshots, skills, and utility scripts are kept outside the R source package through `.Rbuildignore`.

## Validation Summary

- `R CMD check --no-manual --no-build-vignettes BIOSZEN_2.0.4.tar.gz` completed with `Status: OK` on Windows and R 4.6.0.
- The complete core testthat suite passed, including normalization, statistics, growth extraction, filtered downloads, selector stability, plot orientation, composition, metadata, and export coverage.
- The full real-Chrome Shiny integration suite passed with `NOT_CRAN=true`, including rapid selection changes, raw/normalized switching, strain/combined views, graph modes, growth stop/restart, statistics, metadata restoration, DPI, and PowerPoint geometry.
- Preview/download fidelity passed for Boxplot, Bar, Violin, Stacked, Correlation, Correlation Matrix, and Heatmap outputs at the 300 DPI default while preserving vector-page dimensions.
- The optional large-heatmap and cluster-workbook performance smoke tests passed with `NOT_CRAN=true`.
- A clean 2.0.4 installation served the Shiny application successfully and stopped without leaving a listener.
- The GitHub Actions push gate runs the full suite, including Shiny e2e, on `windows-latest` and the core suite on `macos-latest`.

## Notes

- The app continues to declare `R (>= 4.1.0)`.
- Per-version local dependency libraries remain supported through `R_libs/<R-major.minor>` for direct project launches.
- If RStudio reports `all 128 connections are in use`, restart the R session or run `closeAllConnections()` before launching BIOSZEN again; that error means the R session is exhausted before the app can read its startup file.
