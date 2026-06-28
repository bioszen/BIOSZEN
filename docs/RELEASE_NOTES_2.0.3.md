# BIOSZEN 2.0.3 Release Notes

Release date: 2026-06-26

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

## Release Hygiene

- Version metadata is set to `2.0.3` in `DESCRIPTION`, `.zenodo.json`, `CITATION.cff`, and README citation text.
- Local-only files such as R history, launch logs, Codex/session attachments, helper-tool caches, local libraries, and generated archives are excluded from source builds.
- Repository-only documentation, screenshots, skills, and utility scripts are kept outside the R source package through `.Rbuildignore`.

## Validation Summary

- Local Windows package check:
  - `R CMD check --no-manual --no-build-vignettes BIOSZEN_2.0.3.tar.gz`
  - `Status: OK`
- Focused regression tests:
  - Axis interval input guard passed.
  - Plot/composition text metadata styling guard passed.
  - Normalized parameter-switching metadata round trip passed.
  - Plotly browser text hook now applies bold, italic, and underline styles to X/Y title, tick-label, data-label, and group-label nodes.
- GitHub Actions:
  - `windows-latest, R-release`: full testthat suite including Shiny e2e passed.
  - `macos-latest, R-release`: core testthat suite passed.

## Notes

- The app continues to declare `R (>= 4.1.0)`.
- Per-version local dependency libraries remain supported through `R_libs/<R-major.minor>` for direct project launches.
- If RStudio reports `all 128 connections are in use`, restart the R session or run `closeAllConnections()` before launching BIOSZEN again; that error means the R session is exhausted before the app can read its startup file.
