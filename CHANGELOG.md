# Changelog
All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

## [Unreleased]

- Added centralized RRID metadata (`RRID:SCR_028902`), RRID and Methods citation formats, startup citation output, provenance in reports and metadata exports, and a Zenodo alternate-identifier relation.
- Corrected the latest archived release metadata to BIOSZEN 2.1.2 and Zenodo DOI `10.5281/zenodo.22117454` while retaining the concept DOI for the general citation.

## [2.1.2] - 2026-08-24
- Added configurable concentration-response (dose-response) plots for selected strains and replicates, with automatic or editable concentration mapping, raw or normalized responses, linear or logarithmic concentration axes, and publication-oriented appearance controls.
- Added four-parameter response-curve fitting, IC50 estimates and confidence intervals, model diagnostics, susceptibility ranking, pairwise strain comparisons with Holm adjustment, and complete Excel/statistical bundle exports.
- Added flexible growth-parameter extraction from both fixed-interval and irregular/discontinuous numeric time points while preserving the established fixed-interval workflow.
- Improved plot and PowerPoint export fidelity, application diagnostics, update messaging, stability, and general usability, and fixed issues affecting reactive dose-response controls and normalized plot labels.

## [2.1.1] - 2026-08-01
- Matched editable PowerPoint output for stacked plots to the existing preview/raster palette, stack order, and proportions without changing other plot exports.
- Added optional X-axis label angle and multi-line wrapping controls to heatmaps and correlation matrices while preserving their existing defaults.
- Added a non-blocking weekly update check that stays silent when BIOSZEN is current and shows a closable, localized release notification only when a newer version is available.

## [2.1.0] - 2026-07-30
- Added a public browser-first `BIOSZEN()` launcher that uses the operating system's configured default browser, with an optional app-style Chromium mode.
- Added an automatically registered RStudio Addin for launching BIOSZEN without typing commands.
- Added a consent-based in-app update control that checks R-universe, keeps the session open when no update exists, and installs only after BIOSZEN has closed.
- Preserved `run_app()` compatibility and explicit controls for regular-browser and no-browser startup.

## [2.0.5] - 2026-07-30
- Fixed installed-package test discovery so R-universe checks no longer assume individual files remain under `R/` after installation.
- Kept the full cross-platform test matrix on Windows, macOS ARM, and macOS Intel while making browser widget fixtures independent of Pandoc.
- Made the large grouped-workbook fixture resolve correctly in both source-tree and installed-package checks.
- Included the downloadable English and Spanish DOCX manuals in installed packages and documented R-universe installation in both manuals.
- Updated archived Zenodo metadata for the published 2.0.4 release while retaining the concept DOI for the general BIOSZEN citation.

## [2.0.4] - 2026-07-11
- Fixed standalone bundle startup after dependency installation by continuing in a clean R process, preventing loaded-namespace conflicts in RStudio on Windows and macOS.
- Preserved the BIOSZEN version, GitHub link, and approved citation block in the parent terminal when startup uses the clean-process handoff, and made the citation the final startup message after Shiny begins listening during direct launches.
- Changed the shared raster export default from 96 to 300 DPI while keeping the 72-600 DPI control user-editable.
- Centralized export DPI validation and separated the 300 DPI export default from the browser's 96 CSS pixels-per-inch geometry.
- Added safe 300 DPI fallback behavior for missing, malformed, zero, negative, and out-of-range input or metadata values.
- Added backward-compatible metadata restore behavior so legacy files without DPI remain usable and other valid settings are preserved.
- Applied selected DPI consistently to individual/composition PNG exports, Plotly downloads, raster clipboard output, saved plot versions, and PowerPoint raster fallback without changing logical dimensions or layout.
- Added publication-style plot defaults, internal violin boxes, proportional composition rendering, and configurable one-slide PowerPoint dimensions/orientation.
- Added universal composition typography controls for tick labels, legends, font family/styles, X/Y size, angle, and alignment, with metadata round-trip support.
- Expanded unit and real-browser regression coverage for DPI, composition styling, metadata compatibility, PowerPoint geometry, and preview/export fidelity.
- Updated English and Spanish manuals with composition, PowerPoint, DPI, metadata, performance, and compatibility guidance.

## [2.0.3] - 2026-06-26
- Hardened downloads for generated workbooks, metadata, statistics, PNG/PDF graphics, and bundle ZIP exports.
- Improved the standalone launcher so BIOSZEN can start from versioned archives or extracted source folders.
- Expanded metadata reproducibility for plot typography, composition typography, per-axis typography, data-label typography, error-bar statistics, and composition plot metadata.
- Set Boxplot error-bar defaults to Min-Max while preserving SD and SEM options.
- Reviewed and strengthened normalized-data guard behavior to avoid raw fallback plots and app stalls when control selections are unavailable.
- Fixed axis interval inputs so typed values are not overwritten while editing.
- Stabilized technical-replicate outlier deselection and filtered parameter workbook exports.
- Added explicit legend-on-right and legend text style metadata round trips for individual plots and compositions.
- Fixed Plotly text styling so bold, italic, and underline styling is applied to both X/Y axis titles, axis tick labels, and data labels where those texts are visible.
- Added optional independent styling for visible group labels on plot axes, with metadata export/import support.
- Renamed axis text controls from numeric wording to axis tick-label wording for categorical and numeric axes.
- Stabilized parameter switching while normalization is active so axes, plots, and metadata follow the selected raw parameter without requiring an app restart.
- Metadata exports now preserve the selected parameter, normalization flag, and control medium, while metadata imports validate those values before applying them.
- Cleaned release packaging rules to keep local session artifacts, logs, helper folders, and generated archives out of source builds.

## [2.0.2] - 2026-06-05
- Prepared the project for the BIOSZEN 2.0.2 release.
- Declared the required `digest` runtime dependency used by app export logic.
- Declared optional rich text and mixed-model namespaces (`ggtext`, `lme4`, `lmerTest`) in `Suggests`.
- Removed generated tracked helper cache/output files from the release surface.
- Replaced tracked helper/test defaults that pointed at local user folders with explicit path inputs or environment-variable-driven skips.
- Synchronized release metadata references to `2.0.2` in `DESCRIPTION`, `.zenodo.json`, `CITATION.cff`, and README citation text.

## [2.0.1] - 2026-04-21
- Stabilized analysis control persistence so plot-type and advanced palette selections are retained more reliably across UI refreshes.
- Release hygiene updates:
  - Improved package metadata title for CRAN-style checks.
  - Declared `later` and `png` in `Suggests` for test-only namespace usage.
  - Removed unused dependency declarations (`webshot`, `future`, `future.apply`, `parallelly`) to reduce import surface and startup namespace conflicts.
  - Reduced packaging noise by excluding repository-only and local check artifacts from source builds.
  - Removed debug/temporary top-level files that were not part of app functionality.
  - Synchronized release metadata references to `2.0.1` in `.zenodo.json`, `CITATION.cff`, and README citation text.

## [2.0.0]
- Improved interface behavior and usability in key workflows, including growth-processing controls and status feedback.
- Expanded growth processing support with improved input handling, cancellation-safe execution, and stronger stability for long-running jobs.
- Added and extended tests for growth workflows, replicate selection synchronization, export filtering behavior, and processing stability.
- Fixed multiple replicate-selection and export consistency issues across strain/group scopes.
- Updated defaults and synchronization paths in analysis modules to provide more predictable behavior across sessions and exports.

## [1.0.0]
- Baseline release for public archive.
