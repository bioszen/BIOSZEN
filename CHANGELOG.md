# Changelog
All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

## [Unreleased]

## [2.0.4] - 2026-07-11
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
