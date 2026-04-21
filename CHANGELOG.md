# Changelog
All notable changes to this project will be documented in this file.

The format is based on Keep a Changelog and this project adheres to
Semantic Versioning.

## [Unreleased]
- No notable changes yet.

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
