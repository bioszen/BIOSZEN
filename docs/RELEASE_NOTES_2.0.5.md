# BIOSZEN 2.0.5 Release Notes

Release candidate prepared: 2026-07-30

## Highlights

- Preserves all BIOSZEN 2.0.4 application, analysis, normalization, plotting,
  export, growth-parameter, and standalone-launcher behavior.
- Fixes installed-package test discovery for R-universe across Windows, Linux,
  macOS ARM, and macOS Intel builders.
- Keeps the full GitHub Actions application test suite on Windows, macOS ARM,
  and macOS Intel.
- Includes the downloadable English and Spanish DOCX manuals in installed
  packages and adds R-universe installation instructions to both manuals.
- Retains the centralized Zenodo concept DOI citation while recording the
  published 2.0.4 archive DOI in package metadata.

## Public R API

- `BIOSZEN()` launches the packaged Shiny application.
- `run_app()` remains supported for backward compatibility.
- `growth_parameters()` exposes the same robust-first, permissive-fallback
  growth calculation used by the Shiny workflow.
- `bioszen_update_available()` and `bioszen_update()` provide consent-based
  R-universe update handling.
- `bioszen_citation()` and `citation("BIOSZEN")` expose the official citation.

## Publication Notes

- The source package is version `2.0.5`.
- `Config/BIOSZEN/LatestArchivedVersion` remains `2.0.4` until Zenodo publishes
  a version-specific DOI for 2.0.5.
- R-universe follows stable GitHub releases through the `*release` selector;
  publishing a GitHub `v2.0.5` release is therefore required before retrying
  the R-universe build.
