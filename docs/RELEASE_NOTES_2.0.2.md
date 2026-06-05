# BIOSZEN 2.0.2 Release Notes

Release date: 2026-06-05

## Highlights

- Release hygiene refresh for BIOSZEN 2.0.2.
- Added the missing required `digest` dependency used by app export logic.
- Documented optional rich text and mixed-model namespaces through `Suggests`.
- Removed generated tracked helper cache/output files and local development environment state from the release surface.
- Replaced hard-coded local user paths in tracked helper/test code with explicit path inputs or environment-variable-driven skips.

## Validation Summary

- R 4.6.0 full testthat suite:
  - `FAIL 0 | WARN 0 | SKIP 14 | PASS 853`
- R 4.6.0 growth-focused tests:
  - `FAIL 0`; optional private Rapa parity workbook test skipped unless `BIOSZEN_RAPA_CURVES_XLSX` is set.
- R 4.5.2 compatibility:
  - Declared imports installed into `R_libs/4.5`.
  - App startup smoke test passed with HTTP 200 and BIOSZEN UI markers present.
  - Growth-focused tests passed with only package binary-version warnings.
- Package build:
  - `R CMD build .` built `BIOSZEN_2.0.2.tar.gz`.
- Package check:
  - `R CMD check --no-manual --as-cran BIOSZEN_2.0.2.tar.gz`
  - `Status: OK`
  - Local check uses `R_LIBS` pointing to the project R 4.6 dependency library and disables only remote CRAN incoming lookup because this machine cannot reach the `CRAN.R-project.org` index reliably.

## Notes

- The app continues to declare `R (>= 4.1.0)`, which matches its use of base pipe syntax.
- Per-version local dependency libraries remain supported through `R_libs/<R-major.minor>` for direct project launches.
- Existing historical release entries and files for `2.0.1` are intentionally preserved.
