# BIOSZEN 2.0.1 Release Notes

Release date: 2026-04-21

## Highlights

- Improved UI state stability for plotting workflows:
  - Plot type selection is now more persistent during control refreshes.
  - Advanced palette selection is retained more reliably.
- Improved app bootstrap behavior when running from an installed package:
  - Fixed app environment resolution to avoid symbol lookup issues in startup.
- Documentation and release metadata refresh:
  - Version metadata aligned to `2.0.1` in project citation/deposit files.
  - Updated changelog section for 2.0.1.

## Dependency Hygiene

- Removed unused package declarations from `Imports`:
  - `webshot`
  - `future`
  - `future.apply`
  - `parallelly`

This reduces import surface and avoids unnecessary namespace masking warnings.

## Validation Summary

Validated on Windows with R 4.5.2:

- `testthat::test_dir('tests/testthat')`
  - `FAIL 0 | WARN 5 | SKIP 13 | PASS 808`
- `R CMD build .`
  - built `BIOSZEN_2.0.1.tar.gz`
- `R CMD check --no-manual --as-cran BIOSZEN_2.0.1.tar.gz`
  - `Status: 2 NOTEs` (non-blocking; no ERROR/WARNING)

## Notes

- Existing historical release entries (e.g., `2.0.0`) are intentionally preserved in `CHANGELOG.md`.
- Gallery/manual image updates included in this branch remain compatible with this release.
