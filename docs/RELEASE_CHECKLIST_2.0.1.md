# BIOSZEN 2.0.1 Release Checklist

Use this checklist to finalize and publish the 2.0.1 release.

## A) Pre-Release Validation

- [x] Update version in `DESCRIPTION` to `2.0.1`.
- [x] Add dated `2.0.1` entry in `CHANGELOG.md`.
- [x] Align metadata versions:
  - [x] `.zenodo.json`
  - [x] `CITATION.cff`
  - [x] README citation text
- [x] Run test suite:
  - Command: `Rscript -e "testthat::test_dir('tests/testthat')"`
  - Expected: no failures
- [x] Build package:
  - Command: `R CMD build .`
- [x] Run package check:
  - Command: `R CMD check --no-manual --as-cran BIOSZEN_2.0.1.tar.gz`
  - Expected: no ERROR/WARNING

## B) Dependency/Namespace Hygiene

- [x] Remove unused imports (`webshot`, `future`, `future.apply`, `parallelly`).
- [x] Keep runtime package loading aligned with actual usage in `inst/app/global.R`.

## C) Git Release Steps

- [ ] Review diff and confirm no unintended files are included.
- [ ] Commit with message:
  - `release: BIOSZEN 2.0.1`
- [ ] Create annotated tag:
  - `git tag -a v2.0.1 -m "BIOSZEN 2.0.1"`
- [ ] Push branch and tags:
  - `git push`
  - `git push --tags`

## D) Distribution Steps

- [ ] Create GitHub Release `v2.0.1`.
- [ ] Attach release notes from `docs/RELEASE_NOTES_2.0.1.md`.
- [ ] Attach build artifact if needed (`BIOSZEN_2.0.1.tar.gz`).
- [ ] Update Zenodo deposition metadata and publish archive.

## E) Post-Release Smoke

- [ ] Fresh install smoke test on a clean R library.
- [ ] Launch `BIOSZEN::run_app()` and verify:
  - [ ] Home screen loads
  - [ ] Plot generation works
  - [ ] Growth module loads
  - [ ] Export actions produce files
