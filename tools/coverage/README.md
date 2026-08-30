# BIOSZEN application coverage tool

This folder contains the reusable reporting command used by the manual GitHub Actions coverage workflow.

| Script | Purpose | Safe reuse |
| --- | --- | --- |
| `report-app-coverage.R` | Runs the existing `tests/testthat` suite through `covr::file_coverage()`, uses `zero_coverage()`, and writes total, section, file, and uncovered-line reports for `inst/app`. | Reuse whenever application coverage needs to be recalculated. It does not edit application source, enforce a threshold, run `pkgcheck`, or upload to an external coverage service. |

## GitHub Actions

Open **Actions > App test coverage > Run workflow**. The workflow is manual-only. When it finishes:

- the run summary shows total coverage and coverage by section and file;
- the `bioszen-app-coverage-<run number>` artifact contains the derived reports;
- `uncovered-line-ranges.csv` identifies functions and line ranges not executed by the tests.

The artifact does not contain an HTML source listing, package bundle, or application source copy. It is retained for 14 days.

## Local use

From the repository root, with package dependencies plus `covr` and `testthat` installed:

```powershell
Rscript tools/coverage/report-app-coverage.R coverage-report
```

The command runs the full existing test suite, including browser tests. It can therefore take a long time and may open child R/Chrome processes. A failing test stops the report and returns a non-zero exit status.

Coverage for code executed only inside the separate Shiny browser-test process can be conservatively under-counted by `covr::file_coverage()`. This limitation is stated in every generated report.
