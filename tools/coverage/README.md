# BIOSZEN application coverage tool

This folder contains the reusable reporting command used by the manual GitHub Actions coverage workflow.

| Script | Purpose | Safe reuse |
| --- | --- | --- |
| `report-app-coverage.R` | Runs the existing `tests/testthat` suite through `covr::file_coverage()`, uses `zero_coverage()`, and writes total, section, file, and uncovered-line reports for `inst/app`. | Reuse whenever application coverage needs to be recalculated. It does not edit application source, enforce a threshold, run `pkgcheck`, or upload to an external coverage service. |

## GitHub Actions

Open **Actions > App test coverage > Run workflow**. The workflow is manual-only. When it finishes:

- the run summary shows total coverage and coverage by section and file;
- the `bioszen-app-coverage-<run number>` artifact contains the derived reports;
- `uncovered-line-ranges.csv` identifies functions and line ranges not executed by the tests;
- `coverage-test-summary.csv` and `coverage-test-issues.csv` record the test status without suppressing the coverage report.
- `coverage-by-test-lane.csv` separates browser E2E, direct-source, and other in-process test execution.

The artifact does not contain an HTML source listing, package bundle, or application source copy. It is retained for 14 days.

## Local use

From the repository root, with package dependencies plus `covr` and `testthat` installed:

```powershell
Rscript tools/coverage/report-app-coverage.R coverage-report
```

The command runs the full existing test suite, including browser tests. It can therefore take a long time and may open child R/Chrome processes. Test failures and warnings are recorded in derived CSV files but do not suppress the diagnostic coverage report; failures in app loading or coverage instrumentation still return a non-zero exit status.

Focused source-based tests use the instrumented environment when the coverage command provides one, so their executed functions can contribute to line coverage without changing application code. Browser E2E tests still run in a separate Shiny process; their results are reported in a dedicated functional lane instead of being used to inflate parent-process line coverage. This distinction is stated in every generated report.
