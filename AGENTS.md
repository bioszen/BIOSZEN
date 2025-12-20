# AGENT Guidelines for BIOSZEN

## Overview

This repository now contains an R package that bundles a modular Shiny

application. The main script is `app.R`, stored both at the project root and
within `inst/app` so it is included in the package. `shiny::runApp()` works
from the root directory or the installed package. You can also launch the app
via the exported `run_app()` function.

App sources live under `inst/app` and are loaded dynamically when launching the
app.

Directory structure:

- `R/` – package functions like `run_app()`.
- `inst/app/` – all Shiny modules and resources (`global.R`, `helpers.R`,
  `params/`, `stats/`, `graficos/`, `server/`, `ui/`, `www/`).
- `tests/` – automated tests executed with `testthat`.

## Adding Modules

To add new plotting or utility modules, place your `*.R` files in the

appropriate folder inside `inst/app` (`params`, `stats`, `graficos`). They are
sourced automatically via the `source_dir()` helper in `app.R`. Ensure
all files contain valid R code so they pass the parse tests.

## Running Tests

Before committing, run the test suite to verify that all R files parse
correctly:

```bash
Rscript -e "testthat::test_dir('tests/testthat')"
```

These tests also run in GitHub Actions (`.github/workflows/r-tests.yml`).

## Contribution Tips

- Keep functions self-contained and document new inputs or outputs.
- Avoid hard-coding paths; use the `www` directory inside the app for assets.
- Maintain existing functionality when refactoring.

- Test the app locally with `BIOSZEN::run_app()` after making changes.

---

¿Necesitas más información para continuar modificando la aplicación?
