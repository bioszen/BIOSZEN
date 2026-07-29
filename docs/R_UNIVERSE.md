# BIOSZEN on R-universe

BIOSZEN is intended to be built from public GitHub releases by the BIOSZEN
R-universe organization.

## Registry configuration

The separate `bioszen/bioszen.r-universe.dev` repository should contain this
`packages.json` entry:

```json
[
  {
    "package": "BIOSZEN",
    "url": "https://github.com/bioszen/BIOSZEN",
    "branch": "*release"
  }
]
```

The `*release` selector keeps R-universe on stable GitHub releases instead of
building arbitrary development commits.

## User installation

```r
install.packages(
  "BIOSZEN",
  repos = c(
    "https://bioszen.r-universe.dev",
    "https://cloud.r-project.org"
  )
)
```

Then launch the app with:

```r
BIOSZEN::BIOSZEN()
```

## Release checklist

1. Run the complete package and Shiny test matrix on Windows, macOS ARM, and macOS Intel.
2. Run `R CMD build` and `R CMD check --no-manual` on a clean source package.
3. Confirm `citation("BIOSZEN")`, `BIOSZEN::bioszen_citation()`, and the concept DOI.
4. Create the public GitHub release for the package version.
5. Confirm the R-universe build and installation log before announcing the release.

The package uses the Zenodo concept DOI `10.5281/zenodo.18217210`. A
version-specific DOI must only be documented after that exact version has been
published by Zenodo.
