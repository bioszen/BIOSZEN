# BIOSZEN

BIOSZEN is an app for data analysis and visualization from an Excel file. It can
generate boxplot, barplot, violin plot, stacked plots, and correlation plots
between parameters. It includes normality tests such as Shapiro-Wilk,
Kolmogorov-Smirnov, and Anderson-Darling, and significance tests such as ANOVA,
Kruskal-Wallis, t-test, and Wilcoxon. Additionally, the app can extract
microbial growth curve parameters:

- `uMax`: maximum specific growth rate estimated from the exponential phase.
- `max_percap_time`: mean time across the exponential-phase window used for uMax.
- `doub_time`: doubling time (ln(2) / uMax).
- `lag_time`: estimated lag phase duration before exponential growth.
- `ODmax`: maximum observed optical density (or measurement value).
- `max_time`: time at which ODmax occurs.
- `AUC`: area under the growth curve.

## Requirements
- R >= 4.1 (required; RStudio recommended for opening and reviewing scripts).
- Internet connection to install dependencies the first time.

## Quickstart
1. Make sure R is installed on your system.
2. Download `BIOSZEN-vX.Y.Z-bundle.zip`.
3. Unzip it and open the `BIOSZEN-vX.Y.Z` folder.
4. Open Terminal (macOS) or PowerShell (Windows) in that folder.
5. Run one of these launchers
   - `Rscript App.R` (works on macOS and Windows, requires `BIOSZEN-vX.Y.Z.tar.gz`
     in the same directory).
   - macOS: `Rscript BIOSZEN-vX.Y.Z-mac-App.R` (self-contained).
   - Windows: `Rscript BIOSZEN-vX.Y.Z-win-App.R` (self-contained).
6. The app opens at `http://127.0.0.1:4321`.

If you prefer RStudio, open the corresponding `.R` launcher and click Source.

## Files used by the user
- `BIOSZEN-vX.Y.Z.tar.gz`: package used by `App.R`.
- `App.R`: launcher for macOS and Windows; it requires the tarball in the same
  directory.
- `BIOSZEN-vX.Y.Z-mac-App.R`: self-contained launcher for macOS.
- `BIOSZEN-vX.Y.Z-win-App.R`: self-contained launcher for Windows.
- `inst/app/www`: user guides in English and Spanish.

## Troubleshooting
- `Rscript` not found: use the full path to `Rscript` or add R to PATH.
- Missing packages: check Internet access and review `bioszen_r.log` in the folder.
- macOS Gatekeeper: `xattr -dr com.apple.quarantine <folder>`.
- Port 4321 in use: edit `App.R` and change `shiny.port`.

## How to cite
See `CITATION.cff` and the Zenodo DOI once it is published.

## License
GPL-3.0.
