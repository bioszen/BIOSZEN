# BIOSZEN

BIOSZEN is an application for data analysis and visualization based on Excel input files. It can generate boxplots, bar plots, curve plots, violin plots, stacked plots, and correlation plots between parameters. The app includes normality tests such as Shapiro–Wilk, Kolmogorov–Smirnov, and Anderson–Darling, as well as significance tests including ANOVA, Kruskal–Wallis, Student’s t-test, and the Wilcoxon test. In addition, BIOSZEN can extract microbial growth curve parameters:

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

## Get R and RStudio
- For new/basic users who do not already have the required software, open the bundled `Download_R_and_RStudio.html` to get the latest R (Windows/macOS/Linux) and RStudio Desktop links before running BIOSZEN.
- If you already downloaded R from the OS-specific buttons on that page (or directly from CRAN), jump straight to step 2 on the page to download RStudio.

## Quickstart

1. Make sure **R** and **RStudio** are installed on your system (see links below if needed).
2. Download `BIOSZEN-vX.Y.Z-bundle.zip` from the Releases page and unzip it.
3. Inside the unzipped folder you will find two files:
   - `BIOSZEN-vX.Y.Z.tar.gz` (do **not** unpack this file).
   - `App.R` (launcher).
   Keep them together in the same folder.
4. To run with RStudio (recommended):
   - Open `App.R` in RStudio and click **Source**.
5. To run from the terminal:
   - Open Terminal/PowerShell in that folder and run `Rscript App.R`.
6. The app opens at `http://127.0.0.1:4321`.

## First run (one-time setup)

The first time you launch BIOSZEN, it will download and install the required R packages and save them in a local folder called `R_libs` inside the BIOSZEN directory. **Do not delete this folder**. If `R_libs` is removed, BIOSZEN will need to download the dependencies again the next time you run it.

After this initial setup, BIOSZEN can run **offline**, as long as the `R_libs` folder remains in place.


## Files used by the user
- `BIOSZEN-vX.Y.Z.tar.gz`: BIOSZEN package used by the launcher (keep it next to `App.R`; do not unpack).
- `App.R`: cross-platform launcher for macOS and Windows.
- `inst/app/www`: user guides in English and Spanish.

## Gallery (quick view)

Key app screens (full set in `Gallery/screenshots/app`):

- Home overview:  
  ![Home](Gallery/screenshots/app/01_app_home_overview.png)
- Plot setup and layers:  
  ![Plot setup](Gallery/screenshots/app/02_plot_setup_layers.png)
- Correlation and statistics setup:  
  ![Correlation settings](Gallery/screenshots/app/07_correlation_setup.png)
- Normality and significance options:  
  ![Tests](Gallery/screenshots/app/09_significance_tests_setup.png)
- Growth parameter extraction:  
  ![Growth parameters](Gallery/screenshots/app/12_growth_parameters_results.png)

Plot examples (full set in `Gallery/screenshots/plots`):

- Boxplot:  
  ![Boxplot](Gallery/screenshots/plots/01_Boxplot.png)
- Growth curves:  
  ![Growth curves](Gallery/screenshots/plots/04_plot_curves.png)
- Correlation heatmap:  
  ![Correlation heatmap](Gallery/screenshots/plots/06_plot_correlation.png)

## Troubleshooting
- `Rscript` not found: use the full path to `Rscript` or add R to PATH.
- Missing packages: check Internet access and review `bioszen_r.log` in the folder.
- macOS Gatekeeper: `xattr -dr com.apple.quarantine <folder>`.
- Port 4321 in use: edit `App.R` and change `shiny.port`.

## How to cite
Szenfeld, B. (2026). BIOSZEN (1.0.0). Zenodo. https://doi.org/10.5281/zenodo.18217522

## License
GPL-3.0.
