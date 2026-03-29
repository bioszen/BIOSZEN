# BIOSZEN

BIOSZEN is an R/Shiny application for biological data analysis and visualization from Excel workbooks.

It supports complete workflows from data loading and filtering to normalization, statistics, significance annotations, and reproducible exports.

## Core Capabilities

- Plot types: **Boxplot**, **Barplot**, **Violin**, **Curves**, **Stacked**, **Correlation**, **Heatmap**, **Correlation Matrix**.
- Statistical workflows: normality and significance with multiple comparison modes and p-value corrections.
- Control-based normalization with replicate-aware logic.
- Replicate management with manual and QC-assisted filtering.
- Metadata import/export to restore analysis state.
- Bundle ZIP exports for reproducibility.
- Growth module for extracting `uMax`, `doub_time`, `lag_time`, `AUC`, and related metrics.

## What's New in 2.0.0

- Improved growth workflow stability and cancellation behavior.
- Expanded tests for growth, replicate selection, and exports.
- Better consistency in filtering and repeated execution behavior.

## Requirements

- R >= 4.1
- Internet access for first-time package installation

## Quick Start

1. Install R (and optionally RStudio).
2. Download and unzip the release bundle.
3. Keep `App.R` and `BIOSZEN-vX.Y.Z.tar.gz` in the same folder.
4. Run:
   - RStudio: open `App.R` and click **Source**
   - Terminal/PowerShell: `Rscript App.R`
5. Open the app at the local URL shown in the console.

## First Run Notes

On first launch, BIOSZEN installs dependencies into a local `R_libs` folder. Keep this folder to avoid reinstalling packages.

## Input Templates

Reference input files are available in the app and in:

- `inst/app/www/reference_files/`

Templates include:

- `Ejemplo_platemap_parametros.xlsx`
- `Ejemplo_curvas.xlsx`
- `Ejemplo_parametros_agrupados.xlsx`
- `Ejemplo_input_summary_mean_sd.xlsx`

## Documentation

- English manual: `inst/app/www/MANUAL_EN.md`
- Spanish manual: `inst/app/www/MANUAL_ES.md`
- PDF manuals:
  - `inst/app/www/MANUAL_EN.pdf`
  - `inst/app/www/MANUAL_ES.pdf`

Both manuals cover:

- Input formats (platemap, grouped, summary)
- Full analysis workflow
- Plot-specific controls
- Statistics and significance workflows
- Heatmap and correlation matrix usage
- Growth module
- Metadata and bundle reproducibility
- Troubleshooting

## Troubleshooting

- `Rscript` not found: use full path to Rscript or add R to PATH.
- Missing packages: check internet access and review logs.
- Port in use: edit launcher port settings.
- Invalid upload: verify required sheet names and column names.

## Citation

Szenfeld, B. (2026). BIOSZEN (2.0.0). Zenodo. https://doi.org/10.5281/zenodo.18217522

## License

GPL-3.0
