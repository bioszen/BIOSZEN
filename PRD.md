# BIOSZEN - Product Requirements Document (PRD)

## 1. Introduction & Overview
**BIOSZEN** is a specialized data analysis and visualization tool built using **R** and the **Shiny** framework. It allows users—primarily researchers and biologists—to upload data from Excel files and perform complex statistical analyses and visualizations without needing to write code.

It is designed to be a "code-free" solution for:
1.  **Statistical Analysis:** Running normality tests (Shapiro-Wilk, etc.) and significance tests (ANOVA, t-test, etc.).
2.  **Visualization:** Creating publication-ready charts like boxplots, violin plots, and correlation heatmaps.
3.  **Biological Modeling:** Specifically tailored for extracting microbial growth parameters (e.g., doubling time, lag phase) from experimental data.

## 2. Product Details
*   **Product Name:** BIOSZEN
*   **Version:** 1.0.0
*   **Status:** Active / In Development

## 3. Problem Statement
Researchers often struggle with analyzing biological data (such as microbial growth curves) and performing rigorous statistical tests because existing tools are either too complex (requiring programming skills in R/Python) or too manual (Excel). There is a need for a user-friendly, reproducible, and portable tool that automates these workflows.

## 4. Target Audience
*   **Primary:** Microbiologists and biotech researchers working with growth kinetics and experimental comparisons.
*   **Secondary:** Students and data analysts in life sciences who need quick visualization and statistical validation.

## 5. Key Objectives
*   **Accessibility:** Provide a GUI (Graphical User Interface) for R’s powerful statistical libraries.
*   **Portability:** Ensure the app can run on Windows/macOS with minimal setup (bundled dependencies).
*   **Reproducibility:** Standardize the method of calculating growth parameters ($µ_{max}$, lag time) to reduce human error.

## 6. Functional Requirements

### 6.1 Data Input
*   **File Format:** Must support `.xlsx` (Excel) input.
*   **Structure:** Flexible reading of data columns for grouping and variables.

### 6.2 Visualization Module
The system must generate the following interactive/static plots:
*   Boxplots
*   Bar plots
*   Violin plots
*   Stacked plots
*   Curve plots (Growth curves)
*   Correlation plots (Heatmaps)

### 6.3 Statistical Analysis Module
*   **Normality Tests:** Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling.
*   **Significance Tests:**
    *   **Parametric:** ANOVA, Student’s t-test.
    *   **Non-Parametric:** Kruskal-Wallis, Wilcoxon test.
*   **Post-hoc Tests:** Dunn’s test, and others as provided by `PMCMRplus`.

### 6.4 Growth Parameter Extraction Module
Must utilize the `gcplyr` package to calculate:
*   `uMax` (Maximum specific growth rate)
*   `doub_time` (Doubling time)
*   `lag_time` (Lag phase duration)
*   `ODmax` (Maximum Optical Density)
*   `AUC` (Area Under the Curve)

### 6.5 System & Architecture
*   **Platform:** Built on R/Shiny.
*   **Dependency Management:** The app must allow for local library installation (`R_libs` folder) to enable offline execution after the first run.
*   **Modularity:** Code structure should be modular (separated into `ui`, `server`, `stats`, `graficos`, etc.) for easy maintenance.

## 7. Technical Stack
*   **Core Language:** R (>= 4.1.0)
*   **Framework:** Shiny
*   **Key Libraries:**
    *   *UI/UX:* `shiny`, `bslib`, `shinyjs`, `shiny.i18n`
    *   *Data:* `dplyr`, `readxl`, `tidyr`
    *   *Stats:* `rstatix`, `DescTools`, `gcplyr` (Growth Curve), `nortest`
    *   *Plots:* `ggplot2`, `plotly`, `RColorBrewer`, `patchwork`

## 8. Future Roadmap
*   **Report Generation:** Automated PDF/Word reporting of analysis results.
*   **Multi-language Support:** Expansion of `shiny.i18n` implementation for broader accessibility.
