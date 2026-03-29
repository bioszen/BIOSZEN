# BIOSZEN MANUAL

With a single Excel file you will have flexible visualizations and
analyses: filter, normalize, and remove data without creating additional
files. You can also plot multiple parameters at once and relate them;
the app handles it for you.

## 1. Prepare your input files

### Download template

Click “Reference input files (download)” to obtain a ZIP with examples
of a platemap, grouped parameters, and pre-formatted curves.

### 1.1 Platemap-parameters file structure (.xlsx)

#### “Data” sheet

Required columns:

**Well**: well position (e.g., A1, B3). Required for the Curves plot: it
links each curve to its original sample.

**Strain**: strain or group name. This column can also be titled
“Strain”/“Sample” in your language (e.g., “Cepa” or “Muestra”).

**Media**: condition or treatment (e.g., Control, Treatment A). This
column can also be titled “Condition” or “Treatment” (e.g., “Condición”
or “Tratamiento”).

**BiologicalReplicate**: biological replicate (1, 2, 3, etc.).

**TechnicalReplicate**: technical replicate (A, B, etc.). If it does not
exist, leave it blank or set it to “A”.

One column per parameter you want to plot (e.g., Viability,
Fluorescence).

**Order**: number that defines the order of media to display and export.
It is recommended to define it so that per-strain and combined plots
remain consistent.

#### “PlotSettings” sheet

Each row corresponds to a parameter present in “Data”.

Required columns:

**Parameter**: exact name of the column in “Data”.

**Y_Max**: initial upper limit of the Y axis (number).

**Interval**: spacing between Y-axis ticks (number).

**Y_Title**: Y-axis label (text).

### 1.2 Grouped data structure (.xlsx)

This alternative to the platemap allows you to plot and analyze grouped
parameters without specifying wells or technical replicates.

The Excel file must contain one tab (sheet) per parameter you want to
display. Each sheet must be named exactly as the corresponding
parameter.

In each tab, the table must include at least these columns:

**Strain**: strain or group name. This column can also be titled “Cepa”
or “Muestra”.

**BiologicalReplicate**: biological replicate (1, 2, 3, etc.).

**Media**: condition or treatment associated with the strain (if
applicable). To order groups in the plot, the app uses the row order
within each sheet.

**Values**: parameter values for each strain, condition, and replicate.

The order shown in the generated plots follows the order in which
“Strain - Media” combinations appear within each sheet.

With this simplified format, you can load summary data per strain and
replicate directly, without a full platemap.

| Strain  | BioRep | Ampicillin 1 µM | Ampicillin 2 µM | Ampicillin 5 µM | Control    |
|---------|--------|-----------------|-----------------|-----------------|------------|
| Strain1 | 1      | 0.55            | 0.46            | 0.52666667      | 0.63666667 |
| Strain1 | 2      | 0.80333333      | 0.49333333      | 0.65333333      | 0.6        |
| Strain2 | 1      | 0.64333333      | 0.36            | 0.45            | 0.53666667 |
| Strain2 | 2      | 0.55666667      | 0.47            | 0.64            | 0.91       |

### 1.3 Curves file structure (.xlsx)

If you will use the Curves plot type, you need a file with two sheets:

Sheet1 - Raw curve data

First column: Time (or the X-axis parameter).

Next columns: values for each curve, named according to the originating
Well (e.g., A1, B3). The Well must match the “Well” column in the main
file.

Sheet2 - Axis settings

Required columns:

**X_Max**: initial maximum value of the X axis.

**Interval_X**: spacing between X-axis ticks.

**Y_Max**: initial maximum value of the Y axis.

**Interval_Y**: spacing between Y-axis ticks.

**X_Title**: X-axis label.

**Y_Title**: Y-axis label.

## 2. App interface

Follow these steps in order:

**Load Data (.xlsx)**: select your platemap Excel or grouped-parameters
file, using the structure described above.

**Load Curves (.xlsx)**: select your file with Sheet1 and Sheet2
(optional).

**Load design metadata (.xlsx)**: load one or more files with design
specifications (dimensions, font sizes, titles, angles, etc.) to apply a
predefined style. Design files are saved by plot type and applied
automatically when you switch plots. You can download them from
“Download Design Metadata”.

**Instructions (download)**: download this manual.

**Reference input files (download)**: download a ZIP with examples of
platemap, grouped parameters, and curves.

**Scope**: choose Per Strain or Combined.

**Strain**: dropdown menu with available strains (only in Per Strain).

**Plot**: visualization type (Boxplot, Bars, Violin, Curves, Stacked,
Correlation).

Specific settings are shown depending on the selected plot type:

**Boxplot:** box width and point jitter.

**Violin**: outline thickness and violin width.

**Stacked**: parameters, order, error bars, outline only on the total
bar, and error-bar color by parameter.

**Correlation**: X and Y axes, method, regression line, equation, R²,
and labels.

**Curves**: limits, intervals, axis labels, and curve thickness.

**Normalization**: enable “Normalize to a control” and select the
normalizing Media.

**Filters**: depending on scope, adjust media, groups, and replicates.

Settings and style: scale, titles, base size, font sizes, axis line
thickness, error-bar thickness, X label angle, and X label wrapping
(number of lines), plus point size and jitter.

**Parameter to plot and title**: choose the variable, order, Y-axis
label, and a custom title.

**Data table**: shown below the plot with values and means according to
active filters; not available for Curves.

**Action bar below the plot**: optional caption, copy plot to clipboard,
save version, and add to panel.

**Downloads**: PNG, Data, Design metadata, and Statistical results.
Downloaded design metadata contains all plot or panel specifications
(dimensions, font sizes, titles, angles, etc.) and can be uploaded later
through “Load design metadata” to reuse the style.

## 3. Data processing

Each biological replicate is computed as the mean of its corresponding
technical replicates. The app groups the data by Strain, Media, and
BiologicalReplicate.

## 4. Scope and group selection

### Scope

**Per Strain**: one plot at a time for a single strain (or sample, as
applicable).

Combined: all “Strain - Media” groups in the same plot.

### Per Strain

**Strain**: menu with detected strains.

**Filter Media**:

Select/Deselect all.

Checkboxes to include or exclude each media.

**Replicates**: choose which biological replicates to show (per media,
if needed).

Order (csv): order of media on the X axis (comma-separated).

### Combined

**Filter Groups**: “Strain - Media” checkboxes.

Show only the strain in the labels.

**Replicates**: filter replicates per group (per combination).

**Order (csv)**: manual group order.

**Exclude replicate(s) across all groups**: removes biological
replicates globally from plots and table.

## 5. Choose plot type and colors

**Plot:** Boxplot \| Bars \| Violin \| Curves (requires a curves file)
\| Stacked \| Correlation.

**Color palette:** choose among default palettes, uniform monochromatic
palettes for continuous data, and qualitative palettes for groups.

**Repeat colors by strain (Combined):** reuse the same set of colors for
each strain.

**Advanced palette control:** choose sequential/diverging/qualitative
palettes, filter by colorblind/print/photocopy, reverse order, and use
extended schemes beyond the default list. Customize colors by group:
select one or more groups and assign a specific color.

## 6. Normalize data to a control

**Enable normalization**: check “Normalize to a control”.

The app divides each biological replicate value by the value of the same
replicate in the selected control media (replicate 1 by replicate 1,
replicate 2 by replicate 2, etc.).

**Normalizing media**: select the group (e.g., “Control”) that will
equal 1 for each biological replicate.

**Per-strain normalization**: each strain is normalized to its own
control, even in Combined scope.

**Affected plots**:

Boxplot, Bars, and Correlation: display the normalized version.

Statistical tests: you can run them on normalized data. Beforehand,
deselect the normalizing media to avoid constant values.

## 7. Stacked plots

In a stacked bar chart, you group multiple parameters into a single
column per “strain-condition”.

**Included parameters**: select which parameters you want to stack.

**Parameter order**: define bottom-to-top order with a comma-separated
list.

**Scope**: identical to Boxplot/Bars (Per Strain or Combined).

**Normalization**: available as in other plots.

**Deviation bars**: enable or disable standard deviation for each
segment; optionally color by parameter.

**Black outline only on the total bar**: draw the outline only around
the bar total.

**Interactivity:** each parameter remains available separately in
Boxplot or Bars; statistical tests compare parameters.

## 8. Scale and titles

**Y max**: upper limit of the Y axis (0 = value from PlotSettings or
normalization).

**Y interval**: spacing between Y-axis ticks.

**Plot title**: if left empty, the app generates a default title.

**X-axis label angle**: adjust rotation (e.g., 0°, 45°, 90°) and
optionally wrap into multiple lines (choose number of lines) to improve
readability when group names are long or numerous.

## 9. Image size and style

Width px / Height px (used for downloads and clipboard).

Base size plus title, axis, and legend font sizes.

Axis line thickness and error-bar thickness.

Point size (Boxplot/Bars/Violin/Stacked); jitter and box width in
Boxplot; outline thickness and violin width.

Curves: line thickness.

## 10. Statistical analyses

In Select plots -\> Statistical analyses you will find two tabs.

### 10.1 Normality

**Shapiro-Wilk** (stats::shapiro.test).

**Kolmogorov-Smirnov** (stats::ks.test).

**Anderson-Darling** (nortest::ad.test).

Click Run Normality to obtain the table with p-values and “Yes/No” (p \>
0.05). Note: with normalized data, deselect the normalizing media before
running normality; if a group has no variation, the test will error.

### 10.2 Significance

Global test:

**ANOVA** (stats::aov).

**Kruskal-Wallis** (stats::kruskal.test).

**Independent t-test** (rstatix::t_test).

**Independent Wilcoxon** (rstatix::wilcox_test).

**Posthoc** (depending on the test):

Tukey (stats::TukeyHSD).

Bonferroni, Sidak (rstatix::pairwise_t_test).

Dunnett (DescTools::DunnettTest).

Scheffe, Conover, Nemenyi, DSCF (PMCMRplus).

Games-Howell (rstatix::games_howell_test).

**Modes**:

All vs All.

Control vs All.

Paired.

These options are available in the Statistical analyses tab of the
interface.

### 10.3 Dynamic statistical analysis

Tests are always performed on the parameter selected in Boxplot/Bars.

**Per Strain**: compares the active Media.

**Combined**: compares “Strain - Media” combinations.

If you change filters or replicates, click Run Normality or Significance
again to repeat the analysis.

## 11. Significance bars or labels

**Mode**: Bars (comparison) or Labels over group.

Choose Group 1 and Group 2 (in label mode, Group 2 is the group that
will receive the label).

In Stacked + labels, select the parameter and optionally color the label
by parameter.

Type the Label (e.g., \*, \*\*, n.s.) and click Add bar/label. Bars
stack without deleting previous ones.

Use the added-bars list to select, delete, or change order (Up/Down);
Clear all.

Adjust line thickness, distance to the first bar, spacing between bars,
label spacing, and label size; optionally remove vertical lines.

## 12. Correlation plot

This section explores the relationship between two parameters while
keeping the same filtering and customization features.

**Axis selection**: freely choose X and Y parameters, raw or normalized.

**Axis labels**: customize X and Y.

**Normalized correlations**: if you enable normalization, you can apply
it to X, Y, or both, and use normalized values to compute correlations
between parameters.

**Correlation method**: Pearson or Spearman. You can show the
coefficient (r), p-value, and R² on the plot.

**Regression line**: optional linear line (dashed).

**Line equation**: optional.

**Point labels**: configurable; ggrepel avoids overlap.

**Label size**: adjustable.

**Per Strain or Combined scope**: with the same filters as other plots.

**Axis limits and intervals**: define minimums, maximums, and intervals.

**Custom title**: default is “Correlation Y vs X”, editable.

With this module, cross-parameter relationship analysis becomes much
more powerful, letting you identify patterns and associations in a
single step.

## 13. Composition panel

Plots can be added to the panel using the Add to panel button below the
plot area. The Composition panel tab appears after the first addition.
From this panel you can:

Select plots to combine and reorder them (Up/Down).

Show or hide the legend.

Set the number of rows and columns in the mosaic, plus width and height
in pixels.

Adjust base size and text sizes (title, axis titles, ticks, and legend)
and choose the palette (Original or overridden).

Use Overrides (apply to all or a specific plot type) to change titles,
font sizes, axis line thickness, curve thickness/point size in curves,
box width, error-bar thickness, point size/jitter, and significance-text
size/thickness.

Adjust dimensions and proportions of all added plots together. You can
modify aspect ratio per panel or globally so all figures look good on
the same canvas.

Preview the result and download as PNG, PPTX, or PDF; download/upload
composition metadata to reuse settings.

## 14. Download results

**Download PNG or PDF (300 dpi).**

**Download Data:** detailed and summary data (not available when using
grouped parameters).

**Download Design Metadata**: get an Excel file with all design
specifications of the current plot or panel (dimensions, font sizes,
titles, angles, etc.) to reuse in future projects.

**Statistical results**: tests performed for all parameters.

**Download package (ZIP):** enabled after saving versions; includes
datasets (combined/grouped CSVs and the parameters workbook), saved
plots (PNG/PDF), metadata, statistics, and INFO files.

## 15. Growth Rates

In the Growth Rates tab you can compute and download growth parameters
from Excel files containing curves of interest. The interface includes a
field to upload the curve file, checkboxes to set maximum time and
sampling interval, and buttons to calculate and download results. The
figure below shows this section of the app:

### Input files

Load growth curves (.xlsx): select one or more Excel files. Each file
must contain a sheet with raw curve data (Time column and well values
named by Well).

Example file structure (designed for files generated directly by a Tecan
instrument):

| Well positions | Raw data |       |       |       |       |
|----------------|----------|-------|-------|-------|-------|
|                |          | A1    | A2    | A3    | A4    |
| 0              | 37.0°C   | 0.149 | 0.148 | 0.152 | 0.143 |
| 1800           | 37.0°C   | 0.147 | 0.145 | 0.147 | 0.141 |
| 3600           | 37.0°C   | 0.144 | 0.143 | 0.144 | 0.139 |
| 5400           | 37.0°C   | 0.142 | 0.141 | 0.143 | 0.138 |
| ...            | ...      | ...   | ...   | ...   | ...   |

If your file is not a curve file exported from Tecan, adjust it to this
format. The content of the first two columns does not matter; the app
reads from the third column onward (first well). Times are added by the
app when you set the sampling interval and maximum measurement time to
consider.

Note: make sure curves were measured at least up to the maximum time you
specify (e.g., 48). If measurements do not reach that time, parameter
calculation may fail or return incomplete values.

### Required parameters

**Maximum time**: time limit to consider (e.g., 48).

**Interval**: spacing between sampling points (e.g., 0.5). It must use
the same unit as Maximum time.

### Buttons

**Calculate parameters**: starts processing each curve. Once finished,
you get:

**µMax:** maximum growth rate.

**max_percap_time**: average time in exponential phase.

**doub_time**: doubling time (log(2)/µMax).

**lag_time**: lag time.

**ODmax:** maximum OD reached.

**max_time**: time at which ODmax is reached.

**AUC**: area under the curve.

**Download results**: downloads a ZIP including, for each input file:

**Curves\_\<name\>.xlsx**: curve data and axis configuration.

**Parameters\_\<name\>.xlsx**: table with all calculated parameters.

**Import into Plots and Stats**: (only if a single file was loaded)
clicking this button imports the generated parameters into the Plots and
Stats module, allowing you to plot them together with the loaded
platemap. Important: ensure the platemap loaded in Plots and Stats
includes, in its PlotSettings sheet, the names of the parameters you
want to plot from Growth Rates; otherwise, they will not appear in the
plots.

### Preview

The growthTable displays all computed parameters for review before
downloading.

### Usage tips

To speed up computation, delete columns for empty wells from the curves
file beforehand. If you do this, make sure to synchronize your platemap
in Plots and Stats to avoid mismatches.

Always keep the platemap file loaded in Plots and Stats, since the
import function requires correspondence between wells and growth data.

### Recommended workflow

Upload your main file and/or curves file.

Explore the interactive plot.

Adjust scale, colors, and titles.

Filter groups and replicates.

(Optional) Normalize data.

Run Normality and Significance if needed.

Download the image or the ZIP with everything.

### Unified file format for multiple plates

Platemap-parameters file:

Add new plates as additional rows (below), without repeating column
headers.

Keep consecutive numbering (e.g., continuing from H13) to ensure
correspondence between files.

Curves file:

Concatenate new curves to the right of existing ones; this applies both
to curves ready to plot and to curves files used to compute growth
parameters.

Do not repeat column headers. If the first plate ends at H12, you can
create headers H13, H14, etc.

Add as many plates as needed, respecting the experimental design.

Example: plate 1 with replicates 1 and 2; plate 2 with replicates 3 and
4. This assignment must be reflected in both files to combine all
measurements correctly.

### Contact information

For comments, suggestions, or support, write to bioszenf@gmail.com.

© 2025 BioSzen - All rights reserved
