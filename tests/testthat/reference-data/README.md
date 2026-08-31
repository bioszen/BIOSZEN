# BIOSZEN statistical reference data

These deterministic CSV fixtures use the same core columns as a BIOSZEN
`Datos` worksheet. They are intentionally small so numerical failures are easy
to diagnose on Windows, Apple silicon macOS, and Intel macOS.

| File | Purpose |
| --- | --- |
| `normal.csv` | Approximately normal observations for Shapiro-Wilk, Kolmogorov-Smirnov, and Anderson-Darling parity checks. |
| `non_normal.csv` | Strongly right-skewed observations for normality-decision checks. |
| `two_groups.csv` | Two explicitly matched groups for pairwise parametric checks. |
| `three_groups.csv` | Three groups for ANOVA, Kruskal-Wallis, post-hoc, adjusted-p, panel, plot, and workbook checks. |
| `paired.csv` | Matched biological replicates with a known directional shift. |
| `with_na.csv` | Missing-value handling. |
| `identical_values.csv` | Constant-data guards. |
| `extreme_values.csv` | Large but finite values for numerical-stability checks. |
| `unequal_n.csv` | Unequal group sizes that must not be treated as paired. |
| `ties.csv` | Tied observations for non-parametric checks. |
| `nist_sirstv.csv` | NIST StRD SiRstv one-way ANOVA reference dataset. |
| `curves_reference.csv` | Replicate curves for pointwise, endpoint, and AUC checks. |

`nist_sirstv.csv` reproduces the values published by the US National Institute
of Standards and Technology Statistical Reference Datasets project:
https://www.itl.nist.gov/div898/strd/anova/SiRstv.html

Only the SiRstv ANOVA assertions are described as NIST-certified validation.
All other numerical assertions are parity checks against the R functions and
packages used by BIOSZEN.
