# ClinicoPath: Comprehensive Analysis for Clinicopathological Research

ClinicoPath is a comprehensive jamovi module designed specifically for
clinicopathological research and medical data analysis. It provides an
integrated suite of statistical tools, visualization methods, and
reporting functions tailored for pathology, clinical research, and
medical decision-making.

## Details

### Main Analysis Categories

ClinicoPath organizes analysis tools into five main functional areas:

#### ClinicoPath Descriptives

Comprehensive descriptive statistics and data exploration tools:

- Table One generation with automated statistical tests

- Cross-tabulation with chi-square and Fisher's exact tests

- Data quality assessment and missing data analysis

- Outlier detection and data validation

- Summary statistics with clinical interpretation

#### ClinicoPath Survival

Advanced survival analysis for time-to-event data:

- Kaplan-Meier survival curves with risk tables

- Cox proportional hazards regression

- Competing risks analysis

- Log-rank and other survival tests

- Survival power calculations

- Swimmer plots for treatment timelines

#### Medical Decision Analysis (meddecide)

Diagnostic test evaluation and clinical decision support:

- ROC curve analysis with confidence intervals

- Sensitivity, specificity, and predictive values

- Likelihood ratios and diagnostic odds ratios

- Decision curve analysis for clinical utility

- Fagan nomograms for Bayesian diagnosis

- Net reclassification improvement (NRI) and IDI

#### Statistical Plots (JJStatsPlot)

Advanced statistical visualization with publication-ready graphics:

- Between-groups and within-subjects comparisons

- Correlation matrices and scatter plots

- Distribution plots with statistical annotations

- Effect size visualizations

- Grouped and stratified analyses

#### Specialized Visualizations

Domain-specific plots for clinical research:

- Waterfall plots for treatment response

- Alluvial diagrams for patient flow

- Forest plots for meta-analysis

- Upset plots for set intersections

- Age pyramids and demographic visualizations

### Key Features

#### Integration with jamovi

- User-friendly graphical interface

- Point-and-click analysis workflow

- Integrated results viewer

- Export capabilities for reports

#### Clinical Focus

- Terminology and methods familiar to clinicians

- Pathology-specific analysis options

- Medical decision-making frameworks

- Regulatory-compliant reporting

#### Reproducible Research

- Syntax generation for R users

- Version control integration

- Comprehensive documentation

- Example datasets included

#### Advanced Statistics

- Bootstrap confidence intervals

- Multiple comparison corrections

- Effect size calculations

- Power analysis tools

### Getting Started

ClinicoPath can be used in multiple ways:

1.  **jamovi Module**: Install through jamovi's module library

2.  **R Package**: Install and use functions directly in R

3.  **Standalone Functions**: Individual analysis components

### Example Datasets

The package includes several example datasets:

- `histopathology`: Histopathological diagnosis data

- `melanoma`: Melanoma survival data

- `treatmentResponse`: Treatment response analysis

- Various domain-specific test datasets

### Package Architecture

ClinicoPath serves as an umbrella package that coordinates multiple
specialized sub-modules:

- **ClinicoPathDescriptives**: Descriptive statistics

- **jsurvival**: Survival analysis

- **meddecide**: Medical decision analysis

- **jjstatsplot**: Statistical visualizations

Each module can be used independently or as part of the integrated
workflow.

## Author

Serdar Balci MD, Pathologist

- Email: <serdarbalci@serdarbalci.com>

- ORCID: [0000-0002-7852-3851](https://orcid.org/0000-0002-7852-3851)

- Website: <https://www.serdarbalci.com/>

## Citation

To cite ClinicoPath in publications, please use:

Balci, S. (2025). ClinicoPath: Comprehensive Analysis for
Clinicopathological Research. R package version 0.0.3.58.
<https://github.com/sbalci/ClinicoPathJamoviModule>

## License

GPL-2

## Links

- Package website: <https://sbalci.github.io/ClinicoPathJamoviModule/>

- GitHub repository:
  <https://github.com/sbalci/ClinicoPathJamoviModule/>

- Bug reports:
  <https://github.com/sbalci/ClinicoPathJamoviModule/issues/>

- jamovi library: <https://www.jamovi.org/>

## See also

- [`Surv`](https://rdrr.io/pkg/survival/man/Surv.html) for survival
  analysis fundamentals

- [`pROC-package`](https://rdrr.io/pkg/pROC/man/pROC-package.html) for
  ROC analysis

- [`tableone`](https://rdrr.io/pkg/tableone/man/tableone-package.html)
  for descriptive tables

- [`ggplot2-package`](https://ggplot2.tidyverse.org/reference/ggplot2-package.html)
  for visualization

## Author

**Maintainer**: Serdar Balci <serdarbalci@serdarbalci.com>
([ORCID](https://orcid.org/0000-0002-7852-3851))

Authors:

- Serdar Balci <serdarbalci@serdarbalci.com>
  ([ORCID](https://orcid.org/0000-0002-7852-3851))

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the package
library(ClinicoPath)

# Example 1: Descriptive analysis
data(histopathology)
summary(histopathology)

# Example 2: Survival analysis (requires jamovi interface)
# Use jamovi GUI for interactive analysis

# Example 3: ROC analysis
# See meddecide module documentation for examples

# Example 4: Statistical plots
# See jjstatsplot module documentation for examples
} # }
```
