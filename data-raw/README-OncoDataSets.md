# OncoDataSets Integration in ClinicoPath

This document describes the integration of oncology datasets from the [OncoDataSets](https://cran.r-project.org/package=OncoDataSets) package into ClinicoPath.

## Overview

We have imported **55 comprehensive oncology datasets** covering various cancer types, study designs, and analytical scenarios. These datasets provide real-world examples for all ClinicoPath submodules and enable users to learn with authentic clinical data.

## Dataset Categories

### 1. Survival Analysis (5 datasets)
Perfect for **jsurvival** module:
- `Melanoma_df` - Melanoma survival with prognostic factors
- `LeukemiaSurvival_df` - Treatment comparison studies  
- `ProstateSurvival_df` - Large cohort with stratification factors
- `NCCTGLungCancer_df` - Clinical trial data with missing values
- `OvarianCancer_df` - Small sample survival analysis

### 2. Diagnostic/Decision Analysis (3 datasets)
Ideal for **meddecide** module:
- `PSAProstateCancer_df` - ROC analysis and biomarker evaluation
- `CA19PancreaticCancer_df` - Meta-analysis diagnostic accuracy data
- `LungNodulesDetected_df` - Decision tree and clinical rule development

### 3. Descriptive/Comparative Analysis (6 datasets)
Perfect for **ClinicoPathDescriptives** module:
- `BreastCancerWI_df` - Wisconsin Breast Cancer (classification)
- `ChildCancer_df` - Pediatric epidemiology
- `BladderCancer_df` - Clinical characteristics
- `SmokingLungCancer_df` - Risk factor analysis
- `BrainCancerCases_df` - Neurological oncology
- `BrainCancerGeo_df` - Geographic patterns

### 4. Visualization Data (4 datasets)
Great for **jjstatsplot** module:
- `ColonCancerChemo_df` - Treatment response visualization
- `EndometrialCancer_df` - Gynecologic oncology plots
- `HeadNeckCarcinoma_df` - Multi-group comparisons
- `SkinCancerChemo_df` - Before/after treatment plots

### 5. Additional Categories (37 datasets)
- **Biomarker Analysis** (5): BRCA1/2, CASP8 genetic variants
- **Epidemiological** (4): Population statistics, mortality trends
- **Molecular/Genomic** (4): MicroRNA, genomic consortiums
- **Experimental** (3): Animal models, radiation studies
- **Case-Control Studies** (4): Matched designs
- **Risk Factors** (3): Environmental exposures
- **Clinical Outcomes** (5): Treatment responses, methylation
- **Specialized Studies** (9): AI applications, international studies

## Usage

### In R/RStudio
```r
# Load any dataset
data("Melanoma_df")
str(Melanoma_df)

# View all available datasets
data("oncology_datasets_summary")
View(oncology_datasets_summary)

# Find datasets by category
survival_data <- subset(oncology_datasets_summary, 
                       category == "Survival Analysis")
```

### In jamovi
1. These datasets are automatically available in all ClinicoPath modules
2. Use `File > Data > Examples` to browse available datasets
3. Each dataset includes proper documentation and variable labels

## Vignette Examples

Comprehensive vignettes have been created for each submodule:

1. **`clinicopath-descriptives-01-oncodatasets-examples.Rmd`**
   - Table One analysis with breast cancer data
   - Cross-tabulations with chi-square post-hoc tests
   - Data quality assessment workflows

2. **`jsurvival-01-oncodatasets-examples.Rmd`**
   - Kaplan-Meier analysis by patient characteristics
   - Cox proportional hazards modeling
   - Competing risks analysis
   - Clinical trial analysis workflows

3. **`meddecide-01-oncodatasets-examples.Rmd`**
   - ROC analysis for diagnostic biomarkers
   - Decision curve analysis for clinical utility
   - Meta-analysis of diagnostic accuracy
   - Decision tree modeling for clinical decisions

4. **`jjstatsplot-01-oncodatasets-examples.Rmd`**
   - Statistical visualization with cancer data
   - Publication-ready plots with integrated statistics
   - Multi-group comparisons and effect sizes

## Data Processing

### Import Process
- All datasets imported using `data-raw/generate_oncodatasets.R`
- Data saved in compressed `.rda` format for efficiency
- Comprehensive documentation created for each dataset
- Summary metadata available in `oncology_datasets_summary`

### Quality Assurance
- All 55 datasets successfully imported
- Variable types and structures preserved
- Missing data patterns documented
- Dataset sizes range from 22 to 14,294 observations

## Clinical Relevance

These datasets represent real clinical scenarios:

- **Multiple cancer types**: Melanoma, breast, lung, prostate, ovarian, etc.
- **Various study designs**: RCTs, cohort studies, case-control, meta-analyses
- **Different data structures**: Time-to-event, diagnostic accuracy, genomic
- **Clinical decision points**: Screening, diagnosis, prognosis, treatment

## Educational Value

- **Authentic data**: Real patient outcomes and clinical measurements
- **Statistical diversity**: Different distributions, sample sizes, complexity
- **Analysis variety**: Descriptive, comparative, predictive, diagnostic
- **Publication examples**: Data suitable for manuscript preparation

## Integration Benefits

1. **Seamless workflow**: Datasets work immediately with all ClinicoPath tools
2. **Learning continuity**: Same datasets used across multiple analysis types
3. **Clinical context**: Real-world scenarios enhance learning
4. **Reproducible examples**: Standardized datasets for training and documentation

## Technical Details

- **Package dependency**: OncoDataSets added to `Suggests` in DESCRIPTION
- **File storage**: All datasets in `data/` directory as `.rda` files
- **Documentation**: Full roxygen2 documentation in `R/oncology-datasets.R`
- **Size**: Approximately 55 additional datasets (various sizes)
- **Compression**: `bzip2` compression for optimal storage

## Future Enhancements

- Additional specialized datasets as OncoDataSets updates
- Custom preprocessing scripts for specific analysis needs
- Interactive dataset browser in jamovi
- Automated data quality reports
- Integration with external oncology databases

## References

Caceres Rossi, R. (2024). OncoDataSets: A Rich Collection of Data Focused on Cancer Research. R package version 0.1.0. https://CRAN.R-project.org/package=OncoDataSets

## Usage Examples in Research

### Survival Analysis Project
```r
# Complete survival analysis workflow
data("Melanoma_df")
# 1. Descriptive analysis → ClinicoPathDescriptives
# 2. Survival curves → jsurvival  
# 3. Visualizations → jjstatsplot
# 4. Clinical decisions → meddecide
```

### Diagnostic Study
```r
# ROC analysis and clinical utility
data("PSAProstateCancer_df")
# 1. Data summary → ClinicoPathDescriptives
# 2. ROC curves → meddecide
# 3. Decision analysis → meddecide
# 4. Publication plots → jjstatsplot
```

This integration makes ClinicoPath a comprehensive platform for learning and conducting oncology data analysis with real-world clinical datasets.