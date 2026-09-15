# Oncology Datasets from OncoDataSets Package

A collection of 55 oncology datasets imported from the OncoDataSets
package for use in ClinicoPath analyses. These datasets cover various
cancer types, study designs, and analysis scenarios commonly encountered
in clinical pathology and oncology research.

## Details

The datasets are organized into several categories:

**Survival Analysis (5 datasets):**

- `Melanoma_df`: Melanoma patient survival with tumor characteristics

- `LeukemiaSurvival_df`: Leukemia survival with treatment information

- `ProstateSurvival_df`: Prostate cancer survival by grade, stage, age

- `NCCTGLungCancer_df`: NCCTG lung cancer trial data

- `OvarianCancer_df`: Ovarian cancer trial survival data

**Diagnostic/Decision Analysis (3 datasets):**

- `PSAProstateCancer_df`: PSA levels and prostate cancer outcomes

- `CA19PancreaticCancer_df`: CA19-9 diagnostic accuracy studies

- `LungNodulesDetected_df`: Lung nodule characteristics and malignancy

**Descriptive/Comparative Analysis (6 datasets):**

- `BreastCancerWI_df`: Wisconsin Breast Cancer diagnostic features

- `ChildCancer_df`: Childhood cancer epidemiological data

- `BladderCancer_df`: Bladder cancer patient characteristics

- `SmokingLungCancer_df`: Smoking status and lung cancer relationship

- `BrainCancerCases_df`: Brain cancer case characteristics

- `BrainCancerGeo_df`: Brain cancer geographic distribution

**Biomarker Analysis (5 datasets):**

- `BRCA1BreastCancer_df`: BRCA1 mutations in breast cancer

- `BRCA2BreastCancer_df`: BRCA2 mutations in breast cancer

- `BRCA1OvarianCancer_df`: BRCA1 mutations in ovarian cancer

- `BRCA2OvarianCancer_df`: BRCA2 mutations in ovarian cancer

- `CASP8BreastCancer_df`: CASP8 gene variants in breast cancer

**Additional categories include:** Treatment Outcomes, Epidemiological,
Molecular/Genomic, Experimental, Case-Control Studies, Risk Factors,
Clinical Outcomes, and Specialized Studies.

## References

Caceres Rossi, R. (2024). OncoDataSets: A Rich Collection of Data
Focused on Cancer Research. R package version 0.1.0.
https://CRAN.R-project.org/package=OncoDataSets

## See also

- [ClinicoPathDescriptives
  documentation](https://www.serdarbalci.com/ClinicoPathDescriptives/)

- [jsurvival documentation](https://www.serdarbalci.com/jsurvival/)

- [meddecide documentation](https://www.serdarbalci.com/meddecide/)

- [jjstatsplot documentation](https://www.serdarbalci.com/jjstatsplot/)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load and explore melanoma survival data
data("Melanoma_df")
str(Melanoma_df)

# Create survival object
library(survival)
surv_obj <- Surv(Melanoma_df$time, Melanoma_df$status == 1)

# Load PSA data for ROC analysis
data("PSAProstateCancer_df")
# Create binary outcome
PSAProstateCancer_df$high_grade <- ifelse(PSAProstateCancer_df$gleason >= 7, 1, 0)

# Load breast cancer data for descriptive analysis
data("BreastCancerWI_df")
table(BreastCancerWI_df$diagnosis)
} # }
```
