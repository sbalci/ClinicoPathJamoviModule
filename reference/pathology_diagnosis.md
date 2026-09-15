# Pathology Diagnosis Dataset

Simulated breast pathology diagnosis dataset with histological
measurements, immunohistochemistry markers, and tissue characteristics.
Designed to test pathology-based decision tree classification, biomarker
integration, and diagnostic accuracy assessment.

## Usage

``` r
pathology_diagnosis
```

## Format

A data frame with 300 cases and 12 variables:

- case_id:

  Character. Unique case identifier (PATH_0001 to PATH_0300)

- cell_size:

  Numeric. Cell size measurement (um)

- nuclear_area:

  Numeric. Nuclear area measurement (um^2)

- mitotic_count:

  Integer. Mitotic count per high-power field

- pleomorphism_score:

  Integer. Nuclear pleomorphism score (1-3)

- ki67_percentage:

  Numeric. Ki-67 proliferation index (%)

- p53_positive:

  Factor. p53 immunostaining ("Negative", "Positive")

- her2_status:

  Factor. HER2 status ("Negative", "Positive")

- tumor_type:

  Factor. Tumor type ("Ductal", "Lobular", "Mixed")

- differentiation:

  Factor. Tumor differentiation ("Well", "Moderate", "Poor")

- malignancy:

  Factor. Primary outcome - malignancy status ("Benign", "Malignant")

- validation_set:

  Factor. Validation set ("internal", "external")

- patient_age:

  Integer. Patient age (years)

- menopausal_status:

  Factor. Menopausal status ("Pre", "Post")

- x_coord, y_coord:

  Numeric. Tissue microarray coordinates

## Source

Simulated data generated using create_tree_test_data.R

## Details

This dataset simulates a comprehensive breast pathology study combining
traditional histological measurements with modern immunohistochemistry
markers. The dataset reflects realistic patterns of pathological
findings in breast tissue evaluation.

**Clinical Context:**

- Breast cancer pathology diagnosis

- Histological pattern recognition

- Biomarker-guided diagnosis

- Tissue microarray analysis

**Key Features:**

- Quantitative histological measurements

- Immunohistochemistry marker integration

- Tissue microarray spatial coordinates

- Realistic pathological distributions

- Internal/external validation structure

- Age and menopausal status considerations

**Recommended Analysis Scenarios:**

- Pathology-based diagnostic classification

- Biomarker importance ranking

- Spatial tissue analysis with autocart

- Internal vs external validation

- Multi-modal diagnostic integration

- Clinical threshold optimization

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(pathology_diagnosis)

# Pathology classification analysis
result <- tree(
  data = pathology_diagnosis,
  vars = c("cell_size", "nuclear_area", "mitotic_count", "ki67_percentage"),
  facs = c("pleomorphism_score", "p53_positive", "her2_status", "tumor_type"),
  target = "malignancy",
  targetLevel = "Malignant",
  train = "validation_set",
  trainLevel = "internal",
  clinicalContext = "diagnosis",
  featureImportance = TRUE,
  showInterpretation = TRUE
)

# Spatial tissue analysis
result_spatial <- tree(
  data = pathology_diagnosis,
  vars = c("cell_size", "nuclear_area"),
  facs = c("tumor_type", "differentiation"),
  target = "malignancy",
  targetLevel = "Malignant",
  spatialCoords = c("x_coord", "y_coord"),
  useAutocart = TRUE,
  showPartitionPlot = TRUE
)
} # }
```
