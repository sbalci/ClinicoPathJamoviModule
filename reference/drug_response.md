# Drug Response Prediction Dataset

Simulated pharmacogenomics dataset with genomic biomarkers, protein
levels, and clinical characteristics for drug response prediction.
Designed to test precision medicine decision trees, biomarker
integration, and treatment response modeling.

## Usage

``` r
drug_response
```

## Format

A data frame with 350 patients and 13 variables:

- patient_id:

  Character. Unique patient identifier (DRG_0001 to DRG_0350)

- gene_expression_1, gene_expression_2, gene_expression_3:

  Numeric. Gene expression levels

- protein_level_a, protein_level_b:

  Numeric. Protein concentration levels

- mutation_status:

  Factor. Mutation status ("Wild-type", "Mutant")

- age:

  Integer. Patient age (years)

- performance_status:

  Integer. ECOG performance status (0-2)

- prior_treatments:

  Integer. Number of prior treatment regimens (0-3)

- tumor_stage:

  Factor. Tumor stage (II, III, IV)

- histology:

  Factor. Tumor histology ("Adenocarcinoma", "Squamous", "Other")

- drug_response:

  Factor. Primary outcome - response to treatment ("Non-responder",
  "Responder")

- study_phase:

  Factor. Study phase ("phase1", "phase2")

- sex:

  Factor. Patient sex ("Male", "Female")

- x_coord, y_coord:

  Numeric. Spatial coordinates for multi-center analysis

## Source

Simulated data generated using create_tree_test_data.R

## Details

This dataset simulates a comprehensive pharmacogenomics study combining
genomic biomarkers, protein levels, and clinical variables for drug
response prediction. The dataset reflects realistic patterns of
biomarker-response relationships in precision medicine.

**Clinical Context:**

- Precision medicine and personalized treatment

- Pharmacogenomics-guided therapy selection

- Biomarker-based treatment decisions

- Clinical trial design and analysis

**Key Features:**

- Multi-omic biomarker integration

- Realistic genomic-clinical associations

- Treatment response endpoints

- Multi-phase study design

- Patient performance status considerations

- Geographic distribution modeling

**Recommended Analysis Scenarios:**

- Biomarker-based response prediction

- Precision medicine decision trees

- Multi-omic data integration

- Treatment selection optimization

- Clinical trial endpoint analysis

- Bootstrap validation for biomarker stability

## See also

[`tree_datasets`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/tree_datasets.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the dataset
data(drug_response)

# Precision medicine analysis
result <- tree(
  data = drug_response,
  vars = c("gene_expression_1", "gene_expression_2", "protein_level_a", "age"),
  facs = c("mutation_status", "tumor_stage", "histology"),
  target = "drug_response",
  targetLevel = "Responder",
  train = "study_phase",
  trainLevel = "phase1",
  clinicalContext = "treatment",
  featureImportance = TRUE,
  bootstrapValidation = TRUE,
  showInterpretation = TRUE
)
} # }
```
