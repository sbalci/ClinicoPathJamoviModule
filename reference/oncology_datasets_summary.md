# Oncology Datasets Summary

A summary data frame containing information about all 55 oncology
datasets available in the ClinicoPath package, imported from
OncoDataSets.

## Format

A data frame with 55 rows and 3 columns:

- dataset_name:

  Character. Name of the dataset

- category:

  Character. Analysis category (e.g., "Survival Analysis",
  "Diagnostic/Decision Analysis")

- primary_use:

  Character. Primary analytical use case

## Examples

``` r
if (FALSE) { # \dontrun{
# Load the summary
data("oncology_datasets_summary")

# View all datasets by category
table(oncology_datasets_summary$category)

# Find survival analysis datasets
survival_datasets <- subset(oncology_datasets_summary, 
                           category == "Survival Analysis")
print(survival_datasets)

# Find datasets for ROC analysis
roc_datasets <- subset(oncology_datasets_summary, 
                      grepl("ROC|diagnostic", primary_use, ignore.case = TRUE))
print(roc_datasets)
} # }
```
