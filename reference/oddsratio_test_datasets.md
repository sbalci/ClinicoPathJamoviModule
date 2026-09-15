# Test Datasets for Odds Ratio Function

Test Datasets for Odds Ratio Function

## Usage

``` r
oddsratio_test

oddsratio_diagnostic

oddsratio_casecontrol

oddsratio_multiple

oddsratio_small

oddsratio_large
```

## Format

### `oddsratio_test`

Main test dataset (200 observations):

- patient_id:

  Patient identifier

- outcome:

  Binary outcome (Alive/Dead)

- treatment:

  Treatment group

- stage:

  Disease stage (Early/Advanced)

- biomarker_status:

  Biomarker status

- age:

  Patient age

- tumor_size:

  Tumor size (cm)

- psa_level:

  PSA level

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 150
rows and 4 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 200
rows and 5 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 180
rows and 7 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 20
rows and 3 columns.

An object of class `tbl_df` (inherits from `tbl`, `data.frame`) with 500
rows and 4 columns.

## Source

Generated using data-raw/oddsratio_test_data.R (seed = 42)

## See also

[`oddsratio`](https://www.serdarbalci.com/ClinicoPathJamoviModule/reference/oddsratio.md)

## Examples

``` r
data(oddsratio_test)
if (FALSE) { # \dontrun{
oddsratio(
  data = oddsratio_test,
  explanatory = "stage",
  outcome = "outcome",
  outcomeLevel = "Dead"
)
} # }
```
