# Find Retracted Papers from DOI

This module checks DOIs against retraction databases to identify
retracted publications. It supports OpenRetractions database and can
optionally retrieve PubMed IDs for valid DOIs. The function includes
robust error handling, DOI format validation, and rate limiting for API
calls.

## Value

A results object containing retraction status, details, and optional
PubMed IDs

## Examples

``` r
# \donttest{
# Example 1: Basic retraction check
data <- data.frame(
  doi = c("10.1126/science.aac4716", "10.1038/nature12373", "10.1016/j.cell.2014.09.045")
)
result <- retracted(data = data, doi = "doi")

# Example 2: Include PubMed IDs
result_with_pmid <- retracted(
  data = data, 
  doi = "doi",
  pmid = TRUE
)

# Example 3: Using different database
result_rw <- retracted(
  data = data,
  doi = "doi", 
  database = "rw"
)
# }
```
