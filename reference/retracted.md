# Find Retracted Papers from DOI

Checks DOIs against retraction databases to identify retracted
publications. Uses OpenRetractions API to validate publication status
and can optionally retrieve PubMed IDs. Includes DOI format validation,
rate limiting, and robust error handling for reliable results.

## Usage

``` r
retracted(data, doi, database = "or", pmid = FALSE)
```

## Arguments

- data:

  The data as a data frame containing the DOI variable to check. Must
  have at least one column with DOI strings.

- doi:

  Column containing DOI strings to check for retractions. Supports
  multiple DOI formats including bare DOIs (10.1000/example), DOI URIs
  (doi:10.1000/example), and DOI URLs (https://doi.org/10.1000/example).

- database:

  Database to check for retractions. OpenRetractions is recommended as
  it provides comprehensive and up-to-date retraction information.
  RetractionWatch option currently uses OpenRetractions as fallback.

- pmid:

  If TRUE, attempts to retrieve corresponding PubMed IDs for valid DOIs
  using the rcrossref package. Requires internet connection and may
  increase processing time for large datasets.

## Value

A results object containing:

|                   |     |     |     |     |                |
|-------------------|-----|-----|-----|-----|----------------|
| `results$todo`    |     |     |     |     | a html         |
| `results$summary` |     |     |     |     | a table        |
| `results$details` |     |     |     |     | a html         |
| `results$pmids`   |     |     |     |     | a preformatted |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$summary$asDF`

`as.data.frame(results$summary)`
