# Eurostat Map

Creates choropleth maps using Eurostat data. You can either use sample
datasets by providing a Eurostat dataset ID, or import your own Eurostat
CSV data. For local data, ensure it contains a 'geo' column with NUTS
geographic codes.

## Usage

``` r
eurostatmap(
  data,
  dataset_id = "demo_r_gind3",
  indicator,
  geo_var = NULL,
  geo_level = "nuts2",
  year = 2022,
  map_type = "static",
  color_palette = "viridis",
  map_title = "Eurostat Map",
  use_local_data = FALSE,
  classification_method = "quantile",
  n_classes = 5,
  cache_data = TRUE,
  add_to_data = FALSE
)
```

## Arguments

- data:

  The data as a data frame.

- dataset_id:

  The Eurostat dataset identifier to retrieve data from.

- indicator:

  The variable containing the indicator values to map.

- geo_var:

  The variable containing NUTS geographic codes (for local data).

- geo_level:

  The geographic level for mapping.

- year:

  The year for which to display data.

- map_type:

  Choose between static or interactive map visualization.

- color_palette:

  Color palette for the choropleth map.

- map_title:

  The title displayed on the map.

- use_local_data:

  Use local data instead of downloading from Eurostat.

- classification_method:

  Method for classifying data into color categories.

- n_classes:

  Number of classes for data classification.

- cache_data:

  Cache downloaded Eurostat data to avoid repeated downloads.

- add_to_data:

  Add the downloaded Eurostat data to jamovi's data spreadsheet.

## Value

A results object containing:

|                           |     |     |     |     |          |
|---------------------------|-----|-----|-----|-----|----------|
| `results$instructions`    |     |     |     |     | a html   |
| `results$plot`            |     |     |     |     | an image |
| `results$info`            |     |     |     |     | a table  |
| `results$summary`         |     |     |     |     | a table  |
| `results$downloaded_data` |     |     |     |     | a table  |

Tables can be converted to data frames with `asDF` or
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html). For
example:

`results$info$asDF`

`as.data.frame(results$info)`
