# Eurostat Map Analysis Documentation

This document provides a comprehensive overview of the Eurostat Map module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Eurostat Map module is a powerful tool for creating choropleth maps using Eurostat data. It allows users to visualize statistical data across different European regions (NUTS levels) either by downloading data directly from the Eurostat API or by using local data. The module supports both static and interactive map visualizations with customizable styling options.

The module's features can be broadly categorized as follows:

*   **Data Source Selection:** Choose between downloading data from Eurostat or using local data.
*   **Geographic Level Control:** Select the NUTS level for mapping (Countries, Major Regions, Basic Regions, Small Regions).
*   **Map Customization:** Options for map type (static/interactive), color palettes, and classification methods.
*   **Data Summary:** Provides summary statistics for the mapped indicator.
*   **Downloaded Data Display:** Optionally displays the downloaded Eurostat data in a table.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Data Source**                  |                                |                                        |                                     |                                      |
| Eurostat Dataset ID              | `dataset_id`                   | Eurostat Dataset ID                    | `info`, `downloaded_data`           | `.run`                               |
| Indicator Variable               | `indicator`                    | Indicator Variable                     | `plot`, `summary`                   | `.run`, `.plot`                      |
| Year                             | `year`                         | Year                                   | `plot`, `info`, `summary`           | `.run`, `.plot`                      |
| Use Local Data                   | `use_local_data`               | Use Local Data                         | `info`, `downloaded_data`           | `.run`, `.plot`                      |
| Cache Downloaded Data            | `cache_data`                   | Cache Downloaded Data                  | (Internal)                          | `.run`                               |
| Add Downloaded Data to Spreadsheet | `add_to_data`                  | Add Downloaded Data to Spreadsheet     | `downloaded_data`                   | `.run`                               |
| **Map Options**                  |                                |                                        |                                     |                                      |
| Geographic Level                 | `geo_level`                    | Geographic Level                       | `plot`                              | `.plot`                              |
| Map Type                         | `map_type`                     | Map Type                               | `plot`                              | `.plot`                              |
| Color Palette                    | `color_palette`                | Color Palette                          | `plot`                              | `.plot`                              |
| Map Title                        | `map_title`                    | Map Title                              | `plot`                              | `.plot`                              |
| Classification Method            | `classification_method`        | Classification Method                  | `plot`                              | `.plot`                              |
| Number of Classes                | `n_classes`                    | Number of Classes                      | `plot`                              | `.plot`                              |
