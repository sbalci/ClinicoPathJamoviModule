# Treatment Toxicity Profile Analysis Documentation

This document provides a comprehensive overview of the Treatment Toxicity Profile Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Treatment Toxicity Profile Analysis module creates comprehensive visualizations and statistical analyses of adverse event (AE) profiles, particularly useful in clinical trials and safety monitoring. It supports various plot types, detailed incidence calculations, and comparisons between treatment groups.

The module's features can be broadly categorized as follows:

*   **Data Input & Validation:** Handles patient IDs, adverse events, and toxicity grades, with validation checks for data integrity.
*   **Incidence Calculation:** Calculates incidence rates and confidence intervals for adverse events.
*   **Visualization:** Generates stacked bar charts, dot plots, heatmaps, and time-to-event plots to visualize toxicity data.
*   **Statistical Analysis:** Performs statistical comparisons between treatment groups using Fisher's exact test and Chi-square tests, and calculates risk ratios.
*   **Customization:** Offers options for sorting events, filtering by grade, setting minimum incidence, and choosing color schemes.
*   **Reporting:** Provides summary tables for adverse events, grade distribution, group comparisons, and system organ class summaries.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Data Input & Validation**      |                                |                                        |                                     |                                      |
| Patient ID                       | `patientID`                    | Patient ID                             | `summary`, `gradeDistribution`, `groupComparison`, `socSummary` | `.validateData`, `.calculateIncidence`, `.calculateCumulativeIncidence` |
| Adverse Event                    | `adverseEvent`                 | Adverse Event                          | `summary`, `gradeDistribution`, `groupComparison`, `socSummary` | `.validateData`, `.calculateIncidence`, `.calculateCumulativeIncidence` |
| Toxicity Grade                   | `grade`                        | Toxicity Grade                         | `summary`, `gradeDistribution`, `groupComparison`, `socSummary` | `.validateData`, `.calculateIncidence` |
| Treatment Group                  | `treatment`                    | Treatment Group                        | `groupComparison`                   | `.performStatisticalTests`, `.compareGroups` |
| System Organ Class               | `systemOrganClass`             | System Organ Class                     | `socSummary`                        | `.run`                               |
| Time to Event                    | `timeToEvent`                  | Time to Event                          | `summary`                           | `.calculateCumulativeIncidence`      |
| **Visualization**                |                                |                                        |                                     |                                      |
| Primary Plot Type                | `plotType`                     | Primary Plot Type                      | `plot`                              | `.plot`                              |
| Sort Events By                   | `sortBy`                       | Sort Events By                         | `plot`                              | `.plot`                              |
| Show High Grade Only (≥3)        | `showHighGradeOnly`            | Show High Grade Only (≥3)              | `plot`                              | `.run`                               |
| Minimum Incidence (%)            | `minIncidence`                 | Minimum Incidence (%)                  | `plot`                              | `.run`                               |
| Grade Color Scheme               | `gradeColors`                  | Grade Color Scheme                     | `plot`                              | `.getGradeColors`                    |
| Show Percentages                 | `showPercentages`              | Show Percentages                       | `plot`                              | `.plot`                              |
| Show Confidence Intervals        | `showConfidenceIntervals`      | Show Confidence Intervals              | `plot`                              | `.plot`                              |
| **Statistical Analysis**         |                                |                                        |                                     |                                      |
| Compare Between Groups           | `groupComparison`              | Compare Between Groups                 | `groupComparison`                   | `.compareGroups`                     |
| Cumulative Incidence             | `cumulativeIncidence`          | Show Cumulative Incidence              | `plot`                              | `.calculateCumulativeIncidence`      |
| Confidence Level                 | `confidenceLevel`              | Confidence Level                       | `summary`, `groupComparison`        | `.calculateIncidence`, `.compareGroups` |
| **Reporting**                    |                                |                                        |                                     |                                      |
| Adverse Event Summary Table      | `summary`                      | Adverse Event Summary                  | `summary`                           | `.run`                               |
| Grade Distribution Table         | `gradeDistribution`            | Grade Distribution                     | `gradeDistribution`                 | `.run`                               |
| Treatment Group Comparison Table | `groupComparison`              | Treatment Group Comparison             | `groupComparison`                   | `.run`                               |
| System Organ Class Summary Table | `socSummary`                   | System Organ Class Summary             | `socSummary`                        | `.run`                               |
