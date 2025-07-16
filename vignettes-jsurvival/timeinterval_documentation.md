# Comprehensive Time Interval Calculator Analysis Documentation

This document provides a comprehensive overview of the Comprehensive Time Interval Calculator module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Comprehensive Time Interval Calculator is an advanced tool designed for survival analysis, epidemiological studies, and person-time analysis. It offers robust date parsing, flexible time interval calculation, landmark analysis, and comprehensive data quality assessment. This module is crucial for accurately determining follow-up durations and calculating incidence rates in various clinical and research contexts.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Calculation**             |                                |                                        |                                     |                                      |
| Start Date Variable              | `dx_date`                      | Start Date Variable                    | `summary`, `calculated_time`        | `.run`, `.calculate_survival_time`   |
| End Date Variable                | `fu_date`                      | End Date Variable                      | `summary`, `calculated_time`        | `.run`, `.calculate_survival_time`   |
| Date Format                      | `time_format`                  | Date Format                            | `summary`, `calculated_time`        | `.run`, `.calculate_survival_time`, `.detectDateFormat`, `.parseDate` |
| Time Unit for Results            | `output_unit`                  | Time Unit for Results                  | `summary`, `calculated_time`        | `.run`, `.calculate_survival_time`, `.calculateTimeIntervals` |
| **Landmark Analysis**            |                                |                                        |                                     |                                      |
| Enable Landmark Analysis         | `use_landmark`                 | Enable Landmark Analysis               | `summary`, `calculated_time`        | `.run`, `.applyLandmarkAnalysis`     |
| Landmark Time Point              | `landmark_time`                | Landmark Time Point                    | `summary`, `calculated_time`        | `.run`, `.applyLandmarkAnalysis`     |
| **Data Quality**                 |                                |                                        |                                     |                                      |
| Remove Negative Intervals        | `remove_negative`              | Remove Negative Intervals              | `summary`                           | `.assessDataQuality`                 |
| Flag Extreme Values              | `remove_extreme`               | Flag Extreme Values                    | `summary`                           | `.assessDataQuality`                 |
| Include Data Quality Assessment  | `include_quality_metrics`      | Include Data Quality Assessment        | `qualityAssessment`                 | `.assessDataQuality`                 |
| **Output Options**               |                                |                                        |                                     |                                      |
| Add Calculated Times to Dataset  | `add_times`                    | Add Calculated Times to Dataset        | `calculated_time`                   | `.run`                               |
| Confidence Level (%)             | `confidence_level`             | Confidence Level (%)                   | `summary`                           | Not implemented in `.b.R`            |
| **Information & Summary**        |                                |                                        |                                     |                                      |
| Understanding Person-Time Analysis| `personTimeInfo`               | Understanding Person-Time Analysis     | `personTimeInfo`                    | `.run`                               |
| Statistical Summary & Person-Time Analysis| `summary`                      | Statistical Summary & Person-Time Analysis| `summary`                           | `.run`                               |
