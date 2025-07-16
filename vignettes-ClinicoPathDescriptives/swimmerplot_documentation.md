# Swimmer Plot Analysis Documentation

This document provides a comprehensive overview of the Swimmer Plot Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The Swimmer Plot Analysis module creates comprehensive swimmer plots using the `ggswim` package to visualize patient timelines, treatments, milestones, and clinical events. It is designed for clinical research, particularly in oncology, to track individual patient journeys, treatment responses, and event occurrences over time. The module offers extensive customization, robust data validation, and integrated clinical interpretation.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Data Variables**          |                                |                                        |                                     |                                      |
| Patient ID                       | `patientID`                    | Patient ID                             | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processOngoingStatus`, `.processEventMarkers`, `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| Start Time                       | `startTime`                    | Start Time                             | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processOngoingStatus`, `.processEventMarkers`, `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| End Time                         | `endTime`                      | End Time                               | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processOngoingStatus`, `.processEventMarkers`, `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| Response/Status Variable         | `responseVar`                  | Response/Status Variable               | `plot`, `summary`, `personTimeTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| **Time Processing Options**      |                                |                                        |                                     |                                      |
| Time Input Type                  | `timeType`                     | Time Input Type                        | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processEventMarkers` |
| Date Format in Data              | `dateFormat`                   | Date Format in Data                    | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.parseDates`, `.validateAndProcessData`, `.processMilestones`, `.processEventMarkers` |
| Time Unit for Display            | `timeUnit`                     | Time Unit for Display                  | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processEventMarkers`, `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| Time Display Mode                | `timeDisplay`                  | Time Display Mode                      | `plot`, `summary`, `personTimeTable`, `milestoneTable`, `eventMarkerTable`, `timelineData`, `summaryData`, `advancedMetrics` | `.validateAndProcessData`, `.processMilestones`, `.processEventMarkers` |
| **Milestone Configuration**      |                                |                                        |                                     |                                      |
| Maximum Milestones               | `maxMilestones`                | Maximum Milestones                     | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 1 Name                 | `milestone1Name`               | Milestone 1 Name                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 1 Date                 | `milestone1Date`               | Milestone 1 Date                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 2 Name                 | `milestone2Name`               | Milestone 2 Name                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 2 Date                 | `milestone2Date`               | Milestone 2 Date                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 3 Name                 | `milestone3Name`               | Milestone 3 Name                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 3 Date                 | `milestone3Date`               | Milestone 3 Date                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 4 Name                 | `milestone4Name`               | Milestone 4 Name                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 4 Date                 | `milestone4Date`               | Milestone 4 Date                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 5 Name                 | `milestone5Name`               | Milestone 5 Name                       | `milestoneTable`                    | `.processMilestones`                 |
| Milestone 5 Date                 | `milestone5Date`               | Milestone 5 Date                       | `milestoneTable`                    | `.processMilestones`                 |
| **Event Markers**                |                                |                                        |                                     |                                      |
| Show Event Markers               | `showEventMarkers`             | Show Event Markers                     | `plot`, `eventMarkerTable`          | `.processEventMarkers`               |
| Event Type Variable              | `eventVar`                     | Event Type Variable                    | `eventMarkerTable`                  | `.processEventMarkers`               |
| Event Time Variable              | `eventTimeVar`                 | Event Time Variable                    | `eventMarkerTable`                  | `.processEventMarkers`               |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Lane Width                       | `laneWidth`                    | Lane Width                             | `plot`                              | `.createGgswimPlot`                  |
| Marker Size                      | `markerSize`                   | Marker Size                            | `plot`                              | `.createGgswimPlot`                  |
| Plot Theme                       | `plotTheme`                    | Plot Theme                             | `plot`                              | `.createGgswimPlot`                  |
| Show Legend                      | `showLegend`                   | Show Legend                            | `plot`                              | `.createGgswimPlot`                  |
| Reference Lines                  | `referenceLines`               | Reference Lines                        | `plot`                              | `.addReferenceLines`                 |
| Custom Reference Time            | `customReferenceTime`          | Custom Reference Time                  | `plot`                              | `.addReferenceLines`                 |
| **Sorting and Display**          |                                |                                        |                                     |                                      |
| Sort By Variable                 | `sortVariable`                 | Sort By Variable                       | `plot`                              | Not implemented in `.b.R`            |
| Sort Order                       | `sortOrder`                    | Sort Order                             | `plot`                              | Not implemented in `.b.R`            |
| **Analysis Options**             |                                |                                        |                                     |                                      |
| Show Clinical Interpretation     | `showInterpretation`           | Show Clinical Interpretation           | `interpretation`                    | `.generateInterpretationOutput`      |
| Include Person-Time Analysis     | `personTimeAnalysis`           | Include Person-Time Analysis           | `personTimeTable`, `advancedMetrics`| `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| Include Response Analysis        | `responseAnalysis`             | Include Response Analysis              | `summary`, `advancedMetrics`        | `.calculateSummaryStats`, `.generateClinicalInterpretation` |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Timeline Data             | `exportTimeline`               | Export Timeline Data                   | `timelineData`                      | `.exportResults`                     |
| Export Summary Statistics        | `exportSummary`                | Export Summary Statistics              | `summaryData`                       | `.exportResults`                     |
