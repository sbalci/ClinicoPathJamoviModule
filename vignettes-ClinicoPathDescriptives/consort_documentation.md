# CONSORT Diagram Analysis Documentation

This document provides a comprehensive overview of the CONSORT Diagram Analysis module (consort), detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

The consort module is a specialized tool for generating CONSORT flow diagrams, which are essential for transparently reporting the flow of participants through a clinical trial. It helps researchers visualize and document the number of participants at each stage of a study, including enrollment, intervention allocation, follow-up, and analysis.

The module's features can be broadly categorized as follows:

*   **Core CONSORT Diagram Generation:** Create standard CONSORT flow diagrams.
*   **Customizable Flow Paths:** Define and adjust the various stages and transitions of participant flow.
*   **Participant Counts:** Input and display the number of participants at each step.
*   **Exclusion Reasons:** Document reasons for participant exclusions at different stages.
*   **Export Options:** Capabilities to save the generated diagrams in various formats.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Analysis**                |                                |                                        |                                     |                                      |
| Total Participants               | `totalParticipants`            | Total Participants Enrolled            | `consortOverview`                   | `.generateConsortData`               |
| Randomized Participants          | `randomizedParticipants`       | Participants Randomized                | `consortOverview`                   | `.generateConsortData`               |
| Excluded Participants            | `excludedParticipants`         | Participants Excluded                  | `consortOverview`                   | `.generateConsortData`               |
| **Flow Path Customization**      |                                |                                        |                                     |                                      |
| Add Enrollment Stage             | `addEnrollmentStage`           | Add Enrollment Stage                   | `consortDiagram`                    | `.addConsortStage`                   |
| Add Allocation Stage             | `addAllocationStage`           | Add Allocation Stage                   | `consortDiagram`                    | `.addConsortStage`                   |
| Add Follow-up Stage              | `addFollowupStage`             | Add Follow-up Stage                    | `consortDiagram`                    | `.addConsortStage`                   |
| Add Analysis Stage               | `addAnalysisStage`             | Add Analysis Stage                     | `consortDiagram`                    | `.addConsortStage`                   |
| **Exclusion Details**            |                                |                                        |                                     |                                      |
| Exclusion Reasons                | `exclusionReasons`             | Exclusion Reasons                      | `consortDiagram`                    | `.addExclusionReasons`               |
| **Visualization Options**        |                                |                                        |                                     |                                      |
| Show CONSORT Diagram             | `showConsortDiagram`           | Show CONSORT Diagram                   | `consortDiagramPlot`                | `.plotConsortDiagram`                |
| Diagram Title                    | `diagramTitle`                 | Diagram Title                          | `consortDiagramPlot`                | `.setDiagramTitle`                   |
| **Export Options**               |                                |                                        |                                     |                                      |
| Export Plot                      | `exportPlot`                   | Export Plot                            | `exportOptions`                     | `.exportConsortDiagram`              |
| Export Format                    | `exportFormat`                 | Export Format                          | `exportOptions`                     | `.exportConsortDiagram`              |