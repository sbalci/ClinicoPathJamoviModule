# Enhanced Outcome Organizer for Survival Analysis Documentation

This document provides a comprehensive overview of the Enhanced Outcome Organizer for Survival Analysis module, detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

This module is an advanced tool for preparing outcome variables for various types of survival analysis, including overall survival, cause-specific survival, competing risks, recurrence-free survival (RFS), progression-free survival (PFS), disease-free survival (DFS), time to progression (TTP), and multistate models. It offers robust data validation, flexible recoding options based on event types and hierarchies, and supports advanced censoring methods like interval and administrative censoring. The module also provides diagnostic information and visualization of the recoded outcomes.

## Feature Details

The following table provides a detailed mapping of the module's features, from the user interface to the underlying R functions.

| Feature                          | YAML Argument (`.a.yaml`)      | UI Label                               | Results Section (`.r.yaml`)         | R Function (`.b.R`)                  |
| -------------------------------- | ------------------------------ | -------------------------------------- | ----------------------------------- | ------------------------------------ |
| **Core Outcome Preparation**     |                                |                                        |                                     |                                      |
| Outcome Variable                 | `outcome`                      | Outcome Variable                       | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz`, `addOutcome` | `.run`, `.getData`, `.validateInputs`, `.organizeOutcomes` |
| Event Level                      | `outcomeLevel`                 | Event Level                            | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Recurrence/Progression Variable  | `recurrence`                   | Recurrence/Progression Variable        | `summary`, `diagnosticsTable`       | `.run`, `.organizeOutcomes`          |
| Recurrence Event Level           | `recurrenceLevel`              | Recurrence Event Level                 | `summary`                           | `.run`, `.organizeOutcomes`          |
| Patient ID                       | `patientID`                    | Patient ID                             | `diagnosticsTable`                  | `.run`, `.organizeOutcomes`          |
| Survival Analysis Type           | `analysistype`                 | Survival Analysis Type                 | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Multiple Event Levels            | `multievent`                   | Multiple Event Levels                  | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Dead of Disease                  | `dod`                          | Dead of Disease                        | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Dead of Other Causes             | `dooc`                         | Dead of Other Causes                   | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Alive with Disease               | `awd`                          | Alive with Disease                     | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Alive without Disease            | `awod`                         | Alive without Disease                  | `summary`, `outputTable`, `diagnosticsTable`, `outcomeViz` | `.run`, `.organizeOutcomes`          |
| Use Event Hierarchy              | `useHierarchy`                 | Use Event Hierarchy                    | `summary`, `diagnosticsTable`       | `.run`, `.organizeOutcomes`          |
| Priority Event Type              | `eventPriority`                | Priority Event Type                    | `summary`                           | `.run`, `.organizeOutcomes`          |
| Use Interval Censoring           | `intervalCensoring`            | Use Interval Censoring                 | `summary`, `diagnosticsTable`       | `.run`, `.organizeOutcomes`          |
| Interval Start Variable          | `intervalStart`                | Interval Start Variable                | `summary`                           | `.run`, `.organizeOutcomes`          |
| Interval End Variable            | `intervalEnd`                  | Interval End Variable                  | `summary`                           | `.run`, `.organizeOutcomes`          |
| Use Administrative Censoring     | `adminCensoring`               | Use Administrative Censoring           | `summary`, `diagnosticsTable`       | `.run`, `.organizeOutcomes`          |
| Administrative Censoring Date    | `adminDate`                    | Administrative Censoring Date          | `summary`                           | `.run`, `.organizeOutcomes`          |
| **Output & Diagnostics**         |                                |                                        |                                     |                                      |
| Show Output Table                | `outputTable`                  | Show Output Table                      | `outputTable`                       | `.run`                               |
| Show Diagnostic Information      | `diagnostics`                  | Show Diagnostic Information            | `diagnosticsTable`                  | `.run`                               |
| Show Outcome Distribution        | `visualization`                | Show Outcome Distribution              | `outcomeViz`                        | `.run`, `.plotOutcome`               |
| Add Recoded Outcome to Data      | `addOutcome`                   | Add Recoded Outcome to Data            | `addOutcome`                        | `.run`                               |
