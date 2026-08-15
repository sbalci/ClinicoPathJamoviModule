# Patient Follow-Up & Response Guide

An interactive guide to the patient follow-up and tumour response
analyses in ClinicoPath. The choice between them is decided almost
entirely by the SHAPE of your data - one value per patient, one per
visit, or one per lesion per visit - so this guide starts there, shows a
worked layout for each, and states plainly what each analysis can and
cannot do. No data or variables required.

## Usage

``` r
patientfollowupintro(
  showOverview = TRUE,
  showDataStructures = TRUE,
  showLimitations = TRUE,
  showRecistRules = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- showOverview:

  Show the decision table mapping data shape to analysis

- showDataStructures:

  Show an example table for each accepted data layout

- showLimitations:

  Show the limitations and known gaps of each analysis

- showRecistRules:

  Show the response criteria the RECIST analyses apply

- showGlossary:

  Show a glossary of response-assessment terms

## Value

A results object containing:

|                          |     |     |     |     |        |
|--------------------------|-----|-----|-----|-----|--------|
| `results$overview`       |     |     |     |     | a html |
| `results$dataStructures` |     |     |     |     | a html |
| `results$limitations`    |     |     |     |     | a html |
| `results$recistRules`    |     |     |     |     | a html |
| `results$glossary`       |     |     |     |     | a html |
