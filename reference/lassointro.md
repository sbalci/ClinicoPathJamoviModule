# Penalized Cox Regression Guide

An interactive guide to penalized Cox regression methods available in
ClinicoPath. Helps clinicians, pathologists, radiologists, and
oncologists choose the right analysis for their data and research
question. No data or variables required.

## Usage

``` r
lassointro(
  showOverview = TRUE,
  showDecisionGuide = TRUE,
  showClinicalScenarios = FALSE,
  showAssumptions = FALSE,
  showGlossary = FALSE
)
```

## Arguments

- showOverview:

  Show overview of all penalized Cox regression methods

- showDecisionGuide:

  Show decision flowchart for choosing the right method

- showClinicalScenarios:

  Show worked examples for pathology, radiology, and oncology

- showAssumptions:

  Show key assumptions, common pitfalls, and how to avoid them

- showGlossary:

  Show glossary of technical terms in plain language

## Value

A results object containing:

|                             |     |     |     |     |        |
|-----------------------------|-----|-----|-----|-----|--------|
| `results$overview`          |     |     |     |     | a html |
| `results$decisionGuide`     |     |     |     |     | a html |
| `results$clinicalScenarios` |     |     |     |     | a html |
| `results$assumptions`       |     |     |     |     | a html |
| `results$glossary`          |     |     |     |     | a html |
