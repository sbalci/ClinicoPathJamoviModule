# Document Function Templates

Reference templates for `/document-function` Documents 2-4. Read this file when generating feature mappings, testing checklists, or comprehensive vignettes.

---

# DOCUMENT 2: Feature Mapping Table

**File:** `vignettes/${ARGUMENTS}_documentation.md`

Generate a concise feature-to-code mapping document. Follow the pattern from existing examples like `agreement_documentation.md` and `nogoldstandard_documentation.md`.

### Structure

```markdown
# {Function Title} Documentation

This document provides a comprehensive overview of the {Function Title} module,
detailing its features, user interface elements, and the underlying R functions.

## Feature Summary

{2-3 paragraph overview of the module's purpose, target users, and main capabilities.
Group features into logical categories.}

## Feature Details

The following table provides a detailed mapping of the module's features,
from the user interface to the underlying R functions.

| Feature | YAML Argument (`.a.yaml`) | UI Label | Results Section (`.r.yaml`) | R Function (`.b.R`) |
|---------|---------------------------|----------|----------------------------|---------------------|
| **Category Name** | | | | |
| Feature name | `option_name` | UI label text | `result_id` | `.methodName` |
```

### Requirements

- Group features into logical categories (Input Variables, Model Options, Core Results, Plots, Output Variables, Explanatory Output, etc.)
- Map **every** `.a.yaml` option to its UI label, result section, and R method
- Use **bold** for category headers in the Feature column
- Use backtick formatting for option names, result IDs, and R methods
- Include a row for each feature even if the R function is "Not implemented in `.b.R`"
- Note computed/derived features that don't have a direct option (use `—` for missing fields)

---

# DOCUMENT 3: Testing Checklist

**File:** `vignettes/testing_$ARGUMENTS.md`

Generate a comprehensive testing plan. Follow the pattern from existing examples like `testing_agreement.md`, `testing_enhancedROC.md`, and `testing_psychopdaROC.md`.

### Structure

```markdown
# Testing {Function Title}

All test datasets are in `data-raw/` (CSV) or `data/` (RDA).
{Add any generation script info if applicable.}

---

## 1. {SCENARIO CATEGORY NAME}

| # | File | Variables | Options to Test |
|---|------|-----------|-----------------|
| 1 | `dataset_name` | var1: `col1`, var2: `col2`, ... | Option list with specific values to test |

**Options covered:** `opt1`, `opt2`, `opt3`, ...

---

## N. EDGE CASES
...

## COMPLETE OPTION COVERAGE CHECKLIST

- [x] `option_name` — test #N
- [ ] `option_name` — not yet tested
```

### Requirements

- **Group test scenarios** by functional area (e.g., "Standard Clinical", "High-Dimensional", "Small Sample", "Edge Cases", "Display Options")
- **Reference real datasets** from `data/` and `data-raw/` directories; list exact variable names
- **Specify exact option values** to test (not just "test this option" but "set `lambda` to `lambda.min`")
- **Cover every `.a.yaml` option** at least once in the test matrix
- **Include edge cases**: empty data, all censored, perfect separation, single predictor, all categorical, etc.
- **End with a complete option coverage checklist** showing which test number covers each option
- **Include dataset table** if new test data files are needed
- Test scenarios should be **numbered sequentially** across all sections

---

# DOCUMENT 4: Comprehensive Vignette (R Markdown)

**File:** `vignettes/{module}-$ARGUMENTS-comprehensive.Rmd`

Generate an executable R Markdown vignette demonstrating every feature. Follow the pattern from existing examples like `meddecide-enhancedroc-comprehensive.Rmd` and `meddecide-psychopdaroc-comprehensive.Rmd`.

### Structure

````markdown
---
title: "{Function Title} - Comprehensive Guide"
output: rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{{Function Title} - Comprehensive Guide}
  %\VignetteEngine{knitr::rmarkdown}
  %\VignetteEncoding{UTF-8}
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  eval = TRUE, echo = TRUE, collapse = TRUE, comment = "#>",
  error = TRUE, warning = FALSE, message = FALSE, fig.width = 7, fig.height = 5
)
devtools::load_all(".")
data_path <- "data-raw/non-rda/"
data_path2 <- "data/"
```

> **Note:** The `$ARGUMENTS()` function is designed for use within **jamovi's GUI**.
> The code examples below show the R syntax for reference.

# {Function Title}

## Overview
{2-3 paragraph description}

## Datasets Used in This Guide

| Dataset | N | Key Features | Primary Use |
|---------|---|--------------|-------------|
| `dataset1` | 250 | Description | Scenario |

## 1. {First Feature Category}

### Basic usage
```{r basic-example}
data1 <- read.csv(paste0(data_path, "dataset1.csv"))
$ARGUMENTS(data = data1, option1 = "value1", option2 = "value2")
```

## N. Edge Cases
...

## References
...
````

### Requirements

- **YAML front matter** with proper VignetteIndexEntry, VignetteEngine, VignetteEncoding
- **Setup chunk** with `devtools::load_all(".")` (not `library()`) and `error = TRUE`
- **Note about jamovi GUI** — the function is designed for jamovi, R syntax is for reference
- **Datasets table** listing all datasets used with N, key features, and primary use
- **One section per feature category** with subsections for basic/advanced usage
- **Every `.a.yaml` option** demonstrated in at least one code chunk
- **Realistic function calls** using actual parameter names from `.a.yaml`
- **Edge case section** demonstrating small samples, missing data, boundary conditions
- **Named code chunks** (e.g., `{r basic-example}`) for easy navigation
- Use `paste0(data_path, "filename.csv")` for data loading
- Include brief prose explaining what each example demonstrates and what to look for in output

---

## Diagram Specifications

When diagrams are enabled, render **both** of the following (adjust nodes based on actual findings):

**Flowchart (Mermaid)** — same template as Document 1 section 6.

**Execution Flow** — provide a numbered step-by-step description rather than Mermaid sequence diagrams (which often have parsing issues with complex conditional logic).

Optional: Dependency graph of options → outputs.