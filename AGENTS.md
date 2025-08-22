# AGENTS.md

## Introduction

This document defines a suite of specialized LLM-based agents designed to streamline and enhance the development, testing, documentation, and maintenance of Jamovi modules, specifically:

* **meddecide**: Implements decision curve analysis, diagnostic decision models, and clinical decision support tools.
* **jjstatsplot**: Generates statistical plots (e.g., waterfall plots, swimmer plots, heatmaps) for Jamovi modules.
* **jsurvival**: Provides survival analysis routines, including Kaplan–Meier, Cox proportional hazards, and multi-state models.
* **ClinicoPathDescriptives**: Offers descriptive statistics and advanced tables/visualizations tailored to anatomic and surgical pathologists.

Each agent encapsulates a focused set of responsibilities and expertise, enabling efficient collaboration with Large Language Models (LLMs) for code generation, debugging, documentation, quality assurance, refactoring, and domain-specific analysis.

### How to Use This Document

1. **Identify Task**: Determine which aspect of module development you need assistance with (e.g., writing R functions, drafting YAML, creating unit tests, designing a plot).
2. **Select Agent**: Choose the agent from the list below whose responsibilities align with your task.
3. **Launch Prompt**: Use the example prompt templates to invoke the chosen agent. Copy and paste the template into ChatGPT or your preferred LLM interface, replacing placeholders (e.g., `<module_name>`, `<function_name>`, `<description>`) with context-specific details.
4. **Iterate**: Agents are designed for iterative workflows; if the output needs refinement, provide additional context or clarifications.
5. **Integrate Output**: Incorporate the generated code, documentation, tests, or analyses into your Jamovi module project.

---

## Table of Contents

- [Introduction](#introduction)
- [How to Use This Document](#how-to-use-this-document)
- [Agents Overview](#agents-overview)
- [Agent Handoffs & Workflow](#agent-handoffs--workflow)
- [Per-Agent Acceptance Checklists](#per-agent-acceptance-checklists)
- [Decision Boundaries: Which Agent When](#decision-boundaries-which-agent-when)
- [Implementation Patterns (Informed by Repositories and Jamovi Dev Docs)](#implementation-patterns-informed-by-repositories-and-jamovi-dev-docs)
- [YAML Configuration Files](#yaml-configuration-files)
- [R6 Analysis Class Patterns (`.b.R`)](#r6-analysis-class-patterns-br)
- [End-to-End Examples of Agent Workflow](#end-to-end-examples-of-agent-workflow)
- [Style & Naming Conventions (Jamovi + R)](#style--naming-conventions-jamovi--r)
- [Appendix: Common Jamovi Module Components](#appendix-common-jamovi-module-components)
- [Security & PHI Considerations](#security--phi-considerations)
- [Versioning](#versioning)

## Agents Overview

| Agent Name               | Primary Responsibilities                                                                                  |
| ------------------------ | --------------------------------------------------------------------------------------------------------- |
| **ModuleArchitectAgent** | Scaffolding module structure, YAML configuration, metadata, dependencies, import/export logic             |
| **RFunctionGenAgent**    | Generating robust, idiomatic R functions for statistical analyses and data manipulation                   |
| **DocumentationAgent**   | Creating or updating `.R`, `.u.yaml`, `.r.yaml`, `.a.yaml` files with clear comments and user-facing help |
| **PlotDesignAgent**      | Designing and coding statistical plots (ggplot2, Jamovi plotting conventions, bslib themes)               |
| **TestQAAgent**          | Writing unit tests (`testthat`) and integration tests; validating existing functions for edge cases       |
| **YAMLConfigAgent**      | Drafting and validating YAML specification files for Jamovi modules (`.u.yaml`, `.r.yaml`, `.a.yaml`)     |
| **RefactorAgent**        | Refactoring and optimizing existing code for readability, performance, and style compliance               |
| **BugTrackerAgent**      | Identifying, reproducing, and proposing fixes for bugs based on user error messages or failing tests      |
| **CIIntegrationAgent**   | Configuring GitHub Actions, CI pipelines, and automated checks for module builds, linting, and testing    |
| **DomainExpertAgent**    | Providing domain-specific insights (e.g., clinical decision thresholds, interpretation of survival plots) |
| **VisualizationUXAgent** | Enhancing UI/UX: table layouts (DT), tooltips, theme toggles, and accessibility                           |
| **ReleaseManagerAgent**  | Drafting release notes, version bump logic, changelog generation, packaging (tarball, zip)                |

Each agent is described in detail below.

# Agent Handoffs & Workflow

1. **Plan** → *ModuleArchitectAgent* drafts skeleton (folders, DESCRIPTION, jamovi-module.yml).
2. **Specify** → *YAMLConfigAgent* defines `.a.yaml`, `.u.yaml`, `.r.yaml` options and UI.
3. **Implement** → *RFunctionGenAgent* adds core R functions; *PlotDesignAgent* designs figures.
4. **Validate** → *TestQAAgent* writes unit & integration tests; *BugTrackerAgent* triages failures.
5. **Refine** → *RefactorAgent* improves clarity/performance; *DocumentationAgent* updates help.
6. **Ship** → *CIIntegrationAgent* ensures build/lint/tests on GitHub Actions; *ReleaseManagerAgent* prepares notes and version bump.

> Tip: When switching agents, include a 1–3 bullet "context handoff" (inputs, decisions, TODOs) to keep LLMs aligned.

---

## Per-Agent Acceptance Checklists

> Use these as the “definition of done” before handoffs. Each checklist has three parts: **Inputs**, **Outputs**, and **Tests/Verification**.

### ModuleArchitectAgent
- **Inputs**
  - Target module name and short description
  - Supported jamovi/jmvcore versions
  - Initial set of analyses (names only)
- **Outputs**
  - `DESCRIPTION` with Imports and minimum versions
  - `jamovi-module.yml` with module metadata and compatibility
  - Folders scaffolded: `R/`, `analyses/`, `man/`, `inst/`, `resources/`, `tests/testthat/`
  - One minimal analysis stub wired via `.a.yaml`, `.u.yaml`, `.r.yaml`
- **Tests/Verification**
  - `R CMD check` passes locally with no ERRORs
  - `jamovi-module.yml` loads in jamovi (dry-run) without schema errors

### YAMLConfigAgent
- **Inputs**
  - Analysis name, option list, desired UI groups
- **Outputs**
  - `.a.yaml` (options + menu)
  - `.u.yaml` (controls referencing `.a.yaml` options)
  - `.r.yaml` (results with `clearWith`, `visible`, `renderFun`)
- **Tests/Verification**
  - All option names are referenced exactly in R6 via `self$options$...`
  - All result names are referenced exactly in R6 via `self$results$...`
  - YAML passes lint (no duplicate keys; anchors resolved)

### RFunctionGenAgent
- **Inputs**
  - Function/analysis name, exact option names & types, example dataset or schema
- **Outputs**
  - R function(s) with roxygen2 docs
  - R6 `.b.R` implementation filling all declared results
  - Clear error messages via `jmvcore::reject()` for invalid inputs
- **Tests/Verification**
  - Unit tests cover: small n, single-level factor, missing values, numeric/string parsing
  - Deterministic results with fixed seeds
  - No unhandled warnings; messages are actionable

### PlotDesignAgent
- **Inputs**
  - Purpose of plot, expected aesthetics, inputs (columns), optional theme
- **Outputs**
  - ggplot code compatible with jamovi image renderer (`.plotX`)
  - Axis titles, legends, captions; accessibility (sufficient font size)
- **Tests/Verification**
  - Plot object has expected geoms/scales in tests (class/layers)
  - Works with empty/small data without crashing (shows informative message)

### TestQAAgent
- **Inputs**
  - Functions/analyses to test; expected behaviors and edge cases
- **Outputs**
  - `tests/testthat/test-<name>.R` with unit + integration tests
  - Golden tests for plots (class/layers), snapshot tests for tables where feasible
- **Tests/Verification**
  - All tests pass locally; failures are reproducible
  - Coverage for core branches (≥80% for critical modules)

### RefactorAgent
- **Inputs**
  - Target files/functions, pain points (complexity, duplication, speed)
- **Outputs**
  - Clear diffs with behavior preserved
  - Private helpers extracted; names follow conventions
- **Tests/Verification**
  - Pre/post tests identical; microbenchmarks not worse (>10% slowdown triggers rework)

### BugTrackerAgent
- **Inputs**
  - Repro steps, data snippet, current/expected behavior
- **Outputs**
  - Minimal reproducible example (MRE)
  - Root-cause hypothesis and proposed patch
- **Tests/Verification**
  - Regression test added; issue cannot be reproduced after fix

### CIIntegrationAgent
- **Inputs**
  - Supported R versions and OS matrix
- **Outputs**
  - GitHub Actions workflow running: R CMD check, lintr, testthat; cache of packages; artifact upload
- **Tests/Verification**
  - CI green on PR; artifacts include `00check.log`

### DocumentationAgent
- **Inputs**
  - Functions/analyses to document; references for methods
- **Outputs**
  - Roxygen2 docs; vignettes or README section; inline examples
- **Tests/Verification**
  - `devtools::document()` runs cleanly; examples run without error

### VisualizationUXAgent
- **Inputs**
  - Table/plot outputs to style; accessibility requirements
- **Outputs**
  - Improved table layouts (DT opts), tooltips/help text, theme toggles if applicable
- **Tests/Verification**
  - Manual QA checklist passes for readability, focus order, keyboard nav (where applicable)

### DomainExpertAgent
- **Inputs**
  - Clinical/statistical question; dataset schema; acceptance thresholds
- **Outputs**
  - Review notes on assumptions, references, recommended defaults
- **Tests/Verification**
  - Citations included; choices align with referenced guidelines

### ReleaseManagerAgent
- **Inputs**
  - Changelog items, breaking changes, PR list
- **Outputs**
  - Version bump (SEMVER), release notes, tagged release instructions
- **Tests/Verification**
  - Module builds from a clean clone; version appears in jamovi About/help

---

## Decision Boundaries: Which Agent When

| Situation / Signal                                                                 | Choose this Agent         | Do this first                                                            | Not this                                                                 |
|------------------------------------------------------------------------------------|---------------------------|---------------------------------------------------------------------------|---------------------------------------------------------------------------|
| New module or analysis scaffold needed                                             | ModuleArchitectAgent      | Confirm module name, jmvcore version, target analyses                    | Writing R functions before YAML exists                                   |
| You know the options/UI but not the compute yet                                    | YAMLConfigAgent           | Draft `.a.yaml`, `.u.yaml`, `.r.yaml` and validate names                 | Implementing `.b.R` without stable option names                          |
| Core statistics or data-wrangling logic required                                   | RFunctionGenAgent         | Provide option names/types; sample data                                  | Plot styling before compute is correct                                   |
| Plot looks wrong or needs publication quality                                      | PlotDesignAgent           | Freeze the data/columns that feed the plot                               | Rewriting analysis logic                                                  |
| Unsure if changes break existing behavior                                          | TestQAAgent               | List expected behaviors & edge cases                                     | Manual ad-hoc testing only                                               |
| Code works but is messy/slow                                                       | RefactorAgent             | Identify hotspots; set performance budgets                               | Adding features during refactor                                          |
| Error reported by users; need reproducible steps                                   | BugTrackerAgent           | Capture MRE (data + steps + expected vs actual)                          | Large refactors before isolating bug                                     |
| Ensure PRs don’t regress; add checks/lint                                          | CIIntegrationAgent        | Define R version matrix; cache key strategy                              | Only local testing                                                       |
| Gaps in help text, method references, examples                                     | DocumentationAgent        | Collect method references; decide example datasets                       | Shipping code without docs                                               |
| Table is cluttered; need better UX/accessibility                                   | VisualizationUXAgent      | Specify user personas & accessibility needs                              | Changing compute to “fix” layout                                         |
| Need clinical/statistical justification or defaults                                | DomainExpertAgent         | Provide context, outcomes, minimal clinically important differences      | Copying defaults from unrelated modules                                  |
| Preparing a release, bumping version, writing notes                                | ReleaseManagerAgent       | Compile changes, breaking notes                                          | Merging to main without tagging/release notes                            |

# Implementation Patterns (Informed by Repositories and Jamovi Dev Docs)

# Implementation Patterns (Informed by Repositories and Jamovi Dev Docs)

## Overview

ClinicoPath statistical modules (meddecide, jjstatsplot, ClinicoPathDescriptives, jsurvival) follow a consistent structure, treating each analysis as an **agent** with defined **inputs**, **processing logic**, and **outputs**. These agents are implemented as **R6 classes** (in `.b.R` files) and configured by YAML files (`.a.yaml`, `.u.yaml`, `.r.yaml`). This design cleanly separates the user interface, analysis options, and result definitions.

### Core Patterns Extracted

1. **Function Calls Using `private$`**: Agents delegate tasks to private helper methods (e.g., `.prepareData()`, `.computeMetrics()`, `.buildTable()`) defined within the `private = list()` block of the R6 class.

2. **Use of `self$options` and `self$results`**:

   * **`self$options`**: Holds inputs defined in `.a.yaml`. Access values directly (e.g., `self$options$outcome`, `self$options$alpha`).
   * **`self$results`**: Contains output objects defined in `.r.yaml`. Populate outputs using `$setContent()`, `$setRow()`, `$addRow()`, and `$setState()`.

3. **YAML Files Relationship**:

   * **`.a.yaml`**: Defines options (names, types, defaults, constraints) and registers the analysis.
   * **`.u.yaml`**: Describes UI layout (controls, labels, grouping) by referencing options in `.a.yaml`.
   * **`.r.yaml`**: Specifies result objects (tables, plots, text), `renderFun` for images, visibility conditions, and dependencies (`clearWith`).

4. **Jamovi Developer Guidelines Integration**:

   * Based on [Jamovi Module Authoring](https://dev.jamovi.org/index.html), modules include:

     * **`DESCRIPTION` file**: Lists dependencies (e.g., `Imports: jmvcore, survival`).
     * **`jamovi-module.yml` manifest**: Registers version, dependencies, and jmvcore compatibility.
     * **`man/` folder**: Contains Rd documentation generated from roxygen2 comments in R.
     * **`R/` folder**: Contains `.b.R` (implementation), `.h.R` (auto-generated base), and other supporting functions.
     * **`resources/` folder**: Provides icons or additional JS/CSS for custom UI if needed.
     * **Testing structure**: `tests/testthat/` for unit tests; ensure R CMD check passes and CI workflows validate builds.

By examining real-world examples and official guidelines, we see how inputs flow from UI to R code and how outputs are formatted for display.

## YAML Configuration Files

## Prompt Templates (copy‑paste ready)

### ModuleArchitectAgent
```
You are ModuleArchitectAgent. Create the initial skeleton for the jamovi module `<module_name>`. Include:
- DESCRIPTION with Imports: jmvcore (>=2.5.0), survival, ggplot2
- jamovi-module.yml with version `<semver>`, compatible jmvcore
- Folders: R/, man/, inst/, resources/, analyses/
- A minimal analysis stub named `<analysis_name>` wired through `.a.yaml`, `.u.yaml`, `.r.yaml`.
Output: file tree, key files with brief contents, and next steps.
```

### RFunctionGenAgent
```
You are RFunctionGenAgent. Implement `<function_name>` for module `<module_name>`.
Inputs (from options): `<inputs>`.
Tasks: validate, compute, return structures consumable by jmvcore (tables/images). Use tidyverse style; include roxygen2.
Edge cases: small n, single-factor level, missing values.
Tests: provide `testthat` examples with fixed seeds.
```

### YAMLConfigAgent
```
You are YAMLConfigAgent. Draft `.a.yaml`, `.u.yaml`, `.r.yaml` for analysis `<analysis_name>`.
- `.a.yaml`: options (names, types, defaults, constraints), refs
- `.u.yaml`: UI layout mapping to options
- `.r.yaml`: results (tables/plots), clearWith, visible, renderFun hooks
Return: three YAML blocks ready to paste.
```

### TestQAAgent
```
You are TestQAAgent. Write unit & integration tests for `<function_or_analysis>`.
- Deterministic seeds
- Golden-file test for plot object (class & layers)
- Error messages for invalid inputs
Return: `tests/testthat/test-<name>.R`
```

### PlotDesignAgent
```
You are PlotDesignAgent. Produce a publication-quality ggplot for `<purpose>` following jamovi themes. Return code only; no images. Accept `ggtheme` parameter.
```

### CIIntegrationAgent
```
You are CIIntegrationAgent. Provide a GitHub Actions workflow that runs R CMD check, lintr, and testthat on push & PR; caches R packages; uploads check artifacts.
```

### 1. Analysis Definition (`.a.yaml`)

**Purpose**: Register the analysis, define options and defaults, and group in the menu.

**Pattern Example (meddecide/kappaSizeCI.a.yaml)**:

```yaml
analysis-name: kappaSizeCI
class: kappaSizeCIClass
package: meddecide
requiresData: false
title: "Kappa Sample Size Calculation"
menu:
  - name: "Interobserver Analysis"
    sub-menu:
      - name: "Kappa Sample Size"

options:
  - name: outcome
    type: list
    values: ["2", "3", "4", "5"]
    default: "2"
    title: "Number of Outcome Levels"

  - name: kappa0
    type: number
    default: 0.60
    minimum: 0.01
    maximum: 0.99
    title: "Null Kappa (K<sub>0</sub>)"
    description: "Expected kappa under null hypothesis"

  - name: kappaL
    type: number
    default: 0.40
    minimum: 0.01
    maximum: 0.99
    title: "Lower Bound of Kappa (K<sub>L</sub>)"
    description: "Lower limit of clinically acceptable kappa"

  - name: kappaU
    type: number
    default: 0.80
    minimum: 0.01
    maximum: 0.99
    title: "Upper Bound of Kappa (K<sub>U</sub>)"
    description: "Upper limit of clinically acceptable kappa"

  - name: props
    type: string
    default: "0.20, 0.80"
    title: "Category Proportions"
    description: "Comma-separated proportions for each outcome level"

  - name: raters
    type: integer
    default: 2
    minimum: 2
    maximum: 10
    title: "Number of Raters"

  - name: alpha
    type: number
    default: 0.05
    minimum: 0.01
    maximum: 0.10
    title: "Significance Level (α)"

refs:
  - ClinicoPathJamoviModule
  - kappaSize
```

**Notes**:

* Each `option.name` must match a field in the R6 class (`self$options$name`).
* The `refs:` section ensures citations appear in output.
* The `analysis-name` and `class` fields must align with R6 class names.

### 2. User Interface Layout (`.u.yaml`)

**Purpose**: Organize controls, labels, and grouping for the user interface.

**Pattern Example (meddecide/kappaSizeCI.u.yaml)**:

```yaml
- type: label
  title: "Kappa Sample Size"
  text: "Calculate sample size for desired kappa precision."
  footnote: "Based on Donner & Eliasziw (1992)."

- type: group
  title: "Outcome Settings"
  content:
    - type: combobox
      name: outcome
      title: "Number of Outcome Levels"
      values: $(options.outcome.values)

    - type: textbox
      name: props
      title: "Category Proportions"
      placeholder: "e.g., 0.20, 0.80"

- type: group
  title: "Kappa Parameters"
  content:
    - type: textbox
      name: kappa0
      title: "Null Kappa (K0)"

    - type: textbox
      name: kappaL
      title: "Lower Bound (KL)"

    - type: textbox
      name: kappaU
      title: "Upper Bound (KU)"

- type: group
  title: "Study Design"
  content:
    - type: spinner
      name: raters
      title: "Number of Raters"
      min: 2
      max: 10

    - type: textbox
      name: alpha
      title: "Alpha (α)"
      placeholder: "0.05"
```

**Notes**:

* Each control’s `name` must match an option in `.a.yaml`.
* Organize controls into logical groups for clarity.
* Use standard Jamovi control types (`combobox`, `textbox`, `spinner`, `checkbox`, etc.).

### 3. Results Specification (`.r.yaml`)

**Purpose**: Define output objects, rendering functions, visibility conditions, and dependencies.

**Pattern Example (meddecide/kappaSizeCI.r.yaml)**:

```yaml
refs:
  - ClinicoPathJamoviModule
  - kappaSize

output:
  - name: text1
    type: "preformatted"
    title: "Required Sample Size"
    clearWith: [outcome, kappa0, kappaL, kappaU, props, raters, alpha]
    visible: true

  - name: text2
    type: "preformatted"
    title: "Study Explanation"
    clearWith: [outcome, kappa0, kappaL, kappaU, props, raters, alpha]
    visible: true
```

**Pattern Example (meddecide/decision.r.yaml)**:

```yaml
refs:
  - ClinicoPathJamoviModule
  - epiR
  - FaganNomogram
  - pROC

output:
  # Pre-populated Count Table
  - name: cTable
    type: table
    title: "Test vs Gold Standard"
    rows: 0
    columns:
      - name: newtest
        type: text
        title: "Test Result"
      - name: GoldPos
        type: number
        title: "Gold Positive"
      - name: GoldNeg
        type: number
        title: "Gold Negative"
      - name: Total
        type: number
        title: "Total"
    clearWith: [gold, newtest]

  # Single-row summary
  - name: nTable
    type: table
    title: "Basic Counts"
    rows: 1
    columns:
      - name: TotalPop
        type: integer
        title: "Total Population"
      - name: Diseased
        type: integer
        title: "Diseased"
      - name: Healthy
        type: integer
        title: "Healthy"
      - name: TP
        type: integer
        title: "True Positive"
      - name: FP
        type: integer
        title: "False Positive"
      - name: FN
        type: integer
        title: "False Negative"
      - name: TN
        type: integer
        title: "True Negative"
    clearWith: [gold, newtest]

  # Ratio Table (Conditional on CI)
  - name: epirTable_ratio
    type: table
    title: "Effect Estimates (Ratio)"
    rows: 0
    columns:
      - name: statsnames
        type: text
        title: "Statistic"
      - name: est
        type: number
        title: "Estimate"
      - name: lower
        type: number
        title: "Lower 95% CI"
      - name: upper
        type: number
        title: "Upper 95% CI"
    visible: (ci)
    clearWith: [gold, newtest, ci]

  # Numeric Counts Table (Conditional)
  - name: epirTable_number
    type: table
    title: "Effect Estimates (Numbers)"
    rows: 0
    columns:
      - name: statsnames
        type: text
        title: "Statistic"
      - name: value
        type: integer
        title: "Value"
    visible: (ci)
    clearWith: [gold, newtest, ci]

  # Fagan Nomogram Plot
  - name: plot1
    type: image
    title: "Fagan Nomogram"
    renderFun: .plot1
    width: 600
    height: 450
    visible: (fagan)
    requiresData: true
    clearWith: [gold, newtest, fagan]

  # ROC Curve Plot
  - name: plot2
    type: image
    title: "ROC Curve"
    renderFun: .plot2
    width: 600
    height: 450
    visible: (roc)
    requiresData: true
    clearWith: [gold, newtest, roc]
```

**Key Points**:

* **`clearWith`**: Lists options that invalidate the output when changed.
* **`visible`**: Controls conditional display (e.g., `(ci)`, `(roc)`).
* **`renderFun`**: Connects to private plot methods in the R6 class.
* **Jamovi Manifest (`jamovi-module.yml`)**: Must match analysis names and specify version and compatible jmvcore versions.

## R6 Analysis Class Patterns (`.b.R`)

Each agent’s computation is implemented in an R6 class, inheriting from a generated base class (from `.h.R`) that provides active bindings for all `options` and `results`. The `.b.R` file extends this base and implements the analysis logic.

### 1. Structure of the R6 Class

* **Naming**: `MyAnalysisClass` inherits from `MyAnalysisBase` (auto-generated).
* **Guard Clause**: `if (requireNamespace("jmvcore", quietly = TRUE)) { ... }` ensures the Jamovi environment is present.
* **Private List**: Contains methods:

  * **`.init()`**: Pre-configures results.
  * **`.run()`**: Performs main computations.
  * **`.plot1()`, `.plot2()`, etc.**: Generates plots.
  * **Additional Helpers**: e.g., `.prepareData()`, `.computeMetrics()`, `.buildTableRows()`.

**Example Skeleton**:

```r
kappaSizeCIClass <- R6::R6Class(
  "kappaSizeCIClass",
  inherit = kappaSizeCIBase,
  private = list(
    .init = function() {
      # e.g., set initial visibility or pre-populate table rows
    },

    .run = function() {
      # 1. Validate inputs
      # 2. Access inputs: outcome <- self$options$outcome
      # 3. Compute sample size via kappaSize package
      # 4. text1 <- ...; text2 <- ...
      # 5. Populate outputs:
      #    self$results$text1$setContent(text1)
      #    self$results$text2$setContent(text2)
    },

    .plot1 = function(image, ggtheme) {
      # Retrieve data: plotData <- image$state
      # Build ggplot object
      # print(plot)
      TRUE
    }
  )
)
```

### 2. Initialization (`.init`)

* **Pre-populate Tables**: Add rows before computation (e.g., `cTable$addRow(rowKey = "Test Positive", values = list(newtest = "Test Positive"))`).
* **Adjust Visibility**: Hide or show outputs via `self$results$myOutput$setVisible(FALSE)` based on initial conditions (e.g., no data loaded yet).
* **Set Default States**: Pre-compute any values that should persist across runs.

**Example (meddecide/decision.b.R)**:

```r
.decisionClass.init <- function() {
  # Pre-populate cTable with row labels
  cTable <- self$results$cTable
  cTable$addRow(rowKey = 1, values = list(newtest = "Test Positive"))
  cTable$addRow(rowKey = 2, values = list(newtest = "Test Negative"))
  cTable$addRow(rowKey = 3, values = list(newtest = "Total"))
}
```

### 3. Main Analysis (`.run`)

* **Access Inputs**:

  ```r
  outcome <- self$options$outcome
  kappa0   <- self$options$kappa0
  props    <- self$options$props
  raters   <- self$options$raters
  alpha    <- self$options$alpha
  ```
* **Data Validation**: Check `self$data` or required options. If missing, display instructions via a “todo” output and `return()` early.
* **Split, Convert, Compute**: For string inputs (e.g., `props`), split with `strsplit()` and convert to numeric.
* **Call External Packages**: E.g., `kappaSize::CIBinary()`, `epiR::epi.tests()`, `survival::survfit()`, `tableone::CreateTableOne()`.
* **Store Intermediate Results**: For complex or heavy plots, store data in the image’s state:

  ```r
  plotData <- list(Sens = sens, Spec = spec)
  self$results$plot1$setState(plotData)
  ```
* **Populate Results**:

  * Text: `self$results$text1$setContent(text1)`.
  * Table (fixed row): `self$results$irrtable$setRow(rowNo = 1, values = list(Method = "Kappa", Value = kappaVal))`.
  * Table (dynamic rows):

    ```r
    df <- someDataFrame
    for (i in seq_len(nrow(df))) {
      self$results$myTable$addRow(rowKey = i, values = c(df[i, ]))
    }
    ```
* **Clear or Hide Instructions**: After populating outputs, clear the “todo” message:

  ```r
  self$results$todo$setContent("")
  ```

### 4. Plot Rendering (`.plotX`)

* **Retrieve State**:

  ```r
  plotData <- image$state
  ```
* **Construct Plot**: Use ggplot2 or helper functions (e.g., `nomogrammer(plotData$Prevalence, plotData$Sens, ...)`).
* **Apply Theme**: Use `ggtheme` or `jmvcore::theme()` for consistency with Jamovi.
* **Print Plot**: `print(p)` and return `TRUE`.

**Example (meddecide/decision.b.R)**:

```r
.decisionClass.plot1 <- function(image, ggtheme) {
  plotData <- image$state
  p <- nomogrammer(
    prevalence = plotData$Prevalence,
    sens       = plotData$Sens,
    spec       = plotData$Spec,
    plr        = plotData$Plr,
    nlr        = plotData$Nlr
  ) + ggtheme
  print(p)
  TRUE
}
```

### 5. Private Helper Methods

* Encapsulate reusable logic in private methods.
* Common helpers:

  * `.prepareData()`: Subset data, rename variables, handle factor levels.
  * `.computeMetrics()`: Calculate statistics (e.g., sensitivity, specificity, hazard ratios).
  * `.buildTableRows()`: Convert result objects into data frames ready for table insertion.

**Example (jsurvival/survival.b.R)**:

```r
.survivalClass.getData <- function() {
  data <- self$data
  timeVar   <- self$options$time
  eventVar  <- self$options$status
  groupVar  <- self$options$group

  # Convert factor levels if needed
  data[[eventVar]] <- as.numeric(data[[eventVar]] == "Yes")

  # Return list of cleaned data and variable names
  list(
    data    = data,
    timeVar  = timeVar,
    eventVar = eventVar,
    groupVar = groupVar
  )
}

.survivalClass.run <- function() {
  args <- private$.getData()
  survObj <- survival::Surv(time = args$data[[args$timeVar]],
                             event = args$data[[args$eventVar]])
  fit <- survival::survfit(survObj ~ args$data[[args$groupVar]], data = args$data)
  # Populate KM plot state
  self$results$survPlot$setState(list(fit = fit, data = args$data, group = args$groupVar))
  
  # Other outputs: median survival table
  medians <- summary(fit)$table
  for (i in seq_along(medians)) {
    self$results$medianTable$addRow(rowKey = i, values = list(
      Group  = medians$group[i],
      Median = medians$median[i]
    ))
  }
}
```

## End-to-End Examples of Agent Workflow

### Example 1: Kappa Sample Size Calculation (meddecide)

**Context:** Computes required sample size for an inter-rater agreement study.

* **`.a.yaml`**:

  * Defines options: `outcome`, `kappa0`, `kappaL`, `kappaU`, `props`, `raters`, `alpha`.
  * Includes references: `ClinicoPathJamoviModule`, `kappaSize`.

* **`.u.yaml`**:

  * Groups: “Outcome Settings”, “Kappa Parameters”, “Study Design”.
  * Controls: ComboBox `outcome`, TextBoxes `kappa0`, `kappaL`, `kappaU`, Spinner `raters`, TextBox `alpha`.

* **`.r.yaml`**:

  * Outputs: `text1` (Required Sample Size) and `text2` (Study Explanation).
  * Properties: both `visible: true`, `clearWith: [all inputs]`.

* **`kappaSizeCI.b.R`**:

  1. **.init**: (No pre-population needed).
  2. **.run**:

     * Read input values: `outcome`, `kappa0`, `kappaL`, `kappaU`, `props`, `raters`, `alpha`.
     * Convert `props` string to numeric vector: `propsVec <- as.numeric(strsplit(props, ",")[[1]])`.
     * Select appropriate function based on `outcome`: if `"2"`, call `kappaSize::CIBinary()`, else call `kappaSize::CI3Cats()`, etc.
     * Construct results: `text1 <- paste0("Required N = ", res$N)`; `text2 <- paste0("To estimate kappa0 = ", kappa0, " with ...")`.
     * Populate outputs: `self$results$text1$setContent(text1)`; `self$results$text2$setContent(text2[1])`.

**Outcome:** The module displays the computed sample size and an explanatory message illustrating parameter choices.

### Example 2: Decision Analysis (meddecide)

**Context:** Computes diagnostic test accuracy metrics, populates count tables, and renders plots (Fagan nomogram, ROC curve).

* **`.a.yaml`**:

  * Options: `gold`, `newtest` (both variables), `ci` (checkbox), `fagan` (checkbox), `roc` (checkbox).
  * References: `ClinicoPathJamoviModule`, `epiR`, `FaganNomogram`, `pROC`.

* **`.u.yaml`**:

  * Controls grouped under “Data Selection” (variable pickers) and “Options” (checkboxes for CI, Fagan, ROC).

* **`.r.yaml`**:

  * **`cTable`**: Pre-populated with 3 rows in `.init`.
  * **`nTable`**: Single-row summary.
  * **`epirTable_ratio`**: Ratio table (visible if `ci` is true).
  * **`epirTable_number`**: Numeric counts table (visible if `ci` is true).
  * **`plot1`**: Fagan nomogram (render function `.plot1`, visible if `fagan` is true).
  * **`plot2`**: ROC curve (render function `.plot2`, visible if `roc` is true).

* **`decision.b.R`**:

  1. **.init**:

     ```r
     cTable <- self$results$cTable
     cTable$addRow(rowKey = 1, values = list(newtest = "Test Positive"))
     cTable$addRow(rowKey = 2, values = list(newtest = "Test Negative"))
     cTable$addRow(rowKey = 3, values = list(newtest = "Total"))
     ```

  2. **.run**:

     * If `gold` or `newtest` is not selected, display a “todo” instruction and return.
     * Subset data: `df <- self$data[, c(self$options$gold, self$options$newtest)]`.
     * Compute sensitivity and specificity using `epiR::epi.tests()`.
     * Populate `nTable`:

       ```r
       self$results$nTable$setRow(
         1,
         values = list(
           TotalPop = N,
           Diseased  = sum(...),
           Healthy   = sum(...),
           TP        = tp,
           FP        = fp,
           FN        = fn,
           TN        = tn
         )
       )
       ```
     * Populate `cTable`:

       ```r
       self$results$cTable$setRow(
         rowNo = 1,
         values = list(GoldPos = tp, GoldNeg = fp, Total = tp + fp)
       )
       # Similarly for rows 2 and 3
       ```
     * If `ci` is true, compute effect estimates:

       ```r
       epir_ratio_df <- epiR::epi.2by2(..., conf.level = 0.95)
       for (i in seq_len(nrow(epir_ratio_df))) {
         self$results$epirTable_ratio$addRow(rowKey = i, values = c(epir_ratio_df[i, ]))
       }
       # Repeat for epirTable_number
       ```
     * If `fagan` is true, compute Fagan nomogram parameters and store state:

       ```r
       plotData1 <- list(
         Prevalence = prior,
         Sens       = sens,
         Spec       = spec,
         Plr        = plr,
         Nlr        = nlr
       )
       self$results$plot1$setState(plotData1)
       ```
     * If `roc` is true, compute ROC object and store:

       ```r
       rocObj <- pROC::roc(response = ..., predictor = ...)
       self$results$plot2$setState(rocObj)
       ```
     * Clear any “todo” instructions: `self$results$todo$setContent("")`.

  3. **.plot1**:

     ```r
     plotData <- image$state
     p <- nomogrammer(
       prevalence = plotData$Prevalence,
       sens       = plotData$Sens,
       spec       = plotData$Spec,
       plr        = plotData$Plr,
       nlr        = plotData$Nlr
     ) + ggtheme
     print(p)
     TRUE
     ```

  4. **.plot2**:

     ```r
     rocObj <- image$state
     p <- ggroc(rocObj) + ggtheme + ggtitle("ROC Curve")
     print(p)
     TRUE
     ```

### Example 3: Table One (ClinicoPathDescriptives)

**Context:** Generates a descriptive summary table (“Table One”) in multiple formats: tableone, gtsummary, arsenal, or janitor.

* **`.a.yaml`** (excerpt):

  ```yaml
  options:
    - name: vars
      type: variable-list
      title: "Variables to Summarize"

    - name: excl
      type: boolean
      default: false
      title: "Exclude Missing Values"

    - name: sty
      type: list
      values: ["t1", "t2", "t3", "t4"]
      default: "t1"
      title: "Table Style"
  refs:
    - ClinicoPathJamoviModule
    - tableone
    - gtsummary
    - arsenal
    - janitor
  ```

* **`.u.yaml`** (excerpt):

  ```yaml
  - type: label
    title: "Table One"
    text: "Generate descriptive statistics for selected variables."

  - type: variables
    name: vars
    title: "Select Variables (up to 10)"

  - type: checkbox
    name: excl
    title: "Exclude Missing Values"

  - type: combobox
    name: sty
    title: "Table Style"
    values: ["t1", "t2", "t3", "t4"]
    display: ["TableOne", "gtSummary", "Arsenal", "Janitor"]
  ```

* **`.r.yaml`**:

  ```yaml
  refs:
    - ClinicoPathJamoviModule
    - tableone
    - gtsummary
    - arsenal
    - janitor

  output:
    - name: todo
      type: html
      title: "Instructions"
      visible: (vars:empty)

    - name: tablestyle1
      type: preformatted
      title: "TableOne Output"
      visible: (sty:t1)
      clearWith: [vars, excl]

    - name: tablestyle2
      type: html
      title: "gtSummary Output"
      visible: (sty:t2)
      clearWith: [vars, excl]

    - name: tablestyle3
      type: html
      title: "Arsenal Output"
      visible: (sty:t3)
      clearWith: [vars, excl]

    - name: tablestyle4
      type: html
      title: "Janitor Output"
      visible: (sty:t4)
      clearWith: [vars, excl]
  ```

* **`tableone.b.R`**:

  1. **.run**:

     ```r
     if (is.null(self$options$vars) || length(self$options$vars) == 0) {
       todo_msg <- "<ul><li>Select variables...</li></ul>"
       self$results$todo$setContent(todo_msg)
       return()
     }

     self$results$todo$setContent("")

     data <- self$data[, self$options$vars, drop = FALSE]
     if (self$options$excl) data <- jmvcore::naOmit(data)

     style <- self$options$sty
     if (style == "t1") {
       tbl <- tableone::CreateTableOne(vars = self$options$vars, data = data)
       self$results$tablestyle1$setContent(tbl)
     } else if (style == "t2") {
       tbl <- gtsummary::tbl_summary(data = data)
       htmlTbl <- gtsummary::as_kable_extra(tbl)
       self$results$tablestyle2$setContent(htmlTbl)
     } else if (style == "t3") {
       tbl <- arsenal::tableby(~ ., data = data)
       htmlTbl <- arsenal::summary.tbl(by = tbl, text = FALSE)
       self$results$tablestyle3$setContent(htmlTbl)
     } else if (style == "t4") {
       df <- janitor::tabyl(data)
       htmlTbl <- knitr::kable(df, format = "html")
       self$results$tablestyle4$setContent(htmlTbl)
     }
     ```

**Notes**:

* The `todo` output informs users until they select variables.
* Only the chosen style output is visible (via `visible: (sty:tx)`).
* External packages used: tableone, gtsummary, arsenal, janitor.

## Shared Design Conventions and Best Practices

1. **Consistent Naming and Inheritance**

   * R6 classes: `AnalysisClass` inherits from `AnalysisBase`.
   * Base classes auto-generated from YAML provide `self$options` and `self$results`.

2. **Option Access and Types**

   * Access inputs with `self$options$varName` (matching `.a.yaml`).
   * List options return character strings; numeric options return numeric; variable-list returns a vector of column names.
   * Immediately assign to local variables and apply transformations (e.g., `as.integer`, `as.numeric`, `strsplit`).

3. **Result Access and Modification**

   * Text outputs: `self$results$name$setContent(value)`.
   * Fixed-row tables: `self$results$name$setRow(rowNo, values = list(...))`.
   * Dynamic tables: `self$results$name$addRow(rowKey, values = c(...))`.
   * Images/complex objects: `self$results$name$setState(list(...))`.
   * Visibility overrides: `self$results$name$setVisible(TRUE/FALSE)`.

4. **UI Guidance via Outputs**

   * Use a `todo` HTML output to guide users when required inputs are missing.
   * Clear the message once inputs are valid: `self$results$todo$setContent("")`.

5. **ClearWith and Dependencies**

   * List all relevant options in `clearWith` to prevent stale outputs.
   * For images or tables depending on multiple inputs (e.g., `gold`, `newtest`, `ci`), include all in `clearWith`.

6. **Visible Conditions**

   * Use `visible: (option)` or `visible: (option:value)` in `.r.yaml` to control output display.
   * Common patterns: `(ci)`, `(fagan)`, `(roc)`, `(sty:t1)`, `(vars:empty)`.
   * Keeps the interface uncluttered by showing only relevant outputs.

7. **Citing Sources**

   * Add external references in `refs:` under `.a.yaml` or `.r.yaml`.
   * Ensure proper attribution for statistical methods (e.g., `epiR`, `survival`, `kappaSize`, `gtsummary`).

8. **Coding Style and Comments**

   * Use comment headers (e.g., `# ----`) to separate logical sections (data prep, error handling, computations, output).
   * Comment out alternative approaches or TODOs for clarity and future work.
   * Adhere to a consistent code style (tidyverse or base R) as per project guidelines.

9. **Private Helper Functions**

   * Encapsulate reusable logic in private methods (`private$.functionName()`).
   * Examples: `.getData()`, `.computeMetrics()`, `.preparePlotData()`, `.buildTableRows()`.

10. **Storage of Plot Data**

    * Use `image$setState()` in `.run` to store data for plotting.
    * Implement `.plotX()` methods to retrieve and visualize `image$state`.
    * Ensures heavy computations occur once, separated from rendering logic.

## Example Agent Workflow

1. **New Feature: Add Decision Curve Analysis to `meddecide`**

   1. Use `YAMLConfigAgent` to draft the UI (`.u.yaml`) for `computeDecisionCurve`, defining inputs: `data`, `outcome`, `predicted_prob`, `threshold_seq`, `plotROC`.
   2. Use `RFunctionGenAgent` to implement `computeDecisionCurve` in `R/decision_curve.R`: validate inputs, calculate net benefit at each threshold, return a `jmvcore::Table` and a `Plot`.
   3. Use `PlotDesignAgent` to generate a net benefit vs. threshold plot with `ggplot2`, matching Jamovi’s style.
   4. Use `TestQAAgent` to write tests for net benefit computation (using a toy dataset with known expected values).
   5. Use `DocumentationAgent` to add roxygen2 comments and update `.r.yaml` and `.a.yaml` accordingly.
   6. Use `CIIntegrationAgent` to ensure tests run on CI/CD and update the GitHub Actions workflow as needed.
   7. Use `ReleaseManagerAgent` to draft release notes for the new function.

2. **Bug Fix: Handle Single-Level Group Error in `jsurvival`**

   1. Provide the error message and relevant code snippet to `BugTrackerAgent`.
   2. `BugTrackerAgent` diagnoses the issue (single-level grouping) and proposes a guard clause.
   3. Use `RefactorAgent` to insert the guard clause, handle the edge case gracefully, and refactor code for clarity.
   4. Use `TestQAAgent` to write a test ensuring the edge case no longer errors.
   5. Use `DocumentationAgent` to update help text: note that grouping variables must have at least two levels, or else return a descriptive message.

## Best Practices for Crafting Prompts

* **Be Specific**: Include function names, file paths, variable names, and desired behavior.
* **Provide Context**: When modifying existing code, paste relevant snippets or commit history.
* **Set Expectations**: Indicate coding style preferences (e.g., tidyverse vs. base R), testing frameworks, and documentation conventions.
* **Iterate**: If output isn’t as expected, clarify with follow-up prompts, highlighting parts to adjust (e.g., “Use a log scale for the x-axis”).
* **Use Examples**: Provide example inputs and expected outputs when possible.
* **Leverage TDD**: Writing tests first ensures code meets specifications.

---

## Style & Naming Conventions (Jamovi + R)

- **Case**: Use `UpperCamelCase` for R6 classes (e.g., `DecisionClass`), `snake_case` for private helpers (e.g., `private$.compute_metrics`).
- **Options/Results**: Options in `.a.yaml` must map 1:1 to `self$options$...`; result names in `.r.yaml` must match `self$results$...`.
- **Messages**: Prefer actionable errors (what went wrong + how to fix). Use `jmvcore::reject()` for user-facing validation.
- **Dependencies**: Minimize; prefer base R + widely used packages. Gate optional deps with `requireNamespace()`.
- **Docs**: Every user-facing function has roxygen2 with examples; analyses include `refs:` for citations.

## Appendix: Common Jamovi Module Components

1. **jmvcore Basics**:

   * `requirePackage()` to ensure dependencies.
   * `jmvcore::Options` subclasses to collect user options.
   * `jmvcore::Analysis` subclasses that define `init()`, `run()`, and `results`.
   * Use `jmvcore::preprocessData()`, `jmvcore::table()`, and `jmvcore::plot()` for standard functionality.

2. **YAML File Structure**:

   * **`.u.yaml`**: Defines UI elements. Example:

     ```yaml
     - type: choices
       name: group
       label: "Grouping Variable"
       options:
         - var1
         - var2
     - type: integer
       name: alpha
       label: "Significance Level"
       default: 0.05
     ```

   * **`.r.yaml`**: Maps UI options to R function parameters. Example:

     ```yaml
     - function-name: computeSurvival
       parameters:
         group: group
         time: time_col
         status: status_col
     ```

   * **`.a.yaml`**: Registers analyses. Example:

     ```yaml
     - analysis-name: SurvivalAnalysis
       class: AnalysisClass
       package: jsurvival
       requires-data: true
       allows-multiple-dependencies: false
       title: "Survival Analysis"
     ```

3. **Testing Conventions**:

   * Place tests in `tests/testthat/`.
   * Name test files as `test-<function>.R`.
   * Example structure:

     ```r
     test_that("computeSurvival handles single-group edge case", {
       data <- data.frame(time = c(1,2,3), status = c(1,0,1), group = c('A','A','A'))
       expect_error(computeSurvival(data), "group must have at least two levels")
     })
     ```

4. **Plot Export**:

   * Jamovi expects plots returned via `jmvcore::Image$new()` with a `render()` method that returns a `ggplot` object or grid.
   * Example:

     ```r
     results$plot <- jmvcore::Image$new(
       plot = function() {
         p <- ggplot(data, aes(x = ..., y = ...)) + geom_line()
         return(p)
       },
       width = 400,
       height = 300
     )
     ```

---

## Security & PHI Considerations

- **No PHI in examples/tests**: Use synthetic or de-identified data.
- **Reproducibility**: Record package versions in `DESCRIPTION` and lockfiles when possible.
- **Data Handling**: Avoid writing to user disk by default; if needed, ask for a user-selected path via options.
- **Privacy**: When using LLM agents, redact identifiers in prompts and outputs.

## Versioning

This `AGENTS.md` file is versioned at **v1.1.0**. Future updates should follow semantic versioning, reflecting changes to agent responsibilities, new agents, or prompt guidelines.

---

*End of AGENTS.md*
