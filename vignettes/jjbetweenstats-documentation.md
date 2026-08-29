# Box-Violin Plots to Compare Between Groups - Developer Documentation

## 1. Overview

- **Function**: `jjbetweenstats`
- **Title**: Box-Violin Plots to Compare Between Groups
- **Module**: `JJStatsPlotT`
- **Files**:
  - `jamovi/jjbetweenstats.u.yaml` - User Interface Definition
  - `jamovi/jjbetweenstats.a.yaml` - Options & Schema Definition
  - `jamovi/jjbetweenstats.r.yaml` - Results Layout & Tables
  - `R/jjbetweenstats.b.R` - Backend Implementation
- **Summary**: Wrapper Function for ggstatsplot::ggbetweenstats and ggstatsplot::grouped_ggbetweenstats to generate Box-Violin Plots for comparing continuous variables between groups with statistical annotations.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data` | `Data` | `NULL` |  |
| `dep` | `Variables` | `NULL` | Dependent Variables |
| `group` | `Variable` | `NULL` | Grouping Variable |
| `grvar` | `Variable` | `NULL` | Split By (Optional) |
| `centralityplotting` | `Bool` | `FALSE` | Centrality |
| `centralitytype` | `List` | `parametric` | Centrality Type |
| `typestatistics` | `List` | `parametric` | Type of Statistic |
| `pairwisecomparisons` | `Bool` | `FALSE` | Pairwise comparisons |
| `pairwisedisplay` | `List` | `significant` | Pairwise Display |
| `padjustmethod` | `List` | `holm` | Adjustment Method |
| `effsizetype` | `List` | `biased` | Effect Size Needed for Parametric Tests |
| `mytitle` | `String` | `Between Group Comparison` | Title |
| `xtitle` | `String` | `` | X-Title |
| `ytitle` | `String` | `` | Y-Title |
| `originaltheme` | `Bool` | `FALSE` | Add GGStatsPlot layer |
| `resultssubtitle` | `Bool` | `FALSE` | Statistical results |
| `bfmessage` | `Bool` | `FALSE` | Bayes factor message |
| `k` | `Integer` | `2` | Decimal Places |
| `conflevel` | `Number` | `0.95` | Confidence Level |
| `varequal` | `Bool` | `FALSE` | Equal variances |
| `multiEndpointCorrection` | `List` | `none` | Multiple Endpoint Correction Guidance |
| `plotwidth` | `Integer` | `650` | Plot Width |
| `plotheight` | `Integer` | `450` | Plot Height |
| `colorblindSafe` | `Bool` | `FALSE` | Use colorblind-safe palette |
| `showexplanations` | `Bool` | `FALSE` | Explanations and guides |
| `addGGPubrPlot` | `Bool` | `FALSE` | Add ggpubr plot variant |
| `ggpubrPlotType` | `List` | `boxplot` | ggpubr Plot Type |
| `ggpubrPalette` | `List` | `jco` | ggpubr Color Palette |
| `ggpubrAddStats` | `Bool` | `TRUE` | Add statistical comparisons (ggpubr) |
| `ggpubrAddPoints` | `Bool` | `FALSE` | Add individual points (ggpubr) |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `todo` | `Html` | `To Do` |  |
| `mecGuidance` | `Html` | `` |  |
| `diagnostics` | `Html` | `` |  |
| `clinicalSummary` | `Html` | `` |  |
| `about` | `Html` | `About This Analysis` |  |
| `summary` | `Html` | `Analysis Summary` |  |
| `assumptions` | `Html` | `Statistical Assumptions` |  |
| `interpretation` | `Html` | `Interpretation Guide` |  |
| `report` | `Html` | `Copy-Ready Report Template` |  |
| `plot2` | `Image` | ``Violin Plot by ${grvar}`` |  |
| `plot` | `Image` | `Violin Plot` |  |
| `ggpubrPlot` | `Image` | `Publication-Ready Plot (ggpubr)` |  |
| `ggpubrPlot2` | `Image` | ``Publication-Ready Plot by ${grvar} (ggpubr)`` |  |

## 4. Architecture & Data Flow Diagram

```mermaid
flowchart TD
  subgraph UI[jamovi UI / .u.yaml]
    U1[User Input & Variables]
    U2[Analysis Settings & Controls]
  end

  subgraph Opts[Options Schema / .a.yaml]
    O1[Options Parsing & Types]
    O2[Default Value Validation]
  end

  subgraph Backend[Backend Logic / R/jjbetweenstats.b.R]
    B1[Input Validation & Data Sanitization]
    B2[Statistical Computation Engine]
    B3[Result Objects Formatting]
  end

  subgraph Res[Results Schema / .r.yaml]
    R1[Summary & Statistics Tables]
    R2[Visual Plots & Graphics]
    R3[Clinical Interpretation & Notices]
  end

  U1 --> O1
  U2 --> O2
  O1 --> B1
  O2 --> B1
  B1 --> B2
  B2 --> B3
  B3 --> R1
  B3 --> R2
  B3 --> R3
```

## 5. Execution Sequence

```mermaid
sequenceDiagram
  autonumber
  actor User as Clinician / Analyst
  participant UI as jamovi Interface
  participant Backend as R Backend (jjbetweenstatsClass)
  participant Engine as Statistical Packages
  participant Results as Results View

  User->>UI: Selects variables and options
  UI->>Backend: Dispatches .run() with options payload
  Backend->>Backend: Validates observations & factor levels
  Backend->>Engine: Computes statistical models / visual layers
  Engine-->>Backend: Returns model estimates & graphics
  Backend->>Results: Populates tables, charts, and notices
  Results-->>User: Displays formatted tables & interactive plots
```

## 6. Change Impact & Safety Guidelines

- **Data Filtering**: Ensure observations with missing values are handled gracefully according to analysis options.
- **Formula Conflicts**: Use isolated environment calls or base formula methods when interacting with `ggstatsplot` or formula parsers.
- **Safe Deparsing**: Use `deparse(val)` in syntax generation (`asSource()`) to escape column names with spaces or special symbols.

