# Scatter Plot - Developer Documentation

## 1. Overview

- **Function**: `jjscatterstats`
- **Title**: Scatter Plot
- **Module**: `JJStatsPlotT`
- **Files**:
  - `jamovi/jjscatterstats.u.yaml` - User Interface Definition
  - `jamovi/jjscatterstats.a.yaml` - Options & Schema Definition
  - `jamovi/jjscatterstats.r.yaml` - Results Layout & Tables
  - `R/jjscatterstats.b.R` - Backend Implementation
- **Summary**: Wrapper Function for ggstatsplot::ggscatterstats and ggstatsplot::grouped_ggscatterstats to generate scatter plots with correlation analysis and optional marginal distributions.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data` | `Data` | `NULL` |  |
| `dep` | `Variable` | `NULL` | x-axis (First Variable) |
| `group` | `Variable` | `NULL` | y-axis (Second Variable) |
| `grvar` | `Variable` | `NULL` | Split By (Optional) |
| `colorvar` | `Variable` | `NULL` | Color Variable (Optional) |
| `sizevar` | `Variable` | `NULL` | Size Variable (Optional) |
| `shapevar` | `Variable` | `NULL` | Shape Variable (Optional) |
| `alphavar` | `Variable` | `NULL` | Alpha Variable (Optional) |
| `labelvar` | `Variable` | `NULL` | Label Variable (Optional) |
| `showRugPlot` | `Bool` | `FALSE` | Rug plot |
| `marginalType` | `List` | `none` | Marginal Plot Type |
| `smoothMethod` | `List` | `lm` | Smooth Method |
| `typestatistics` | `List` | `parametric` | Statistical Test Type |
| `mytitle` | `String` | `` | Title |
| `xtitle` | `String` | `` | X-Title |
| `ytitle` | `String` | `` | Y-Title |
| `originaltheme` | `Bool` | `FALSE` | Add GGStatsPlot layer |
| `resultssubtitle` | `Bool` | `FALSE` | Statistical results |
| `conflevel` | `Number` | `0.95` | Confidence Level |
| `bfmessage` | `Bool` | `FALSE` | Bayes factor message |
| `k` | `Integer` | `2` | Decimal Places |
| `marginal` | `Bool` | `FALSE` | Marginal histograms |
| `xsidefill` | `String` | `#009E73` | X-axis Marginal Fill Color |
| `ysidefill` | `String` | `#D55E00` | Y-axis Marginal Fill Color |
| `pointsize` | `Number` | `3` | Point Size |
| `pointalpha` | `Number` | `0.4` | Point Transparency |
| `smoothlinesize` | `Number` | `1.5` | Smooth Line Size |
| `smoothlinecolor` | `String` | `blue` | Smooth Line Color |
| `plotwidth` | `Integer` | `600` | Plot Width |
| `plotheight` | `Integer` | `450` | Plot Height |
| `addGGPubrPlot` | `Bool` | `FALSE` | Add ggpubr scatter plot |
| `ggpubrPalette` | `List` | `jco` | ggpubr Color Palette |
| `ggpubrAddCorr` | `Bool` | `FALSE` | Add correlation (ggpubr) |
| `ggpubrCorrMethod` | `List` | `pearson` | Correlation Method (ggpubr) |
| `ggpubrAddSmooth` | `Bool` | `FALSE` | Add smooth line (ggpubr) |
| `showExplanations` | `Bool` | `FALSE` | Explanations |
| `clinicalPreset` | `List` | `custom` | Clinical Preset |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `todo` | `Html` | `To Do` |  |
| `presetInfo` | `Html` | `Clinical Preset Info` |  |
| `explanations` | `Html` | `Explanations` |  |
| `warnings` | `Html` | `Warnings` |  |
| `plot2` | `Image` | ``${dep} vs {group} by {grvar}`` |  |
| `plot` | `Image` | ``${dep} vs {group}`` |  |
| `plot3` | `Image` | ``Plot with Aesthetics - ${dep} vs {group}`` |  |
| `ggpubrPlot` | `Image` | `Publication-Ready Scatter Plot (ggpubr)` |  |
| `ggpubrPlot2` | `Image` | ``Publication-Ready Scatter Plot by ${grvar} (ggpubr)`` |  |

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

  subgraph Backend[Backend Logic / R/jjscatterstats.b.R]
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
  participant Backend as R Backend (jjscatterstatsClass)
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

