# Confidence Interval Approach for the Number of Subjects Required - Developer Documentation

## 1. Overview

- **Function**: `kappaSizeCI`
- **Title**: Confidence Interval Approach for the Number of Subjects Required
- **Module**: `PowerT`
- **Files**:
  - `jamovi/kappaSizeCI.u.yaml` - User Interface Definition
  - `jamovi/kappaSizeCI.a.yaml` - Options & Schema Definition
  - `jamovi/kappaSizeCI.r.yaml` - Results Layout & Tables
  - `R/kappaSizeCI.b.R` - Backend Implementation
- **Summary**: Number of subjects an interobserver agreement study needs for the confidence interval around the anticipated kappa (kappa0) to reach the requested precision - the confidence-interval approach of the kappaSize package. This is NOT a power calculation and tests no hypothesis; use kappaSizePower to size a study for a hypothesis test, and kappaSizeFixedN when the number of subjects is already fixed.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `outcome` | `List` | `2` | Number of outcome level |
| `citype` | `List` | `two_sided` | Confidence Interval Type |
| `kappa0` | `Number` | `0.6` | kappa0 |
| `kappaL` | `Number` | `0.4` | kappaL |
| `kappaU` | `Number` | `0.8` | kappaU |
| `props` | `String` | `0.20, 0.80` | Proportions of outcome level |
| `raters` | `List` | `2` | raters |
| `alpha` | `Number` | `0.05` | alpha |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `notices` | `Html` | `Notes` |  |
| `text1` | `Preformatted` | `Analysis result` |  |
| `text_summary` | `Preformatted` | `Summary` |  |
| `text2` | `Preformatted` | `Study Explanation` |  |

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

  subgraph Backend[Backend Logic / R/kappaSizeCI.b.R]
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
  participant Backend as R Backend (kappaSizeCIClass)
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

