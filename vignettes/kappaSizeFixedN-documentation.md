# Lowest Expected Value for a fixed sample size - Developer Documentation

## 1. Overview

- **Function**: `kappaSizeFixedN`
- **Title**: Lowest Expected Value for a fixed sample size
- **Module**: `PowerT`
- **Files**:
  - `jamovi/kappaSizeFixedN.u.yaml` - User Interface Definition
  - `jamovi/kappaSizeFixedN.a.yaml` - Options & Schema Definition
  - `jamovi/kappaSizeFixedN.r.yaml` - Results Layout & Tables
  - `R/kappaSizeFixedN.b.R` - Backend Implementation
- **Summary**: For an interobserver agreement study whose number of subjects is already fixed, the lowest kappa the study would still be unable to rule out - the lower bound of the one-sided confidence interval around the anticipated kappa (kappa0); every value below it is excluded. Computed with the kappaSize package. Use kappaSizePower to size a study for a hypothesis test and kappaSizeCI to size it for a target interval width.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `outcome` | `List` | `2` | Number of outcome levels |
| `kappa0` | `Number` | `0.6` | kappa0 |
| `props` | `String` | `0.20, 0.80` | Expected proportion of cases in each category |
| `raters` | `List` | `2` | raters |
| `alpha` | `Number` | `0.05` | alpha |
| `n` | `Integer` | `100` | FALSE |

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

  subgraph Backend[Backend Logic / R/kappaSizeFixedN.b.R]
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
  participant Backend as R Backend (kappaSizeFixedNClass)
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

