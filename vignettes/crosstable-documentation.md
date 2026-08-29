# Cross Tables - Developer Documentation

## 1. Overview

- **Function**: `crosstable`
- **Title**: Cross Tables
- **Module**: `ExplorationT`
- **Files**:
  - `jamovi/crosstable.u.yaml` - User Interface Definition
  - `jamovi/crosstable.a.yaml` - Options & Schema Definition
  - `jamovi/crosstable.r.yaml` - Results Layout & Tables
  - `R/crosstable.b.R` - Backend Implementation
- **Summary**: Function for making Cross Tables with multiple table styles.  Currently implemented features: - Multiple table styles (arsenal, finalfit, gtsummary, NEJM, Lancet, hmisc) - Test selection (chi-square, Fisher's exact, ANOVA, Kruskal-Wallis; which one   applies depends on the table style and on whether means or medians are shown) - Multiple testing correction (Bonferroni, Holm, Benjamini-Hochberg, Benjamini-Yekutieli) - Variable name safety (special characters, spaces) - Data quality validation warnings  Note: Advanced features (pairwise comparisons, effect sizes, residual analysis, correspondence analysis) are planned but not yet available.

## 1a. Changelog

- **Date**: 2026-08-29
- **Summary**: Comprehensive documentation suite created & verified against active schemas and backend implementation.

## 2. Options Reference (`.a.yaml`)

| Option | Type | Default | Description |
| :--- | :--- | :--- | :--- |
| `data` | `Data` | `NULL` |  |
| `vars` | `Variables` | `NULL` | Dependent variables |
| `group` | `Variable` | `NULL` | Grouping variable |
| `sty` | `List` | `nejm` | Table style |
| `excl` | `Bool` | `FALSE` | Missing-value exclusion (NA) |
| `cont` | `List` | `mean` | Mean vs median |
| `pcat` | `List` | `chisq` | Chi-square vs Fisher's exact test |
| `p_adjust` | `List` | `none` | P-value adjustment method |
| `showSMD` | `Bool` | `FALSE` | Standardized mean differences (balance) |

## 3. Results Definition (`.r.yaml`)

| Output ID | Type | Title | Description |
| :--- | :--- | :--- | :--- |
| `notices` | `Preformatted` | `Important Information` |  |
| `errorNotice` | `Html` | `Error` |  |
| `dataQualityNotice` | `Html` | `Data Quality Warnings` |  |
| `analysisInfo` | `Html` | `Analysis Information` |  |
| `subtitle` | `Preformatted` | ``Cross Table - ${group}`` |  |
| `todo` | `Html` | `To Do` |  |
| `todo2` | `Html` | `Method Note (finalfit)` |  |
| `varNameWarnings` | `Html` | `Variable Name Warnings` |  |
| `tablestyle1` | `Html` | ``Cross Table - ${group}`` |  |
| `tablestyle2` | `Html` | ``Cross Table - ${group}`` |  |
| `tablestyle3` | `Html` | ``Cross Table - ${group}`` |  |
| `tablestyle4` | `Html` | ``Cross Table - ${group}`` |  |
| `qvalueExplanation` | `Html` | `Q-value Explanation` |  |
| `testInformation` | `Html` | `Q-value Information` |  |
| `smdTable` | `Table` | `Standardized Mean Differences (Balance)` |  |

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

  subgraph Backend[Backend Logic / R/crosstable.b.R]
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
  participant Backend as R Backend (crosstableClass)
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

