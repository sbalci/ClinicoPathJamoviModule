# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ClinicoPath is a comprehensive jamovi module for clinicopathological research analysis. It provides statistical analysis tools specifically designed for pathology and clinical research, including survival analysis, decision analysis, descriptive statistics, and specialized plots.

## Core Architecture

### Jamovi Module Structure

This is a jamovi R module with a specific 4-file architecture pattern:

- **`.b.R` files**: Backend implementation classes (e.g., `crosstable.b.R`, `survival.b.R`)
- **`.a.yaml` files**: Analysis definitions with options/parameters (e.g., `crosstable.a.yaml`)
- **`.u.yaml` files**: User interface definitions (e.g., `crosstable.u.yaml`)
- **`.r.yaml` files**: Results/output definitions (e.g., `crosstable.r.yaml`)
- **`.h.R` files**: Auto-generated header files (compiled from .yaml files)

### Key Backend Pattern

All analysis classes inherit from auto-generated base classes and use R6 class system:

```r
crosstableClass <- R6::R6Class(
    "crosstableClass", 
    inherit = crosstableBase,  # Auto-generated from .yaml files
    private = list(
        .init = function() { ... },
        .run = function() { ... }
    )
)
```

### Main Functional Areas

1. **ClinicoPath Descriptives**: Summary statistics, Table One, cross tables, data checking
2. **ClinicoPath Survival**: Survival analysis, Cox regression, Kaplan-Meier, competing risks
3. **meddecide**: Medical decision analysis, ROC curves, sensitivity/specificity, diagnostic tests
4. **JJStatsPlot**: Statistical plots using ggstatsplot wrappers
5. **Patient Follow-Up Plots**: Swimmer plots, waterfall plots, treatment response visualization

## Development Commands

### Testing

```bash
# Run all tests
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "testthat::test_file('tests/testthat/test-roc.R')"
```

### Building and Checking

```bash
# Check package
Rscript -e "devtools::check()"

# Build package
Rscript -e "devtools::build()"

# Install development version
Rscript -e "devtools::install()"
```

### Jamovi Module Development

```bash
# Build jamovi module (.jmo file)
# This requires jamovi development tools
Rscript -e "jmvtools::prepare()"
Rscript -e "devtools::document()"
Rscript -e "jmvtools::install()"

# Install module in jamovi for testing
# Copy .jmo file to jamovi modules directory
```

## Key Dependencies and Patterns

### Core Dependencies

- **jmvcore**: Jamovi module framework
- **R6**: Class system for all analysis classes
- **magrittr**: Pipe operators
- **finalfit**: Survival analysis and medical tables
- **ggstatsplot**: Statistical plotting
- **gtsummary**: Summary tables
- **survival/survminer**: Survival analysis

### Data Processing Pattern

Most modules follow this pattern:

1. Clean variable names with `janitor::clean_names()`
2. Set variable labels with `labelled::set_variable_labels()`
3. Build formulas dynamically based on user inputs
4. Apply appropriate statistical tests
5. Format results for display

### Output Types

- **HTML tables**: Primary output format for most analyses
- **Plots**: ggplot2-based visualizations
- **Text summaries**: Natural language interpretations
- **Export options**: CSV downloads for some modules

## Module Categories by Menu Structure

### Exploration Menu

- **ClinicoPath Descriptives**: `tableone`, `summarydata`, `reportcat`, `benford`
- **ClinicoPath Comparisons**: `crosstable`, `chisqposttest`
- **ClinicoPath Descriptive Plots**: `agepyramid`, `alluvial`, `vartree`, `venn`
- **Patient Follow-Up Plots**: `waterfall`, `swimmerplot`

### Survival Menu

- **ClinicoPath Survival**: `survival`, `survivalcont`, `multisurvival`, `oddsratio`, `singlearm`

### meddecide Menu

- **Agreement**: `agreement`, `icccoeff`
- **Decision**: `decision`, `decisioncalculator`, `nogoldstandard`, `decisioncompare`
- **ROC**: `psychopdaroc`, `roc`
- **Power Analysis**: `kappasizeci`, `kappasizefixedn`, `kappasizepower`

### JJStatsPlot Menu

- **Categorical vs Continuous**: `jjbetweenstats`, `jjwithinstats`, `jjdotplotstats`
- **Categorical vs Categorical**: `jjbarstats`, `jjpiestats`
- **Continuous vs Continuous**: `jjcorrmat`, `jjscatterstats`

## Common Development Tasks

### Adding New Analysis Module

1. Create 4 jamovi files: `.a.yaml`, `.u.yaml`, `.r.yaml`, `.b.R`
2. Add entry to `jamovi/0000.yaml`
3. Implement R6 class inheriting from auto-generated base
4. Add to appropriate menu group/subgroup

### Testing Strategy

- Unit tests in `tests/testthat/`
- Test data in `data/` directory (many .csv, .omv, .rda files)
- Example analyses in jamovi format (.omv files)

### Documentation

- Function documentation in .R files using roxygen2
- Vignettes in `vignettes/` directory using R Markdown/Quarto
- Package website built with pkgdown

## File Structure Notes

- `R/`: Backend R code (.b.R analysis classes, utility functions)
- `jamovi/`: Module definitions (.yaml files)
- `data/`: Example datasets in multiple formats
- `man/`: Auto-generated documentation
- `inst/extdata/`: Additional example data files
- `vignettes/`: Documentation and tutorials
