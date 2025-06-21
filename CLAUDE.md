# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

ClinicoPath is a comprehensive jamovi module for clinicopathological research analysis. It provides statistical analysis tools specifically designed for pathology and clinical research, including survival analysis, decision analysis, descriptive statistics, and specialized plots. The project serves as an umbrella package that coordinates multiple sub-modules distributed across separate repositories.

## Core Architecture

### Jamovi Module Structure

This is a jamovi R module with a specific 4-file architecture pattern that must be followed for all analyses:

- **`.b.R` files**: Backend implementation classes (e.g., `crosstable.b.R`, `survival.b.R`, `decisiongraph.b.R`)
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
3. **meddecide**: Medical decision analysis, ROC curves, sensitivity/specificity, diagnostic tests, decision trees with Markov chain modeling
4. **JJStatsPlot**: Statistical plots using ggstatsplot wrappers
5. **Patient Follow-Up Plots**: Swimmer plots, waterfall plots, treatment response visualization

### Advanced Decision Analysis Architecture

The `decisiongraph` module supports both traditional decision trees and advanced Markov chain models:

**Decision Trees**: One-time decisions with immediate outcomes

- Acute medical conditions (surgery vs. conservative treatment)
- Emergency decisions with clear cost/utility trade-offs
- Point-in-time cost-effectiveness analysis

**Markov Chain Models**: Long-term disease progression modeling

- Chronic disease management with multiple health states
- Transition probability matrices for state changes over time
- Cohort trace analysis with discounted cost-effectiveness calculations
- Multi-cycle analysis for lifetime economic evaluations

The module automatically handles different analysis types based on `treeType` option (`simple`, `costeffectiveness`, `markov`).

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

### Adding a new function

```r
Rscript -e "jmvtools::addAnalysis(name = '<newfunctionname>')"
# This will create the necessary .b.R, .a.yaml, .u.yaml, and .r.yaml files
# and update the module structure accordingly.
# Then update these files.
```

### .u.yaml File Structure

The `.u.yaml` file defines the user interface for the analysis module. It includes:

- **title**: The name of the analysis module
- **description**: A brief description of the analysis
- **options**: List of user-configurable options (e.g., variables, statistical tests)
- **results**: Output definitions, including tables and plots

`compilerMode:` is either `aggressive`or `tame`. If aggressive it generates automatically from the `.a.yaml` file, if tame it uses specific changes and organisation defined by the user in the `.u.yaml` file.

### .a.yaml File Structure

The `.a.yaml` file defines the analysis parameters and options. It includes:

- **name**: Unique identifier for the analysis
- **title**: Display name in the jamovi interface
- **description**: Detailed description of the analysis
- **options**: List of parameters that users can configure (e.g., variables, statistical methods)

# TODO add more details about the .a.yaml options, permitted suggested

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
- **Decision**: `decision`, `decisioncalculator`, `decisiongraph`, `nogoldstandard`, `decisioncompare`
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

## Module Update Workflow

### Automated Module Distribution

This project uses `_updateModules.R` script to synchronize code across 4 separate repositories:

- **Main repository**: ClinicoPathJamoviModule (umbrella package)
- **jjstatsplot**: Statistical plotting functions using ggstatsplot
- **meddecide**: Medical decision analysis tools (includes decision trees, ROC analysis)
- **jsurvival**: Survival analysis functions
- **ClinicoPathDescriptives**: Descriptive statistics and summary tables

The script automatically:

1. Updates version numbers across all DESCRIPTION and YAML files
2. Copies module files (`.b.R`, `.a.yaml`, `.u.yaml`, `.r.yaml`) to appropriate repos
3. Updates documentation and installs modules
4. Commits changes with consistent commit messages

### Version Management

- Version format: `X.Y.Z.BUILD` (e.g., 0.0.3.24)
- Date format: `YYYY-MM-DD`
- Versions are updated centrally and distributed to all sub-modules
- Each module maintains 3-part semantic version in `.a.yaml` files

### Module Distribution Strategy

**WIP vs Release Modes:**

- WIP mode: Limited module sets for testing
- Release mode: Full module distribution
- Controlled by `WIP` flag in update script

### Test Data Generation

- Test data generation scripts in `data-raw/` directory (e.g., `decisiontree_testdata.R`)
- CSV versions automatically created in `inst/extdata/`
- Comprehensive datasets covering all module functionalities
- Edge case testing data for robust validation
- Specialized datasets for decision analysis:
  - Medical treatment comparisons (acute conditions)
  - Pharmaceutical cost-effectiveness studies
  - Disease progression modeling (chronic conditions)
  - Screening program evaluations

## Testing and Quality Assurance

### Test Structure

- Unit tests in `tests/testthat/` directory
- Test data provided in multiple formats (`.csv`, `.omv`, `.rda`)
- Example analyses embedded in jamovi files (`.omv`)
- Key test files: `test-decision.R`, `test-roc.R`, `test-decisiongraph.R`
- Manual testing scripts for complex modules (decision trees, Markov models)
- Performance validation for computationally intensive analyses

### Data Management

- Example datasets in `data/` directory with multiple format support
- Comprehensive test data generation scripts in `data-raw/`
- Each sub-module receives relevant subset of example data
- Extensive test datasets for decision tree analysis:
  - `basic_decision_data`: Surgery vs medical treatment comparisons
  - `markov_decision_data`: Multi-state disease progression models
  - `pharma_decision_data`: Pharmaceutical cost-effectiveness studies
  - `screening_decision_data`: Cancer screening program analysis
  - `appendicitis_decision_tree.csv`: Acute treatment decision example
  - `heart_disease_markov.csv`: Chronic disease Markov model example
  - `minimal_test_data`: Basic functionality testing
  - `edge_case_data`: Error handling and boundary conditions

## Project Distribution Strategy

### Multi-Repository Architecture

The main ClinicoPathJamoviModule repository serves as an umbrella package that coordinates four specialized sub-modules:

1. **ClinicoPathDescriptives**: Basic statistical summaries and descriptive analysis
2. **jsurvival**: Survival analysis (Kaplan-Meier, Cox regression, competing risks)
3. **meddecide**: Medical decision analysis (ROC curves, decision trees, diagnostic tests)
4. **jjstatsplot**: Statistical plotting using ggstatsplot framework

### Installation Methods

- **jamovi Library**: Direct installation through jamovi's module library
- **Sideload**: Manual `.jmo` file installation for development versions
- **R Package**: Direct installation via `devtools::install_github()`
- **Development**: Local builds using `_updateModules.R` script

### Version Synchronization

All sub-modules maintain synchronized version numbers (currently 0.0.3.24) through the automated update script, ensuring compatibility across the ecosystem.

## Special Development Considerations

### Decision Analysis Module Development

When working on decision tree or Markov chain analyses:

1. **Dual Architecture Support**: The `decisiongraph` module handles both decision trees and Markov models based on the `treeType` option
2. **Mathematical Validation**: Transition matrices must be stochastic (rows sum to 1.0)
3. **Performance Considerations**: Markov models with large state spaces or long time horizons require optimization
4. **Extensive Test Data**: Use specialized test datasets that cover realistic medical scenarios

### Module Synchronization Workflow

Use `_updateModules.R` script for coordinated updates across repositories:

```bash
# Update version and synchronize all modules
Rscript _updateModules.R
```

This script:

- Updates version numbers across all DESCRIPTION and YAML files
- Distributes module files to appropriate sub-repositories
- Handles WIP vs release mode configurations
- Maintains version consistency across the ecosystem

### Example Analysis Development

The `examples/` directory contains comprehensive demonstrations:

- `decision_tree_examples.R`: Complete comparison of decision trees vs Markov chains
- `analysis_interpretation_guide.md`: Clinical interpretation guide
- `jamovi_workflow_example.R`: Step-by-step jamovi usage instructions

### Testing Complex Modules

For computationally intensive modules (like Markov models):

1. Use manual testing scripts when unit tests are insufficient
2. Validate mathematical properties (e.g., matrix properties, discounting)
3. Test with multiple realistic datasets
4. Performance profiling for large models

# important-instruction-reminders

Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
