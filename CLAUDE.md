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
Rscript -e "devtools::install(quick=TRUE)"

# Test vignette rendering
Rscript -e "pkgdown::build_articles()"
```

### Jamovi Module Development

```bash
# Build jamovi module (.jmo file) - CRITICAL WORKFLOW
Rscript -e "jmvtools::prepare()"
Rscript -e "devtools::document()"
Rscript -e "jmvtools::install()"

# Install module in jamovi for testing
# Copy .jmo file to jamovi modules directory
```

**CRITICAL**:

- After adding a new function, always ensure `jmvtools::prepare()` and `devtools::document()` run without errors to verify module compilation success
- After adding a new vignette, always ensure `pkgdown::build_articles()` run without errors to verify vignette, data, and function success
- Before closing any GitHub issue, always ensure `jmvtools::prepare()` and `devtools::document()` run without errors to verify module compilation success

## Function Parameter Architecture

### Common Parameter Patterns

Functions must include ALL required parameters, even if set to NULL:

```r
# vartree function requires complete parameter list
vartree(
  data = data,
  vars = variables,
  percvar = NULL,
  percvarLevel = NULL,
  summaryvar = NULL,
  prunebelow = NULL,
  pruneLevel1 = NULL,
  pruneLevel2 = NULL,
  follow = NULL,
  followLevel1 = NULL,
  followLevel2 = NULL,
  excl = FALSE,
  vp = TRUE,
  horizontal = FALSE,
  sline = TRUE,
  varnames = FALSE,
  nodelabel = TRUE,
  pct = FALSE
)
```

### Function-Specific Parameter Requirements

- **agepyramid**: Use `gender = "Sex", female = "Female"` (not `sex = "Sex"`)
- **crosstable**: Use `vars` and `group` parameters (not `rows/cols`)
- **jjstatsplot functions**: Always include `grvar` parameter (can be NULL)
- **summarydata**: Requires `date_vars` and `grvar` parameters
- **alluvial**: Must include `condensationvar` parameter
- **venn**: Use `var1`, `var2`, `var3`, `var4` with corresponding `var1true`, `var2true`, etc.
- **vartree**: Must include ALL level parameters (`followLevel1`, `followLevel2`, etc.)

### Deprecated Parameters

Never use these parameters (removed from functions):

- `title`, `subtitle`, `mytitle` - Not supported in most functions
- `total`, `percentages` - Removed from crosstable
- `group_comparisons` - Use other tableone parameters instead

## Data Architecture

### Included Datasets

- **histopathology**: Main example dataset (250 patients, 38 variables)
- **BreastCancer**: Wisconsin dataset for classification
- **colon**: Colon cancer survival data
- **melanoma**: Melanoma survival data
- **treatmentResponse**: Oncology treatment response data
- **rocdata**: ROC analysis examples

### Dataset Pattern

All datasets include comprehensive variables for testing function parameters:

- Demographics (Age, Sex, Race)
- Pathological features (Grade, TStage, LVI, PNI)
- Outcomes (Death, Outcome, OverallTime)
- Biomarkers (MeasurementA, MeasurementB)

## Vignette Organization

### Vignette Requirements

- Each function must have a comprehensive dataset for testing all arguments
- Vignettes should be explanatory for clinicians
- Use `pkgdown::build_articles()` to test rendering
- All function calls must use correct, complete parameter lists
- **When rendering vignette files if outputs or artefacts are generated like figures, they should be under vignettes folder**

### Vignette Naming Convention

- Vignette names follow a structured format: `{domain}-{number}-{EXPLANATORY_HEADING}`
- Domains include: `general`, `clinicopath-descriptives`, `jsurvival`, `meddecide`, `jjstatsplot`
- Examples:
  - `general-03-BBC_STYLE_IMPLEMENTATION.md`
  - `general-04-ECONOMIST_FONTS_INSTALLATION.md`
  - `general-05-QUICK_FONT_SETUP.md`
  - `meddecide-06-DECISION_TREE_MODULE_SUMMARY.md`
  - `clinicopath-descriptives-01-tableone.qmd`
  - `clinicopath-descriptives-02-summarydata.qmd`
  - `clinicopath-descriptives-03-reportcat.qmd`
  - `clinicopath-descriptives-04-benford.qmd`
  - `clinicopath-descriptives-05-alluvial.qmd`

## Key Dependencies

### Core Dependencies

- **jmvcore**: jamovi core framework
- **R6**: Object-oriented programming
- **magrittr**: Pipe operators

### Analysis Packages

- **survival, survminer**: Survival analysis
- **ggstatsplot**: Statistical plotting
- **tableone, gtsummary**: Table generation
- **pROC, cutpointr**: ROC analysis
- **arsenal, janitor**: Data manipulation

### Visualization Packages

- **ggplot2**: Core plotting
- **alluvial, ggalluvial**: Alluvial diagrams
- **ggvenn**: Venn diagrams
- **vtree**: Variable trees

## Module Menu Organization

### jamovi Menu Structure

- **Exploration**: Descriptive analysis, cross-tables, visualizations
- **Survival**: Survival analysis, Cox regression, person-time analysis
- **meddecide**: Medical decision analysis, ROC curves, diagnostic tests
- **JJStatsPlot**: Statistical plots and visualizations

### Analysis Distribution

- **170+ analysis functions** across 5 main functional areas
- Each analysis has 4-file jamovi structure (.a.yaml, .b.R, .u.yaml, .r.yaml)
- Auto-generated .h.R header files from yaml definitions

## Environment Configuration

- **Pandoc**: Version 3.4 available via RStudio at `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64`
- **R Version**: Requires R >= 4.1.0
- **jamovi**: Module version 0.0.3.32, requires jamovi >= 1.8.0

## Development Reminders

### Repository Management

- Do not rename pkgnet-report users-of-clinicopath module-development-jamovi vignettes

## Code Analysis Tools

### Using Gemini CLI for Large Codebase Analysis

When analyzing large codebases or multiple files that might exceed context limits, use the Gemini CLI with its massive
context window. Use `gemini -p` to leverage Google Gemini's large context capacity.

### File and Directory Inclusion Syntax

Use the `@` syntax to include files and directories in your Gemini prompts. The paths should be relative to WHERE you run the
  gemini command:

#### Examples

**Single file analysis:**
gemini -p "@src/main.py Explain this file's purpose and structure"

Multiple files:
gemini -p "@package.json @src/index.js Analyze the dependencies used in the code"

Entire directory:
gemini -p "@src/ Summarize the architecture of this codebase"

Multiple directories:
gemini -p "@src/ @tests/ Analyze test coverage for the source code"

Current directory and subdirectories:
gemini -p "@./ Give me an overview of this entire project"

Or use --all_files flag

gemini --all_files -p "Analyze the project structure and dependencies"

### Implementation Verification Examples

Check if a feature is implemented:
gemini -p "@src/ @lib/ Has dark mode been implemented in this codebase? Show me the relevant files and functions"

Verify authentication implementation:
gemini -p "@src/ @middleware/ Is JWT authentication implemented? List all auth-related endpoints and middleware"

Check for specific patterns:
gemini -p "@src/ Are there any React hooks that handle WebSocket connections? List them with file paths"

Verify error handling:
gemini -p "@src/ @api/ Is proper error handling implemented for all API endpoints? Show examples of try-catch blocks"

Check for rate limiting:
gemini -p "@backend/ @middleware/ Is rate limiting implemented for the API? Show the implementation details"

Verify caching strategy:
gemini -p "@src/ @lib/ @services/ Is Redis caching implemented? List all cache-related functions and their usage"

Check for specific security measures:
gemini -p "@src/ @api/ Are SQL injection protections implemented? Show how user inputs are sanitized"

Verify test coverage for features:
gemini -p "@src/payment/ @tests/ Is the payment processing module fully tested? List all test cases"

### When to Use Gemini CLI

Use gemini -p when:

- Analyzing entire codebases or large directories
- Comparing multiple large files
- Need to understand project-wide patterns or architecture
- Current context window is insufficient for the task
- Working with files totaling more than 100KB
- Verifying if specific features, patterns, or security measures are implemented
- Checking for the presence of certain coding patterns across the entire codebase

### Important Notes

- Paths in @ syntax are relative to your current working directory when invoking gemini
- The CLI will include file contents directly in the context
- No need for --yolo flag for read-only analysis
- Gemini's context window can handle entire codebases that would overflow Claude's context
- When checking implementations, be specific about what you're looking for to get accurate results

## YAML File Development Patterns

### Argument Configuration Guidelines

- **Default NULL Pattern**: For optional arguments in `.a.yaml` files, explicitly set `default: NULL`
- **Example Configuration**:
  - Add `default: NULL` for optional variables
  - Specify `allowNone: true` for level-based parameters
  - Use `suggested` and `permitted` to control variable type selection

### Specific YAML Configuration Examples

- **Percentage Variable**:

  ```yaml
  - name: percvar
    title: Variable for Percentage
    type: Variable
    suggested: [ ordinal, nominal ]
    permitted: [ factor ]
    default: NULL

  - name: percvarLevel
    title: Level
    type: Level
    variable: (percvar)
    allowNone: true

  - name: summaryvar
    title: Continuous Variable for Summaries
    type: Variable
    suggested: [ continuous ]
    permitted: [ numeric ]
    default: NULL
  ```

### Key Recommendations

- Always provide clear, descriptive `title` for each parameter
- Use `suggested` and `permitted` to guide appropriate variable selection
- Set `default: NULL` for optional parameters
- Use `allowNone: true` for optional level-based parameters

## Development Workflow Memories

### Testing Errors and Piping to Claude

- Use bash command to test errors and pipe them to claude:

```bash
RSTUDIO_PANDOC="/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64" \
Rscript -e 'tryCatch(pkgdown::build_articles(), error = function(e) cat("ERROR:", conditionMessage(e), "\n"))' | claude
```

### Function Development Workflow

- Checking function step by step:
  - read .b.R file and associated yaml files for each function
  - find relevant vignettes and documentation for this function
  - check if the function has appropriate data to test it
    - if not, create a test dataset
  - check if the function has appropriate vignette to explain it
    - if not, create an explanatory vignette
  - check if the data, vignettes and documentation reflect all the features of the function
    - if not, update the data, vignettes and documentation accordingly
  - run `jmvtools::prepare()` and `devtools::document()` to see if the function compiles correctly
    - if not, fix the issues
  - run `devtools::install(quick=TRUE);pkgdown::build_articles()` to see if the vignettes render correctly
    - if not, fix the issues
  - check if the function is tested in the tests folder
    - if not, write tests for the function
  - Fix any issues found in the process

### YAML Development Memories

- Add default: NULL to optional variables
- Required variables should not be with default: NULL

## Checkpoint Pattern in Jamovi Analyses

### Using Checkpoints in Jamovi

When the user updates any analysis option, results are streamed incrementally rather than waiting for the entire analysis to finish. This is handled by calling `private$.checkpoint()`, which:
    1. Emits Progress
    Pushes whatever results have been computed so far back to Jamovi, so the user sees the table fill in gradually.
    2. Detects Changes
    Checks whether the user has modified any settings since the last checkpoint. If so, it aborts the current run and restarts with the new parameters—avoiding wasted computation on analyses the user no longer wants.

For example, in the non-parametric ANOVA, if you change an option while pairwise comparisons are still running, the remaining tests are skipped immediately when you hit your next checkpoint.

To use it:

```r
# Push results and check for user changes:
private$.checkpoint()   

# If you only want to poll for changes without re-pushing identical results:
private$.checkpoint(flush = FALSE)
```

**Best Practice**:

- Don't sprinkle checkpoints everywhere—most R code returns almost instantly, so checkpoints would have no effect and only clutter your code.
- Place checkpoints immediately before any expensive operation.
- In the ANOVA example, only the `pSDCFlig()` calls take significant time, so that's where checkpoints belong.

Example Implementation:

```r
for (pair in pairs) {
    if (table$getCell(rowKey = pair, 'W')$isEmpty) {

        table$setStatus('running')
        private$.checkpoint()

        pairData <- list(sdata[[pair[1]]], sdata[[pair[2]]])
        result   <- pSDCFlig(pairData, method = "Asymptotic", n.g = nGroups)

        table$setRow(
            rowKey = pair,
            list(
                p1 = pair[1],
                p2 = pair[2],
                W  = result$obs.stat,
                p  = result$p.val
            )
        )

        table$setStatus('complete')
    }
}
```

## Internationalization and Weblate Translation

### Integrating a jamovi Module with the Weblate Translation System

To enable internationalization and integrate your jamovi module with the Weblate translation platform, follow these steps:

1. Prepare the Code for Translation
   • In the NAMESPACE file, include:

importFrom(jmvcore, .)

 • Wrap all translatable strings in your *.b.R files using the . function:

.("Your translatable string here")

 • Lines defined in YAML files will be handled and transferred automatically.

2. Generate and Update Translation Files
   • Run the following command to create the initial English translation file:

jmvtools::i18nCreate("en")

This will generate an en.po file in the jamovi/i18n/ directory.

 • After any changes to the module that affect text, update the translation template:

jmvtools::i18nUpdate("en")

3. Add Additional Languages (Optional, Without Weblate)
   • You can manually create and update translation files for other languages:

jmvtools::i18nCreate("tr")   # Turkish
jmvtools::i18nUpdate("tr")

4. Prepare for Weblate Integration
   • Copy the generated en.po file and rename it to catalog.pot.
   • In the catalog.pot file, set the language header line to:

Language: c\n

5. Set Up a GitHub Repository for Translations
   • Create a new repository named `<modulename>`-i18n.
   • Add a README.md and a license file.
   • Upload the catalog.pot file to this repository.
6. Configure GitHub Repository for Weblate
   • In your `<modulename>`-i18n repository:
   • Go to Settings → Collaborators → Add People, and add Weblate (bot).
   • Then go to Settings → Webhooks → Add webhook:
   • Payload URL: [https://hosted.weblate.org/hooks/github/](https://hosted.weblate.org/hooks/github/)
   • Complete the webhook creation.
7. Finalize Integration
   • Contact the jamovi development team and ask them to add your `<modulename>`-i18n repository to Weblate.

## Development Best Practices

### R Package Structure Best Practices

- **R/**: Function definitions only
- **data-raw/**: Data generation scripts (run manually when needed)
- **data/**: Final .rda datasets (committed to repository)

## Development Workflow Memories

- Inside .run function if you are calling another function from private group, call it as `private$.functionName()`

## Development Memories

### Warnings and File Management

- **do not change these files: .Rd, .h.R, 00jmv.R. They are auto-generated during documentation and installation.**
