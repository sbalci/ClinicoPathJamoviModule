# asSource() Batch Fix Template

## Overview

This template provides a systematic approach to add `asSource()` methods to jamovi functions that are missing variable name escaping. This fixes the R syntax generation error when variable names contain spaces or special characters.

**Status**: 5 of 187 functions completed (2.7%)
- ✅ Completed: agreement, tableone, crosstable, pathagreement, conttables, waterfall
- ⏳ Remaining: ~181 functions

## Problem

When variable names contain spaces (e.g., "Rater 1", "Patient ID"), the generated R syntax fails with:
```
Error in parse(text = x, srcfile = src): <text>:5:23: unexpected numeric constant
vars = vars(Rater 1, Rater 2, Rater 3)
                     ^
```

## Solution Pattern

Add `asSource()` public method to escape variable names with backticks.

## Implementation Steps

### 1. Identify Variable Options

Check the `.a.yaml` file to identify which variable options the function uses:
```bash
grep -E '^\s*-\s*name:\s*(vars|dep|grouping|outcome|group|rows|cols|responseVar|etc)' jamovi/FUNCTION.a.yaml
```

Common patterns:
- `vars` - Variable list (most common)
- `dep` - Dependent variable
- `grouping` or `group` - Grouping variable
- `rows`, `cols` - Contingency table variables
- Custom names like `responseVar`, `groupVar`, etc.

### 2. Check Current Structure

Verify if the function already has a public list:
```bash
grep -E '^\s*(private|public)\s*=\s*list\(' R/FUNCTION.b.R
```

Read the end of the file to see where to add the public list.

### 3. Add asSource() Method

#### Pattern A: Single Variable List (vars)

**Example**: tableone, pathagreement, agreement

```r
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for FUNCTION analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            vars <- self$options$vars
            if (is.null(vars) || length(vars) == 0)
                return('')

            # Escape variable names that contain spaces or special characters
            vars_escaped <- sapply(vars, function(v) {
                if (!is.null(v) && !identical(make.names(v), v))
                    paste0('`', v, '`')
                else
                    v
            })

            # Build vars argument
            vars_arg <- paste0('vars = c(', paste(sapply(vars_escaped, function(v) paste0('"', v, '"')), collapse = ', '), ')')

            # Get other arguments using base helper (if available)
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Build complete function call
            paste0('ClinicoPath::FUNCTION(\n    data = data,\n    ',
                   vars_arg, args, ')')
        }
    ) # End of public list
) # End of R6Class definition
```

#### Pattern B: Multiple Single Variables (vars + group)

**Example**: crosstable

```r
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for FUNCTION analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            vars <- self$options$vars
            group <- self$options$group

            if (is.null(vars) || length(vars) == 0 || is.null(group))
                return('')

            # Escape vars
            vars_escaped <- sapply(vars, function(v) {
                if (!is.null(v) && !identical(make.names(v), v))
                    paste0('`', v, '`')
                else
                    v
            })

            # Escape group
            group_escaped <- if (!is.null(group) && !identical(make.names(group), group)) {
                paste0('`', group, '`')
            } else {
                group
            }

            # Build arguments
            vars_arg <- paste0('vars = c(', paste(sapply(vars_escaped, function(v) paste0('"', v, '"')), collapse = ', '), ')')
            group_arg <- paste0('group = "', group_escaped, '"')

            # Get other arguments
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Build complete function call
            paste0('ClinicoPath::FUNCTION(\n    data = data,\n    ',
                   vars_arg, ',\n    ', group_arg, args, ')')
        }
    ) # End of public list
) # End of R6Class definition
```

#### Pattern C: Custom Variable Names (rows, cols, counts)

**Example**: conttables

```r
    ), # End of private list
    public = list(
        #' @description
        #' Generate R source code for FUNCTION analysis
        #' @return Character string with R syntax for reproducible analysis
        asSource = function() {
            rows <- self$options$rows
            cols <- self$options$cols
            counts <- self$options$counts

            if (is.null(rows) || is.null(cols))
                return('')

            # Escape variable names
            rows_escaped <- if (!is.null(rows) && !identical(make.names(rows), rows)) {
                paste0('`', rows, '`')
            } else {
                rows
            }

            cols_escaped <- if (!is.null(cols) && !identical(make.names(cols), cols)) {
                paste0('`', cols, '`')
            } else {
                cols
            }

            counts_escaped <- if (!is.null(counts) && !identical(make.names(counts), counts)) {
                paste0('`', counts, '`')
            } else {
                counts
            }

            # Build arguments
            rows_arg <- paste0('rows = "', rows_escaped, '"')
            cols_arg <- paste0('cols = "', cols_escaped, '"')

            counts_arg <- ''
            if (!is.null(counts)) {
                counts_arg <- paste0(',\n    counts = "', counts_escaped, '"')
            }

            # Get other arguments
            args <- ''
            if (!is.null(private$.asArgs)) {
                args <- private$.asArgs(incData = FALSE)
            }
            if (args != '')
                args <- paste0(',\n    ', args)

            # Build complete function call
            paste0('ClinicoPath::FUNCTION(\n    data = data,\n    ',
                   rows_arg, ',\n    ', cols_arg, counts_arg, args, ')')
        }
    ) # End of public list
) # End of R6Class definition
```

### 4. Verify Syntax

Always verify R syntax after adding asSource():
```bash
Rscript -e "tryCatch({ source('R/FUNCTION.b.R', echo=FALSE); cat('✅ PASSED\n') }, error = function(e) { cat('❌ ERROR:', conditionMessage(e), '\n') })"
```

## Batch Processing Script

For automating multiple functions:

```bash
#!/bin/bash
# batch_add_asSource.sh

FUNCTIONS=(
    "raincloud"
    "lollipop"
    "jjwithinstats"
    "jwaffle"
    "jjridges"
    # ... add more function names
)

for func in "${FUNCTIONS[@]}"; do
    echo "Processing $func..."

    # 1. Identify variable options
    echo "=== Variable options ==="
    grep -E '^\s*-\s*name:\s*(vars|dep|grouping|outcome|group)' "jamovi/${func}.a.yaml" | head -5

    # 2. Check if asSource exists
    if grep -q "asSource.*=.*function" "R/${func}.b.R"; then
        echo "⚠️  asSource already exists in ${func}"
    else
        echo "❌ Missing asSource in ${func}"
    fi

    echo ""
done
```

## Completed Functions (Reference)

1. **agreement.b.R** - Pattern A (vars) + .sourcifyOption()
2. **tableone.b.R** - Pattern A (vars)
3. **crosstable.b.R** - Pattern B (vars + group)
4. **pathagreement.b.R** - Pattern A (vars)
5. **conttables.b.R** - Pattern C (rows + cols + counts) + .sourcifyOption()
6. **waterfall.b.R** - Pattern C (responseVar + groupVar)

## Priority Queue (Suggested Order)

High-priority functions based on usage frequency and importance:

### Statistical Analysis (High Priority)
1. ✅ tableone - Common descriptive statistics
2. ✅ crosstable - Cross-tabulation
3. ✅ conttables - Contingency tables
4. conttablespaired - Paired contingency tables
5. statsplot2 - Statistical plots
6. correlation (jcorrelation) - Correlation analysis

### Agreement/Diagnostic (High Priority)
7. ✅ pathagreement - Pathology agreement
8. ✅ agreement - General agreement
9. pathologyagreement - Another agreement variant

### Survival Analysis (Medium Priority)
10. multisurvival - Multiple survival curves
11. survivalcont - Continuous survival
12. finegray - Competing risks
13. condsurvival - Conditional survival

### Visualization (Medium Priority)
14. ✅ waterfall - Waterfall plots
15. swimmerplot - Swimmer plots
16. raincloud - Raincloud plots
17. lollipop - Lollipop plots
18. jjhistostats - Histogram statistics
19. jjbarstats - Bar statistics
20. jjscatterstats - Scatter statistics

### Decision Analysis (Medium Priority)
21. decisiongraph - Decision trees
22. decisionpanel - Decision panels
23. decisioncombine - Combined decisions

### Specialized (Lower Priority)
- All remaining jj* functions (ggstatsplot wrappers)
- Advanced statistical models
- Specialized medical tools

## Remaining Functions by Category

### ggstatsplot Wrappers (jj* functions)
- jjhistostats, jjbarstats, jjbetweenstats, jjwithinstats
- jjcoefstats, jjcorrmat, jjdotplotstats, jjpiestats
- jjarcdiagram, jjpubr, jjridges, jjscatterstats
- jjsegmentedtotalbar, jjsyndromicplot, jjradarplot
- jjcoefstats, jextractggstats, jggstats, jforestmodel
- jforester, jggheatmap, jggridges, jcorrelation
- jjsankeyfier, jcomplexupset

### Visualization
- raincloud, advancedraincloud, lollipop, jwaffle
- swimmerplot, linechart, parallelplot, hullplot
- basegraphics, dendrogram, alluvial, agepyramid
- clinicalheatmap, vartree, grafify, groupedbar

### Survival Analysis
- multisurvival, survivalcont, finegray, simonmakuch
- condsurvival, recurrentsurvival, frailtysurvival
- mixedeffectscox, flexparametric, flexrstpm2
- multistatesurvival, illnessdeath, landmarkanalysis
- timedependent, continuousrelationship, survivalbart
- intervalsurvival, pseudosurvival, semimarkov
- epidemiosurvival, progressionsurvival, principalcox
- rmstregression, aalenhazard, comprehensiveSurvivalPower
- survivalcalibration, survivalvalidation, survivalmodelvalidation
- survivalpower (from examples)

### Decision/Diagnostic
- decisiongraph, decisionpanel, decisioncombine
- enhancedROC, psychopdaROC, timeroc, greyzoneroc
- rocreg, diagnosticmeta
- concordanceindex, brierscore, idi
- biomarkerresponse, timedependentdca, bayesdca
- nogoldstandard, cotest, betabinomialdiagnostic

### Clinical/Pathology Tools
- pathologyagreement, ihcscoring, stereology
- multiplexanalysis, haralicktexture, spatialanalysis
- spatialautocorrelation, patientsimilarity
- irecist, qtwist, toxicityprofile

### Data Quality/Preprocessing
- dataquality, checkdata, categorize, datecorrection
- datevalidator, datetimeconverter, outlierdetection
- missingdata, missingdataexplorer, advancedimputation
- batcheffect

### Modeling/Prediction
- predmodel, classification, geemodel, mixedmodelanova
- deeplearningprediction, explainableai, treemedical
- treeadvanced, treecompare, treeensemble, extratrees
- conditionalinference, gradientboosting
- nonparametricregression, adaptivelasso, grouplasso
- sparsegrouplasso, spikeslabpriors
- clinicalprediction, biomarkerdiscovery
- modelperformance, segmentationmetrics
- assayoptimization, jointmodeling

### Statistical Tests
- chisqposttest, pagetrendtest, friedmantest
- advancedanova, nonparametric, effectsize
- polychoriccorr, partialcorrelation, enhancedcorrelation
- robustcorrelation, methodcomparison, icccoeff

### Bayesian/Meta-Analysis
- bayesianma, bayesiannetworkma, hierarchicalbayes
- metaanalysis, treatmentmeta

### Summary Tables
- summarydata, summarydata2, reportcat, reportcat2
- groupsummary, tableone, crosstable2, crosstablepivot
- enhancedcrosstable, tinytable, enhancedtables
- gtsummary, jsummarytools, toolssummary
- enhancedtwowayfrequency, enhancedfrequency
- enhancedcorrelation, enhancedfactorvariable

### PCA/Factor Analysis
- pcaloadingtest, pcacomponenttest, pcaloadingheatmap
- pcacox, factoranalysis, entropyanalysis

### Advanced Clinical
- stagemigration, hierarchicalpathology, outbreakanalysis
- patientreported, qualityoflife, referenceintervals
- advancedtrials, conditionalinference, conditionalgee
- raftgee, pheval

### Specialty Plots
- bbcplots, economistplots, ggprism
- coefplot, groupedforest, autoeda
- esquisseauto, jviolin, eurostatmap

### Other
- jpps, cisingle, jrecode, jsjplot
- jcomplexupset, venn

## Testing Strategy

After adding asSource() to a function, test with variables containing:
1. Spaces: "Rater 1", "Patient ID"
2. Special characters: "Response (%)", "Time (days)"
3. Unicode: "Température", "年齢"
4. Mixed: "Grade (I-IV)", "Size (mm²)"

Example test:
```r
# Create test data with problematic variable names
test_data <- data.frame(
    `Rater 1` = c("A", "B", "C"),
    `Rater 2` = c("A", "B", "C"),
    `Patient ID` = 1:3,
    check.names = FALSE
)

# Test asSource() generation
result <- ClinicoPath::FUNCTION(data = test_data, vars = c("Rater 1", "Rater 2"))
syntax <- result$asSource()
print(syntax)

# Verify syntax is valid R code
tryCatch({
    parse(text = syntax)
    cat("✅ Syntax is valid\n")
}, error = function(e) {
    cat("❌ Syntax error:", e$message, "\n")
})
```

## Notes

- Always preserve existing `.sourcifyOption()` methods if present (e.g., conttables)
- The `private$.asArgs(incData = FALSE)` helper handles other options automatically
- Use `ClinicoPath::FUNCTION` namespace prefix in generated syntax
- Add roxygen documentation for the asSource() method
- Verify R syntax with `Rscript -e` before committing
- Update this document as patterns evolve

## Progress Tracking

Last updated: 2026-01-01
Completed: 6 / 187 functions (3.2%)
Estimated time per function: 3-5 minutes
Estimated total time: ~10-15 hours for remaining 181 functions
