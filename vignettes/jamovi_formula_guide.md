# A Comprehensive Guide to Formula Construction and Usage in jamovi Development

This document provides an exhaustive guide to formula construction, manipulation, and usage in jamovi module development. Formulas are fundamental to statistical modeling in R and jamovi, serving as the bridge between user interface selections and statistical computations.

## Table of Contents

1. [Introduction: Formula Architecture in jamovi](#1-introduction-formula-architecture-in-jamovi)
2. [R Formula Fundamentals](#2-r-formula-fundamentals)
3. [jamovi-Specific Formula Construction](#3-jamovi-specific-formula-construction)
4. [Advanced Formula Patterns](#4-advanced-formula-patterns)
5. [Specialized Formula Types](#5-specialized-formula-types)
6. [Dynamic Formula Building](#6-dynamic-formula-building)
7. [Formula Validation and Error Handling](#7-formula-validation-and-error-handling)
8. [Clinical and Research Applications](#8-clinical-and-research-applications)
9. [Performance Optimization](#9-performance-optimization)
10. [Complete Implementation Examples](#10-complete-implementation-examples)
11. [Best Practices](#11-best-practices)
12. [Troubleshooting Guide](#12-troubleshooting-guide)

## 1. Introduction: Formula Architecture in jamovi

### The Role of Formulas in jamovi

Formulas in jamovi serve multiple critical functions:

- **Statistical Model Definition**: Specify relationships between variables
- **User Interface Translation**: Convert UI selections into R code
- **Analysis Reproducibility**: Enable transparent statistical modeling
- **Code Generation**: Support automatic R code generation
- **Result Interpretation**: Facilitate clear communication of model structure

### jamovi Formula Workflow

```mermaid
graph TD
    A[User Variable Selection] -->|UI Options| B[jamovi .b.R Processing]
    B -->|constructFormula()| C[Safe Variable Names]
    C -->|paste() / glue()| D[Formula String Construction]
    D -->|as.formula()| E[R Formula Object]
    E -->|Statistical Functions| F[Model Fitting]
    F --> G[Results]
```

### Formula Integration Points

- **`.a.yaml` Options**: Define variable selectors and formula components
- **`.b.R` Processing**: Transform user selections into formulas
- **`.r.yaml` Results**: Display formula-based results
- **Statistical Packages**: Integration with R modeling functions

## 2. R Formula Fundamentals

### Basic Formula Structure

The fundamental R formula follows this pattern:

```R
# Basic structure
response ~ predictor1 + predictor2 + predictor3

# Components
# Left side (LHS): Response/Dependent variable
# ~ (tilde): Formula operator
# Right side (RHS): Predictor/Independent variables
```

### Comprehensive Operator Reference

| Operator | Function | Example | Interpretation |
|----------|----------|---------|----------------|
| `~` | Formula separator | `y ~ x` | "y is modeled by x" |
| `+` | Include term | `y ~ x1 + x2` | "y depends on x1 and x2" |
| `-` | Exclude term | `y ~ x1 + x2 - x1` | "exclude x1 (equals y ~ x2)" |
| `*` | Interaction with main effects | `y ~ x1 * x2` | "x1 + x2 + x1:x2" |
| `:` | Interaction only | `y ~ x1:x2` | "interaction without main effects" |
| `^` | Interactions to degree | `(x1 + x2 + x3)^2` | "all main effects + 2-way interactions" |
| `.` | All other variables | `y ~ .` | "all variables except y" |
| `I()` | Inhibit interpretation | `y ~ I(x1 + x2)` | "treat x1+x2 as single term" |
| `poly()` | Polynomial terms | `y ~ poly(x, 2)` | "quadratic polynomial in x" |
| `ns()`, `bs()` | Spline terms | `y ~ ns(x, df=3)` | "natural spline with 3 df" |
| `log()`, `sqrt()` | Transformations | `y ~ log(x)` | "logarithmic transformation" |
| `offset()` | Offset term | `y ~ x + offset(log(n))` | "known coefficient term" |

### Advanced Formula Syntax

#### Nested and Grouped Terms

```R
# Nested factors (hierarchical structure)
y ~ treatment/subject  # equivalent to treatment + treatment:subject

# Grouped terms with specific interactions
y ~ (x1 + x2 + x3)^2 - x1:x3  # all 2-way interactions except x1:x3

# Conditional terms
y ~ x1 * (x2 + x3)  # x1 + x2 + x3 + x1:x2 + x1:x3
```

#### Special Function Integration

```R
# Survival analysis formulas
Surv(time, event) ~ treatment + age + gender

# Mixed effects model formulas
y ~ fixed_effect + (1|random_effect)

# Generalized additive model formulas
y ~ s(x1) + s(x2, k=10) + te(x3, x4)
```

## 3. jamovi-Specific Formula Construction

### The `jmvcore::constructFormula()` Function

**Purpose**: Safely construct formula terms from user-selected variable names.

**Syntax**:
```R
jmvcore::constructFormula(terms = variable_names)
```

**Key Features**:
- Handles special characters in variable names
- Provides proper quoting when needed
- Ensures compatibility with R formula syntax
- Prevents injection attacks through user input

#### Basic Usage Examples

```R
# Single variable
time_var <- jmvcore::constructFormula(terms = self$options$timeVariable)
# Result: "time_to_event" or "`time to event`" (quoted if needed)

# Multiple variables
covariates <- jmvcore::constructFormula(terms = self$options$covariateList)
# Result: c("age", "gender", "`treatment group`")

# Factor levels
event_level <- jmvcore::constructFormula(terms = self$options$eventLevel)
# Result: "Death" or "`Disease Progression`"
```

#### Advanced constructFormula Applications

```R
# Handling complex variable selections
.buildCovariateTerms = function() {
    # Get base covariates
    base_covariates <- self$options$baselineCovariates
    time_covariates <- self$options$timeVaryingCovariates
    
    # Construct safe variable names
    if (!is.null(base_covariates) && length(base_covariates) > 0) {
        base_terms <- jmvcore::constructFormula(terms = base_covariates)
    } else {
        base_terms <- NULL
    }
    
    if (!is.null(time_covariates) && length(time_covariates) > 0) {
        time_terms <- jmvcore::constructFormula(terms = time_covariates)
        # Add time-varying indicator
        time_terms <- paste0("tt(", time_terms, ")")
    } else {
        time_terms <- NULL
    }
    
    # Combine terms
    all_terms <- c(base_terms, time_terms)
    
    return(all_terms)
}
```

### Formula String Construction Patterns

#### Simple Linear Models

```R
.buildLinearFormula = function() {
    # Get variable components
    dependent <- jmvcore::constructFormula(terms = self$options$dependentVar)
    predictors <- self$options$predictorVars
    
    if (is.null(predictors) || length(predictors) == 0) {
        # Intercept-only model
        formula_string <- paste(dependent, "~ 1")
    } else {
        # Multiple predictor model
        predictor_terms <- jmvcore::constructFormula(terms = predictors)
        predictor_string <- paste(predictor_terms, collapse = " + ")
        formula_string <- paste(dependent, "~", predictor_string)
    }
    
    # Handle interactions if specified
    if (self$options$includeInteractions) {
        formula_string <- private$.addInteractionTerms(formula_string)
    }
    
    # Handle polynomial terms
    if (self$options$polynomialDegree > 1) {
        formula_string <- private$.addPolynomialTerms(formula_string)
    }
    
    return(as.formula(formula_string))
}
```

#### Survival Analysis Formulas

```R
.buildSurvivalFormula = function() {
    # Get survival components
    time_var <- jmvcore::constructFormula(terms = self$options$timeVariable)
    event_var <- jmvcore::constructFormula(terms = self$options$eventVariable)
    
    # Build Surv object
    if (self$options$survivalType == "right_censored") {
        surv_object <- paste0("survival::Surv(", time_var, ", ", event_var, ")")
    } else if (self$options$survivalType == "interval_censored") {
        time2_var <- jmvcore::constructFormula(terms = self$options$time2Variable)
        surv_object <- paste0("survival::Surv(", time_var, ", ", time2_var, ", ", event_var, ", type = 'interval')")
    } else if (self$options$survivalType == "counting_process") {
        start_var <- jmvcore::constructFormula(terms = self$options$startTimeVariable)
        surv_object <- paste0("survival::Surv(", start_var, ", ", time_var, ", ", event_var, ")")
    }
    
    # Build predictor terms
    predictors <- private$.buildPredictorTerms()
    
    # Combine into formula
    if (length(predictors) > 0) {
        predictor_string <- paste(predictors, collapse = " + ")
        formula_string <- paste(surv_object, "~", predictor_string)
    } else {
        formula_string <- paste(surv_object, "~ 1")
    }
    
    return(as.formula(formula_string))
}

.buildPredictorTerms = function() {
    terms <- c()
    
    # Main effects
    if (!is.null(self$options$treatmentVariable)) {
        treatment_term <- jmvcore::constructFormula(terms = self$options$treatmentVariable)
        terms <- c(terms, treatment_term)
    }
    
    # Covariates
    if (!is.null(self$options$covariates)) {
        covariate_terms <- jmvcore::constructFormula(terms = self$options$covariates)
        terms <- c(terms, covariate_terms)
    }
    
    # Stratification terms
    if (self$options$useStratification && !is.null(self$options$stratificationVars)) {
        strata_vars <- jmvcore::constructFormula(terms = self$options$stratificationVars)
        strata_terms <- paste0("strata(", strata_vars, ")")
        terms <- c(terms, strata_terms)
    }
    
    # Frailty terms for mixed effects
    if (self$options$includeFrailty && !is.null(self$options$clusterVariable)) {
        cluster_var <- jmvcore::constructFormula(terms = self$options$clusterVariable)
        frailty_term <- paste0("frailty(", cluster_var, ")")
        terms <- c(terms, frailty_term)
    }
    
    return(terms)
}
```

## 4. Advanced Formula Patterns

### Conditional Formula Building

Build different formulas based on analysis options and data characteristics:

```R
.buildConditionalFormula = function() {
    base_formula <- private$.buildBaseFormula()
    
    # Add complexity based on options
    if (self$options$includeInteractions) {
        base_formula <- private$.addInteractionTerms(base_formula)
    }
    
    if (self$options$includeQuadratic) {
        base_formula <- private$.addQuadraticTerms(base_formula)
    }
    
    if (self$options$useSplines) {
        base_formula <- private$.addSplineTerms(base_formula)
    }
    
    # Validate formula complexity
    if (private$.isFormulaToComplex(base_formula)) {
        warning("Formula complexity may lead to convergence issues")
        base_formula <- private$.simplifyFormula(base_formula)
    }
    
    return(base_formula)
}

.addInteractionTerms = function(base_formula) {
    # Extract current terms
    formula_terms <- terms(base_formula)
    current_terms <- attr(formula_terms, "term.labels")
    
    # Determine interaction strategy
    interaction_type <- self$options$interactionType
    
    switch(interaction_type,
        "all_pairwise" = {
            # Add all 2-way interactions
            if (length(current_terms) > 1) {
                interactions <- combn(current_terms, 2, 
                                    FUN = function(x) paste(x, collapse = ":"),
                                    simplify = TRUE)
                new_formula_string <- paste(c(current_terms, interactions), collapse = " + ")
            } else {
                new_formula_string <- paste(current_terms, collapse = " + ")
            }
        },
        "selected_interactions" = {
            # Use user-specified interactions
            interaction_pairs <- self$options$interactionPairs
            interaction_terms <- sapply(interaction_pairs, function(pair) {
                paste(pair, collapse = ":")
            })
            new_formula_string <- paste(c(current_terms, interaction_terms), collapse = " + ")
        },
        "hierarchical" = {
            # Build hierarchical interaction structure
            new_formula_string <- private$.buildHierarchicalInteractions(current_terms)
        }
    )
    
    # Reconstruct formula
    response_var <- all.vars(base_formula)[1]
    new_formula <- as.formula(paste(response_var, "~", new_formula_string))
    
    return(new_formula)
}
```

### Mixed Effects Model Formulas

```R
.buildMixedEffectsFormula = function() {
    # Fixed effects component
    fixed_effects <- private$.buildFixedEffects()
    
    # Random effects component  
    random_effects <- private$.buildRandomEffects()
    
    # Combine components
    if (length(random_effects) > 0) {
        formula_string <- paste(fixed_effects, "+", paste(random_effects, collapse = " + "))
    } else {
        formula_string <- fixed_effects
    }
    
    return(as.formula(formula_string))
}

.buildRandomEffects = function() {
    random_terms <- c()
    
    # Random intercepts
    if (!is.null(self$options$randomInterceptVars)) {
        intercept_vars <- jmvcore::constructFormula(terms = self$options$randomInterceptVars)
        for (var in intercept_vars) {
            random_terms <- c(random_terms, paste0("(1|", var, ")"))
        }
    }
    
    # Random slopes
    if (!is.null(self$options$randomSlopeSpecs)) {
        for (slope_spec in self$options$randomSlopeSpecs) {
            slope_var <- jmvcore::constructFormula(terms = slope_spec$variable)
            group_var <- jmvcore::constructFormula(terms = slope_spec$grouping)
            
            if (slope_spec$includeIntercept) {
                random_terms <- c(random_terms, paste0("(", slope_var, "|", group_var, ")"))
            } else {
                random_terms <- c(random_terms, paste0("(0 + ", slope_var, "|", group_var, ")"))
            }
        }
    }
    
    # Nested random effects
    if (!is.null(self$options$nestedRandomEffects)) {
        for (nested_spec in self$options$nestedRandomEffects) {
            higher_level <- jmvcore::constructFormula(terms = nested_spec$higher)
            lower_level <- jmvcore::constructFormula(terms = nested_spec$lower)
            random_terms <- c(random_terms, paste0("(1|", higher_level, "/", lower_level, ")"))
        }
    }
    
    return(random_terms)
}
```

### Time-Varying Coefficient Models

```R
.buildTimeVaryingFormula = function() {
    base_formula <- private$.buildSurvivalFormula()
    
    if (!self$options$timeVaryingEffects) {
        return(base_formula)
    }
    
    # Extract base terms
    formula_terms <- terms(base_formula)
    surv_object <- attr(formula_terms, "response")
    predictor_terms <- attr(formula_terms, "term.labels")
    
    # Identify time-varying variables
    tv_vars <- self$options$timeVaryingVariables
    
    if (is.null(tv_vars) || length(tv_vars) == 0) {
        return(base_formula)
    }
    
    # Build time-varying terms
    tv_terms <- sapply(tv_vars, function(var) {
        safe_var <- jmvcore::constructFormula(terms = var)
        
        switch(self$options$timeVaryingMethod,
            "step_function" = {
                # Use tt() function for step functions
                paste0("tt(", safe_var, ")")
            },
            "spline" = {
                # Use pspline for smooth time-varying effects
                paste0("pspline(", safe_var, ", df=4)")
            },
            "interaction" = {
                # Time interaction
                paste0(safe_var, ":log(time)")
            }
        )
    })
    
    # Combine all terms
    static_terms <- setdiff(predictor_terms, tv_vars)
    all_terms <- c(static_terms, tv_terms)
    
    # Reconstruct formula
    new_formula_string <- paste(surv_object, "~", paste(all_terms, collapse = " + "))
    
    return(as.formula(new_formula_string))
}
```

## 5. Specialized Formula Types

### Generalized Additive Model (GAM) Formulas

```R
.buildGAMFormula = function() {
    base_terms <- private$.getLinearTerms()
    smooth_terms <- private$.getSmoothTerms()
    tensor_terms <- private$.getTensorTerms()
    
    all_terms <- c(base_terms, smooth_terms, tensor_terms)
    
    dependent <- jmvcore::constructFormula(terms = self$options$dependentVariable)
    formula_string <- paste(dependent, "~", paste(all_terms, collapse = " + "))
    
    return(as.formula(formula_string))
}

.getSmoothTerms = function() {
    smooth_vars <- self$options$smoothVariables
    if (is.null(smooth_vars)) return(NULL)
    
    smooth_terms <- sapply(smooth_vars, function(spec) {
        var_name <- jmvcore::constructFormula(terms = spec$variable)
        
        # Different smooth types
        switch(spec$smoother_type,
            "thin_plate" = paste0("s(", var_name, ", k=", spec$basis_size, ")"),
            "cubic_spline" = paste0("s(", var_name, ", bs='cr', k=", spec$basis_size, ")"),
            "cyclic" = paste0("s(", var_name, ", bs='cc', k=", spec$basis_size, ")"),
            "adaptive" = paste0("s(", var_name, ", bs='ad', k=", spec$basis_size, ")")
        )
    })
    
    return(smooth_terms)
}

.getTensorTerms = function() {
    tensor_specs <- self$options$tensorProductSpecs
    if (is.null(tensor_specs)) return(NULL)
    
    tensor_terms <- sapply(tensor_specs, function(spec) {
        var_names <- jmvcore::constructFormula(terms = spec$variables)
        
        if (length(var_names) == 2) {
            paste0("te(", paste(var_names, collapse = ", "), ")")
        } else {
            paste0("ti(", paste(var_names, collapse = ", "), ")")
        }
    })
    
    return(tensor_terms)
}
```

### Bayesian Model Formulas

```R
.buildBayesianFormula = function() {
    # Standard formula structure
    base_formula <- private$.buildBaseFormula()
    
    # Add priors specification for brms
    if (self$options$useBayesian && requireNamespace("brms", quietly = TRUE)) {
        prior_specs <- private$.buildPriorSpecifications()
        
        return(list(
            formula = base_formula,
            priors = prior_specs,
            family = private$.getBayesianFamily()
        ))
    }
    
    return(base_formula)
}

.buildPriorSpecifications = function() {
    priors <- list()
    
    # Intercept prior
    if (!is.null(self$options$interceptPrior)) {
        priors$intercept <- list(
            prior = self$options$interceptPrior$distribution,
            class = "Intercept"
        )
    }
    
    # Coefficient priors
    if (!is.null(self$options$coefficientPriors)) {
        for (coef_spec in self$options$coefficientPriors) {
            coef_name <- jmvcore::constructFormula(terms = coef_spec$variable)
            priors[[coef_name]] <- list(
                prior = coef_spec$distribution,
                class = "b",
                coef = coef_name
            )
        }
    }
    
    # Variance priors
    if (!is.null(self$options$variancePrior)) {
        priors$sigma <- list(
            prior = self$options$variancePrior$distribution,
            class = "sigma"
        )
    }
    
    return(priors)
}
```

### Competing Risks Formulas

```R
.buildCompetingRisksFormula = function() {
    # Time and event variables
    time_var <- jmvcore::constructFormula(terms = self$options$timeVariable)
    event_var <- jmvcore::constructFormula(terms = self$options$eventVariable)
    
    # Different approaches for competing risks
    approach <- self$options$competingRisksApproach
    
    switch(approach,
        "cause_specific" = {
            # Cause-specific hazard models
            formulas <- private$.buildCauseSpecificFormulas(time_var, event_var)
        },
        "subdistribution" = {
            # Fine-Gray subdistribution hazard
            formulas <- private$.buildSubdistributionFormulas(time_var, event_var)
        },
        "multi_state" = {
            # Multi-state model approach
            formulas <- private$.buildMultiStateFormulas(time_var, event_var)
        }
    )
    
    return(formulas)
}

.buildCauseSpecificFormulas = function(time_var, event_var) {
    # Get event types
    event_types <- self$options$eventTypes
    predictors <- private$.buildPredictorString()
    
    formulas <- list()
    
    for (event_type in event_types) {
        # Create binary indicator for this event type
        event_indicator <- paste0("(", event_var, " == '", event_type, "')")
        
        # Survival object for this event type
        surv_object <- paste0("survival::Surv(", time_var, ", ", event_indicator, ")")
        
        # Complete formula
        formula_string <- paste(surv_object, "~", predictors)
        formulas[[event_type]] <- as.formula(formula_string)
    }
    
    return(formulas)
}

.buildSubdistributionFormulas = function(time_var, event_var) {
    # Fine-Gray model formulas
    primary_event <- self$options$primaryEvent
    predictors <- private$.buildPredictorString()
    
    # Subdistribution hazard formula
    if (requireNamespace("cmprsk", quietly = TRUE)) {
        # cmprsk uses different syntax
        formula_components <- list(
            time = time_var,
            event = event_var,
            predictors = predictors,
            failcode = primary_event
        )
    } else {
        # Alternative implementation
        surv_object <- paste0("survival::Surv(", time_var, ", ", event_var, ")")
        formula_string <- paste(surv_object, "~", predictors)
        formula_components <- as.formula(formula_string)
    }
    
    return(formula_components)
}
```

### Machine Learning Model Formulas

```R
.buildMLFormula = function() {
    # Feature engineering for ML models
    base_features <- private$.getBaseFeatures()
    engineered_features <- private$.getEngineeredFeatures()
    
    all_features <- c(base_features, engineered_features)
    
    # Different ML approaches
    ml_method <- self$options$mlMethod
    
    switch(ml_method,
        "random_forest" = {
            # Random forest can handle factors and interactions automatically
            formula <- private$.buildSimpleFormula(all_features)
        },
        "gradient_boosting" = {
            # XGBoost-style formula
            formula <- private$.buildGBMFormula(all_features)
        },
        "neural_network" = {
            # Neural network preprocessing
            formula <- private$.buildNeuralNetFormula(all_features)
        },
        "svm" = {
            # Support vector machine formula
            formula <- private$.buildSVMFormula(all_features)
        }
    )
    
    return(formula)
}

.getEngineeredFeatures = function() {
    engineered <- c()
    
    # Polynomial features
    if (self$options$includePolynomial) {
        numeric_vars <- private$.getNumericVariables()
        degree <- self$options$polynomialDegree
        
        poly_features <- sapply(numeric_vars, function(var) {
            paste0("poly(", var, ", ", degree, ", raw=TRUE)")
        })
        
        engineered <- c(engineered, poly_features)
    }
    
    # Interaction features
    if (self$options$includeInteractions) {
        interaction_features <- private$.generateInteractionFeatures()
        engineered <- c(engineered, interaction_features)
    }
    
    # Spline features
    if (self$options$includeSplines) {
        spline_features <- private$.generateSplineFeatures()
        engineered <- c(engineered, spline_features)
    }
    
    return(engineered)
}
```

## 6. Dynamic Formula Building

### Adaptive Model Selection

```R
.buildAdaptiveFormula = function() {
    # Start with base model
    current_formula <- private$.buildBaseFormula()
    
    # Adaptive model building based on data characteristics
    data_summary <- private$.analyzeDataCharacteristics()
    
    # Add complexity based on data size and structure
    if (data_summary$n_observations > 1000) {
        # Can handle more complex models with larger data
        if (data_summary$n_predictors > 5) {
            # Use regularization or dimension reduction
            current_formula <- private$.addRegularizationTerms(current_formula)
        }
        
        # Add nonlinear terms if appropriate
        if (data_summary$nonlinearity_detected) {
            current_formula <- private$.addNonlinearTerms(current_formula)
        }
    } else {
        # Keep model simple for smaller datasets
        current_formula <- private$.simplifyFormula(current_formula)
    }
    
    # Cross-validation based term selection
    if (self$options$useCV) {
        optimal_formula <- private$.cvBasedSelection(current_formula)
        current_formula <- optimal_formula
    }
    
    return(current_formula)
}

.analyzeDataCharacteristics = function() {
    data <- self$data
    
    characteristics <- list(
        n_observations = nrow(data),
        n_predictors = ncol(data) - 1,
        missing_proportion = sum(is.na(data)) / (nrow(data) * ncol(data)),
        nonlinearity_detected = private$.detectNonlinearity(data),
        multicollinearity_issues = private$.detectMulticollinearity(data),
        outlier_proportion = private$.calculateOutlierProportion(data)
    )
    
    return(characteristics)
}

.cvBasedSelection = function(base_formula) {
    # Perform cross-validation to select optimal model
    candidate_formulas <- private$.generateCandidateFormulas(base_formula)
    
    cv_results <- list()
    
    for (i in seq_along(candidate_formulas)) {
        formula <- candidate_formulas[[i]]
        
        # Perform k-fold cross-validation
        cv_score <- private$.performCrossValidation(formula)
        
        cv_results[[i]] <- list(
            formula = formula,
            score = cv_score,
            complexity = private$.calculateFormulaComplexity(formula)
        )
    }
    
    # Select best formula based on CV score and complexity
    optimal_formula <- private$.selectOptimalFormula(cv_results)
    
    return(optimal_formula)
}
```

### Progressive Model Building

```R
.buildProgressiveFormula = function() {
    # Start with intercept-only model
    current_formula <- as.formula(paste(self$options$dependentVar, "~ 1"))
    
    # Available predictors
    available_predictors <- jmvcore::constructFormula(terms = self$options$predictorVars)
    
    # Progressive addition strategy
    strategy <- self$options$buildingStrategy
    
    switch(strategy,
        "forward" = {
            final_formula <- private$.forwardSelection(current_formula, available_predictors)
        },
        "backward" = {
            # Start with full model
            full_formula <- private$.buildFullFormula(available_predictors)
            final_formula <- private$.backwardElimination(full_formula)
        },
        "stepwise" = {
            final_formula <- private$.stepwiseSelection(current_formula, available_predictors)
        },
        "hierarchical" = {
            final_formula <- private$.hierarchicalBuilding(available_predictors)
        }
    )
    
    return(final_formula)
}

.forwardSelection = function(base_formula, predictors) {
    current_formula <- base_formula
    remaining_predictors <- predictors
    
    while (length(remaining_predictors) > 0) {
        best_addition <- private$.findBestAddition(current_formula, remaining_predictors)
        
        if (best_addition$improvement > self$options$entryThreshold) {
            # Add the best predictor
            current_terms <- attr(terms(current_formula), "term.labels")
            new_terms <- c(current_terms, best_addition$predictor)
            
            response_var <- all.vars(current_formula)[1]
            formula_string <- paste(response_var, "~", paste(new_terms, collapse = " + "))
            current_formula <- as.formula(formula_string)
            
            # Remove from remaining predictors
            remaining_predictors <- setdiff(remaining_predictors, best_addition$predictor)
        } else {
            break  # No significant improvement found
        }
    }
    
    return(current_formula)
}

.findBestAddition = function(current_formula, candidates) {
    best_score <- -Inf
    best_predictor <- NULL
    
    for (predictor in candidates) {
        # Test adding this predictor
        test_formula <- private$.addTermToFormula(current_formula, predictor)
        
        # Evaluate improvement (e.g., AIC, BIC, R-squared)
        score <- private$.evaluateFormulaImprovement(current_formula, test_formula)
        
        if (score > best_score) {
            best_score <- score
            best_predictor <- predictor
        }
    }
    
    return(list(
        predictor = best_predictor,
        improvement = best_score
    ))
}
```

### Context-Aware Formula Generation

```R
.buildContextAwareFormula = function() {
    # Analyze analysis context
    context <- private$.analyzeAnalysisContext()
    
    # Build formula based on context
    switch(context$analysis_type,
        "exploratory" = {
            formula <- private$.buildExploratoryFormula(context)
        },
        "confirmatory" = {
            formula <- private$.buildConfirmatoryFormula(context)
        },
        "predictive" = {
            formula <- private$.buildPredictiveFormula(context)
        },
        "descriptive" = {
            formula <- private$.buildDescriptiveFormula(context)
        }
    )
    
    return(formula)
}

.analyzeAnalysisContext = function() {
    # Determine analysis context from options and data
    context <- list()
    
    # Analysis purpose
    if (self$options$purpose == "hypothesis_testing") {
        context$analysis_type <- "confirmatory"
    } else if (self$options$purpose == "prediction") {
        context$analysis_type <- "predictive"
    } else {
        context$analysis_type <- "exploratory"
    }
    
    # Data characteristics
    context$data_size <- nrow(self$data)
    context$predictor_count <- length(self$options$predictorVars)
    context$outcome_type <- private$.determineOutcomeType()
    
    # Domain knowledge
    if (!is.null(self$options$priorKnowledge)) {
        context$domain_constraints <- self$options$priorKnowledge
    }
    
    return(context)
}

.buildConfirmatoryFormula = function(context) {
    # For confirmatory analysis, use theory-driven approach
    theoretical_predictors <- private$.getTheoreticalPredictors()
    
    # Build hierarchical formula based on theory
    formula_layers <- list(
        core_predictors = theoretical_predictors$core,
        moderators = theoretical_predictors$moderators,
        confounders = theoretical_predictors$confounders
    )
    
    # Construct final formula
    all_terms <- unlist(formula_layers)
    safe_terms <- jmvcore::constructFormula(terms = all_terms)
    
    response_var <- jmvcore::constructFormula(terms = self$options$dependentVar)
    formula_string <- paste(response_var, "~", paste(safe_terms, collapse = " + "))
    
    return(as.formula(formula_string))
}
```

## 7. Formula Validation and Error Handling

### Comprehensive Formula Validation

```R
.validateFormula = function(formula) {
    validation_results <- list(
        valid = TRUE,
        errors = list(),
        warnings = list()
    )
    
    # Basic syntax validation
    syntax_check <- private$.validateFormulaSyntax(formula)
    if (!syntax_check$valid) {
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors, syntax_check$errors)
    }
    
    # Variable existence validation
    variable_check <- private$.validateFormulaVariables(formula)
    if (!variable_check$valid) {
        validation_results$valid <- FALSE
        validation_results$errors <- c(validation_results$errors, variable_check$errors)
    }
    
    # Data type compatibility
    type_check <- private$.validateDataTypes(formula)
    if (!type_check$valid) {
        validation_results$warnings <- c(validation_results$warnings, type_check$warnings)
    }
    
    # Model complexity assessment
    complexity_check <- private$.assessModelComplexity(formula)
    if (complexity_check$too_complex) {
        validation_results$warnings <- c(validation_results$warnings, complexity_check$warnings)
    }
    
    return(validation_results)
}

.validateFormulaSyntax = function(formula) {
    tryCatch({
        # Test formula parsing
        test_terms <- terms(formula)
        
        # Check for common syntax errors
        formula_string <- deparse(formula)
        
        # Missing tilde
        if (!grepl("~", formula_string)) {
            return(list(
                valid = FALSE,
                errors = list("Formula missing ~ operator")
            ))
        }
        
        # Empty sides
        sides <- strsplit(formula_string, "~")[[1]]
        if (any(trimws(sides) == "")) {
            return(list(
                valid = FALSE,
                errors = list("Formula has empty left or right side")
            ))
        }
        
        # Unmatched parentheses
        open_parens <- lengths(regmatches(formula_string, gregexpr("\\(", formula_string)))
        close_parens <- lengths(regmatches(formula_string, gregexpr("\\)", formula_string)))
        
        if (open_parens != close_parens) {
            return(list(
                valid = FALSE,
                errors = list("Unmatched parentheses in formula")
            ))
        }
        
        return(list(valid = TRUE, errors = list()))
        
    }, error = function(e) {
        return(list(
            valid = FALSE,
            errors = list(paste("Formula syntax error:", e$message))
        ))
    })
}

.validateFormulaVariables = function(formula) {
    formula_vars <- all.vars(formula)
    data_vars <- names(self$data)
    
    missing_vars <- setdiff(formula_vars, data_vars)
    
    if (length(missing_vars) > 0) {
        return(list(
            valid = FALSE,
            errors = list(paste("Variables not found in data:", paste(missing_vars, collapse = ", ")))
        ))
    }
    
    # Check for reserved names
    reserved_names <- c("data", "subset", "weights", "na.action")
    conflicting_names <- intersect(formula_vars, reserved_names)
    
    if (length(conflicting_names) > 0) {
        return(list(
            valid = FALSE,
            errors = list(paste("Variables use reserved names:", paste(conflicting_names, collapse = ", ")))
        ))
    }
    
    return(list(valid = TRUE, errors = list()))
}

.validateDataTypes = function(formula) {
    warnings <- list()
    
    formula_vars <- all.vars(formula)
    response_var <- formula_vars[1]
    predictor_vars <- formula_vars[-1]
    
    # Check response variable type
    response_type <- class(self$data[[response_var]])[1]
    analysis_type <- private$.inferAnalysisType()
    
    type_compatibility <- private$.checkTypeCompatibility(response_type, analysis_type)
    if (!type_compatibility$compatible) {
        warnings <- c(warnings, type_compatibility$warning)
    }
    
    # Check predictor types
    for (pred_var in predictor_vars) {
        pred_type <- class(self$data[[pred_var]])[1]
        
        # Check for factors with too many levels
        if (pred_type %in% c("factor", "character")) {
            n_levels <- length(unique(self$data[[pred_var]]))
            if (n_levels > 20) {
                warnings <- c(warnings, 
                    paste("Variable", pred_var, "has", n_levels, "levels - consider grouping"))
            }
        }
        
        # Check for near-zero variance
        if (pred_type %in% c("numeric", "integer")) {
            var_coef <- sd(self$data[[pred_var]], na.rm = TRUE) / mean(self$data[[pred_var]], na.rm = TRUE)
            if (var_coef < 0.01) {
                warnings <- c(warnings,
                    paste("Variable", pred_var, "has very low variance"))
            }
        }
    }
    
    return(list(
        valid = length(warnings) == 0,
        warnings = warnings
    ))
}
```

### Safe Formula Construction

```R
.safeFormulaConstruction = function() {
    tryCatch({
        # Build formula with validation at each step
        base_components <- private$.gatherFormulaComponents()
        
        # Validate components
        component_validation <- private$.validateComponents(base_components)
        if (!component_validation$valid) {
            private$.handleComponentErrors(component_validation$errors)
            return(NULL)
        }
        
        # Construct formula string
        formula_string <- private$.assembleFormulaString(base_components)
        
        # Validate string before conversion
        string_validation <- private$.validateFormulaString(formula_string)
        if (!string_validation$valid) {
            private$.handleStringErrors(string_validation$errors)
            return(NULL)
        }
        
        # Convert to formula object
        formula_object <- as.formula(formula_string)
        
        # Final validation
        final_validation <- private$.validateFormula(formula_object)
        if (!final_validation$valid) {
            private$.handleValidationErrors(final_validation$errors)
            return(NULL)
        }
        
        # Log successful formula creation
        private$.logFormulaCreation(formula_object)
        
        return(formula_object)
        
    }, error = function(e) {
        error_msg <- paste("Formula construction failed:", e$message)
        private$.displayUserError(error_msg)
        return(NULL)
    })
}

.handleValidationErrors = function(errors) {
    # Create user-friendly error messages
    error_html <- "<div class='formula-error'>"
    error_html <- paste0(error_html, "<h4>Formula Construction Error</h4>")
    error_html <- paste0(error_html, "<ul>")
    
    for (error in errors) {
        user_friendly_error <- private$.translateFormulaError(error)
        error_html <- paste0(error_html, "<li>", user_friendly_error, "</li>")
    }
    
    error_html <- paste0(error_html, "</ul>")
    error_html <- paste0(error_html, "<p><strong>Suggestion:</strong> ", private$.getFormulaSuggestion(), "</p>")
    error_html <- paste0(error_html, "</div>")
    
    # Display in appropriate result object
    if (!is.null(self$results$instructions)) {
        self$results$instructions$setContent(error_html)
    }
}

.translateFormulaError = function(error) {
    # Common error patterns and translations
    if (grepl("not found", error)) {
        return("One or more selected variables are not available in the dataset")
    } else if (grepl("syntax", error)) {
        return("The formula has incorrect syntax - please check variable selections")
    } else if (grepl("complex", error)) {
        return("The model is too complex for the available data - try reducing the number of predictors")
    } else {
        return(paste("Formula error:", error))
    }
}
```

## 8. Clinical and Research Applications

### Clinical Trial Analysis Formulas

```R
.buildClinicalTrialFormula = function() {
    # Primary endpoint analysis
    primary_endpoint <- jmvcore::constructFormula(terms = self$options$primaryEndpoint)
    treatment_arm <- jmvcore::constructFormula(terms = self$options$treatmentArm)
    
    # Baseline covariates for adjustment
    baseline_covariates <- private$.getBaselineCovariates()
    
    # Stratification factors
    stratification_factors <- private$.getStratificationFactors()
    
    # Build analysis-specific formula
    analysis_type <- self$options$primaryAnalysisType
    
    switch(analysis_type,
        "survival" = {
            time_to_event <- jmvcore::constructFormula(terms = self$options$timeToEvent)
            event_indicator <- jmvcore::constructFormula(terms = self$options$eventIndicator)
            
            # Survival formula with covariates
            surv_object <- paste0("survival::Surv(", time_to_event, ", ", event_indicator, ")")
            all_terms <- c(treatment_arm, baseline_covariates)
            
            if (length(stratification_factors) > 0) {
                strata_terms <- paste0("strata(", paste(stratification_factors, collapse = ", "), ")")
                all_terms <- c(all_terms, strata_terms)
            }
            
            formula_string <- paste(surv_object, "~", paste(all_terms, collapse = " + "))
        },
        "binary" = {
            # Logistic regression for binary endpoints
            all_terms <- c(treatment_arm, baseline_covariates, stratification_factors)
            formula_string <- paste(primary_endpoint, "~", paste(all_terms, collapse = " + "))
        },
        "continuous" = {
            # Linear model for continuous endpoints
            all_terms <- c(treatment_arm, baseline_covariates)
            
            # Include baseline value if available
            baseline_value <- self$options$baselineValue
            if (!is.null(baseline_value) && baseline_value != "") {
                baseline_term <- jmvcore::constructFormula(terms = baseline_value)
                all_terms <- c(all_terms, baseline_term)
            }
            
            formula_string <- paste(primary_endpoint, "~", paste(all_terms, collapse = " + "))
        }
    )
    
    return(as.formula(formula_string))
}

.getBaselineCovariates = function() {
    covariates <- c()
    
    # Standard demographic variables
    if (!is.null(self$options$includeAge) && self$options$includeAge) {
        age_var <- jmvcore::constructFormula(terms = self$options$ageVariable)
        covariates <- c(covariates, age_var)
    }
    
    if (!is.null(self$options$includeGender) && self$options$includeGender) {
        gender_var <- jmvcore::constructFormula(terms = self$options$genderVariable)
        covariates <- c(covariates, gender_var)
    }
    
    # Disease-specific covariates
    disease_covariates <- self$options$diseaseCovariates
    if (!is.null(disease_covariates)) {
        disease_terms <- jmvcore::constructFormula(terms = disease_covariates)
        covariates <- c(covariates, disease_terms)
    }
    
    # Prognostic factors
    prognostic_factors <- self$options$prognosticFactors
    if (!is.null(prognostic_factors)) {
        prognostic_terms <- jmvcore::constructFormula(terms = prognostic_factors)
        covariates <- c(covariates, prognostic_terms)
    }
    
    return(covariates)
}
```

### Biomarker Analysis Formulas

```R
.buildBiomarkerFormula = function() {
    # Outcome variable
    outcome_var <- jmvcore::constructFormula(terms = self$options$outcomeVariable)
    
    # Biomarker variables
    biomarkers <- self$options$biomarkerVariables
    biomarker_terms <- jmvcore::constructFormula(terms = biomarkers)
    
    # Model type for biomarker analysis
    model_type <- self$options$biomarkerModelType
    
    switch(model_type,
        "individual" = {
            # Individual biomarker models
            formulas <- sapply(biomarker_terms, function(biomarker) {
                as.formula(paste(outcome_var, "~", biomarker))
            }, simplify = FALSE)
        },
        "combined_linear" = {
            # Linear combination of biomarkers
            formula_string <- paste(outcome_var, "~", paste(biomarker_terms, collapse = " + "))
            formulas <- as.formula(formula_string)
        },
        "combined_nonlinear" = {
            # Nonlinear combinations
            nonlinear_terms <- private$.createNonlinearBiomarkerTerms(biomarker_terms)
            all_terms <- c(biomarker_terms, nonlinear_terms)
            formula_string <- paste(outcome_var, "~", paste(all_terms, collapse = " + "))
            formulas <- as.formula(formula_string)
        },
        "interaction_model" = {
            # Biomarker interactions
            if (length(biomarker_terms) > 1) {
                interaction_string <- paste(biomarker_terms, collapse = " * ")
                formula_string <- paste(outcome_var, "~", interaction_string)
                formulas <- as.formula(formula_string)
            } else {
                formula_string <- paste(outcome_var, "~", biomarker_terms[1])
                formulas <- as.formula(formula_string)
            }
        }
    )
    
    return(formulas)
}

.createNonlinearBiomarkerTerms = function(biomarker_terms) {
    nonlinear_terms <- c()
    
    # Quadratic terms
    if (self$options$includeQuadratic) {
        quad_terms <- paste0("I(", biomarker_terms, "^2)")
        nonlinear_terms <- c(nonlinear_terms, quad_terms)
    }
    
    # Log transformations
    if (self$options$includeLog) {
        log_terms <- paste0("log(", biomarker_terms, " + 1)")
        nonlinear_terms <- c(nonlinear_terms, log_terms)
    }
    
    # Spline terms
    if (self$options$includeSplines) {
        spline_terms <- paste0("ns(", biomarker_terms, ", df = 3)")
        nonlinear_terms <- c(nonlinear_terms, spline_terms)
    }
    
    # Biomarker ratios
    if (self$options$includeRatios && length(biomarker_terms) > 1) {
        ratio_pairs <- combn(biomarker_terms, 2, simplify = FALSE)
        ratio_terms <- sapply(ratio_pairs, function(pair) {
            paste0("I(", pair[1], "/", pair[2], ")")
        })
        nonlinear_terms <- c(nonlinear_terms, ratio_terms)
    }
    
    return(nonlinear_terms)
}
```

### Epidemiological Study Formulas

```R
.buildEpidemiologicalFormula = function() {
    # Study design influences formula structure
    study_design <- self$options$studyDesign
    
    switch(study_design,
        "case_control" = {
            formula <- private$.buildCaseControlFormula()
        },
        "cohort" = {
            formula <- private$.buildCohortFormula()
        },
        "cross_sectional" = {
            formula <- private$.buildCrossSectionalFormula()
        },
        "nested_case_control" = {
            formula <- private$.buildNestedCaseControlFormula()
        }
    )
    
    return(formula)
}

.buildCaseControlFormula = function() {
    # Case-control study formula
    case_status <- jmvcore::constructFormula(terms = self$options$caseStatusVariable)
    exposure_vars <- jmvcore::constructFormula(terms = self$options$exposureVariables)
    confounders <- jmvcore::constructFormula(terms = self$options$confoundingVariables)
    
    # Include matching variables if matched design
    if (self$options$matchedDesign) {
        matching_vars <- jmvcore::constructFormula(terms = self$options$matchingVariables)
        
        # Conditional logistic regression formula
        all_terms <- c(exposure_vars, confounders)
        formula_string <- paste(case_status, "~ ", paste(all_terms, collapse = " + "))
        
        # Add strata for matching
        if (length(matching_vars) > 0) {
            strata_term <- paste0("strata(", paste(matching_vars, collapse = ", "), ")")
            formula_string <- paste(formula_string, "+", strata_term)
        }
    } else {
        # Unconditional logistic regression
        all_terms <- c(exposure_vars, confounders)
        formula_string <- paste(case_status, "~", paste(all_terms, collapse = " + "))
    }
    
    return(as.formula(formula_string))
}

.buildCohortFormula = function() {
    # Cohort study - can be survival or logistic depending on outcome
    outcome_type <- private$.determineOutcomeType()
    
    exposure_vars <- jmvcore::constructFormula(terms = self$options$exposureVariables)
    confounders <- jmvcore::constructFormula(terms = self$options$confoundingVariables)
    
    if (outcome_type == "time_to_event") {
        # Survival analysis
        time_var <- jmvcore::constructFormula(terms = self$options$followUpTime)
        event_var <- jmvcore::constructFormula(terms = self$options$eventIndicator)
        
        surv_object <- paste0("survival::Surv(", time_var, ", ", event_var, ")")
        all_terms <- c(exposure_vars, confounders)
        
        formula_string <- paste(surv_object, "~", paste(all_terms, collapse = " + "))
    } else {
        # Binary or continuous outcome
        outcome_var <- jmvcore::constructFormula(terms = self$options$outcomeVariable)
        all_terms <- c(exposure_vars, confounders)
        
        formula_string <- paste(outcome_var, "~", paste(all_terms, collapse = " + "))
    }
    
    return(as.formula(formula_string))
}
```

## 9. Performance Optimization

### Formula Caching Strategies

```R
# Cache frequently used formulas
private = list(
    .formula_cache = list(),
    
    .getCachedFormula = function(cache_key) {
        if (cache_key %in% names(private$.formula_cache)) {
            return(private$.formula_cache[[cache_key]])
        }
        return(NULL)
    },
    
    .cacheFormula = function(cache_key, formula) {
        private$.formula_cache[[cache_key]] <- formula
        
        # Limit cache size
        if (length(private$.formula_cache) > 50) {
            # Remove oldest entries
            private$.formula_cache <- private$.formula_cache[-1]
        }
    },
    
    .buildCachedFormula = function() {
        # Create cache key from current options
        cache_key <- private$.generateFormulaCacheKey()
        
        # Check cache first
        cached_formula <- private$.getCachedFormula(cache_key)
        if (!is.null(cached_formula)) {
            return(cached_formula)
        }
        
        # Build new formula
        new_formula <- private$.buildFormula()
        
        # Cache result
        private$.cacheFormula(cache_key, new_formula)
        
        return(new_formula)
    },
    
    .generateFormulaCacheKey = function() {
        # Create unique key from relevant options
        key_components <- list(
            dependent = self$options$dependentVar,
            predictors = sort(self$options$predictorVars),
            interactions = self$options$includeInteractions,
            transformations = self$options$transformations
        )
        
        # Create hash of components
        cache_key <- digest::digest(key_components, algo = "md5")
        return(cache_key)
    }
)
```

### Efficient Formula Construction for Large Models

```R
.buildLargeModelFormula = function() {
    # Pre-validate components to avoid expensive operations
    components <- private$.prevalidateComponents()
    if (!components$valid) {
        return(NULL)
    }
    
    # Use vectorized operations for large predictor sets
    if (length(self$options$predictorVars) > 100) {
        formula <- private$.buildLargeFormulaVectorized()
    } else {
        formula <- private$.buildStandardFormula()
    }
    
    return(formula)
}

.buildLargeFormulaVectorized = function() {
    # Efficient processing for many predictors
    predictors <- self$options$predictorVars
    
    # Batch process variable name construction
    safe_predictors <- jmvcore::constructFormula(terms = predictors)
    
    # Use efficient string operations
    dependent <- jmvcore::constructFormula(terms = self$options$dependentVar)
    
    # Build formula string efficiently
    predictor_string <- paste(safe_predictors, collapse = " + ")
    formula_string <- paste(dependent, "~", predictor_string)
    
    return(as.formula(formula_string))
}
```

### Memory-Efficient Formula Handling

```R
.handleLargeFormulaMemoryEfficiently = function() {
    # Process formula in chunks to manage memory
    predictor_chunks <- private$.chunkPredictors()
    
    chunk_formulas <- list()
    
    for (i in seq_along(predictor_chunks)) {
        chunk_predictors <- predictor_chunks[[i]]
        
        # Build formula for this chunk
        chunk_formula <- private$.buildChunkFormula(chunk_predictors)
        
        # Process immediately to reduce memory usage
        chunk_result <- private$.processFormulaChunk(chunk_formula)
        
        # Store only essential results
        chunk_formulas[[i]] <- chunk_result$essential_info
        
        # Clean up chunk data
        rm(chunk_formula, chunk_result)
        gc()
    }
    
    # Combine chunk results
    final_result <- private$.combineChunkResults(chunk_formulas)
    
    return(final_result)
}

.chunkPredictors = function(chunk_size = 20) {
    predictors <- self$options$predictorVars
    
    if (length(predictors) <= chunk_size) {
        return(list(predictors))
    }
    
    # Split into chunks
    n_chunks <- ceiling(length(predictors) / chunk_size)
    chunks <- split(predictors, cut(seq_along(predictors), n_chunks, labels = FALSE))
    
    return(chunks)
}
```

## 10. Complete Implementation Examples

### Example 1: Comprehensive Survival Analysis Formula Builder

```R
.buildComprehensiveSurvivalFormula = function() {
    tryCatch({
        # Step 1: Validate survival-specific requirements
        survival_validation <- private$.validateSurvivalComponents()
        if (!survival_validation$valid) {
            private$.displaySurvivalErrors(survival_validation$errors)
            return(NULL)
        }
        
        # Step 2: Build survival object
        surv_object <- private$.buildSurvivalObject()
        
        # Step 3: Build predictor components
        predictor_components <- private$.buildSurvivalPredictors()
        
        # Step 4: Combine into complete formula
        complete_formula <- private$.assembleSurvivalFormula(surv_object, predictor_components)
        
        # Step 5: Validate final formula
        final_validation <- private$.validateFormula(complete_formula)
        if (!final_validation$valid) {
            private$.handleValidationErrors(final_validation$errors)
            return(NULL)
        }
        
        return(complete_formula)
        
    }, error = function(e) {
        private$.handleFormulaError("Survival formula construction", e)
        return(NULL)
    })
}

.buildSurvivalObject = function() {
    # Get time and event variables
    time_var <- jmvcore::constructFormula(terms = self$options$timeVariable)
    event_var <- jmvcore::constructFormula(terms = self$options$eventVariable)
    
    # Handle different survival data types
    survival_type <- self$options$survivalDataType
    
    switch(survival_type,
        "right_censored" = {
            surv_call <- paste0("survival::Surv(", time_var, ", ", event_var, ")")
        },
        "left_censored" = {
            surv_call <- paste0("survival::Surv(", time_var, ", ", event_var, ", type='left')")
        },
        "interval_censored" = {
            time2_var <- jmvcore::constructFormula(terms = self$options$time2Variable)
            surv_call <- paste0("survival::Surv(", time_var, ", ", time2_var, ", ", event_var, ", type='interval')")
        },
        "counting_process" = {
            start_var <- jmvcore::constructFormula(terms = self$options$startTimeVariable)
            surv_call <- paste0("survival::Surv(", start_var, ", ", time_var, ", ", event_var, ")")
        },
        # Default to right censored
        {
            surv_call <- paste0("survival::Surv(", time_var, ", ", event_var, ")")
        }
    )
    
    return(surv_call)
}

.buildSurvivalPredictors = function() {
    predictors <- list()
    
    # Main treatment/exposure variables
    if (!is.null(self$options$treatmentVariables)) {
        treatment_vars <- jmvcore::constructFormula(terms = self$options$treatmentVariables)
        predictors$treatment <- treatment_vars
    }
    
    # Baseline covariates
    if (!is.null(self$options$baselineCovariates)) {
        baseline_vars <- jmvcore::constructFormula(terms = self$options$baselineCovariates)
        predictors$baseline <- baseline_vars
    }
    
    # Time-varying covariates
    if (self$options$includeTimeVarying && !is.null(self$options$timeVaryingCovariates)) {
        tv_vars <- jmvcore::constructFormula(terms = self$options$timeVaryingCovariates)
        tv_terms <- paste0("tt(", tv_vars, ")")
        predictors$time_varying <- tv_terms
    }
    
    # Stratification variables
    if (self$options$useStratification && !is.null(self$options$stratificationVariables)) {
        strata_vars <- jmvcore::constructFormula(terms = self$options$stratificationVariables)
        strata_terms <- paste0("strata(", paste(strata_vars, collapse = ", "), ")")
        predictors$strata <- strata_terms
    }
    
    # Frailty/random effects
    if (self$options$includeFrailty && !is.null(self$options$clusterVariable)) {
        cluster_var <- jmvcore::constructFormula(terms = self$options$clusterVariable)
        frailty_term <- paste0("frailty(", cluster_var, ")")
        predictors$frailty <- frailty_term
    }
    
    # Spline terms for nonlinear effects
    if (self$options$includeSplines && !is.null(self$options$splineVariables)) {
        spline_specs <- self$options$splineVariables
        spline_terms <- sapply(spline_specs, function(spec) {
            var_name <- jmvcore::constructFormula(terms = spec$variable)
            df <- spec$degrees_freedom %||% 4
            paste0("pspline(", var_name, ", df=", df, ")")
        })
        predictors$splines <- spline_terms
    }
    
    return(predictors)
}

.assembleSurvivalFormula = function(surv_object, predictor_components) {
    # Flatten predictor components
    all_predictors <- unlist(predictor_components, use.names = FALSE)
    
    if (length(all_predictors) == 0) {
        # Intercept-only model
        formula_string <- paste(surv_object, "~ 1")
    } else {
        # Full model
        predictor_string <- paste(all_predictors, collapse = " + ")
        formula_string <- paste(surv_object, "~", predictor_string)
    }
    
    return(as.formula(formula_string))
}
```

## 11. Best Practices

### Formula Construction Standards

1. **Always Use `jmvcore::constructFormula()`**
   - Ensures safe variable name handling
   - Handles special characters and spaces
   - Prevents formula injection attacks

2. **Validate at Every Step**
   - Check component validity before assembly
   - Validate final formula before use
   - Provide meaningful error messages

3. **Handle Edge Cases**
   - Empty predictor lists (intercept-only models)
   - Single predictor scenarios
   - Special characters in variable names
   - Missing or invalid data types

4. **Optimize for Performance**
   - Cache frequently used formulas
   - Use vectorized operations for large models
   - Implement memory-efficient processing

5. **Maintain Transparency**
   - Log formula construction steps
   - Display final formulas to users
   - Provide clear error explanations

### Code Organization Patterns

```R
# Recommended structure for formula-heavy analyses
private = list(
    # Core formula building
    .buildMainFormula = function() { ... },
    .buildAlternativeFormulas = function() { ... },
    
    # Component builders
    .buildPredictorTerms = function() { ... },
    .buildInteractionTerms = function() { ... },
    .buildTransformationTerms = function() { ... },
    
    # Validation functions
    .validateFormulaComponents = function() { ... },
    .validateFinalFormula = function() { ... },
    
    # Error handling
    .handleFormulaErrors = function() { ... },
    .translateErrorMessages = function() { ... },
    
    # Utility functions
    .formatFormulaForDisplay = function() { ... },
    .generateFormulaCode = function() { ... }
)
```

## 12. Troubleshooting Guide

### Common Formula Issues

#### Issue: "Object not found" Errors

**Symptoms:**
- Error messages mentioning missing variables
- Formula evaluation fails
- Analysis doesn't run

**Solutions:**
```R
# Always validate variable existence
.validateVariableExistence = function(formula) {
    formula_vars <- all.vars(formula)
    data_vars <- names(self$data)
    missing_vars <- setdiff(formula_vars, data_vars)
    
    if (length(missing_vars) > 0) {
        error_msg <- paste("Variables not found in data:", 
                          paste(missing_vars, collapse = ", "))
        stop(error_msg)
    }
}
```

#### Issue: Formula Syntax Errors

**Symptoms:**
- "Invalid formula" messages
- Unexpected formula parsing results
- Formula conversion fails

**Solutions:**
```R
# Validate formula syntax before use
.validateFormulaSyntax = function(formula_string) {
    tryCatch({
        test_formula <- as.formula(formula_string)
        terms(test_formula)  # Test parsing
        return(TRUE)
    }, error = function(e) {
        private$.handleSyntaxError(e)
        return(FALSE)
    })
}
```

#### Issue: Model Complexity Problems

**Symptoms:**
- Convergence failures
- Memory issues
- Extremely slow processing

**Solutions:**
```R
# Monitor and control model complexity
.assessModelComplexity = function(formula) {
    n_predictors <- length(attr(terms(formula), "term.labels"))
    n_observations <- nrow(self$data)
    
    complexity_ratio <- n_predictors / n_observations
    
    if (complexity_ratio > 0.1) {
        warning("Model may be too complex for available data")
        return(private$.suggestSimplification())
    }
    
    return(formula)
}
```

This comprehensive guide provides the foundation for implementing sophisticated, reliable, and maintainable formula systems in jamovi modules. The patterns and examples demonstrate best practices for clinical research applications while ensuring robust error handling and optimal performance.