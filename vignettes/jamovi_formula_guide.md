# Guide to Writing Formulas in Jamovi Development

This guide provides a comprehensive overview of how to write and use formulas when developing analyses for the jamovi platform. Formulas are a core concept in R and are used extensively in jamovi to define the relationships between variables in an analysis.

## 1. The Basics of R Formulas

An R formula is an object of class `formula` that has the following basic structure:

```R
response ~ predictor1 + predictor2
```

- The `~` (tilde) is the central operator that separates the response variable (or dependent variable) from the predictor variables (or independent variables).
- The `+` (plus) sign is used to include multiple predictors in the model.

### Common Formula Operators

| Operator | Meaning                  | Example                               |
| :------- | :----------------------- | :------------------------------------ |
| `~`      | Separates response and predictors | `y ~ x`                               |
| `+`      | Adds a predictor         | `y ~ x1 + x2`                         |
| `-`      | Removes a predictor      | `y ~ x1 + x2 - x1` (same as `y ~ x2`) |
| `*`      | Interaction term         | `y ~ x1 * x2` (same as `y ~ x1 + x2 + x1:x2`) |
| `:`      | Explicit interaction     | `y ~ x1:x2`                           |
| `^`      | Crossing to a degree     | `(x1 + x2 + x3)^2` (all main effects and 2-way interactions) |
| `.`      | All other variables      | `y ~ .`                               |
| `I()`    | "As is" operator         | `y ~ I(x1 + x2)` (treats `x1 + x2` as a single variable) |

## 2. Using Formulas in Jamovi Analyses

In jamovi, formulas are typically constructed dynamically based on the user's input in the UI. The `.b.R` files in your jamovi module are responsible for taking the user's selections and building the appropriate R code to run the analysis.

### 2.1. Constructing Formulas from User Input

The most common way to build a formula in a jamovi analysis is to use the `paste()` or `glue::glue()` function to create a string, and then convert it to a formula object using `as.formula()`.

**Example from `survival.b.R`:**

In the `survival` analysis, the formula for the Kaplan-Meier analysis is constructed as follows:

```R
formula <-
    paste('survival::Surv(',
          mytime,
          ',',
          myoutcome,
          ') ~ ',
          myfactor)

formula <- as.formula(formula)

km_fit <- survival::survfit(formula, data = mydata)
```

Here:
- `mytime`, `myoutcome`, and `myfactor` are variables that hold the names of the columns selected by the user in the jamovi UI.
- `paste()` is used to create the formula string.
- `as.formula()` converts the string to a formula object.
- The resulting formula is then used in the `survival::survfit()` function.

### 2.2. Using `jmvcore::constructFormula`

The `jmvcore` package provides a helper function, `constructFormula()`, which is the recommended way to build formulas from user-supplied variable names. This function handles quoting and other potential issues with variable names.

**Example from `survival.b.R`:**

```R
mytime <- jmvcore::constructFormula(terms = self$options$elapsedtime)
myoutcome <- jmvcore::constructFormula(terms = self$options$outcome)
myfactor <- jmvcore::constructFormula(terms = self$options$explanatory)

myformula <- paste("Surv(", mytime, ",", myoutcome, ")")

finalfit::finalfit(
    .data = mydata,
    dependent = myformula,
    explanatory = myfactor,
    metrics = TRUE
) -> tCox
```

In this example, `jmvcore::constructFormula` is used to safely get the variable names from the user options.

## 3. Handling Different Types of Formulas

Your analysis may need to handle different types of formulas depending on the user's selections.

### 3.1. Simple Formulas (e.g., t-test)

For a simple analysis like an independent samples t-test, the formula would be `dependent_variable ~ grouping_variable`.

### 3.2. Formulas with Multiple Predictors (e.g., Regression)

For a linear regression with multiple predictors, the formula would be `dependent_variable ~ predictor1 + predictor2 + predictor3`.

**Example from `gtsummary.b.R`:**

```R
.createRegressionTable = function(data) {
    # ... (error checking) ...

    outcome <- self$options$vars[1]
    predictors <- self$options$vars[-1]

    formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = data)

    table <- gtsummary::tbl_regression(model)

    return(table)
},
```

This function takes the first selected variable as the outcome and the rest as predictors. It then constructs the formula string and uses it to fit a linear model.

### 3.3. Formulas for Survival Analysis

As seen in the `survival.b.R` example, survival analysis formulas use the `Surv()` function on the left-hand side of the `~`. The `Surv()` function takes the survival time and the event status as arguments.

```R
survival::Surv(time, event) ~ group
```

## 4. Parsing and Manipulating Formulas

Sometimes, you may need to extract parts of a formula. The `terms()` function is useful for this.

```R
my_formula <- y ~ x1 + x2
my_terms <- terms(my_formula)

# Get the response variable
response <- all.vars(my_terms)[1]

# Get the predictor variables
predictors <- all.vars(my_terms)[-1]
```

## 5. Best Practices

- **Always use `jmvcore::constructFormula`** to get variable names from user options. This is the safest and most robust way to handle variable names.
- **Build formulas as strings** using `paste()` or `glue::glue()` and then convert them to formula objects with `as.formula()`.
- **Provide clear error messages** if the user selects an invalid combination of variables for the analysis.
- **Consider generating the R code** that your analysis is running and displaying it to the user (as seen in `gtsummary.b.R`). This improves transparency and reproducibility.

By following these guidelines and examples, you can effectively use formulas to build powerful and flexible analyses in jamovi.

## 6. Advanced Formula Examples from the `jmv` Repository

This section provides more advanced examples of formula construction from the `jmv` repository, which is the core of jamovi's analyses.

### 6.1. Independent Samples t-test (`ttestis.b.R`)

The independent samples t-test in `jmv` uses a private method called `.formula()` to define the formula for the analysis. This is a very clean and concise way to specify the formula.

```R
.formula=function() {
    jmvcore:::composeFormula(self$options$vars, self$options$group)
}
```

Here, `jmvcore:::composeFormula` is a function that takes the dependent variables (`self$options$vars`) and the grouping variable (`self$options$group`) as input and returns a formula object. This approach is more direct than building a formula string and converting it with `as.formula()`.

### 6.2. ANCOVA (`ancova.b.R`)

The ANCOVA analysis in `jmv` demonstrates how to handle more complex models with multiple factors, covariates, and model terms.

#### Defining the Model Terms

The `.modelTerms()` private method is responsible for building the list of model terms. It can either use the user-specified model terms or generate a full factorial model using the `.ff()` method.

```R
.modelTerms=function() {
    modelTerms <- self$options$modelTerms
    if (length(modelTerms) == 0)
        modelTerms <- private$.ff()
    lengths <- vapply(modelTerms, length, 1)
    modelTerms <- modelTerms[order(lengths)]
    modelTerms
},
.ff=function() {
    factors <- self$options$factors
    if (length(factors) > 1) {
        formula <- as.formula(paste('~', paste(paste0('`', factors, '`'), collapse='*')))
        terms <- attr(stats::terms(formula), 'term.labels')
        modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
    } else {
        modelTerms <- as.list(factors)
    }

    for (i in seq_along(modelTerms)) {
        term <- modelTerms[[i]]
        quoted <- grepl('^`.*`$', term)
        term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
        modelTerms[[i]] <- term
    }

    covs <- NULL
    if ('covs' %in% names(self$options))
        covs <- self$options$covs

    for (covariate in covs)
        modelTerms[[ length(modelTerms) + 1 ]] <- covariate

    modelTerms
},
```

#### Constructing the Formula

The `.formula()` method then uses `jmvcore::constructFormula` to build the final formula from the dependent variable and the model terms.

```R
.formula=function() {
    jmvcore:::composeFormula(self$options$dep, self$options$modelTerms)
},
```

These examples from the `jmv` repository demonstrate how to build robust and flexible analyses in jamovi by using a combination of private methods and helper functions from `jmvcore` to handle formula construction.