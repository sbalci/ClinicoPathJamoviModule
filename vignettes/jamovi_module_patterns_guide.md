# Jamovi Module Development Patterns Guide

**Source:** Analysis of jmvbaseR example module and ClinicoPath implementations
**Date:** 2025-01-17
**Purpose:** Document key implementation patterns for jamovi module development

---

## Table of Contents

1. [Module Structure](#module-structure)
2. [Four-File Architecture](#four-file-architecture)
3. [Data Handling Patterns](#data-handling-patterns)
4. [State Management](#state-management)
5. [Formula Building](#formula-building)
6. [Syntax Generation (.asSource)](#syntax-generation)
7. [Output Patterns](#output-patterns)
8. [Best Practices](#best-practices)

---

> **Before a library submission, read `vignettes/jamovi_library_review_guide.md`.**
> It distils five real jamovi library audit reports into a pre-submission
> checklist and the ten findings that recur across every module — unguarded
> `image$state`, opaque light-theme HTML, fixed table rows built in `.run()`,
> `setVisible(FALSE)` used to signal failure, named HTML entities, invisible
> `warning()` calls, spliced `.()` fragments, and undeclared base packages.

## Module Structure

### Standard Directory Layout

```
ModuleName/
├── DESCRIPTION           # R package metadata
├── NAMESPACE            # R package exports
├── jamovi/
│   ├── 0000.yaml       # Module metadata & menu structure
│   ├── function.a.yaml # Analysis definition (options)
│   ├── function.r.yaml # Results definition (outputs)
│   ├── function.u.yaml # User interface definition
│   └── js/             # Custom JavaScript (optional)
├── R/
│   ├── function.b.R    # Backend implementation
│   └── function.h.R    # Auto-generated header (DO NOT EDIT)
├── data/               # Example datasets
├── data-raw/           # Data generation scripts
└── vignettes/          # Documentation
```

### Module Metadata (0000.yaml)

**Example from jmvbaseR:**

```yaml
---
title: Base R
name: jmvbaseR
version: 1.1.2
jms: '1.0'
authors:
  - Jonathon Love
maintainer: Jonathon Love <jon@thon.cc>
date: '2018-02-15'
type: R
description: >
  A simple module which makes the analyses from the stats package useable from jamovi.
analyses:
  - title: One Sample T-Test
    name: ttestOneS
    ns: jmvbaseR                    # Namespace
    menuGroup: Base R
    menuSubgroup: T-Test
    menuTitle: One Sample T-Test
```

**Key Fields:**
- `name`: Package/module name (must match DESCRIPTION)
- `ns`: Namespace for all analyses
- `analyses`: List of functions available in menu
- `menuGroup` / `menuSubgroup`: Menu organization

### DESCRIPTION: Dependencies (`Imports` vs `Remotes`)

The `DESCRIPTION` file declares the module's package dependencies. Two fields matter for
non-CRAN packages:

- **`Imports:`** - *declares* the dependency. This is where you list every package your
  code actually needs at run time, regardless of where it lives.
- **`Remotes:`** - *locates* the dependency. This tells the installer where to fetch a
  package that isn't on CRAN (e.g. a GitHub repo like `gastonstat/arcdiagram`).

**A package can now appear in BOTH `Imports` and `Remotes`.**

> **jmvtools change (current behavior):** You can list the same package in both `Imports`
> and `Remotes`. Previously this was harmful: jmvtools would first try to install the
> import from the CRAN mirror, *then* install it again from the remote - redundant at best,
> and a hard failure at worst when the package isn't on CRAN. The current jmvtools
> **suppresses the CRAN-mirror download of an import when that package also appears in
> `Remotes`**, fetching it only from the remote. This is the intended design:
> **`Imports` declares the dependency; `Remotes` says where to find it.**

```dcf
Imports:
    arcdiagram,
    gtExtras,
    tidyplots,
    waffle
Remotes:
    gastonstat/arcdiagram,
    jthomasmock/gtExtras,
    jbengler/tidyplots,
    ClinicoPath/waffle
```

**Base-priority packages count too.** `grDevices`, `grid`, `stats`, `utils`,
`methods`, `tools` ship with every R installation and cannot go missing, so there
is no runtime fragility — but `R CMD check` treats an undeclared `::` call or
`importFrom()` as a **declaration error**:

```
WARNING: '::' or ':::' import not declared from: 'grDevices'
WARNING: Namespace dependencies not required: 'stats' 'utils'
```

That's a check failure standing between you and a clean build, and it's easy to
miss because the module runs perfectly in jamovi regardless. The jamovi library
reviewer raised it against two modules. If your dependency guard test excludes
base-priority packages wholesale, narrow the exclusion to `base` itself.

**Runtime dependencies must be `Imports`, never `Suggests`.** jamovi installs
`Imports` on first run and cannot install a missing package on demand, so a
`requireNamespace()`-guarded runtime dependency parked in `Suggests` is simply a
broken analysis for the user. Accept the CRAN "unused Imports" NOTE. Only
`testthat` / `knitr` / `rmarkdown` belong in `Suggests`.

**Declared-but-unused is a finding too.** A package held alive only by an
`@importFrom` roxygen tag with no call site is a package every user installs for
nothing. So is a `Remotes:` entry for a dependency you have since vendored or
dropped — it can only cause an unnecessary GitHub fetch at install time.

**Pin every `Remotes:` entry to a full commit SHA**, so a push to the upstream
default branch cannot silently change what a build pulls in.

**Practical notes:**

- Today there's no *functional* difference for installation if a dependency is listed only
  in `Remotes` and omitted from `Imports` - it will still be installed. However, the
  package reviewer ("claudia") may encourage you to add the package to `Imports` too, since
  `Imports` is the canonical place to declare what your code depends on.
- Best practice going forward: **list every real run-time dependency in `Imports`**, and add
  a `Remotes:` entry only for those not available on CRAN.

---

## Four-File Architecture

Every jamovi analysis consists of **4 core files**:

### 1. Analysis Definition (.a.yaml)

Defines **options** (function arguments).

**Example: One Sample T-Test**

```yaml
---
name: ttestOneS
title: One Sample T-Test
version: '1.0.0'
jas: '1.2'

options:
    - name: data
      type: Data
      description:
          R: the data as a data frame

    - name: x
      title: x
      type: Variable
      description:
          R: a string specifying the variable in `data`

    - name: alternative
      title: alternative
      type: List
      options:
        - two.sided
        - less
        - greater
      default: two.sided
      description:
          R: `'two.sided'`, `'less'`, or `'greater'`

    - name: mu
      title: mu
      type: Number
      default: 0
      description:
          R: a number specifying the mean of the null hypothesis

    - name: confLevel
      title: conf.level
      type: Number
      default: 0.95
      description:
          R: a number between 0 and 1 specifying the confidence interval width
```

**Option Types:**
- `Data` - The dataset
- `Variable` - Single variable selector
- `Variables` - Multiple variable selector
- `Terms` - Model terms (interactions)
- `Level` - Factor level selector
- `List` - Dropdown options
- `Bool` - Checkbox
- `Number` - Numeric input
- `String` - Text input

### 2. Backend Implementation (.b.R)

Contains R6 class with analysis logic.

**Structure:**

```r
functionClass <- R6::R6Class(
    "functionClass",
    inherit = functionBase,  # Auto-generated from YAML
    private = list(
        .init = function() {
            # Initialize results objects
        },
        .run = function() {
            # Main analysis logic
        },
        # Helper methods
        .formula = function() { ... }
    ),
    public = list(
        asSource = function() {
            # Generate R syntax (for syntax mode)
        }
    )
)
```

### 3. Results Definition (.r.yaml)

Defines **outputs** (tables, plots, HTML).

**Example (Empty for Preformatted):**

```yaml
---
name: ttestOneS
title: One Sample T-Test
jrs: '1.1'

items: []  # Preformatted output added programmatically
```

**Example (Structured Results):**

```yaml
---
name: anova
title: ANOVA
jrs: '1.1'

items:
    - name: main
      title: ANOVA
      type: Table
      rows: 0
      columns:
        - name: name
          title: ''
          type: text
        - name: ss
          title: Sum of Squares
          type: number
        - name: df
          title: df
          type: integer
        - name: ms
          title: Mean Square
          type: number
        - name: F
          title: F
          type: number
        - name: p
          title: p
          type: number
          format: zto,pvalue
```

### 4. User Interface Definition (.u.yaml)

Defines UI layout and controls.

**Example:**

```yaml
name: ttestOneS
title: One Sample T-Test
jus: '2.0'
children:
  - type: VariableSupplier
    persistentItems: false
    stretchFactor: 1
    children:
      - type: TargetLayoutBox
        children:
          - name: x
            type: VariablesListBox
            label: x
            maxItemCount: 1
            isTarget: true
  - type: LayoutBox
    margin: large
    children:
      - type: ComboBox
        name: alternative
        label: Alternative hypothesis
        options:
          - title: Two sided
            name: two.sided
          - title: Less
            name: less
          - title: Greater
            name: greater
      - type: TextBox
        name: mu
        label: mu
        format: number
      - type: TextBox
        name: confLevel
        label: Confidence interval width
        format: number
```

**UI Control Types:**
- `VariablesListBox` - Variable selector
- `ComboBox` - Dropdown
- `CheckBox` - Boolean toggle
- `TextBox` - Numeric/text input
- `LevelSelector` - Factor level picker
- `LayoutBox` - Container for grouping
- `CollapseBox` - Collapsible section
- `TargetLayoutBox` - Drop target for variables

---

## Two top-level functions with the same name silently shadow each other

Every `R/*.b.R` file contributes its top-level functions to **one** package
namespace. Two files defining `.getDisplayName()` do not get one each — the file
that comes **last in the `Collate:` field of `DESCRIPTION` wins**, and every
caller in the package gets that one, including callers in the other file.

This is silent. No warning at build, no warning at load, nothing in `R CMD check`.

It cost a real bug here: `R/crosstable.b.R` defined `.getDisplayName()` using
`mapping[[name]]` (drops names); `R/survival.b.R` defined a *different*
`.getDisplayName()` using `mapping[name]` (**keeps** names). `survival.b.R`
collates later, so `crosstable` had been calling survival's version all along.
Harmless while the result only went into a plot title — and fatal the moment the
value became a `setRow()` rowKey, because a named character is not `identical()`
to a plain one.

Find them:

```bash
python3 - <<'EOF'
import re, glob, collections
pat = re.compile(r'^([A-Za-z._][\w.]*)\s*(?:<-|=)\s*function\s*\(', re.M)
defs = collections.defaultdict(list)
for f in sorted(glob.glob('R/*.R')):
    src = open(f, encoding='utf-8').read()
    for m in pat.finditer(src):
        defs[m.group(1)].append('%s:%d' % (f, src[:m.start()].count(chr(10)) + 1))
for k, v in defs.items():
    if len(v) > 1:
        print(k, '->', ' | '.join(v))
EOF
```

(Unindented members of an R6 `private = list(...)` produce false positives —
confirm a hit is really at top level before acting on it.)

**Rules:**

- Prefix a shared helper with the analysis that owns it —
  `.crosstableDisplayName()`, `.survivalDisplayName()` — or move the single
  canonical version into `R/utils.R` and delete the copies.
- Never hand-write the public wrapper in `.b.R`: the generated `.h.R` defines it
  too, collates after, and wins, leaving your version dead while still emitting a
  duplicate `@export`.

## Data Handling Patterns

### Accessing Data

```r
# Get the full dataset
data <- self$data

# Access a specific column (by option name)
x <- self$data[[self$options$x]]

# Convert to numeric (handles factors, strings)
x_numeric <- jmvcore::toNumeric(data[[self$options$x]])

# Select multiple columns
mydata <- jmvcore::select(data, c(var1, var2, var3))

# Remove missing values
mydata <- jmvcore::naOmit(mydata)
```

### Variable Name Safety

```r
# Escape variable names with special characters
.escapeVar <- function(x) {
    gsub("[^A-Za-z0-9_]+", "_", make.names(x))
}

# Use in data access
time_var <- data[[private$.escapeVar(self$options$time)]]
```

### Type Conversion

```r
# Numeric conversion (safe)
age_numeric <- jmvcore::toNumeric(mydata[[age]])

# Factor conversion
gender_factor <- as.factor(mydata[[gender]])

# Check for conversion failures
if (any(is.na(age_numeric)) && !all(is.na(mydata[[age]]))) {
    # Some values failed to convert
}
```

---

## State Management

### Why Use State?

jamovi uses **state** to determine when to re-render plots. If state doesn't change, plots won't update even if options change.

### State for Plots

**Problem:** Visual options (colors, titles) don't trigger plot updates.

**Solution:** Include options in state.

```r
# .run() method
plotState <- list(
    data = plotData,                      # The actual data
    plot_title = self$options$plot_title, # Visual options
    color_palette = self$options$color_palette,
    color1 = self$options$color1,
    color2 = self$options$color2,
    plot_engine = self$options$plot_engine
)
image$setState(plotState)
```

**In .plot() method:**

```r
# Retrieve state
plotState <- image$state

# Extract data and options
if (is.list(plotState) && !is.null(plotState$data)) {
    plotData <- plotState$data
    plot_title <- plotState$plot_title
    color_palette <- plotState$color_palette
    # ...
} else {
    # Fallback for legacy state format
    plotData <- plotState
}
```

### State Serialization Issues

**Problem:** jamovi's protobuf serialization converts tibbles → lists, causing `dplyr::pull()` errors.

**Solution:** Convert to base data.frame before `setState()`.

```r
# Helper function
.ensureDataFrame <- function(data) {
    if (is.null(data)) return(NULL)

    if (is.list(data) && !is.data.frame(data)) {
        data <- as.data.frame(data, stringsAsFactors = FALSE)
    }

    if (is.data.frame(data)) {
        data <- as.data.frame(data, stringsAsFactors = FALSE)
    }

    return(data)
}

# Usage
plotData <- private$.ensureDataFrame(plotData)
image$setState(plotState)
```

---

## Formula Building

### Pattern from jmvbaseR

**Helper Method: `.formula()`**

```r
.formula = function() {
    terms <- self$options$modelTerms
    if (is.null(terms))
        terms <- private$.ff()  # Generate default full-factorial

    # Compose terms into formula strings
    terms <- jmvcore::composeTerms(terms)
    rhs <- paste0(terms, collapse=' + ')

    # Compose dependent variable (handles special characters)
    lhs <- jmvcore::composeTerm(self$options$dep)

    # Build formula
    formula <- paste0(lhs, ' ~ ', rhs)
    formula
}
```

**Helper Method: `.ff()` (Full Factorial)**

```r
.ff = function() {
    fixedFactors <- self$options$ind

    if (length(fixedFactors) > 1) {
        # Create full factorial interaction formula
        formula <- as.formula(paste('~', paste(paste0('`', fixedFactors, '`'), collapse='*')))
        terms <- attr(stats::terms(formula), 'term.labels')
        modelTerms <- sapply(terms, function(x) as.list(strsplit(x, ':')), USE.NAMES=FALSE)
    } else {
        modelTerms <- as.list(fixedFactors)
    }

    # Remove backticks from variable names
    for (i in seq_along(modelTerms)) {
        term <- modelTerms[[i]]
        quoted <- grepl('^`.*`$', term)
        term[quoted] <- substring(term[quoted], 2, nchar(term[quoted])-1)
        modelTerms[[i]] <- term
    }

    modelTerms
}
```

### Using in Analysis

```r
.run = function() {
    # Build formula
    formula <- private$.formula()
    formula <- as.formula(formula)

    # Fit model
    model <- stats::lm(formula=formula, data=data)

    # Get results
    r <- stats::anova(model)
}
```

---

## Syntax Generation (.asSource)

For teaching/learning, jamovi can show equivalent R code via "syntax mode".

### Public Method: `asSource()`

```r
public = list(
    asSource = function() {
        # Generate R code string
        formula <- private$.formula()

        model <- paste0('model <- lm(\n',
                       '    formula = ', formula, ',\n',
                       '    data = data\n',
                       ')\n')

        anova <- paste0('anova(model)\n')

        paste(model, anova, sep='\n')
    }
)
```

### Private Method: `.sourcifyOption()`

Controls which options appear in generated syntax.

```r
.sourcifyOption = function(option) {
    # Omit 'x' from syntax (it's the main argument)
    if (option$name == 'x')
        return('')

    # Only show varEqual if TRUE
    if (option$name == 'varEqual') {
        if (isTRUE(option$value))
            return('var.equal=TRUE')
        else
            return('')
    }

    # Only show confLevel if different from default
    if (option$name == 'confLevel') {
        if (isTRUE(all.equal(option$value, 0.95)))
            return('')
        else
            return(paste0('conf.level=', option$value))
    }

    # Use parent implementation for other options
    super$.sourcifyOption(option)
}
```

### Helper Method: `.asArgs()`

Combines all options into argument string.

```r
public = list(
    asSource = function() {
        args <- private$.asArgs()  # Calls .sourcifyOption() for each
        if (args != '')
            args <- paste0(',', args)

        paste0('t.test(\n    x=data$x', args, ')')
    }
)
```

**Generated Syntax Example:**

```r
t.test(
    x=data$x,
    alternative='greater',
    mu=10
)
```

---

## Output Patterns

### Pattern 1: Preformatted R Output

**Use Case:** Teaching R, showing raw R output

```r
.init = function() {
    # Create preformatted output object
    preformatted <- jmvcore::Preformatted$new(self$options, 'pre')
    self$results$add(preformatted)
}

.run = function() {
    # Get the preformatted object
    pre <- self$results$get('pre')
    pre$content <- ''

    if (is.null(self$options$x))
        return()

    # Run analysis
    result <- t.test(x=x, ...)

    # Capture R console output
    pre$content <- paste0(capture.output(result), collapse='\n')
}
```

### Pattern 2: Structured Tables

**Define in .r.yaml:**

```yaml
items:
    - name: main
      type: Table
      columns:
        - name: term
          title: Term
          type: text
        - name: estimate
          title: Estimate
          type: number
        - name: se
          title: SE
          type: number
        - name: t
          title: t
          type: number
        - name: p
          title: p
          type: number
          format: zto,pvalue
```

**Populate in .b.R:**

```r
.run = function() {
    table <- self$results$main

    # Clear previous results
    table$deleteRows()

    # Add rows
    for (i in 1:nrow(results_df)) {
        table$addRow(rowKey=i, values=results_df[i,])
    }
}
```

### Pattern 3: Images/Plots

**Define in .r.yaml:**

```yaml
items:
    - name: plot
      title: Age Pyramid
      type: Image
      width: 600
      height: 450
      renderFun: .plot
      requiresData: true
      clearWith:
        - age
        - gender
        - bin_width
        - color_palette
```

**Implement in .b.R:**

```r
.run = function() {
    # Prepare data
    plotData <- # ... data preparation

    # Save state
    image <- self$results$plot
    image$setState(plotData)
}

.plot = function(image, ggtheme, theme, ...) {
    # Retrieve state
    plotData <- image$state

    # Create plot
    plot <- ggplot2::ggplot(data = plotData, ...) + ...

    # Apply jamovi theme
    plot <- plot + ggtheme

    # Render
    print(plot)
    return(TRUE)
}
```

### Pattern 4: HTML Content

```r
.run = function() {
    html_content <- "<div style='...'>
        <h3>Data Summary</h3>
        <p>N = ... </p>
    </div>"

    self$results$dataInfo$setContent(html_content)
}
```

---

## Best Practices

### 1. R6 Class Structure

✅ **DO:**
- Define helper methods in `private` list
- Put public APIs in `public` list
- Use `.methodName` convention for private methods
- Inherit from auto-generated `functionBase` class

❌ **DON'T:**
- Try to modify `private` environment after initialization (it's locked)
- Edit `.h.R` files (auto-generated)

### 2. Data Access

✅ **DO:**
- Use `jmvcore::toNumeric()` for safe type conversion
- Use `jmvcore::select()` for column selection
- Use `jmvcore::naOmit()` for removing missing values
- Escape variable names with special characters

❌ **DON'T:**
- Assume data types (always convert)
- Use `data$columnName` directly (column names may have spaces)

### 3. State Management

✅ **DO:**
- Include visual options in state to trigger plot updates
- Convert to base data.frame before `setState()`
- Validate state in `.plot()` before use
- Provide fallback for legacy state formats

❌ **DON'T:**
- Assume state is always a data.frame
- Store complex objects that can't serialize

### 4. Error Handling

✅ **DO:**
- Check for NULL/empty variables before processing
- Use early returns for invalid states
- Provide helpful error messages
- Validate data types and ranges

❌ **DON'T:**
- Let errors crash without user-friendly messages
- Continue processing with invalid data

### 5. Options & Results

✅ **DO:**
- Use descriptive option names
- Provide defaults for all options
- Document options in `.a.yaml` descriptions
- List all options in `clearWith` for dependent results

❌ **DON'T:**
- Use undocumented magic values
- Forget to clear results when dependencies change

### 6. Performance

✅ **DO:**
- Use `private$.checkpoint()` before expensive operations
- Add checkpoints periodically in long loops
- Inform users about long-running analyses
- Cache intermediate results when appropriate

❌ **DON'T:**
- Run expensive computations in `.init()`
- Recompute unchanged results

### 7. UI/UX

✅ **DO:**
- Group related options in `CollapseBox`
- Use descriptive labels and titles
- Show/hide options with `enable` conditions
- Provide welcome messages when no variables selected

❌ **DON'T:**
- Create flat, unorganized option panels
- Use cryptic abbreviations

---

## Key jmvcore Functions

| Function | Purpose | Example |
|----------|---------|---------|
| `jmvcore::toNumeric()` | Safe numeric conversion | `x <- jmvcore::toNumeric(data[[var]])` |
| `jmvcore::select()` | Safe column selection | `mydata <- jmvcore::select(data, c(v1, v2))` |
| `jmvcore::naOmit()` | Remove missing values | `mydata <- jmvcore::naOmit(mydata)` |
| `jmvcore::composeTerm()` | Build formula term (escapes names) | `lhs <- jmvcore::composeTerm(depVar)` |
| `jmvcore::composeTerms()` | Build formula terms | `rhs <- jmvcore::composeTerms(modelTerms)` |
| `jmvcore::Preformatted$new()` | Create preformatted output | `pre <- jmvcore::Preformatted$new(options, 'pre')` |
| `jmvcore::Notice$new()` | Create user notice | `notice <- jmvcore::Notice$new(options, name, type)` |

---

## References

- **jmvbaseR Example Module:** `/Users/serdarbalci/Documents/GitHub/jmvbaseR`
- **jamovi Developer Documentation:** `./vignettes/dev.jamovi.org-master`
- **ClinicoPath Examples:** This repository

---

**End of Guide**