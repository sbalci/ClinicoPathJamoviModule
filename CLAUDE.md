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

## Namespace and Package Configuration

### Package Namespace Analysis

Current Situation: ✅ The configuration is actually CORRECT

Your package namespace is properly configured as ClinicoPath. Here's the evidence:

✅ Correct Configuration:

1. DESCRIPTION file (line 2): Package: ClinicoPath
2. jamovi 0000.yaml (line 3): name: ClinicoPath
3. jamovi 0000.yaml (line 18): ns: ClinicoPath (namespace)
4. NAMESPACE file: Contains proper exports with ClinicoPath functions
5. All function exports: Use ClinicoPath namespace consistently

📁 Repository vs Package Name:

- Repository/Folder: ClinicoPathJamoviModule
- Package Name: ClinicoPath ✅

This is completely normal and acceptable. Many R packages have descriptive repository names that differ from the package name.

🔍 Why "ClinicoPathJamoviModule" Sometimes Appears:

Installation scenarios where this occurs:

1. Local Development Installation:
   devtools::install()  # Uses folder name if in root directory
2. GitHub Installation with Full Repo Name:
   devtools::install_github("user/ClinicoPathJamoviModule")
3. Manual Installation from Source: Using folder name

✅ Correct Installation Methods:

To ensure "ClinicoPath" namespace is used:

1. From CRAN (future):
   install.packages("ClinicoPath")
2. From GitHub with Package Name:
   devtools::install_github("user/ClinicoPathJamoviModule",
   subdir = ".",
   build_vignettes = FALSE)
3. Development Installation:

# From package root directory

devtools::install(pkg = ".", quick = TRUE)

🚀 Recommendations:

Your current setup is correct! However, to avoid confusion:

1. Documentation: Always refer to the package as "ClinicoPath" in:

- README files
- Documentation
- Installation instructions
- User guides

2. Installation Instructions: Provide clear installation commands:

# Correct installation

devtools::install_github("sbalci/ClinicoPathJamoviModule")
library(ClinicoPath)  # Always loads as ClinicoPath
3. Repository Description: Update GitHub repository description to clarify:
"ClinicoPath R Package - Analysis for Clinicopathological Research"

📋 Summary:

- ✅ Package namespace is correctly configured as "ClinicoPath"
- ✅ All jamovi analyses use ns: ClinicoPath
- 📁 Repository name ClinicoPathJamoviModule is just the container name
- 🎯 Users should always library(ClinicoPath) regardless of installation method

The confusion comes from folder/repository naming vs package naming, which is a common and acceptable practice in R package development.

## Reference Materials and Learning Resources

### jmvbaseR Example Module

**Location:** `/Users/serdarbalci/Documents/GitHub/jmvbaseR`

This is an official jamovi example module designed for teaching R syntax. It demonstrates:
- Clean 4-file architecture patterns
- Formula building (`.formula()` and `.ff()` methods)
- Syntax generation (`.asSource()` and `.sourcifyOption()` methods)
- Preformatted R output display
- Model terms handling with `jmvcore::composeTerms()`

**Available Analyses:**
1. One Sample T-Test (`ttestones`)
2. Independent Samples T-Test (`ttestis`)
3. Paired Samples T-Test (`ttestps`)
4. ANOVA (`anova`)
5. Correlation (`correlation`)
6. Linear Regression (`regression`)

**Note:** This module uses `jmvcore::Preformatted` for all outputs (shows raw R console output) rather than structured tables. It's excellent for learning basic patterns but ClinicoPath uses more sophisticated result presentation.

### Development Guides

Comprehensive guides are available in `vignettes/`:

1. **`jamovi_module_patterns_guide.md`** - NEW comprehensive guide covering:
   - Module structure and 4-file architecture
   - Data handling patterns (jmvcore functions)
   - State management for plots (serialization solutions)
   - Formula building patterns
   - Syntax generation (asSource methods)
   - Output patterns (Preformatted, Tables, Plots, HTML)
   - Best practices and common pitfalls
   - Reference examples from jmvbaseR

2. **Other guides** (use when relevant):
   - `*_guide.md` files for specific feature implementations

### Quick Reference: Key Patterns

**State Management for Plots:**
```r
# Include visual options in state to trigger updates
plotState <- list(
    data = plotData,
    plot_title = self$options$plot_title,
    color_palette = self$options$color_palette
)
image$setState(plotState)
```

**Data Frame Serialization Fix:**
```r
# Convert to base data.frame before setState() to avoid protobuf issues
plotData <- private$.ensureDataFrame(plotData)
```

**Formula Building:**
```r
# Use jmvcore helpers for safe variable names
lhs <- jmvcore::composeTerm(self$options$dep)
rhs <- jmvcore::composeTerms(modelTerms)
formula <- paste0(lhs, ' ~ ', paste(rhs, collapse=' + '))
```

## Development Memories

### Vignette Management System

The project now uses a sophisticated domain-based vignette copying system in updateModules:

**Domain-Based Vignette Distribution**:

- Vignettes are automatically copied to modules based on domain prefixes in filenames
- Domain patterns: `clinicopath-descriptives-*`, `jjstatsplot-*`, `meddecide-*`, `jsurvival-*`, `general-*`
- Configuration: `updateModules_config.yaml` > `vignette_domains` section
- Special files and exclude patterns are handled via configuration

**Domain-to-Module Mapping**:

```yaml
domain_mapping:
  clinicopath-descriptives: ["ClinicoPathDescriptives"]
  jjstatsplot: ["jjstatsplot"]
  meddecide: ["meddecide"] 
  jsurvival: ["jsurvival"]
  general: ["jjstatsplot", "meddecide", "jsurvival", "ClinicoPathDescriptives"]
```

**Key Features**:

- Automatic discovery of all vignette files (.qmd, .Rmd, .md)
- Pattern-based exclusion of legacy/temp files
- Special file handling for cross-module content
- Statistical reporting of copy operations
- Backward compatibility with manual vignette lists

**When Creating New Vignettes**:

- Use domain prefixes in filenames: `{domain}-{number}-{description}.{ext}`
- For comprehensive guides: `{domain}-{number}-{function}-comprehensive.qmd`
- For legacy versions: `{domain}-{number}-{description}-legacy.Rmd`
- General content goes to all modules: `general-{number}-{topic}.Rmd`

**Configuration Control**:

- `use_domain_based: true` - Enable automatic domain-based copying
- `use_manual_lists: false` - Disable manual vignette file lists
- `include_general: true` - Copy general domain to all modules
- `overwrite_existing: true` - Overwrite existing vignette files

This replaces the previous manual approach where vignette files had to be individually listed in each module's configuration.

### Other Development Notes

- When generating new example data and vignettes add them to appropriate place in updateModules configuration
- Use gemini CLI for large codebase analysis. See instructions here: @CLAUDE-GEMINI.md

## Development Memories

### Using Guide Files

**IMPORTANT:** When working on jamovi module development, implementing features, or troubleshooting issues, **always consult the relevant guide files** in the `vignettes/` directory:

#### Guide Index

**`vignettes/README_GUIDES.md`** - Complete guide index with descriptions and use cases for all 11 available guides.

#### Primary Guides

1. **`vignettes/jamovi_module_patterns_guide.md`** - **START HERE** - Comprehensive jamovi development guide
   - **Use when:** Creating new analyses, debugging state management, implementing plots, building formulas
   - **Contains:** Module structure, 4-file architecture, data handling, state management, formula building, output patterns, best practices
   - **Based on:** jmvbaseR official example + ClinicoPath production code

2. **File-specific guides** (`.a.yaml`, `.b.R`, `.r.yaml`, `.u.yaml`)
   - `jamovi_a_yaml_guide.md` - Analysis definitions (options)
   - `jamovi_b_R_guide.md` - Backend implementation (R6 classes)
   - `jamovi_r_yaml_guide.md` - Results definitions (outputs)
   - `jamovi_u_yaml_guide.md` - User interface definitions

3. **Feature-specific guides**
   - `jamovi_tables_guide.md` - Table output
   - `jamovi_plots_guide.md` - Plot/image output
   - `jamovi_notices_guide.md` - User notices (ERROR/WARNING/INFO)
   - `jamovi_formula_guide.md` - Statistical formulas
   - `jamovi_js_guide.md` - Custom JavaScript
   - `jamovi_actions_guide.md` - UI actions & events

#### When to Use Guides

✅ **Before** implementing a new jamovi analysis → Read `jamovi_module_patterns_guide.md`
✅ **When** encountering state serialization errors → Check State Management section
✅ **When** building statistical models → Reference Formula Building patterns
✅ **When** plots don't update with option changes → Check State Management
✅ **When** variables have special characters/spaces → Reference Data Handling patterns
✅ **When** implementing syntax generation → Check Syntax Generation section

#### How to Use

```bash
# List all available guides
ls vignettes/*_guide.md

# Read a specific guide
cat vignettes/jamovi_module_patterns_guide.md
```

**Note:** These guides are based on analysis of official jamovi examples (jmvbaseR) and production ClinicoPath implementations. They contain validated patterns and solutions to common problems.

### Documentation Structure

#### Submodule Documentation Links
All submodule documentation is hosted on their respective websites following a consistent pattern:
- Base URL: `https://www.serdarbalci.com/{module-name}/`
- Articles/Vignettes: `https://www.serdarbalci.com/{module-name}/articles/{document-name}.html`

The four main submodules and their documentation sites:
1. **ClinicoPathDescriptives**: https://www.serdarbalci.com/ClinicoPathDescriptives/
2. **jjstatsplot**: https://www.serdarbalci.com/jjstatsplot/
3. **jsurvival**: https://www.serdarbalci.com/jsurvival/
4. **meddecide**: https://www.serdarbalci.com/meddecide/

When updating documentation links in README.Rmd, ensure they point to these submodule-specific sites rather than the main ClinicoPathJamoviModule documentation.

### File Update Instructions

- Update NEWS.md when there is version change
- When updating NEWS.md use the current version from DESCRIPTION file

### Module Update Command

- Use this to check and update modules: `Rscript _updateModules.R`

### Development Memory

- `.Rd` and `.h.R` files are autogenerated. make changes on `.b.R` and yaml files

### Context Limit Management

- If there are errors similar to following, use Gemini CLI with the `-p` flag to analyze large codebases or multiple files that exceed the context limits of Claude. details are in @CLAUDE-GEMINI.md  
  - "Context low" or "Context window exceeded" or "Error: File content (40897 tokens) exceeds maximum allowed tokens (25000). Please use offset and limit parameters to read specific portions of the file, or use the GrepTool to search for specific content."
- in .u.yaml Label is not allowed to have the additional property "visible"
- in .u.yaml description is not allowed
- in .a.yaml type: Level is not allowed to have default
- official jamovi documentation is here './vignettes/dev.jamovi.org-master'
- README.md is overwritten. make changes in README.Rmd
- errors or warnings with jmvtools::prepare() means that the module cannot function in jamovi. there should be no errors.
- private$.checkpoint() is internal jamovi function we do not define it
- jmvtools::check() does not evaluate functions. it checks the presence of jamovi program. To evaluate functions use jmvtools::prepare and devtools::document()
- **ALWAYS consult guides** under `vignettes/` folder when generating codes and features:
  - Primary: `vignettes/jamovi_module_patterns_guide.md` (comprehensive jamovi development guide)
  - Specific features: `vignettes/*_guide.md` (targeted implementation guides)
  - See "Using Guide Files" section above for when and how to use these guides
- The notices feature does not allow new lines for the time being. So we need to update the implementation. For the
  time being we need to have both previous html and the new notices features to be present at the same time.
- you are an expert R-package and jamovi developer. you are an expert in biostatistics working with pathologists and clinicians.
critically evaluate functions. is it mathematically and statistically accurate? is it ready to be used by clinicians and pathologists? is it ready for release?
- The error "attempt to apply non-function" during serialization was caused by using jmvcore::Notice objects that
  were dynamically inserted with self$results$insert(). These Notice objects contain function references that
  cannot be serialized by jamovi's protobuf system.
- jmvtools::check() only locates jamovi program bin file location. it does not check anything regarding module structure or code.