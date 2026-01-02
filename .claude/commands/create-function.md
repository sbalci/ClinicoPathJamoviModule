---
name: create-function
description: Scaffold a new jamovi function with optional interactive wizard
interactive: true
args:
  function_name:
    description: Name of the new jamovi function
    required: true
  function_type:
    description: Type of function (survival, descriptive, plot, decision, diagnostic, agreement)
    required: false
  --wizard:
    description: Launch interactive wizard to gather requirements
    required: false
    default: false
  --template:
    description: Use specific template (minimal, standard, comprehensive, clinical)
    required: false
    default: standard
  --skip-examples:
    description: Skip generating example usage code
    required: false
    default: false
usage: /create-function <function_name> [function_type] [--wizard] [--template=standard]
examples:
  /create-function myanalysis --wizard                    # Interactive wizard mode
  /create-function survival_analysis survival             # Quick create with type
  /create-function diagnostic_test diagnostic --template=clinical
---

# New Jamovi Function Scaffolding with Interactive Wizard

You are an expert jamovi developer creating a complete, well-structured jamovi function named `$ARGUMENTS`. Generate all 4 required files following best practices and module conventions.

## Wizard Mode

When `--wizard` is specified, guide the user through an interactive questionnaire to gather requirements before generating code. This ensures the generated function matches exact needs.

### Wizard Flow

**Step 1: Function Type Classification**
```
🧙 Jamovi Function Creation Wizard
═══════════════════════════════════

Creating function: {function_name}

❓ What type of analysis will this function perform?

1. Descriptive Statistics (summary tables, frequencies, distributions)
2. Survival Analysis (Kaplan-Meier, Cox regression, competing risks)
3. Diagnostic/ROC Analysis (sensitivity, specificity, AUC, decision curves)
4. Agreement Analysis (kappa, ICC, Bland-Altman)
5. Decision Analysis (decision trees, Markov models, cost-effectiveness)
6. Plot/Visualization (specialized statistical plots)
7. Other/Custom

Your choice [1-7]:
```

**Step 2: Input Variables**
```
❓ What types of input variables will this function use?

☐ Continuous variables (numeric measurements)
☐ Categorical variables (groups, factors)
☐ Time-to-event data (survival times, censoring)
☐ Paired/matched data
☐ Multiple groups
☐ Covariates/adjustment variables

Select all that apply [space to toggle, enter when done]:
```

**Step 3: Primary Output**
```
❓ What is the primary output?

1. Statistical table (estimates, CIs, p-values)
2. Plot/graph
3. Both table and plot
4. HTML report
5. Multiple outputs (will configure later)

Your choice [1-5]:
```

**Step 4: Statistical Methods**
```
❓ What statistical methods will be used?

Examples for {detected_type}:
- For Survival: Kaplan-Meier, Log-rank test, Cox regression
- For Diagnostic: ROC curves, DeLong test, Youden index
- For Descriptive: Mean/SD, Median/IQR, Frequencies

Enter methods (comma-separated):
```

**Step 5: Special Features**
```
❓ Which features should be included?

☐ Explanatory text for clinicians (showExplanations option)
☐ Reproducible R code generation (showRCode option)
☐ Multiple testing corrections (Bonferroni, BH, BY)
☐ Bootstrapping/resampling options
☐ Subgroup analysis capabilities
☐ Export options (CSV, clipboard)

Select all that apply:
```

**Step 6: Template Selection**
```
❓ Choose template complexity:

1. Minimal - Basic structure, you'll add most logic
2. Standard - Common patterns, moderate scaffolding
3. Comprehensive - Full error handling, validation, notices
4. Clinical - Comprehensive + clinical thresholds, interpretations

Your choice [1-4]:
```

**Step 7: Confirmation**
```
📋 Function Specification Summary
═════════════════════════════════

Function name: {function_name}
Type: {function_type}
Inputs: {input_types}
Primary output: {output_type}
Methods: {methods}
Features: {features}
Template: {template}

Proceed with generation? [Y/n]:
```

## Templates

### Minimal Template
- Basic R6 class structure
- Placeholder `.init()` and `.run()` methods
- TODO comments for key areas
- Minimal error handling
- **Use when:** You want maximum flexibility

### Standard Template (default)
- R6 class with common patterns
- Input validation stubs
- Error handling framework
- TODO output generation
- Help text placeholders
- **Use when:** Normal development

### Comprehensive Template
- Full error handling with jmvcore::Notice
- Complete input validation
- Data preparation helpers
- Explanatory content scaffolding
- Best practices comments
- **Use when:** Production-ready function

### Clinical Template
- All comprehensive features PLUS:
- Clinical threshold checks (EPV, sample size, prevalence)
- Domain-specific validation (pathology/oncology)
- Plain-language explanations
- Clinical interpretation helpers
- **Use when:** Clinical/pathology module functions

## Function Specification

**Function Name:** `$ARGUMENTS`
**Function Type:** $ARGUMENTS (if specified)
**Module:** ClinicoPath

## Implementation Requirements

Create a complete jamovi function with:

1. **Proper R6 class structure** following module patterns
2. **Comprehensive error handling** with user-friendly messages
3. **Educational content** with explanatory outputs
4. **Well-organized UI** with logical grouping
5. **Complete integration** between all files

## Response Format

### 🚀 NEW FUNCTION: `$ARGUMENTS`

#### 📁 **FILE STRUCTURE**

I'll create these 4 files:
- `jamovi/$ARGUMENTS.a.yaml` - Analysis definition
- `jamovi/$ARGUMENTS.r.yaml` - Results definition  
- `jamovi/$ARGUMENTS.u.yaml` - User interface definition
- `R/$ARGUMENTS.b.R` - Backend implementation

#### ⚙️ **1. ANALYSIS DEFINITION (.a.yaml)**

```yaml
---
name: $ARGUMENTS
title: [Descriptive Title]
menuGroup: [Appropriate Group]
menuSubgroup: [Subgroup]
version: '0.0.1'
jas: '1.2'

description:
    main: [Function description]
    R:
        dontrun: false
        usage: |
            # Usage example

options:
    - name: data
      type: Data
      description:
          R: The data as a data frame.
    
    # Core analysis options
    [... specific options ...]
    
    # Explanatory options
    - name: showExplanations
      title: Show Method Explanations
      type: Bool
      default: false
      description:
          R: Display detailed explanations and methodology notes.

...
```

#### 📊 **2. RESULTS DEFINITION (.r.yaml)**

```yaml
---
name: $ARGUMENTS
title: [Function Title]
jrs: '1.1'

items:
    - name: todo
      title: To Do
      type: Html
      clearWith:
          - [relevant options]

    # Main results
    [... result definitions ...]
    
    # Explanatory outputs
    - name: methodologyExplanation
      title: Understanding [Method]
      type: Html
      visible: (showExplanations)
      clearWith:
          - showExplanations

refs:
    - [relevant packages/references]

...
```

#### 🎨 **3. USER INTERFACE (.u.yaml)**

```yaml
---
name: $ARGUMENTS
title: [Function Title]
jus: '3.0'
stage: 0
compilerMode: tame

children:
  - type: LayoutBox
    margin: large
    children:
      - type: Label
        label: [Section Title]
        children:
          # UI elements with logical grouping
          [... UI structure ...]

...
```

#### 💻 **4. BACKEND IMPLEMENTATION (.b.R)**

```r
# [Function Name] Implementation
# [Brief description]

$ARGUMENTSClass <- R6::R6Class(
    "$ARGUMENTSClass",
    inherit = $ARGUMENTSBase,
    private = list(
        .init = function() {
            # Initialize TODO and explanatory content
            private$.initializeTodo()
            
            if (self$options$showExplanations) {
                private$.populateExplanations()
            }
        },
        
        .run = function() {
            # Main analysis logic
            tryCatch({
                # Data preparation
                data <- private$.prepareData()
                
                # Validation
                private$.validateData(data)
                
                # Analysis
                results <- private$.performAnalysis(data)
                
                # Populate outputs
                private$.populateResults(results)
                
            }, error = function(e) {
                self$results$todo$setContent(private$.generateErrorMessage(e))
            })
        },
        
        .prepareData = function() {
            # Data preparation with validation
            # Return cleaned data
        },
        
        .validateData = function(data) {
            # Comprehensive data validation
            # Throw informative errors for issues
        },
        
        .performAnalysis = function(data) {
            # Core analysis implementation
            # Return results object
        },
        
        .populateResults = function(results) {
            # Populate all output tables/plots
        },
        
        .initializeTodo = function() {
            # Generate helpful TODO content
        },
        
        .populateExplanations = function() {
            # Educational content for users
        },
        
        .generateErrorMessage = function(error) {
            # User-friendly error messages
        }
    )
)
```

#### 🔧 **SETUP COMMANDS**

**Create Files:**
```bash
# Commands to create all files
touch jamovi/$ARGUMENTS.a.yaml
touch jamovi/$ARGUMENTS.r.yaml  
touch jamovi/$ARGUMENTS.u.yaml
touch R/$ARGUMENTS.b.R
```

**Generate Headers:**
```bash
Rscript -e "jmvtools::prepare()"
```

**Test Function:**
```r
library(ClinicoPath)
data('histopathology')
result <- $ARGUMENTS(data = histopathology, ...)
```

#### ✅ **QUALITY CHECKLIST**

**Architecture:**
- [ ] R6 class properly inherits from base
- [ ] Error handling implemented throughout
- [ ] Data validation comprehensive
- [ ] Results properly populated

**Integration:**
- [ ] All .a.yaml options used in .b.R
- [ ] All .r.yaml outputs populated
- [ ] UI elements match options
- [ ] Help text informative

**User Experience:**
- [ ] TODO content helpful
- [ ] Error messages user-friendly
- [ ] Explanatory content available
- [ ] UI logically organized

#### 📚 **DEVELOPMENT NOTES**

**Best Practices Applied:**
1. [List specific patterns used]
2. [Error handling approach]
3. [UI organization principles]

**Next Steps:**
1. Customize the template for specific functionality
2. Add domain-specific validation
3. Implement specialized analysis methods
4. Test with real data scenarios

Provide a complete, functional template that follows ClinicoPath module patterns and jamovi best practices.