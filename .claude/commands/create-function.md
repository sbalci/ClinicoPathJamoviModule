---
name: create-function
description: Scaffold a new jamovi function with all required files
args:
  function_name:
    description: Name of the new jamovi function
    required: true
  function_type:
    description: Type of function (survival, descriptive, plot, decision)
    required: false
usage: /create-function <function_name> [function_type]
---

# New Jamovi Function Scaffolding

You are an expert jamovi developer creating a complete, well-structured jamovi function named `$ARGUMENTS`. Generate all 4 required files following best practices and module conventions.

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