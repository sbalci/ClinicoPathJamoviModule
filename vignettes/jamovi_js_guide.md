# JavaScript in Jamovi Modules: A Comprehensive Guide

## Overview

JavaScript in jamovi modules provides dynamic UI interactions, intelligent defaults, and real-time validation. This guide covers when to use JavaScript, how to implement event handlers, and best practices based on successful implementations in the ClinicoPath module.

## Table of Contents
1. [When to Use JavaScript](#when-to-use-javascript)
2. [File Structure and Naming](#file-structure-and-naming)
3. [Event System Architecture](#event-system-architecture)
4. [Common Event Patterns](#common-event-patterns)
5. [UI Object Reference](#ui-object-reference)
6. [Best Practices](#best-practices)
7. [Debugging and Testing](#debugging-and-testing)

## When to Use JavaScript

JavaScript is essential for:

### 1. **Dynamic UI Updates**
- Changing UI element values based on user selections
- Enabling/disabling options conditionally
- Updating dependent controls

### 2. **Preset Systems**
- Clinical presets that configure multiple parameters
- Study design templates
- Analysis workflows

### 3. **Input Validation**
- Range checking with automatic correction
- Format validation
- Consistency checks between related inputs

### 4. **Intelligent Defaults**
- Context-aware parameter suggestions
- Adaptive thresholds based on study type
- Performance optimization hints

### 5. **User Guidance**
- Real-time feedback on parameter choices
- Warning messages for invalid combinations
- Clinical interpretation helpers

## File Structure and Naming

### Standard Convention
```
jamovi/
├── js/                            # MUST be in this exact location
│   ├── analysisname.events.js    # Event handlers for complex analyses
│   ├── simpleanalysis.js         # Simple event handlers
│   └── waterfall.events.js       # Example: waterfall plot events
```

### Important Requirements
- **Location**: JavaScript files MUST be placed in the `jamovi/js/` folder
- **No manifest registration**: Unlike other jamovi files, JS files are NOT listed in 0000.yaml
- **Direct reference**: Files are referenced directly from .u.yaml files

### Naming Patterns
- **Complex analyses**: `{analysisname}.events.js`
- **Simple handlers**: `{analysisname}.js`
- Use `.events.js` suffix for analyses with multiple event handlers

## Event System Architecture

### Basic Module Structure

```javascript
// For .events.js files
const events = {
    // Event handler functions
    onChange_optionName: function(ui) {
        // Handler logic
    },

    // Helper functions
    helperFunction: function(ui, params) {
        // Shared logic
    }
};

module.exports = events;
```

```javascript
// For simple .js files
module.exports = {
    // Direct event handler
    outcome_changed: function(ui, event) {
        // Handler logic
    }
};
```

### Connecting JavaScript to UI (in .u.yaml)

```yaml
# Standard event binding
- type: ComboBox
  name: clinical_preset
  events:
    change: ./analysisname.events::onChange_clinicalPreset

# Alternative for ActionButton
- type: ActionButton
  name: myAction
  events:
    change: ./test::myAction_clicked

# ActionButton with full configuration
- name: doDemo1A1Btn
  type: ActionButton
  title: 'do'  # Button label
  margin: none
  maxWidth: 79  # Pixel width
  maxHeight: 24  # Pixel height
  events:
    change: ./brawsim.events::onChange_project1A1
```

### Alternative: Handling Events in R

For simple button actions, you can also handle events directly in R instead of JavaScript:

```r
# In your .b.R file
.run = function() {
    if (self$options$myAction) {
        # Button was clicked - perform action
        # Reset the button state
        self$options$myAction <- FALSE
    }
}
```

## Common Event Patterns

### 1. Preset System Pattern

**Use Case**: Configure multiple parameters with a single selection

```javascript
onChange_clinicalPreset: function(ui) {
    let preset = ui.clinical_preset.value();

    switch(preset) {
        case "phase2":
            // Phase II trial settings
            ui.effect_size.setValue(0.75);
            ui.alpha_level.setValue(0.05);
            ui.power_level.setValue(0.80);
            ui.allocation_ratio.setValue(1.0);
            ui.dropout_rate.setValue(0.05);
            break;

        case "biomarker":
            // Biomarker study settings
            ui.effect_size.setValue(0.67);
            ui.alpha_level.setValue(0.05);
            ui.allocation_ratio.setValue(2.0);
            break;

        case "custom":
            // Don't change anything for custom
            break;
    }

    // Update dependent options
    this.updatePresetGuidance(ui, preset);
}
```

### 2. Cascading Updates Pattern

**Use Case**: Update dependent controls based on primary selection

```javascript
onChange_colorPalette: function(ui) {
    let palette = ui.color_palette.value();
    let engine = ui.plot_engine.value();

    switch(palette) {
        case "standard":
            if (engine === "ggcharts") {
                ui.color1.setValue("#FF69B4");  // Hot pink
                ui.color2.setValue("#4169E1");  // Royal blue
            } else {
                ui.color1.setValue("#F8766D");  // ggplot2 red
                ui.color2.setValue("#00BFC4");  // ggplot2 teal
            }
            break;

        case "accessible":
            // Colorblind-friendly palette
            ui.color1.setValue("#E69F00");  // Orange
            ui.color2.setValue("#56B4E9");  // Sky blue
            break;

        case "custom":
            // Keep current colors
            break;
    }
}
```

### 3. Validation with Auto-Correction Pattern

**Use Case**: Ensure valid input ranges with automatic fixes

```javascript
onChange_bootstrapSamples: function(ui) {
    let samples = ui.bootstrap_samples.value();
    let guidance = "";

    if (samples < 500) {
        guidance = "⚠️ Too few samples - may be unreliable";
        ui.bootstrap_samples.setValue(500);  // Auto-correct
    } else if (samples < 1000) {
        guidance = "⚠️ Consider increasing to ≥1000 for stable estimates";
    } else if (samples > 3000) {
        guidance = "ℹ️ High computational cost - consider reducing";
    } else {
        guidance = "✅ Good balance of accuracy and efficiency";
    }

    // Update guidance label (if supported)
    try {
        ui.bootstrap_guidance.setValue(guidance);
    } catch(e) {
        // Fallback if dynamic labels not supported
    }
}
```

### 4. Conditional Visibility Pattern

**Use Case**: Show/hide options based on context

```javascript
onChange_inputType: function(ui) {
    let inputType = ui.inputType.value();

    if (inputType === "raw") {
        // Raw measurements - enable time-based analysis
        ui.showSpiderPlot.setValue(true);
        ui.showWaterfallPlot.setValue(false);
    } else {
        // Percentage data - focus on waterfall
        ui.showWaterfallPlot.setValue(true);
        ui.showSpiderPlot.setValue(false);
    }
}
```

### 5. Smart Defaults Pattern

**Use Case**: Provide context-aware default values

```javascript
onChange_validationMethod: function(ui) {
    let method = ui.validation_method.value();

    // Auto-adjust parameters based on method
    switch(method) {
        case "bootstrap":
            ui.bootstrap_samples.setValue(1000);
            break;
        case "cross_validation":
            ui.cv_folds.setValue(10);
            ui.cv_repeats.setValue(1);
            break;
        case "repeated_cv":
            ui.cv_folds.setValue(5);
            ui.cv_repeats.setValue(5);
            break;
    }

    this.updateValidationGuidance(ui);
}
```

### 6. Real-time Calculation Pattern

**Use Case**: Show live calculations based on inputs

```javascript
onChange_prevalence: function(ui) {
    let prev = ui.population_prevalence.value();

    // Validate range
    if (prev < 1) {
        ui.population_prevalence.setValue(1);
        prev = 1;
    } else if (prev > 50) {
        ui.population_prevalence.setValue(50);
        prev = 50;
    }

    // Calculate PPV/NPV impact
    let sens = 0.85; // Example sensitivity
    let spec = 0.85; // Example specificity

    let ppv = (sens * prev) / (sens * prev + (1 - spec) * (100 - prev));
    let npv = (spec * (100 - prev)) / (spec * (100 - prev) + (1 - sens) * prev);

    ppv = (ppv * 100).toFixed(1);
    npv = (npv * 100).toFixed(1);

    let impact = `PPV≈${ppv}%, NPV≈${npv}%`;

    try {
        ui.prevalence_impact.setValue(impact);
    } catch(e) {}
}
```

### 7. Action Button Coordination Pattern

**Use Case**: Coordinate multiple action buttons

```javascript
onChange_project1As: function(ui) {
    let BtnOn1 = ui.doDemo1AsBtn.value();
    let BtnOn2A = ui.doDemo2AsBtn.value();

    // Coordinate button states
    // Perform actions based on button clicks

    // Reset button after action if needed
    ui.doDemo1AsBtn.setValue(false);
}
```

### 8. Conditional Reset Pattern

**Use Case**: Reset dependent checkboxes based on selection

```javascript
onChange_outlierCheck: function(ui) {
    // If outlier method is not IQR, disable box plot
    if (ui.outlcheck.value() !== "IQR") {
        ui.boxpl.setValue(false);
    }

    // Similar pattern for other conditional resets
    if (ui.someOption.value() === "specific_value") {
        // Reset dependent options
        ui.dependentOption1.setValue(false);
        ui.dependentOption2.setValue("");
    }
}
```

### 9. Complex Multi-Button Demo System Pattern

**Use Case**: Coordinate multiple demo buttons with shared state management

```javascript
onChange_Project1As: function(ui) {
    let BtnOn1 = ui.doDemo1AsBtn.value();
    let BtnOn2A = ui.doDemo2AsBtn.value();
    let BtnOn2B = ui.doDemo2BsBtn.value();
    let BtnOn3A = ui.doDemo3AsBtn.value();

    if (BtnOn1 == true) {
        demo1SetUp(ui, "n");
        ui.makeSampleBtn.setValue(true);
    }
    if (BtnOn2A == true) {
        demo2SetUp(ui, "n");
        ui.makeSampleBtn.setValue(true);
    }
    if (BtnOn2B == true) {
        demo2SetUp(ui, "n");
        ui.makeMultipleBtn.setValue(true);
    }
    // Continue for other buttons...
}
```

### 10. Mathematical Validation with Auto-Correction

**Use Case**: Validate complex mathematical relationships and auto-correct

```javascript
onChange_effectSize: function() {
    let rIV = ui.EffectSize1.value();
    let rIV2 = ui.EffectSize2.value();
    let rIVIV2 = ui.EffectSize3.value();
    let rIVIV2DV = ui.EffectSize12.value();

    // Check mathematical constraint
    let fullES = rIV^2 + rIV2^2 + 2*rIV*rIV2*rIVIV2 + rIVIV2DV^2;

    if (fullES >= 1) {
        // Auto-correct by scaling down all values
        while (fullES >= 1) {
            rIV = rIV * 0.9;
            rIV2 = rIV2 * 0.9;
            rIVIV2 = rIVIV2 * 0.9;
            rIVIV2DV = rIVIV2DV * 0.9;
            fullES = rIV^2 + rIV2^2 + 2*rIV*rIV2*rIVIV2 + rIVIV2DV^2;
        }
        // Update all affected UI elements
        ui.EffectSize1.setValue(rIV);
        ui.EffectSize2.setValue(rIV2);
        ui.EffectSize3.setValue(rIVIV2);
        ui.EffectSize12.setValue(rIVIV2DV);
    }
}
```

### 11. Dynamic Range Updates Based on Exploration Mode

**Use Case**: Update exploration ranges based on selected variable

```javascript
onChange_exploreMode: function(ui) {
    var newRange = {min:0.3, max:0.7, xlog:false, np:8};
    let mode = ui.exploreMode.value();
    let value = "n";

    switch(mode) {
        case "hypothesisExplore":
            value = ui.hypothesisExploreList.value();
            var newRange = updateRange(value);
            ui.exploreMinValH.setValue(newRange.min);
            ui.exploreMaxValH.setValue(newRange.max);
            ui.exploreXLogH.setValue(newRange.xlog);
            ui.exploreNPointsH.setValue(newRange.np);
            break;
        case "designExplore":
            value = ui.designExploreList.value();
            var newRange = updateRange(value);
            ui.exploreMinValD.setValue(newRange.min);
            ui.exploreMaxValD.setValue(newRange.max);
            ui.exploreXLogD.setValue(newRange.xlog);
            ui.exploreNPointsD.setValue(newRange.np);
            break;
    }
    return;
}
```

### 12. Variable Preset System with Complex Objects

**Use Case**: Apply comprehensive variable configurations from presets

```javascript
onChange_presetDV: function(ui) {
    let presetDV = ui.presetDV.value();
    let variable = makeVar(presetDV);

    // Apply all variable properties at once
    ui.DVname.setValue(variable.name);
    ui.DVtype.setValue(variable.type);
    ui.DVmu.setValue(variable.mu);
    ui.DVsd.setValue(variable.sd);
    ui.DVskew.setValue(variable.skew);
    ui.DVkurt.setValue(variable.kurt);
    ui.DVnlevs.setValue(variable.nlevs);
    ui.DViqr.setValue(variable.iqr);
    ui.DVncats.setValue(variable.ncats);
    ui.DVcases.setValue(variable.cases);
    ui.DVprops.setValue(variable.props);
}
```

## UI Object Reference

### Getting Values
```javascript
// Get current value of UI element
let value = ui.element_name.value();
```

### Setting Values
```javascript
// Set value of UI element
ui.element_name.setValue(newValue);

// Different types
ui.text_input.setValue("text");
ui.numeric_input.setValue(123);
ui.checkbox.setValue(true);
ui.combo_box.setValue("option_key");
```

### Common UI Elements

| Element Type | Get Value | Set Value | Notes |
|-------------|-----------|-----------|--------|
| TextBox | `ui.name.value()` | `ui.name.setValue("text")` | String input |
| ComboBox | `ui.name.value()` | `ui.name.setValue("option")` | Dropdown selection |
| CheckBox | `ui.name.value()` | `ui.name.setValue(true/false)` | Boolean |
| RadioButton | `ui.name.value()` | `ui.name.setValue("option")` | Single choice |
| Slider | `ui.name.value()` | `ui.name.setValue(0.5)` | Numeric range |
| VariableSupplier | `ui.name.value()` | `ui.name.setValue(["var1"])` | Variable selection |
| ActionButton | `ui.name.value()` | `ui.name.setValue(false)` | Returns true when clicked, reset to false after handling |

## Best Practices

### 1. Error Handling
Always wrap UI updates in try-catch for optional elements:

```javascript
try {
    ui.optional_element.setValue(value);
} catch(e) {
    // Element may not exist in all configurations
}
```

### 2. Avoid Recursive Calls
Prevent infinite loops by checking if value actually changed:

```javascript
onChange_element: function(ui) {
    let current = ui.element.value();
    let newValue = calculateNewValue();

    if (current !== newValue) {
        ui.element.setValue(newValue);
    }
}
```

### 3. Helper Functions
Extract common logic to helper functions:

```javascript
const events = {
    onChange_option1: function(ui) {
        this.updateGuidance(ui);
    },

    onChange_option2: function(ui) {
        this.updateGuidance(ui);
    },

    // Helper function
    updateGuidance: function(ui) {
        // Shared logic
    }
};
```

### 4. Complex Data Factories
Create factory functions for generating complex configuration objects:

```javascript
// Factory function outside events object
let makeVar = function(name) {
    switch (name) {
        case "IQ":
            return {
                name: "IQ",
                type: "Interval",
                mu: 100,
                sd: 15,
                skew: 0,
                kurt: 0,
                nlevs: 7,
                iqr: 3,
                ncats: 2,
                cases: "C1,C2",
                props: "1,1"
            };
        case "ExamGrade":
            return {
                name: "ExamGrade",
                type: "Interval",
                mu: 55,
                sd: 10,
                skew: -0.6,
                kurt: 0,
                nlevs: 7,
                iqr: 3,
                ncats: 2,
                cases: "C1,C2",
                props: "1,1"
            };
        default:
            return {
                name: "Default",
                type: "Interval",
                mu: 0,
                sd: 1,
                skew: 0,
                kurt: 0,
                nlevs: 7,
                iqr: 3,
                ncats: 2,
                cases: "C1,C2",
                props: "1,1"
            };
    }
};
```

### 5. Range Update Systems
Create systematic range update functions for exploration:

```javascript
let updateRange = function(value) {
    switch (value) {
        case "n":
            return {min: 10, max: 250, xlog: true, np: 11};
        case "rIV":
            return {min: 0, max: 0.75, xlog: false, np: 11};
        case "Alpha":
            return {min: 0.001, max: 0.5, xlog: true, np: 11};
        case "Power":
            return {min: 0.1, max: 0.9, xlog: false, np: 11};
        default:
            return {min: 0, max: 1, xlog: false, np: 11};
    }
};
```

### 4. Consistent Naming
- Use camelCase for JavaScript functions
- Prefix event handlers with `onChange_`
- Match option names from .yaml files

### 5. User Feedback
Provide clear feedback for user actions:

```javascript
onChange_parameter: function(ui) {
    let value = ui.parameter.value();
    let feedback = "";

    if (value < threshold) {
        feedback = "⚠️ Value too low - may affect results";
    } else if (value > maxValue) {
        feedback = "⚠️ Value too high - computation intensive";
    } else {
        feedback = "✅ Value within recommended range";
    }

    // Show feedback if UI supports it
    try {
        ui.parameter_feedback.setValue(feedback);
    } catch(e) {}
}
```

### 6. Clinical Context
For medical/clinical modules, provide interpretation:

```javascript
onChange_costRatio: function(ui) {
    let ratio = ui.fn_fp_cost_ratio.value();
    let interpretation = "";

    if (ratio < 1) {
        interpretation = "False positives more costly than false negatives";
    } else if (ratio === 1) {
        interpretation = "Equal cost for false positives and negatives";
    } else if (ratio < 3) {
        interpretation = "Moderate preference to avoid false negatives";
    } else {
        interpretation = "Strong preference to avoid false negatives (screening scenario)";
    }

    ui.cost_interpretation.setValue(interpretation);
}
```

### 7. Advanced Demo State Management
For complex demo systems with state tracking:

```javascript
// Demo state tracking pattern
let demo1Defaults = function(ui, thisDemo) {
    let variable1 = ui.lastDemo.value();

    // Only reset if switching to different demo
    if (variable1 != thisDemo) {
        ui.doDemo1sLst.setValue("Perfectionism");
        ui.doDemo1sLstA.setValue("ExamGrade");
        ui.doDemo1sLstB.setValue(0.3);
        ui.doDemo1sLstD.setValue("Random");
        ui.doDemo1sLstE.setValue(42);
    }

    // Track current demo
    ui.lastDemo.setValue(thisDemo);
}

// Complex demo setup with conditional logic
let demo1SetUp = function(ui, show) {
    let variable1 = ui.doDemo1sLst.value();
    variable1 = variable1.replace("?", "");  // Clean input
    ui.presetIV.setValue(variable1);

    let variable3 = ui.doDemo1sLstB.value();
    // Constrain effect size
    if (variable3 > 0.95) variable3 = 0.95;
    if (variable3 < -0.95) variable3 = -0.95;
    ui.EffectSize1.setValue(variable3);

    // Conditional display mode
    if (show == "h") {
        ui.showHypothesisLst.setValue("Hypothesis");
    }
}
```

### 8. Multi-Level Cascading Updates
For hierarchical option dependencies:

```javascript
onChange_metaDefaultN: function(ui) {
    let variable1 = ui.metaDefaultN.value();
    variable1 = Number(variable1);

    // Cascade to multiple related controls
    ui.meta3SampleSize.setValue(variable1 * 2);
    ui.meta3SampleBudget.setValue(variable1 * 2);
    ui.meta4SampleSize.setValue(variable1);
}

onChange_metaDefaultNullp: function(ui) {
    let variable1 = ui.metaDefaultNullp.value();
    variable1 = Number(variable1);

    // Update multiple null probability settings
    ui.meta1pNull.setValue(variable1);
    ui.meta2pNull.setValue(variable1);
    ui.meta3pNull.setValue(variable1);
}
```

## Debugging and Testing

### Developer Console Access
- **Windows/Linux**: Press F10 in jamovi to open developer console
- **Mac**: Use Cmd+Option+I or enable Developer menu in jamovi preferences
- Check console for JavaScript errors and log messages

### 1. Console Logging
Use console statements during development:

```javascript
onChange_option: function(ui) {
    let value = ui.option.value();
    console.log("Option changed to:", value);

    // Development debugging
    console.warn("Validation warning:", message);
    console.error("Error occurred:", error);
}
```

### 2. Validation Testing
Test edge cases:

```javascript
onChange_numeric: function(ui) {
    let value = ui.numeric.value();

    // Test boundaries
    if (value === null || value === undefined) {
        console.error("Null value received");
        return;
    }

    if (isNaN(value)) {
        console.error("Non-numeric value:", value);
        ui.numeric.setValue(defaultValue);
        return;
    }

    // Normal processing
}
```

### 3. Event Chaining
Track event cascades:

```javascript
onChange_primary: function(ui) {
    console.log("Primary changed, updating secondary...");
    ui.secondary.setValue(newValue);

    // This will trigger onChange_secondary
    this.updateDependents(ui);
}
```

## Examples from Real Modules

### ClinicoPath Module Implementations

1. **waterfall.events.js**: Complex preset system with 224 lines
   - Multiple clinical presets (phase2, biomarker, publication)
   - Intelligent color scheme selection
   - Validation with user guidance

2. **agepyramid.events.js**: Color management with accessibility
   - Standard, accessible, and custom palettes
   - Engine-specific color adaptation
   - Validation without recursive calls

3. **simpleSurvivalPower.events.js**: Clinical trial presets
   - Phase-specific configurations
   - Comprehensive parameter updates
   - Context-aware defaults

4. **clinicalvalidationinteractive.events.js**: Advanced validation
   - 526 lines of sophisticated event handling
   - Real-time PPV/NPV calculations
   - Cost-benefit analysis integration

### BrawStats Module Implementation

5. **brawsim.events.js**: Ultra-comprehensive system with 1147 lines
   - **Mathematical validation**: Complex effect size constraints with auto-correction
   - **Multi-demo coordination**: Coordinated button systems with state tracking
   - **Factory patterns**: Variable and range generation systems
   - **Advanced state management**: Demo tracking with conditional resets
   - **Hierarchical updates**: Multi-level cascading parameter updates
   - **Complex exploration modes**: Dynamic range updates based on exploration type
   - **Helper function architecture**: Extensive use of external helper functions

## Troubleshooting JavaScript in Jamovi

### Known Issues and Solutions

#### JavaScript Not Loading
**Symptoms**: Events not firing, no console errors
**Solutions**:
1. Verify file is in `jamovi/js/` folder
2. Check .u.yaml references are correct (path and function name)
3. Test with a portable jamovi installation
4. Clear jamovi cache and restart
5. Check for syntax errors using external JS validator

#### Intermittent Loading (Desktop Version)
**Issue**: JS handlers may not load consistently in custom modules on Windows
**Workaround**:
1. Use portable jamovi version for development
2. Test on multiple systems
3. Consider implementing fallback R-based event handling

#### Event Reference Syntax
**Correct**:
```yaml
events:
  change: ./filename::functionName    # Note: no .js extension
```
**Incorrect**:
```yaml
events:
  change: ./filename.js::functionName  # Don't include .js
```

## Common Pitfalls and Solutions

| Pitfall | Solution |
|---------|----------|
| JS not loading in custom module | Check file location is exactly `jamovi/js/` |
| Events not firing | Verify .u.yaml syntax and use F10 console |
| Infinite loops from setValue() | Check if value actually changed before setting |
| Missing UI elements | Wrap in try-catch blocks |
| Type mismatches | Validate and convert types explicitly |
| Event not firing | Check .u.yaml syntax and path |
| Recursive event calls | Use flags or value comparison |
| Performance issues | Throttle expensive calculations |
| Desktop version issues | Test with portable jamovi installation |

## Summary

JavaScript in jamovi modules enables sophisticated user interactions beyond static forms. Key takeaways:

1. **Use JavaScript for dynamic behavior** - presets, validation, intelligent defaults
2. **Follow naming conventions** - `.events.js` for complex handlers
3. **Handle errors gracefully** - try-catch for optional elements
4. **Provide user feedback** - validation messages, guidance text
5. **Avoid recursive calls** - check values before updating
6. **Test thoroughly** - edge cases, null values, event chains

JavaScript events transform jamovi modules from simple input forms into intelligent analysis assistants that guide users toward appropriate statistical choices.