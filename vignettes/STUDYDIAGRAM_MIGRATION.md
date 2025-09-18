# Study Diagram Migration Guide

## Overview

The previous `jflowchart` and `jconsort` functions have been consolidated into a unified `studydiagram` function. This migration provides enhanced functionality, better package integration, and support for multiple data formats.

## What Changed

### New Unified Function: `studydiagram`
- **Location**: ClinicoPath → OncoPathologyT → Study Design → Study Diagram
- **Replaces**: Both `jflowchart` and `jconsort` functions
- **Enhanced Features**: Multiple rendering engines, flexible data formats, robust package integration

### Archived Functions
The following functions have been moved to the `legacy/` folder:
- `jflowchart` (all files: .a.yaml, .b.R, .h.R, .r.yaml, .u.yaml)
- `jconsort` (all files: .a.yaml, .b.R, .h.R, .r.yaml, .u.yaml)

## New Features

### Four Rendering Modes
1. **CONSORT (Standard)** - Traditional DiagrammeR-based CONSORT diagrams
2. **CONSORT (ggplot2)** - Modern ggconsort package integration
3. **Flowchart (Standard)** - Professional consort package flowcharts
4. **Flowchart (ggplot2)** - Advanced ggflowchart/flowchart package integration

### Three Data Input Formats

#### Format 1: Participant Tracking with Step Numbers
- **Variables Required**: Participant ID, Step Number Excluded
- **Optional**: Exclusion Reason Variable
- **Use Case**: Patient-level data where each participant has a step number indicating where they were excluded

#### Format 2: Step Summary with Counts
- **Variables Required**: Step Name, Participant Count
- **Optional**: Exclusion Reason Variable
- **Use Case**: Pre-summarized data with step names and participant counts

#### Format 3: Exclusion Reason Mapping
- **Variables Required**: Participant ID, Exclusion Reason Variable
- **Special Feature**: Level selectors for mapping exclusion reasons to study steps (Step 1-5)
- **Step Labels**: Customizable step names (default: Screening, Enrollment, Treatment, Follow-up, Analysis)
- **Use Case**: Complex exclusion reason mapping to specific study phases

## Package Dependencies

The new `studydiagram` function integrates with multiple upstream R packages:

### Automatically Available (CRAN)
- `consort` - Publication-ready CONSORT diagrams
- `ggflowchart` - ggplot2-based flowcharts
- `flowchart` - Tidy flowchart generation

### GitHub Installation Required
- `ggconsort` - Install with: `remotes::install_github('tgerke/ggconsort')`

## Migration Benefits

1. **Unified Interface**: Single function replaces two separate tools
2. **Enhanced Flexibility**: Three data input formats accommodate different research workflows
3. **Modern Packages**: Integration with latest flowchart and CONSORT packages
4. **Fallback Support**: Graceful degradation when optional packages are unavailable
5. **Manual Variable Selection**: International user-friendly (no automatic column detection)
6. **Level Selectors**: Advanced exclusion reason mapping for complex studies

## Technical Implementation

- **Backend**: R6 class with comprehensive data processing for all three formats
- **UI**: Conditional sections that show/hide based on selected data format
- **Results**: Summary table, interactive diagram, publication-ready plot, and interpretation text
- **Error Handling**: Robust package availability checking with informative error messages
- **Styling**: Multiple color schemes and layout options

## Compilation Status

✅ **Successfully Compiled**: The unified studydiagram function compiles without errors and is ready for use.

## Package Documentation Resources

### CONSORT Package
- **CRAN README**: https://cran.r-project.org/web/packages/consort/readme/README.html
- **Vignette**: https://cran.r-project.org/web/packages/consort/vignettes/consort_diagram.html
- **Tutorial**: https://www.riinu.me/2024/02/consort/

### ggconsort Package (GitHub)
- **Repository**: https://github.com/tgerke/ggconsort
- **Documentation**: https://tgerke.github.io/ggconsort/

### ggflowchart Package
- **Documentation**: https://nrennie.rbind.io/ggflowchart/

### flowchart Package
- **CRAN**: https://cran.r-project.org/web/packages/flowchart/

### Additional Resources
- **R Flow Diagrams**: https://hbiostat.org/rflow/doverview.html

## Next Steps

Users should migrate their existing workflows from `jflowchart` and `jconsort` to the new `studydiagram` function for enhanced functionality and continued support. Consult the package documentation links above for advanced usage examples and customization options.