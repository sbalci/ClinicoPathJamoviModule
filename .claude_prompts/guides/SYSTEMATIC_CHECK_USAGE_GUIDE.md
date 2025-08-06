# Systematic Function Check Usage Guide

## Overview

This systematic checking system helps ensure jamovi module functions are robust, well-integrated, and ready for production. It automatically validates the integration between the 4 core jamovi files (.a.yaml, .b.R, .r.yaml, .u.yaml) and identifies potential issues.

## Quick Start

### 1. Check a Single Function
```r
# Load the checking system
source("systematic_check_implementation.R")

# Check a specific function
results <- systematic_function_check("lassocox")

# Generate detailed report
report <- generate_detailed_report("lassocox", results, "lassocox_report.md")
```

### 2. Run Comprehensive Batch Check
```r
# Run the complete quality assessment
source("RUN_SYSTEMATIC_CHECKS.R")
```

This will:
- Check all priority functions
- Generate summary statistics
- Create detailed reports
- Provide actionable next steps

## What Gets Checked

### ✅ File Integrity
- All required files exist (.a.yaml, .b.R, .r.yaml, .u.yaml)
- Header file (.h.R) generated successfully

### ✅ Argument Integration
- All .a.yaml options are used in .b.R
- Options are accessed correctly (`self$options$argname`)
- Unused options are identified

### ✅ Output Population
- All .r.yaml outputs are populated in .b.R
- Output population code exists (`self$results$outputname`)
- Unpopulated outputs are flagged

### ✅ Error Handling
- Presence of tryCatch blocks
- Input validation patterns
- Graceful error handling

### ✅ Explanatory Features
- Educational content patterns detected
- Methodology explanations
- Clinical guidance features

## Output Files Generated

### Summary Reports
- `reports/function_check_summary.csv` - Tabular overview of all functions
- `reports/quality_assessment_report.md` - Executive summary with next steps

### Detailed Function Reports
- `reports/[function]_detailed_report.md` - Comprehensive analysis per function

## Interpreting Results

### Status Levels
- ✅ **PASS** - No issues found, ready for production
- ⚠️ **MINOR_ISSUES** - 1-2 minor improvements needed
- ❌ **NEEDS_WORK** - 3+ issues requiring attention
- 🚫 **MISSING_FILES/ERROR** - Function incomplete or broken

### Key Metrics
- **Options Used**: Ratio of .a.yaml options actually used in .b.R
- **Outputs Populated**: Ratio of .r.yaml outputs populated in .b.R
- **Error Handling**: Number of error handling patterns detected

## Systematic Quality Improvement Process

### Phase 1: Assessment
1. Run `source("RUN_SYSTEMATIC_CHECKS.R")`
2. Review `reports/quality_assessment_report.md`
3. Prioritize functions needing attention

### Phase 2: Function-by-Function Improvement
For each function identified:

1. **Run Individual Check**
   ```r
   results <- systematic_function_check("functionname")
   ```

2. **Address Issues Systematically**
   - Fix unused options (remove or implement)
   - Implement unpopulated outputs
   - Add error handling where missing
   - Enhance explanatory features

3. **Re-test Until Clean**
   ```r
   # Re-run check after fixes
   results <- systematic_function_check("functionname")
   ```

4. **Manual Testing in Jamovi**
   - Test all argument combinations
   - Verify outputs render correctly
   - Check error scenarios
   - Validate explanatory content

### Phase 3: Integration Testing
1. Test function interactions
2. Verify module-wide consistency
3. Performance testing with large datasets
4. User experience evaluation

## Best Practices for Using This System

### Before Each Release
- [ ] Run comprehensive batch check
- [ ] Ensure all priority functions PASS
- [ ] Address at least NEEDS_WORK issues
- [ ] Document any remaining MINOR_ISSUES

### During Development
- [ ] Check new functions immediately after creation
- [ ] Re-check after significant modifications
- [ ] Use as debugging tool for integration issues

### Quality Gates
- **Alpha Release**: No MISSING_FILES/ERROR status
- **Beta Release**: No NEEDS_WORK status
- **Production Release**: Majority should PASS

## Advanced Usage

### Custom Function Lists
```r
# Check specific functions
my_functions <- c("lassocox", "survival", "decisiongraph")
for(func in my_functions) {
  results <- systematic_function_check(func)
}
```

### Integration with CI/CD
```r
# Example CI/CD integration
results <- check_all_functions()
critical_issues <- sum(sapply(results, function(x) length(x$recommendations) > 2))
if(critical_issues > 0) {
  stop("Critical issues found in ", critical_issues, " functions")
}
```

### Custom Reporting
```r
# Generate custom reports
results <- systematic_function_check("myfunction")
custom_report <- generate_detailed_report("myfunction", results)

# Add custom analysis
writeLines(c(custom_report, "", "## Custom Notes", "- Function tested with dataset X"), 
           "my_custom_report.md")
```

## Troubleshooting

### Common Issues

**"yaml package not available"**
```r
install.packages("yaml")
```

**"File not found" errors**
- Ensure you're running from the module root directory
- Check that function names are spelled correctly
- Verify files exist in jamovi/ and R/ directories

**"Incomplete final line" warnings**
- These are harmless warnings about file formatting
- Can be fixed by ensuring files end with a newline

### Getting Help

1. Check the `SYSTEMATIC_FUNCTION_CHECK_PROMPT.md` for detailed methodology
2. Review example outputs in the reports/ directory
3. Use `systematic_function_check("functionname")` for debugging specific issues

## Contributing Improvements

This checking system can be enhanced with:
- Additional validation patterns
- More sophisticated error detection
- Performance benchmarking
- UI/UX quality metrics
- Scientific accuracy validation

Add new checks to the `systematic_function_check()` function and update this guide accordingly.

---

**Remember**: This system is a tool to help maintain quality, but human judgment is still essential for evaluating scientific accuracy, user experience, and domain-specific requirements.