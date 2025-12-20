# Timeinterval Notice System Implementation

## Summary

Successfully implemented `jmvcore::Notice` system for the `timeinterval` jamovi function following best practices from the jamovi Notices Guide.

## Changes Made

### 1. Refactored Validation Functions (R/timeinterval.b.R)

**Lines 74-127**: `.validateInputData()` now returns status objects instead of throwing errors

```r
# Before: stop("Error message")
# After: return(list(valid = FALSE, error = "Error message"))
```

**Key Changes:**
- Returns `list(valid = TRUE)` on success
- Returns `list(valid = FALSE, error = "message")` on failure
- All error messages made concise and actionable

### 2. Updated .run() Function with Notice Integration

**Lines 547-612**: Added comprehensive error handling with Notices

#### a) Input Validation with ERROR Notice
```r
validation <- private$.validateInputData(...)
if (!validation$valid) {
    errorNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'validationError',
        type = jmvcore::NoticeType$ERROR
    )
    errorNotice$setContent(validation$error)  # Single line
    self$results$insert(1, errorNotice)
    return()
}
```

#### b) Calculation with tryCatch and ERROR Notice
```r
tryCatch({
    calculated_times <- private$.calculate_survival_time(...)
}, error = function(e) {
    calcErrorNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'calculationError',
        type = jmvcore::NoticeType$ERROR
    )
    calcErrorNotice$setContent(as.character(e$message))
    self$results$insert(1, calcErrorNotice)
})
```

### 3. Replaced HTML Warnings with Notices

**Lines 955-998**: Replaced HTML warning generation with Notice objects

#### a) High Missing Data WARNING (Single Line)
```r
if (pct > 10) {
    missingNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'missingData',
        type = jmvcore::NoticeType$WARNING
    )
    missingNotice$setContent(
        sprintf('%d observations (%.1f%%) have missing time intervals. Investigate missing date values as this may affect study conclusions.',
                quality$missing_values, pct)
    )
    self$results$insert(notice_position, missingNotice)
}
```

#### b) Future Dates STRONG_WARNING (Single Line)
```r
if (quality$future_dates > 0) {
    futureDatesNotice <- jmvcore::Notice$new(
        options = self$options,
        name = 'futureDates',
        type = jmvcore::NoticeType$STRONG_WARNING
    )
    futureDatesNotice$setContent(
        sprintf('%d date values are in the future. Review date columns for data entry errors or incorrect date formats.',
                quality$future_dates)
    )
    self$results$insert(notice_position, futureDatesNotice)
}
```

### 4. Added Completion INFO Notice

**Lines 835-846**: Added success confirmation at bottom

```r
completionNotice <- jmvcore::Notice$new(
    options = self$options,
    name = 'analysisComplete',
    type = jmvcore::NoticeType$INFO
)
completionNotice$setContent(
    sprintf('Analysis completed using %d observations with mean follow-up %.1f %s (total person-time: %.1f person-%s).',
            summary_stats$n, summary_stats$mean, self$options$output_unit,
            summary_stats$total_person_time, self$options$output_unit)
)
self$results$insert(999, completionNotice)
```

## Notice Types Implemented

| Notice Name | Type | Trigger | Position |
|-------------|------|---------|----------|
| `validationError` | ERROR | Invalid data structure, missing columns | 1 (top) |
| `calculationError` | ERROR | Any calculation failure | 1 (top) |
| `missingData` | WARNING | >10% missing time intervals | 1+ (top) |
| `futureDates` | STRONG_WARNING | Any dates in future | 1+ (top) |
| `analysisComplete` | INFO | Successful completion | 999 (bottom) |

## Key Design Decisions

### 1. **Single-Line Content Only**
Following CLAUDE.md guidance: "Notices are currently **single-line only** and cannot contain line breaks."

All notice content is formatted as single lines without `\n` characters.

### 2. **No .r.yaml Definitions**
Following serialization best practices: Notice objects are **NOT** defined in `.r.yaml` to avoid protobuf serialization errors. All notices created dynamically in `.b.R`.

### 3. **Kept Detailed Html Outputs**
Html outputs (`todo`, `summary`, `aboutPanel`, etc.) remain unchanged for detailed, multi-line explanations. Notices serve as **concise banners** that complement Html content.

### 4. **Error Handling Strategy**
- Validation errors: Return status from helper functions, create ERROR notice in `.run()`
- Calculation errors: Caught by tryCatch, create ERROR notice
- Data quality warnings: Create WARNING/STRONG_WARNING notices after successful calculation

### 5. **Negative Intervals Logic**
Negative intervals with `remove_negative=FALSE` trigger `stop()` at line 491, which is caught by tryCatch and displayed as ERROR notice. No separate WARNING notice needed (would be unreachable code).

## Testing Checklist

- [x] Syntax validation passed
- [ ] Test ERROR notice: Remove both date variables
- [ ] Test validation errors: Empty data, missing columns
- [ ] Test calculation error: Invalid date format
- [ ] Test WARNING: Dataset with >10% missing
- [ ] Test STRONG_WARNING: Dataset with future dates
- [ ] Test INFO: Successful analysis completion
- [ ] Test negative intervals error (remove_negative=FALSE)
- [ ] Test negative intervals removed (remove_negative=TRUE)
- [ ] Verify all 17 arguments still work
- [ ] Verify all 9 outputs still populate

## Benefits

### User Experience
✅ **Clear error hierarchy** - ERROR/STRONG_WARNING/WARNING/INFO visually distinct
✅ **Graceful degradation** - Errors don't crash session, display in results
✅ **Actionable messages** - Users know what to do next
✅ **Single-line conciseness** - Quick to read, no scrolling

### Code Quality
✅ **No serialization errors** - Notices created dynamically only
✅ **Clean separation** - Validation returns status, UI handles display
✅ **Consistent pattern** - All notices follow same creation pattern
✅ **Modern jamovi UX** - Follows official jamovi Notice best practices

## References

- `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/vignettes/jamovi_notices_guide.md`
- `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/CLAUDE.md`
- Official jamovi API: https://dev.jamovi.org/api_notices.html

## Next Steps

1. Test all notice scenarios with real data
2. Verify notice positioning and visibility
3. Check that Html outputs still render correctly
4. Test with jamovi application
5. Update other functions to use same Notice pattern

---

**Implementation Date:** 2025-12-20
**Status:** ✅ Syntax Validated, Ready for Testing
**Production Ready:** After testing completion
