# survivalPower Notice Type Access Fix

## Issue
The `survivalPower` module was encountering a runtime error when trying to sort notices by priority:

```
Error: 'type' does not exist in this results element
private$.run()
order(sapply(validation_result$notices, function(n) {
    type_str <- as.character(n$type)
    priority_map[type_str]
}))
```

## Root Cause
The code was attempting to access the `type` field of `jmvcore::Notice` objects via `n$type`, but Notice objects do not expose their type field directly through the `$` accessor.

## Solution
Modified the notice collection and sorting approach to store notices with their types in a structured format:

### Before
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'invalidHazardRatio',
    type = jmvcore::NoticeType::ERROR
)
notice$setContent(sprintf(...))
notices <- append(notices, list(notice))  # ❌ Type not accessible later

# Later when sorting:
sorted_notices <- validation_result$notices[order(sapply(validation_result$notices, function(n) {
    type_str <- as.character(n$type)  # ❌ Fails - type not accessible
    priority_map[type_str]
}))]
```

### After
```r
notice <- jmvcore::Notice$new(
    options = self$options,
    name = 'invalidHazardRatio',
    type = jmvcore::NoticeType::ERROR
)
notice$setContent(sprintf(...))
notices <- append(notices, list(list(notice = notice, type = "ERROR")))  # ✅ Store type separately

# Later when sorting:
sorted_notices <- validation_result$notices[order(sapply(validation_result$notices, function(n) {
    priority_map[n$type]  # ✅ Access stored type string
}))]

# When inserting:
for (i in seq_along(sorted_notices)) {
    self$results$insert(i, sorted_notices[[i]]$notice)  # ✅ Extract notice object
}
```

## Changes Made

### 1. Updated `.validate_inputs()` function (R/survivalPower.b.R)
- Modified all 11 instances of notice collection in main validation
- Changed from `list(notice)` to `list(list(notice = notice, type = "TYPE"))`
- Types: ERROR, WARNING, STRONG_WARNING, INFO

### 2. Updated `.validate_parameter_combinations()` function
- Modified 4 instances of notice collection
- Applied same structured format for notices_list

### 3. Updated sorting and insertion code (R/survivalPower.b.R:49-62)
- Line 55: Changed from `as.character(n$type)` to `n$type` (direct access to string)
- Line 60: Changed from `sorted_notices[[i]]` to `sorted_notices[[i]]$notice` (extract Notice object)

## Total Fixes
- **15 notice collection statements** modified across two functions
- **2 sorting/insertion** code sections updated

## Additional Syntax Fix

### Issue
After the initial fix, a parse error was discovered:
```
Error in parse(): R/survivalPower.b.R:87:43: unexpected '::'
87:                 type = jmvcore::NoticeType::INFO
                                              ^
```

### Root Cause
In R, `::` is the namespace operator used for accessing exported package functions (e.g., `package::function`). It cannot be chained. To access enum/list elements like NoticeType values, you must use `$`, not `::`.

### Syntax Correction
- **Incorrect**: `jmvcore::NoticeType::ERROR` ❌ (cannot use :: twice)
- **Correct**: `jmvcore::NoticeType$ERROR` ✅ (use $ to access list element)

All NoticeType declarations were corrected:
```r
# Before (invalid):
type = jmvcore::NoticeType::ERROR

# After (correct):
type = jmvcore::NoticeType$ERROR
```

## Testing
- ✅ File sources successfully: `source('R/survivalPower.b.R')`
- ✅ Module loads successfully: `devtools::load_all()`
- ✅ No parse errors or syntax errors

## Files Modified
- `/Users/serdarbalci/Documents/GitHub/ClinicoPathJamoviModule/R/survivalPower.b.R`

## Date
2025-12-28
