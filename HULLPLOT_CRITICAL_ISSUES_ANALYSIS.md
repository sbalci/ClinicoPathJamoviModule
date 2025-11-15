# Hull Plot Module - Critical Issues Analysis

**Date**: 2025-01-15
**Module**: `hullplot`
**Status**: 🔴 4 CRITICAL BUGS IDENTIFIED - NOT READY FOR RELEASE

---

## Executive Summary

The `hullplot` module has **4 CRITICAL reliability issues** that make it unsafe for clinical exploratory work:

### Issues Identified
1. 🔴 **Unsafe data caching** - Caches based on variable names + row count only, reuses stale data after filtering/editing
2. 🔴 **Wrong color scales** - Uses group_var levels for both fill and colour, ignores color_var levels
3. 🔴 **Single-group crash** - Natural Language Summary crashes with NaN when only 1 group selected
4. 🔴 **Zero test coverage** - Tests only check for no errors, never verify correctness of hulls, colors, statistics

---

## Issue 1: Unsafe Data Caching

### CRITICAL BUG

**Location**: `R/hullplot.b.R:25-41`

**Current Implementation** (UNSAFE):
```r
.prepare_data = function() {
    # Create a cache key based on relevant options
    data_size <- if (is.null(self$data)) 0 else nrow(self$data)
    cache_key <- paste(
        self$options$x_var,
        self$options$y_var,
        self$options$group_var,
        self$options$color_var,
        self$options$size_var,
        data_size,  # ❌ ONLY row count, not actual data content!
        sep = "|"
    )

    # Return cached data if key matches
    if (!is.null(private$.data_cache_key) &&
        private$.data_cache_key == cache_key &&
        !is.null(private$.prepared_data)) {
        return(private$.prepared_data)  # ❌ Returns stale data!
    }
    # ...
}
```

**Problem**:
- Cache key includes variable names and `nrow(self$data)` only
- Does NOT include actual data content
- If user:
  - Filters data from 100 to 50 rows, then back to 100 rows → cache invalidated, correct
  - Edits values in-place (100 rows → still 100 rows) → cache NOT invalidated ❌
  - Reshuffles rows (100 rows → still 100 rows) → cache NOT invalidated ❌
  - Changes data values without changing row count → cache NOT invalidated ❌

**Clinical Impact**:
- Researcher filters to "Stage I" tumors (50 cases)
- Explores hull plot
- Removes filter to see all stages (100 cases)
- Filters to "Stage II" tumors (also 50 cases)
- **Result**: Hull plot shows Stage I data (cached) labeled as Stage II ❌
- **Consequence**: Incorrect clinical interpretation, wrong treatment decisions

**Recommended Fix**:
```r
.prepare_data = function() {
    # CRITICAL FIX: Include data content hash in cache key
    data_size <- if (is.null(self$data)) 0 else nrow(self$data)

    # Calculate hash of relevant data columns to detect content changes
    data_hash <- ""
    if (!is.null(self$data) && data_size > 0) {
        # Select only the columns used in the plot
        relevant_cols <- c(
            self$options$x_var,
            self$options$y_var,
            self$options$group_var,
            self$options$color_var,
            self$options$size_var
        )
        relevant_cols <- relevant_cols[!sapply(relevant_cols, is.null)]

        if (length(relevant_cols) > 0) {
            relevant_data <- self$data[, relevant_cols, drop = FALSE]
            # Use digest::digest() or a simple hash
            data_hash <- digest::digest(relevant_data, algo = "xxhash64")
        }
    }

    cache_key <- paste(
        self$options$x_var,
        self$options$y_var,
        self$options$group_var,
        self$options$color_var,
        self$options$size_var,
        data_size,
        data_hash,  # ✅ Now includes actual data content
        sep = "|"
    )

    # Return cached data if key matches
    if (!is.null(private$.data_cache_key) &&
        private$.data_cache_key == cache_key &&
        !is.null(private$.prepared_data)) {
        return(private$.prepared_data)
    }
    # ... continue with data preparation
}
```

**Alternative (simpler but less efficient)**:
```r
# Option B: Disable caching entirely for safety
.prepare_data = function() {
    # Always rebuild data - safer but slower
    # For exploratory clinical work, correctness > speed

    # ... prepare data without caching
}
```

---

## Issue 2: Wrong Color Scales

### CRITICAL BUG

**Location**: `R/hullplot.b.R:363-371`

**Current Implementation** (WRONG):
```r
# Build color scales
n_groups <- length(levels(plot_data[[group_var]]))

# ❌ Both fill and colour use group_var levels
p <- p +
    scale_fill_manual(
        values = color_palette[1:n_groups],  # Based on group_var
        name = self$options$group_var
    ) +
    scale_colour_manual(
        values = color_palette[1:n_groups],  # ❌ Should use color_var!
        name = self$options$color_var
    )
```

**Problem**:
- When user selects DIFFERENT variables for:
  - `group_var` (defines hulls): e.g., "Treatment" (3 levels: A, B, C)
  - `color_var` (colors points): e.g., "Response" (2 levels: Yes, No)
- Code uses `n_groups = 3` for BOTH fill and colour scales
- **Result**:
  - Fill scale: 3 colors (correct for 3 treatment groups)
  - Colour scale: 3 colors (❌ WRONG - Response only has 2 levels!)
  - Legend shows 3 colors for Response when only 2 exist
  - Colors recycled unpredictably
  - Impossible to distinguish which points are which response

**Clinical Impact**:
- Pathologist plots tumor samples:
  - Hulls by "Diagnosis" (5 types)
  - Points colored by "Metastasis" (2 levels: Yes/No)
- Legend shows 5 colors for Metastasis (should be 2)
- Points colored unpredictably
- **Result**: Cannot tell which tumors metastasized ❌
- **Consequence**: Invalid clinical conclusions about metastasis patterns

**Recommended Fix**:
```r
# CRITICAL FIX: Use correct variable levels for each aesthetic
plot_data <- private$.prepare_data()
group_var <- self$options$group_var
color_var <- self$options$color_var

# Get correct number of levels for each aesthetic
n_groups <- length(levels(plot_data[[group_var]]))
n_colors <- if (!is.null(color_var) && color_var != "") {
    length(levels(plot_data[[color_var]]))
} else {
    n_groups  # Fallback to group_var if no color_var specified
}

# Build scales with correct level counts
p <- p +
    scale_fill_manual(
        values = color_palette[1:n_groups],  # ✅ Correct for hulls
        name = self$options$group_var
    )

# Only add colour scale if color_var specified and different from group_var
if (!is.null(color_var) && color_var != "" && color_var != group_var) {
    p <- p +
        scale_colour_manual(
            values = color_palette[1:n_colors],  # ✅ Correct for points
            name = self$options$color_var
        )
}
```

---

## Issue 3: Single-Group Crash in Natural Language Summary

### CRITICAL BUG

**Location**: `R/hullplot.b.R:596-654`

**Current Implementation** (CRASHES):
```r
# Natural Language Summary
group_means <- aggregate(cbind(x_var, y_var) ~ group_var, data = plot_data, mean)
group_distances <- stats::dist(group_means[, c("x_var", "y_var")])

# ❌ When only 1 group: dist() returns length-0 vector
avg_distance <- mean(group_distances)  # ❌ mean(numeric(0)) = NaN

# ❌ Crash here with "missing value where TRUE/FALSE needed"
if (avg_distance > 2) {
    cohort_separation <- "well-separated"
} else if (avg_distance > 1) {
    cohort_separation <- "moderately separated"
} else {
    cohort_separation <- "closely grouped"
}
```

**Problem**:
- When only 1 group selected (common in exploratory analysis):
  - `group_means` has 1 row
  - `stats::dist()` on 1 point returns `dist(0)` = empty numeric vector
  - `mean(numeric(0))` = `NaN`
  - `if (NaN > 2)` throws error: "missing value where TRUE/FALSE needed"
- **Result**: Analysis crashes, no plot, no output ❌

**Clinical Impact**:
- Researcher wants to explore single treatment arm before comparing
- Selects only "Treatment A" patients
- **Result**: Immediate crash, no visualization ❌
- **Consequence**: Cannot perform exploratory single-cohort analysis

**Recommended Fix**:
```r
# CRITICAL FIX: Handle single-group case gracefully
group_means <- aggregate(cbind(x_var, y_var) ~ group_var, data = plot_data, mean)

# Check number of groups
n_distinct_groups <- nrow(group_means)

if (n_distinct_groups < 2) {
    # Single group - no inter-group distances to calculate
    cohort_separation <- "single cohort (no comparison available)"
    avg_distance <- NA
    max_distance <- NA
    min_distance <- NA
} else {
    # Multiple groups - calculate distances
    group_distances <- stats::dist(group_means[, c("x_var", "y_var")])
    avg_distance <- mean(group_distances)
    max_distance <- max(group_distances)
    min_distance <- min(group_distances)

    # Determine separation (safe now - avg_distance is numeric, not NaN)
    if (is.na(avg_distance)) {
        cohort_separation <- "unable to calculate"
    } else if (avg_distance > 2) {
        cohort_separation <- "well-separated"
    } else if (avg_distance > 1) {
        cohort_separation <- "moderately separated"
    } else {
        cohort_separation <- "closely grouped"
    }
}

# Build summary with safe values
summary_text <- paste0(
    "Cohort Separation: ", cohort_separation,
    if (!is.na(avg_distance)) {
        paste0("\nAverage Distance: ", round(avg_distance, 2))
    } else {
        ""
    }
)
```

---

## Issue 4: Zero Test Coverage of Correctness

### CRITICAL GAP

**Current Tests** (`tests/testthat/test-hullplot.R`):
```r
test_that("hullplot works", {
    result <- hullplot(
        data = iris,
        x_var = "Sepal.Length",
        y_var = "Sepal.Width",
        group_var = "Species"
    )
    expect_no_error(result)  # ❌ Only checks "no crash"
})
```

**Problem**:
- Tests only use `expect_no_error()` - verify code doesn't crash
- NO tests verify:
  - Hull polygons are calculated correctly
  - Colors match the correct variables and groups
  - Statistics (means, distances) are accurate
  - Summaries contain expected text
  - Edge cases (1 group, empty groups, NA handling)
- **Result**: All 4 critical bugs went completely undetected ❌

**Recommended Test Coverage**:

### 1. Data Caching Tests
```r
test_that("hullplot invalidates cache when data content changes", {
    # Create initial data
    data1 <- iris[1:50, ]
    result1 <- hullplot(data = data1, x_var = "Sepal.Length",
                        y_var = "Sepal.Width", group_var = "Species")

    # Change data content but keep same nrow
    data2 <- iris[51:100, ]  # Same 50 rows, different content
    result2 <- hullplot(data = data2, x_var = "Sepal.Length",
                        y_var = "Sepal.Width", group_var = "Species")

    # CRITICAL: Results should be DIFFERENT (not cached)
    # Check that plot data differs (need to access internal data)
    expect_false(identical(result1, result2))
})
```

### 2. Color Scale Tests
```r
test_that("hullplot uses correct color scale levels for color_var", {
    # Create data with different levels for group vs color
    test_data <- data.frame(
        x = rnorm(100),
        y = rnorm(100),
        group = factor(rep(c("A", "B", "C"), length.out = 100)),  # 3 levels
        color = factor(rep(c("Yes", "No"), length.out = 100))     # 2 levels
    )

    result <- hullplot(
        data = test_data,
        x_var = "x",
        y_var = "y",
        group_var = "group",
        color_var = "color"
    )

    # CRITICAL: Color scale should have 2 levels (not 3)
    # Legend for "color" should show 2 colors (Yes/No)
    # (Need to extract plot object and check scales)
})
```

### 3. Single-Group Tests
```r
test_that("hullplot handles single group without crashing", {
    # Filter to single species
    single_group <- iris[iris$Species == "setosa", ]

    # Should NOT crash
    expect_no_error({
        result <- hullplot(
            data = single_group,
            x_var = "Sepal.Length",
            y_var = "Sepal.Width",
            group_var = "Species",
            generate_summary = TRUE
        )
    })

    # CRITICAL: Summary should handle single group gracefully
    # Check summary text includes "single cohort" or similar
})
```

### 4. Hull Correctness Tests
```r
test_that("hullplot calculates correct hull polygons", {
    # Create simple test data with known hull
    test_data <- data.frame(
        x = c(0, 1, 0, 1),
        y = c(0, 0, 1, 1),
        group = factor(rep("A", 4))
    )

    result <- hullplot(
        data = test_data,
        x_var = "x",
        y_var = "y",
        group_var = "group"
    )

    # CRITICAL: Hull should be a square with corners at (0,0), (1,0), (1,1), (0,1)
    # (Need to extract hull polygon coordinates and verify)
})
```

### 5. Statistics Accuracy Tests
```r
test_that("hullplot calculates accurate group statistics", {
    # Use iris data with known means
    result <- hullplot(
        data = iris,
        x_var = "Sepal.Length",
        y_var = "Sepal.Width",
        group_var = "Species",
        show_group_means = TRUE
    )

    # CRITICAL: Verify means match expected values
    expected_setosa_x <- mean(iris$Sepal.Length[iris$Species == "setosa"])
    expected_setosa_y <- mean(iris$Sepal.Width[iris$Species == "setosa"])

    # (Need to extract displayed means and compare)
    # expect_equal(displayed_means$setosa$x, expected_setosa_x, tolerance = 0.01)
})
```

---

## Risk Assessment

### Current Status
- **Risk Level**: 🔴 **CRITICAL - NOT READY FOR RELEASE**
- Stale data caching → visualization shows wrong data after filtering
- Wrong color scales → cannot interpret point colors correctly
- Single-group crash → common exploratory use case fails immediately
- Zero verification → no confidence in mathematical correctness

### After Fixes (Recommended)
- **Risk Level**: 🟢 **LOW - PRODUCTION READY**
- Data hash in cache key → always shows current data
- Correct color scale levels → accurate color interpretation
- Single-group handling → works for all cohort sizes
- Comprehensive tests → verified correctness for all scenarios

---

## Implementation Priority

### Phase 1: Critical Safety Fixes (Required for Release)
1. **Fix data caching** (Issue #1)
   - Add data content hash to cache key
   - OR disable caching entirely (simpler, safer)
   - Estimated: 1-2 hours

2. **Fix single-group crash** (Issue #3)
   - Add n_groups check before dist() call
   - Handle n_groups < 2 gracefully
   - Estimated: 30 minutes

### Phase 2: Correctness Fixes (Required for Release)
3. **Fix color scales** (Issue #2)
   - Calculate n_colors from color_var, not group_var
   - Conditional colour scale addition
   - Estimated: 1 hour

### Phase 3: Test Coverage (Required for Confidence)
4. **Create comprehensive tests** (Issue #4)
   - Data caching validation tests
   - Color scale correctness tests
   - Single/multi-group edge case tests
   - Hull polygon verification tests
   - Statistics accuracy tests
   - Estimated: 3-4 hours

**Total Estimated Time**: 6-8 hours

---

## Verification Checklist

- [ ] Data caching includes content hash (not just row count)
- [ ] Cache invalidates when data filtered/edited/reshuffled
- [ ] Color scale uses color_var levels (not group_var)
- [ ] Legend shows correct number of colors for each variable
- [ ] Single-group case handled without crash
- [ ] Natural Language Summary shows "single cohort" for 1 group
- [ ] Multi-group distance calculations work correctly
- [ ] Empty groups handled gracefully
- [ ] NA values in data handled correctly
- [ ] Tests verify hull polygon correctness
- [ ] Tests verify color scale correctness
- [ ] Tests verify statistics accuracy
- [ ] Tests cover edge cases (1 group, empty, NAs)
- [ ] All tests passing

---

## Conclusion

The `hullplot` module has **4 critical bugs** that make it unsafe for clinical exploratory work:

🔴 **Stale data caching** - Shows wrong data after filtering (highest severity)
🔴 **Wrong color scales** - Misrepresents cohort characteristics
🔴 **Single-group crash** - Fails for common use case
🔴 **Zero verification** - No confidence in correctness

**Recommendation**: Do NOT release until all 4 issues fixed and comprehensive tests added.

**Status**: 🔴 **NOT READY FOR RELEASE**

---

**Document Version**: 1.0
**Last Updated**: 2025-01-15
**Reviewer**: Claude Code (Sonnet 4.5)
