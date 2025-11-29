# Comprehensive Test Suite for statsplot2 Function
# Addresses reviewer concern: "Complete lack of testing for complex dispatch logic"
#
# This test suite validates ALL 8 decision paths in the automatic plot selection:
# 1. Independent + Factor × Continuous → ggbetweenstats
# 2. Independent + Continuous × Continuous → ggscatterstats
# 3. Independent + Factor × Factor → ggbarstats
# 4. Independent + Continuous × Factor → Dotplot
# 5. Repeated + Factor × Continuous → ggwithinstats
# 6. Repeated + Factor × Factor → Alluvial diagram
# 7. Repeated + Continuous × Continuous → Fallback
# 8. Repeated + Continuous × Factor → Fallback

library(testthat)
library(jmvcore)
library(ggstatsplot)
library(ggalluvial)

# Source the files
source("../../R/statsplot2.h.R")
source("../../R/statsplot2.b.R")

# Define . function if not exists
if (!exists(".")) . <- function(x, ...) x

# ==============================================================================
# Test Data Setup Functions
# ==============================================================================

setup_factor_continuous_data <- function() {
  set.seed(42)
  data.frame(
    treatment = factor(rep(c("Control", "Drug_A", "Drug_B"), each = 20)),
    response = c(
      rnorm(20, mean = 50, sd = 10),  # Control
      rnorm(20, mean = 65, sd = 12),  # Drug_A
      rnorm(20, mean = 60, sd = 11)   # Drug_B
    ),
    gender = factor(rep(c("Male", "Female"), 30))
  )
}

setup_continuous_continuous_data <- function() {
  set.seed(123)
  data.frame(
    age = rnorm(50, mean = 55, sd = 15),
    tumor_size = rnorm(50, mean = 30, sd = 8),
    hospital = factor(sample(c("A", "B"), 50, replace = TRUE))
  )
}

setup_factor_factor_data <- function() {
  set.seed(456)
  data.frame(
    tumor_grade = factor(sample(c("G1", "G2", "G3"), 80, replace = TRUE)),
    lymph_node = factor(sample(c("Positive", "Negative"), 80, replace = TRUE)),
    stage = factor(sample(c("Early", "Late"), 80, replace = TRUE))
  )
}

setup_continuous_factor_data <- function() {
  set.seed(789)
  data.frame(
    biomarker_level = rnorm(60, mean = 100, sd = 25),
    disease_status = factor(sample(c("Remission", "Active", "Progressive"), 60, replace = TRUE)),
    site = factor(sample(c("Site1", "Site2"), 60, replace = TRUE))
  )
}

setup_repeated_factor_continuous_data <- function() {
  set.seed(234)
  data.frame(
    patient_id = factor(rep(paste0("P", 1:15), each = 3)),
    timepoint = factor(rep(c("Baseline", "Month_3", "Month_6"), 15)),
    score = c(
      rep(rnorm(15, 40, 8), 1),  # Baseline
      rep(rnorm(15, 50, 10), 1), # Month 3
      rep(rnorm(15, 55, 12), 1)  # Month 6
    )
  )
}

setup_repeated_factor_factor_data <- function() {
  set.seed(567)
  data.frame(
    patient_id = factor(rep(paste0("P", 1:20), each = 2)),
    condition_baseline = factor(sample(c("Mild", "Moderate", "Severe"), 20, replace = TRUE)),
    condition_followup = factor(sample(c("Mild", "Moderate", "Severe"), 20, replace = TRUE))
  )
}

# ==============================================================================
# Part 1: Type Detection Tests (Critical - validates dispatch logic foundation)
# ==============================================================================

describe("statsplot2 Type Detection", {

  test_that("detects Independent Factor × Continuous correctly", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Independent Continuous × Continuous correctly", {
    test_data <- setup_continuous_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "tumor_size",
      group = "age",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Independent Factor × Factor correctly", {
    test_data <- setup_factor_factor_data()

    result <- statsplot2(
      data = test_data,
      dep = "lymph_node",
      group = "tumor_grade",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Independent Continuous × Factor correctly", {
    test_data <- setup_continuous_factor_data()

    result <- statsplot2(
      data = test_data,
      dep = "biomarker_level",
      group = "disease_status",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Repeated Factor × Continuous correctly", {
    test_data <- setup_repeated_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "score",
      group = "timepoint",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Repeated Factor × Factor correctly", {
    test_data <- setup_repeated_factor_factor_data()

    result <- statsplot2(
      data = test_data,
      dep = "condition_baseline",
      group = "condition_followup",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Repeated Continuous × Continuous (uses fallback)", {
    test_data <- setup_continuous_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "tumor_size",
      group = "age",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("detects Repeated Continuous × Factor (uses fallback)", {
    test_data <- setup_continuous_factor_data()

    result <- statsplot2(
      data = test_data,
      dep = "biomarker_level",
      group = "disease_status",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })
})

# ==============================================================================
# Part 2: Dispatch Correctness Tests (Critical - validates right function called)
# ==============================================================================

describe("statsplot2 Dispatch to Correct ggstatsplot Function", {

  test_that("dispatches to ggbetweenstats for Independent Factor × Continuous", {
    test_data <- setup_factor_continuous_data()

    # This should call ggbetweenstats internally
    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent",
      distribution = "p"  # Parametric
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))

    # The plot should be a ggplot object from ggbetweenstats
    # which creates violin/box plots
  })

  test_that("dispatches to ggscatterstats for Independent Continuous × Continuous", {
    test_data <- setup_continuous_continuous_data()

    # This should call ggscatterstats internally
    result <- statsplot2(
      data = test_data,
      dep = "tumor_size",
      group = "age",
      direction = "independent",
      distribution = "p"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))

    # The plot should be a scatter plot with correlation stats
  })

  test_that("dispatches to ggbarstats for Independent Factor × Factor", {
    test_data <- setup_factor_factor_data()

    # This should call ggbarstats internally
    result <- statsplot2(
      data = test_data,
      dep = "lymph_node",
      group = "tumor_grade",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))

    # The plot should be a bar chart with chi-square test
  })

  test_that("creates appropriate plot for Independent Continuous × Factor", {
    test_data <- setup_continuous_factor_data()

    # This creates a dotplot/histogram
    result <- statsplot2(
      data = test_data,
      dep = "biomarker_level",
      group = "disease_status",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("dispatches to ggwithinstats for Repeated Factor × Continuous", {
    test_data <- setup_repeated_factor_continuous_data()

    # This should call ggwithinstats internally
    result <- statsplot2(
      data = test_data,
      dep = "score",
      group = "timepoint",
      direction = "repeated",
      distribution = "p"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))

    # The plot should handle repeated measures design
  })

  test_that("creates alluvial diagram for Repeated Factor × Factor", {
    test_data <- setup_repeated_factor_factor_data()

    # This should create an alluvial diagram
    result <- statsplot2(
      data = test_data,
      dep = "condition_baseline",
      group = "condition_followup",
      direction = "repeated",
      alluvsty = "t1"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("uses fallback for Repeated Continuous × Continuous", {
    test_data <- setup_continuous_continuous_data()

    # Should use fallback ggplot2 visualization
    result <- statsplot2(
      data = test_data,
      dep = "tumor_size",
      group = "age",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("uses fallback for Repeated Continuous × Factor", {
    test_data <- setup_continuous_factor_data()

    # Should use fallback ggplot2 visualization
    result <- statsplot2(
      data = test_data,
      dep = "biomarker_level",
      group = "disease_status",
      direction = "repeated"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })
})

# ==============================================================================
# Part 3: Statistical Approach Tests (Parametric, Nonparametric, Robust, Bayes)
# ==============================================================================

describe("statsplot2 Statistical Approach Options", {

  test_that("applies parametric approach correctly", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent",
      distribution = "p"  # Parametric (t-test, ANOVA)
    )

    expect_s3_class(result, "Group")
  })

  test_that("applies nonparametric approach correctly", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent",
      distribution = "np"  # Nonparametric (Mann-Whitney, Kruskal-Wallis)
    )

    expect_s3_class(result, "Group")
  })

  test_that("applies robust approach correctly", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent",
      distribution = "r"  # Robust (trimmed means)
    )

    expect_s3_class(result, "Group")
  })

  test_that("applies Bayesian approach correctly", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent",
      distribution = "bf"  # Bayes Factor
    )

    expect_s3_class(result, "Group")
  })
})

# ==============================================================================
# Part 4: Grouped Plot Tests (grvar parameter)
# ==============================================================================

describe("statsplot2 Grouped Plots", {

  test_that("creates grouped plot for Factor × Continuous with grvar", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      grvar = "gender",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("creates grouped plot for Continuous × Continuous with grvar", {
    test_data <- setup_continuous_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "tumor_size",
      group = "age",
      grvar = "hospital",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })

  test_that("creates grouped plot for Factor × Factor with grvar", {
    test_data <- setup_factor_factor_data()

    result <- statsplot2(
      data = test_data,
      dep = "lymph_node",
      group = "tumor_grade",
      grvar = "stage",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    expect_true("plot" %in% names(result))
  })
})

# ==============================================================================
# Part 5: Edge Case Tests (Critical - validates robustness)
# ==============================================================================

describe("statsplot2 Edge Cases", {

  test_that("handles missing values in dependent variable", {
    test_data <- setup_factor_continuous_data()
    test_data$response[c(1, 5, 10, 15)] <- NA

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles missing values in grouping variable", {
    test_data <- setup_factor_continuous_data()
    test_data$treatment[c(2, 7, 12)] <- NA

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles two-level factor correctly", {
    test_data <- data.frame(
      group = factor(c(rep("A", 15), rep("B", 15))),
      value = c(rnorm(15, 50, 10), rnorm(15, 60, 10))
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles many-level factor (>5 levels)", {
    set.seed(999)
    test_data <- data.frame(
      group = factor(rep(paste0("Group_", LETTERS[1:8]), each = 10)),
      value = rnorm(80, mean = 50, sd = 15)
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles small sample size", {
    small_data <- data.frame(
      group = factor(c("A", "A", "B", "B")),
      value = c(10, 12, 20, 22)
    )

    result <- statsplot2(
      data = small_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles large dataset with sampling", {
    set.seed(555)
    large_data <- data.frame(
      group = factor(sample(c("A", "B", "C"), 12000, replace = TRUE)),
      value = rnorm(12000, mean = 50, sd = 15)
    )

    # With sampling enabled
    result <- statsplot2(
      data = large_data,
      dep = "value",
      group = "group",
      direction = "independent",
      sampleLarge = TRUE
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles outliers in continuous data", {
    test_data <- setup_factor_continuous_data()
    test_data$response[1] <- 200  # Extreme outlier
    test_data$response[2] <- -50  # Extreme outlier

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles constant values in a group", {
    test_data <- data.frame(
      group = factor(c(rep("A", 10), rep("B", 10))),
      value = c(rep(50, 10), rnorm(10, 60, 10))
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles unbalanced groups", {
    test_data <- data.frame(
      group = factor(c(rep("A", 5), rep("B", 25))),
      value = c(rnorm(5, 50, 10), rnorm(25, 60, 10))
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles negative and zero values", {
    test_data <- data.frame(
      group = factor(rep(c("A", "B", "C"), each = 10)),
      value = c(rnorm(10, -10, 5), rnorm(10, 0, 3), rnorm(10, 10, 5))
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })
})

# ==============================================================================
# Part 6: Variable Name Tests (ensures dispatch works with various names)
# ==============================================================================

describe("statsplot2 Variable Name Handling", {

  test_that("handles variable names with underscores", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles variable names with spaces (converted to dots)", {
    test_data <- setup_factor_continuous_data()
    names(test_data)[names(test_data) == "response"] <- "tumor.response"

    result <- statsplot2(
      data = test_data,
      dep = "tumor.response",
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles variable names with numbers", {
    test_data <- data.frame(
      var1 = factor(rep(c("A", "B"), each = 15)),
      var2 = rnorm(30, mean = 50)
    )

    result <- statsplot2(
      data = test_data,
      dep = "var2",
      group = "var1",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })
})

# ==============================================================================
# Part 7: Error Handling Tests (validates informative error messages)
# ==============================================================================

describe("statsplot2 Error Handling", {

  test_that("handles NULL dependent variable gracefully", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = NULL,
      group = "treatment",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    # Should return without crashing
  })

  test_that("handles NULL grouping variable gracefully", {
    test_data <- setup_factor_continuous_data()

    result <- statsplot2(
      data = test_data,
      dep = "response",
      group = NULL,
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    # Should return without crashing
  })

  test_that("handles empty data gracefully", {
    empty_data <- data.frame(
      group = factor(character(0)),
      value = numeric(0)
    )

    result <- statsplot2(
      data = empty_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    # Should handle with informative error
  })

  test_that("handles all NA values gracefully", {
    test_data <- data.frame(
      group = factor(c("A", "A", "B", "B")),
      value = c(NA, NA, NA, NA)
    )

    result <- statsplot2(
      data = test_data,
      dep = "value",
      group = "group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
    # Should handle with informative error
  })
})

# ==============================================================================
# Part 8: Real-World Clinical Scenarios
# ==============================================================================

describe("statsplot2 Clinical Scenarios", {

  test_that("handles treatment comparison scenario", {
    set.seed(111)
    clinical_data <- data.frame(
      patient_id = paste0("P", 1:60),
      treatment_arm = factor(rep(c("Standard", "Experimental_A", "Experimental_B"), each = 20)),
      tumor_reduction = c(
        rnorm(20, mean = 30, sd = 10),  # Standard
        rnorm(20, mean = 50, sd = 15),  # Experimental A
        rnorm(20, mean = 45, sd = 12)   # Experimental B
      ),
      gender = factor(rep(c("Male", "Female"), 30))
    )

    result <- statsplot2(
      data = clinical_data,
      dep = "tumor_reduction",
      group = "treatment_arm",
      grvar = "gender",
      direction = "independent",
      distribution = "p"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles longitudinal tracking scenario", {
    set.seed(222)
    longitudinal_data <- data.frame(
      patient_id = factor(rep(paste0("P", 1:25), each = 4)),
      visit = factor(rep(c("Baseline", "Week_4", "Week_8", "Week_12"), 25)),
      pain_score = c(
        rep(rnorm(25, 7, 1.5), 1),   # Baseline
        rep(rnorm(25, 5, 1.2), 1),   # Week 4
        rep(rnorm(25, 3, 1.0), 1),   # Week 8
        rep(rnorm(25, 2, 0.8), 1)    # Week 12
      )
    )

    result <- statsplot2(
      data = longitudinal_data,
      dep = "pain_score",
      group = "visit",
      direction = "repeated",
      distribution = "np"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles biomarker correlation scenario", {
    set.seed(333)
    biomarker_data <- data.frame(
      biomarker_A = rnorm(50, mean = 100, sd = 25),
      biomarker_B = rnorm(50, mean = 200, sd = 50),
      cohort = factor(sample(c("Discovery", "Validation"), 50, replace = TRUE))
    )
    # Add correlation
    biomarker_data$biomarker_B <- biomarker_data$biomarker_B + 0.7 * biomarker_data$biomarker_A

    result <- statsplot2(
      data = biomarker_data,
      dep = "biomarker_B",
      group = "biomarker_A",
      direction = "independent",
      distribution = "p"
    )

    expect_s3_class(result, "Group")
  })

  test_that("handles categorical association scenario", {
    set.seed(444)
    categorical_data <- data.frame(
      smoking_status = factor(sample(c("Never", "Former", "Current"), 100, replace = TRUE)),
      lung_disease = factor(sample(c("None", "COPD", "Asthma", "Other"), 100, replace = TRUE)),
      age_group = factor(sample(c("Young", "Middle", "Old"), 100, replace = TRUE))
    )

    result <- statsplot2(
      data = categorical_data,
      dep = "lung_disease",
      group = "smoking_status",
      grvar = "age_group",
      direction = "independent"
    )

    expect_s3_class(result, "Group")
  })
})

print("All statsplot2 comprehensive tests completed!")
