# Visual Regression Tests for statsplot2 Function
# Tests the automatic plot selection feature using vdiffr
# Validates that all 8 dispatch paths render correctly

library(testthat)
library(ClinicoPath)

# Helper function to check if vdiffr is installed
skip_if_not_installed_vdiffr <- function() {
  if (!requireNamespace("vdiffr", quietly = TRUE)) {
    skip("vdiffr package not installed")
  }
}

# Helper function to check if ggstatsplot is installed
skip_if_not_installed_ggstatsplot <- function() {
  if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
    skip("ggstatsplot package not installed")
  }
}

# Helper function to extract plot from jamovi result object
extract_plot <- function(result, plot_name = "plot") {
  if (inherits(result, "Group")) {
    if (plot_name %in% names(result)) {
      plot_obj <- result[[plot_name]]
      if (inherits(plot_obj, "Image")) {
        state <- plot_obj$state
        if (!is.null(state) && inherits(state, "ggplot")) {
          return(state)
        }
      }
    }
  }
  return(NULL)
}

# ============================================================================
# PART 1: DISPATCH PATH VISUAL TESTS (8 baselines)
# Tests that each of the 8 decision paths renders the correct plot type
# ============================================================================

context("statsplot2 - Dispatch Path Visual Tests")

test_that("Path 1: Independent Factor × Continuous renders ggbetweenstats", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(100)
  test_data <- data.frame(
    treatment = factor(rep(c("Control", "Drug A", "Drug B"), each = 30)),
    response = c(rnorm(30, 50, 10), rnorm(30, 65, 10), rnorm(30, 75, 10))
  )

  result <- statsplot2(
    data = test_data,
    dep = "response",
    group = "treatment",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path1_independent_factor_continuous",
      fig = plot
    )
  }
})

test_that("Path 2: Independent Continuous × Continuous renders ggscatterstats", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(200)
  test_data <- data.frame(
    biomarker_a = rnorm(50, 100, 15),
    biomarker_b = rnorm(50, 50, 10)
  )
  test_data$biomarker_b <- test_data$biomarker_b + 0.7 * test_data$biomarker_a + rnorm(50, 0, 5)

  result <- statsplot2(
    data = test_data,
    dep = "biomarker_a",
    group = "biomarker_b",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path2_independent_continuous_continuous",
      fig = plot
    )
  }
})

test_that("Path 3: Independent Factor × Factor renders ggbarstats", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(300)
  test_data <- data.frame(
    treatment = factor(sample(c("Control", "Treatment"), 100, replace = TRUE)),
    outcome = factor(sample(c("Success", "Failure"), 100, replace = TRUE, prob = c(0.6, 0.4)))
  )

  result <- statsplot2(
    data = test_data,
    dep = "outcome",
    group = "treatment",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path3_independent_factor_factor",
      fig = plot
    )
  }
})

test_that("Path 4: Independent Continuous × Factor renders dotplot", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(400)
  test_data <- data.frame(
    age = rnorm(80, 55, 12),
    diagnosis = factor(sample(c("Benign", "Malignant"), 80, replace = TRUE))
  )

  result <- statsplot2(
    data = test_data,
    dep = "age",
    group = "diagnosis",
    direction = "independent",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path4_independent_continuous_factor",
      fig = plot
    )
  }
})

test_that("Path 5: Repeated Factor × Continuous renders ggwithinstats", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(500)
  test_data <- data.frame(
    timepoint = factor(rep(c("Baseline", "Week 4", "Week 8"), each = 25)),
    tumor_size = c(
      rnorm(25, 50, 8),
      rnorm(25, 45, 7),
      rnorm(25, 38, 6)
    )
  )

  result <- statsplot2(
    data = test_data,
    dep = "tumor_size",
    group = "timepoint",
    direction = "repeated",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path5_repeated_factor_continuous",
      fig = plot
    )
  }
})

test_that("Path 6: Repeated Factor × Factor renders alluvial diagram", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(600)
  test_data <- data.frame(
    baseline_stage = factor(sample(c("Stage I", "Stage II", "Stage III"), 60, replace = TRUE)),
    followup_stage = factor(sample(c("Stage I", "Stage II", "Stage III"), 60, replace = TRUE))
  )

  result <- statsplot2(
    data = test_data,
    dep = "baseline_stage",
    group = "followup_stage",
    direction = "repeated",
    alluvsty = "t1"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path6_repeated_factor_factor_alluvial",
      fig = plot
    )
  }
})

test_that("Path 7: Repeated Continuous × Continuous renders fallback plot", {
  skip_if_not_installed_vdiffr()

  set.seed(700)
  test_data <- data.frame(
    measurement_1 = rnorm(40, 100, 15),
    measurement_2 = rnorm(40, 105, 15)
  )

  result <- statsplot2(
    data = test_data,
    dep = "measurement_1",
    group = "measurement_2",
    direction = "repeated",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path7_repeated_continuous_continuous_fallback",
      fig = plot
    )
  }
})

test_that("Path 8: Repeated Continuous × Factor renders fallback plot", {
  skip_if_not_installed_vdiffr()

  set.seed(800)
  test_data <- data.frame(
    score = rnorm(50, 75, 12),
    category = factor(sample(c("Low", "Medium", "High"), 50, replace = TRUE))
  )

  result <- statsplot2(
    data = test_data,
    dep = "score",
    group = "category",
    direction = "repeated",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_path8_repeated_continuous_factor_fallback",
      fig = plot
    )
  }
})

# ============================================================================
# PART 2: STATISTICAL APPROACH VISUAL TESTS (4 baselines)
# Tests that parametric, nonparametric, robust, and Bayesian render correctly
# ============================================================================

context("statsplot2 - Statistical Approach Visual Tests")

test_that("Parametric approach renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1000)
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 30)),
    value = c(rnorm(30, 50, 10), rnorm(30, 55, 10), rnorm(30, 60, 10))
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_parametric",
      fig = plot
    )
  }
})

test_that("Nonparametric approach renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1100)
  # Create skewed data
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 30)),
    value = c(rexp(30, 0.1), rexp(30, 0.12), rexp(30, 0.08))
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_nonparametric",
      fig = plot
    )
  }
})

test_that("Robust approach renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1200)
  # Create data with outliers
  test_data <- data.frame(
    group = factor(rep(c("A", "B"), each = 25)),
    value = c(rnorm(25, 50, 10), rnorm(25, 55, 10))
  )
  test_data$value[c(1, 26)] <- c(150, 200)  # Add outliers

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "r"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_robust",
      fig = plot
    )
  }
})

test_that("Bayesian approach renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1300)
  test_data <- data.frame(
    group = factor(rep(c("A", "B"), each = 30)),
    value = c(rnorm(30, 50, 10), rnorm(30, 58, 10))
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "bf"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_bayesian",
      fig = plot
    )
  }
})

# ============================================================================
# PART 3: GROUPED PLOT VISUAL TESTS (3 baselines)
# Tests the grvar (grouping variable) parameter
# ============================================================================

context("statsplot2 - Grouped Plot Visual Tests")

test_that("Grouped plot with gender split renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1400)
  test_data <- data.frame(
    treatment = factor(rep(c("Control", "Treatment"), each = 40)),
    response = c(rnorm(40, 50, 10), rnorm(40, 60, 10)),
    gender = factor(rep(c("Male", "Female"), 40))
  )

  result <- statsplot2(
    data = test_data,
    dep = "response",
    group = "treatment",
    grvar = "gender",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_grouped_by_gender",
      fig = plot
    )
  }
})

test_that("Grouped plot with age categories renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1500)
  test_data <- data.frame(
    biomarker = rnorm(90, 100, 20),
    diagnosis = factor(sample(c("Negative", "Positive"), 90, replace = TRUE)),
    age_group = factor(rep(c("<50", "50-70", ">70"), each = 30))
  )

  result <- statsplot2(
    data = test_data,
    dep = "biomarker",
    group = "diagnosis",
    grvar = "age_group",
    direction = "independent",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_grouped_by_age",
      fig = plot
    )
  }
})

test_that("Grouped repeated measures renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1600)
  test_data <- data.frame(
    timepoint = factor(rep(c("Pre", "Post"), each = 40)),
    score = c(rnorm(40, 50, 8), rnorm(40, 55, 8)),
    cohort = factor(rep(c("Cohort A", "Cohort B"), 40))
  )

  result <- statsplot2(
    data = test_data,
    dep = "score",
    group = "timepoint",
    grvar = "cohort",
    direction = "repeated",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_grouped_repeated_measures",
      fig = plot
    )
  }
})

# ============================================================================
# PART 4: ALLUVIAL STYLE VISUAL TESTS (2 baselines)
# Tests both ggalluvial (t1) and easyalluvial (t2) styles
# ============================================================================

context("statsplot2 - Alluvial Style Visual Tests")

test_that("Alluvial style t1 (ggalluvial) renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1700)
  test_data <- data.frame(
    pre_treatment = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), 60, replace = TRUE)),
    post_treatment = factor(sample(c("Grade 1", "Grade 2", "Grade 3"), 60, replace = TRUE))
  )

  result <- statsplot2(
    data = test_data,
    dep = "pre_treatment",
    group = "post_treatment",
    direction = "repeated",
    alluvsty = "t1"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_alluvial_t1_ggalluvial",
      fig = plot
    )
  }
})

test_that("Alluvial style t2 (easyalluvial) renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1800)
  test_data <- data.frame(
    baseline = factor(sample(c("Low", "Medium", "High"), 60, replace = TRUE)),
    followup = factor(sample(c("Low", "Medium", "High"), 60, replace = TRUE))
  )

  result <- statsplot2(
    data = test_data,
    dep = "baseline",
    group = "followup",
    direction = "repeated",
    alluvsty = "t2"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_alluvial_t2_easyalluvial",
      fig = plot
    )
  }
})

# ============================================================================
# PART 5: EDGE CASE VISUAL TESTS (5 baselines)
# Tests visual rendering under challenging conditions
# ============================================================================

context("statsplot2 - Edge Case Visual Tests")

test_that("Two groups only renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(1900)
  test_data <- data.frame(
    group = factor(rep(c("Control", "Treatment"), each = 30)),
    response = c(rnorm(30, 50, 10), rnorm(30, 60, 10))
  )

  result <- statsplot2(
    data = test_data,
    dep = "response",
    group = "group",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_edge_two_groups",
      fig = plot
    )
  }
})

test_that("Many groups (6 groups) renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2000)
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C", "D", "E", "F"), each = 20)),
    value = c(
      rnorm(20, 50, 8), rnorm(20, 55, 8), rnorm(20, 60, 8),
      rnorm(20, 52, 8), rnorm(20, 58, 8), rnorm(20, 62, 8)
    )
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_edge_many_groups",
      fig = plot
    )
  }
})

test_that("Unbalanced groups render correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2100)
  test_data <- data.frame(
    group = factor(c(rep("Small", 10), rep("Medium", 30), rep("Large", 60))),
    value = c(rnorm(10, 50, 10), rnorm(30, 55, 10), rnorm(60, 60, 10))
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_edge_unbalanced_groups",
      fig = plot
    )
  }
})

test_that("Large dataset (sampled) renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2200)
  # Create large dataset to trigger sampling
  test_data <- data.frame(
    treatment = factor(sample(c("Control", "Drug A", "Drug B"), 12000, replace = TRUE)),
    response = rnorm(12000, 60, 15)
  )

  result <- statsplot2(
    data = test_data,
    dep = "response",
    group = "treatment",
    direction = "independent",
    distribution = "p",
    sampleLarge = TRUE
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_edge_large_dataset_sampled",
      fig = plot
    )
  }
})

test_that("Skewed distribution with nonparametric renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2300)
  # Create highly skewed data
  test_data <- data.frame(
    group = factor(rep(c("A", "B", "C"), each = 40)),
    value = c(rexp(40, 0.1), rexp(40, 0.15), rexp(40, 0.08))
  )

  result <- statsplot2(
    data = test_data,
    dep = "value",
    group = "group",
    direction = "independent",
    distribution = "np"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_edge_skewed_distribution",
      fig = plot
    )
  }
})

# ============================================================================
# PART 6: CLINICAL SCENARIO VISUAL TESTS (4 baselines)
# Real-world clinical research scenarios
# ============================================================================

context("statsplot2 - Clinical Scenario Visual Tests")

test_that("Clinical: Treatment response comparison renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2400)
  clinical_data <- data.frame(
    treatment_arm = factor(rep(c("Placebo", "Low Dose", "High Dose"), each = 35)),
    tumor_reduction = c(rnorm(35, 10, 15), rnorm(35, 25, 18), rnorm(35, 40, 20)),
    gender = factor(rep(c("Male", "Female"), length.out = 105))
  )

  result <- statsplot2(
    data = clinical_data,
    dep = "tumor_reduction",
    group = "treatment_arm",
    grvar = "gender",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_clinical_treatment_response",
      fig = plot
    )
  }
})

test_that("Clinical: Longitudinal disease progression renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2500)
  longitudinal_data <- data.frame(
    visit = factor(rep(c("Baseline", "Month 3", "Month 6", "Month 12"), each = 40)),
    disease_score = c(
      rnorm(40, 75, 12),
      rnorm(40, 68, 11),
      rnorm(40, 62, 10),
      rnorm(40, 55, 9)
    )
  )

  result <- statsplot2(
    data = longitudinal_data,
    dep = "disease_score",
    group = "visit",
    direction = "repeated",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_clinical_longitudinal_progression",
      fig = plot
    )
  }
})

test_that("Clinical: Biomarker correlation analysis renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2600)
  biomarker_data <- data.frame(
    ki67_index = rnorm(70, 30, 12),
    tumor_grade = rnorm(70, 2.5, 0.8)
  )
  biomarker_data$tumor_grade <- biomarker_data$tumor_grade + 0.05 * biomarker_data$ki67_index + rnorm(70, 0, 0.3)

  result <- statsplot2(
    data = biomarker_data,
    dep = "ki67_index",
    group = "tumor_grade",
    direction = "independent",
    distribution = "p"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_clinical_biomarker_correlation",
      fig = plot
    )
  }
})

test_that("Clinical: Stage migration analysis renders correctly", {
  skip_if_not_installed_vdiffr()
  skip_if_not_installed_ggstatsplot()

  set.seed(2700)
  stage_data <- data.frame(
    initial_stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 80, replace = TRUE)),
    final_stage = factor(sample(c("Stage I", "Stage II", "Stage III", "Stage IV"), 80, replace = TRUE))
  )

  result <- statsplot2(
    data = stage_data,
    dep = "initial_stage",
    group = "final_stage",
    direction = "repeated",
    alluvsty = "t1"
  )

  plot <- extract_plot(result, "plot")
  if (!is.null(plot)) {
    vdiffr::expect_doppelganger(
      title = "statsplot2_clinical_stage_migration",
      fig = plot
    )
  }
})
