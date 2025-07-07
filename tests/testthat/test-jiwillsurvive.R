context("test-jiwillsurvive")

# Load required library
library(ClinicoPath)

test_that("jiwillsurvive works with basic survival analysis", {
  
  # Load test data
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test basic survival model without grouping
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event",
      confidence_level = 0.95,
      show_risk_table = TRUE,
      show_median_survival = TRUE,
      show_confidence_bands = TRUE,
      show_censoring_marks = TRUE,
      show_statistics = TRUE,
      show_survival_table = TRUE,
      show_interpretation = TRUE
    ),
    NA
  )
  
  # Test survival model with grouping
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event",
      group_var = "treatment",
      confidence_level = 0.95,
      show_risk_table = TRUE,
      show_statistics = TRUE
    ),
    NA
  )
})

test_that("jiwillsurvive works with different event variable formats", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test with numeric event variable (0/1)
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event"
    ),
    NA
  )
  
  # Test with logical event variable (TRUE/FALSE)
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model", 
      time_var = "survival_months",
      event_var = "death_logical"
    ),
    NA
  )
  
  # Test with factor event variable
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_factor"
    ),
    NA
  )
})

test_that("jiwillsurvive works with Kaplan-Meier analysis", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test basic Kaplan-Meier
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "kaplan_meier",
      time_var = "survival_months",
      event_var = "death_event",
      confidence_level = 0.95,
      show_risk_table = TRUE,
      show_confidence_bands = TRUE
    ),
    NA
  )
  
  # Test Kaplan-Meier with grouping
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "kaplan_meier",
      time_var = "survival_months",
      event_var = "death_event",
      group_var = "treatment",
      show_median_survival = TRUE
    ),
    NA
  )
})

test_that("jiwillsurvive works with date-based follow-up derivation", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test data preparation with date derivation
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "data_prep",
      start_date_var = "enrollment_date",
      end_date_var = "last_contact_date",
      derive_followup = TRUE,
      followup_units = "months"
    ),
    NA
  )
  
  # Test with different time units
  for (unit in c("days", "weeks", "months", "years")) {
    test_that(paste("jiwillsurvive works with", unit, "time units"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "data_prep",
          start_date_var = "enrollment_date",
          end_date_var = "last_contact_date",
          derive_followup = TRUE,
          followup_units = unit
        ),
        NA
      )
    })
  }
  
  # Test survival analysis with derived follow-up
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",  # Will be overridden by derived time
      event_var = "death_event",
      start_date_var = "enrollment_date",
      end_date_var = "last_contact_date",
      derive_followup = TRUE,
      followup_units = "months"
    ),
    NA
  )
})

test_that("jiwillsurvive works with follow-up visualization", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test different follow-up plot types
  plot_types <- c("histogram", "timeline", "summary")
  
  for (plot_type in plot_types) {
    test_that(paste("jiwillsurvive works with", plot_type, "follow-up plot"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "followup_plot",
          time_var = "survival_months",
          followup_plot_type = plot_type,
          followup_units = "months"
        ),
        NA
      )
    })
  }
  
  # Test follow-up visualization with derived time
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "followup_plot",
      start_date_var = "enrollment_date",
      end_date_var = "last_contact_date",
      derive_followup = TRUE,
      followup_units = "months",
      followup_plot_type = "histogram"
    ),
    NA
  )
})

test_that("jiwillsurvive works with different plot styles and colors", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test different plot styles
  plot_styles <- c("iwillsurvive", "classic", "modern", "minimal", "publication")
  
  for (style in plot_styles) {
    test_that(paste("jiwillsurvive works with", style, "plot style"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "survival_model",
          time_var = "survival_months",
          event_var = "death_event",
          group_var = "treatment",
          plot_style = style
        ),
        NA
      )
    })
  }
  
  # Test different color palettes
  color_palettes <- c("default", "colorblind", "viridis", "set1", "dark2", "pastel")
  
  for (palette in color_palettes) {
    test_that(paste("jiwillsurvive works with", palette, "color palette"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "survival_model",
          time_var = "survival_months",
          event_var = "death_event",
          group_var = "treatment",
          color_palette = palette
        ),
        NA
      )
    })
  }
})

test_that("jiwillsurvive works with custom plot options", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test with custom labels and breaks
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event",
      group_var = "treatment",
      plot_title = "Custom Survival Analysis",
      x_label = "Time (months)",
      y_label = "Survival Probability",
      time_breaks = "0,12,24,36,48,60",
      legend_position = "top"
    ),
    NA
  )
  
  # Test different legend positions
  legend_positions <- c("right", "left", "top", "bottom", "none")
  
  for (position in legend_positions) {
    test_that(paste("jiwillsurvive works with", position, "legend position"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "survival_model",
          time_var = "survival_months",
          event_var = "death_event",
          group_var = "treatment",
          legend_position = position
        ),
        NA
      )
    })
  }
})

test_that("jiwillsurvive works with edge case datasets", {
  
  # Test with short follow-up data
  data("jiwillsurvive_short_followup", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_short_followup,
      analysis_type = "survival_model",
      time_var = "survival_days",
      event_var = "death_event",
      group_var = "treatment",
      followup_units = "days"
    ),
    NA
  )
  
  # Test with long follow-up data
  data("jiwillsurvive_long_followup", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_long_followup,
      analysis_type = "survival_model",
      time_var = "survival_years",
      event_var = "death_event",
      group_var = "treatment",
      followup_units = "years"
    ),
    NA
  )
  
  # Test with single group data (no grouping variable)
  data("jiwillsurvive_single_group", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_single_group,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "event"
    ),
    NA
  )
})

test_that("jiwillsurvive works with different grouping variables", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test with different grouping variables
  grouping_vars <- c("treatment", "stage", "grade", "sex", "biomarker_status", "risk_group")
  
  for (var in grouping_vars) {
    test_that(paste("jiwillsurvive works with", var, "as grouping variable"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "survival_model",
          time_var = "survival_months",
          event_var = "death_event",
          group_var = var
        ),
        NA
      )
    })
  }
})

test_that("jiwillsurvive works with confidence level variations", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test different confidence levels
  conf_levels <- c(0.80, 0.90, 0.95, 0.99)
  
  for (conf in conf_levels) {
    test_that(paste("jiwillsurvive works with", conf*100, "% confidence level"), {
      expect_error(
        jiwillsurvive(
          data = jiwillsurvive_test_data,
          analysis_type = "survival_model",
          time_var = "survival_months",
          event_var = "death_event",
          confidence_level = conf,
          show_confidence_bands = TRUE
        ),
        NA
      )
    })
  }
})

test_that("jiwillsurvive works with summary time points", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test with specific time points for summary
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event",
      group_var = "treatment",
      time_points = "6,12,18,24,36,48",
      show_survival_table = TRUE
    ),
    NA
  )
})

test_that("jiwillsurvive handles character date variables", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test with character date variables
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "data_prep",
      start_date_var = "start_date_char",
      end_date_var = "end_date_char",
      derive_followup = TRUE,
      followup_units = "months"
    ),
    NA
  )
})

test_that("jiwillsurvive works with export data option", {
  
  data("jiwillsurvive_test_data", package = "ClinicoPath")
  
  # Test data preparation with export option
  expect_error(
    jiwillsurvive(
      data = jiwillsurvive_test_data,
      analysis_type = "data_prep",
      start_date_var = "enrollment_date",
      end_date_var = "last_contact_date",
      derive_followup = TRUE,
      followup_units = "months",
      export_data = TRUE
    ),
    NA
  )
})

# Test with existing survival datasets from the package
test_that("jiwillsurvive works with existing package datasets", {
  
  # Test with basic_survival_data
  data("basic_survival_data", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = basic_survival_data,
      analysis_type = "survival_model",
      time_var = "survival_months",
      event_var = "death_event",
      group_var = "treatment"
    ),
    NA
  )
  
  # Test with colon dataset
  data("colon", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = colon,
      analysis_type = "survival_model",
      time_var = "time",
      event_var = "status",
      group_var = "rx"
    ),
    NA
  )
  
  # Test with melanoma dataset
  data("melanoma", package = "ClinicoPath")
  
  expect_error(
    jiwillsurvive(
      data = melanoma,
      analysis_type = "survival_model",
      time_var = "time",
      event_var = "status",
      group_var = "sex"
    ),
    NA
  )
})