test_that("agepyramid module loads correctly", {
  skip_if_not_installed('jmvReadWrite')
  expect_true(exists("agepyramidClass"))
  expect_true(is.function(agepyramid))
})

test_that("agepyramid handles basic input validation", {
  # Test with missing required variables
  expect_error(
    agepyramid(data = histopathology, age = NULL, gender = "Sex", female = "Female", male = "Male"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    agepyramid(data = histopathology, age = "Age", gender = NULL, female = "Female", male = "Male"),
    NA  # Should not error during initialization, only during run
  )
  
  expect_error(
    agepyramid(data = histopathology, age = "Age", gender = "Sex", female = NULL, male = "Male"),
    NA  # Should not error during initialization, only during run
  )
})

test_that("agepyramid works with valid inputs", {
  # Test basic functionality
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male"
  )
  
  expect_s3_class(result, "ResultsElement")
  expect_true("Age" %in% names(histopathology))
  expect_true("Sex" %in% names(histopathology))
})

test_that("agepyramid handles different bin widths correctly", {
  # Test different bin width values
  bin_widths <- c(2, 5, 10, 15)
  
  for (bin_width in bin_widths) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        male = "Male",
        bin_width = bin_width
      )
    }, NA, info = paste("bin_width:", bin_width))
  }
})

test_that("agepyramid handles custom plot titles correctly", {
  # Test custom plot titles
  titles <- c("Custom Title", "Age Distribution", "Population Pyramid", "")
  
  for (title in titles) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        male = "Male",
        plot_title = title
      )
    }, NA, info = paste("plot_title:", title))
  }
})

test_that("agepyramid handles custom colors correctly", {
  # Test different color specifications
  color_combinations <- list(
    list(female_color = "#FF5733", male_color = "#3498DB"),  # Hex codes
    list(female_color = "red", male_color = "blue"),         # Named colors
    list(female_color = "#E74C3C", male_color = "#2ECC71"), # Different hex codes
    list(female_color = "darkgreen", male_color = "orange") # More named colors
  )
  
  for (i in seq_along(color_combinations)) {
    colors <- color_combinations[[i]]
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        male = "Male",
        female_color = colors$female_color,
        male_color = colors$male_color
      )
    }, NA, info = paste("color combination", i))
  }
})

test_that("agepyramid handles different gender levels correctly", {
  # Test with different female level specifications
  # First check what levels exist in the data
  gender_levels <- unique(histopathology$Sex)
  
  if ("Female" %in% gender_levels) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        male = "Male"
      )
    }, NA)
  }
  
  if ("Male" %in% gender_levels) {
    # Test with Male as "female" level (should work but flip the pyramid)
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Male",
        male = "Female"
      )
    }, NA)
  }
})

test_that("agepyramid handles missing data appropriately", {
  # Create dataset with missing values
  test_data <- histopathology
  test_data$Age[1:5] <- NA
  test_data$Sex[6:10] <- NA
  
  expect_error({
    result <- agepyramid(
      data = test_data,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male"
    )
  }, NA)
})

test_that("agepyramid parameter combinations work correctly", {
  # Test complex parameter combinations
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male",
      bin_width = 8,
      plot_title = "Comprehensive Age Analysis",
      female_color = "#9B59B6",
      male_color = "#1ABC9C"
    )
  }, NA)
})

test_that("agepyramid handles edge cases for bin width", {
  # Test boundary values for bin width
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male",
      bin_width = 1  # Very small bins
    )
  }, NA)
  
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male",
      bin_width = 20  # Large bins
    )
  }, NA)
})

test_that("agepyramid handles different data types", {
  # Test with factor gender variable
  test_data <- histopathology
  test_data$Sex <- factor(test_data$Sex)
  
  expect_error({
    result <- agepyramid(
      data = test_data,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male"
    )
  }, NA)
  
  # Test with numeric age variable (should work)
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male"
    )
  }, NA)
})

test_that("agepyramid handles small datasets", {
  # Test with small dataset
  small_data <- histopathology[1:20, ]
  
  expect_error({
    result <- agepyramid(
      data = small_data,
      age = "Age",
      gender = "Sex",
      female = "Female",
      male = "Male"
    )
  }, NA)
})

test_that("agepyramid results have expected structure", {
  # Test that results object has expected components
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male"
  )
  
  # Check for expected result components
  expect_true(exists("pyramidTable", envir = result))
  expect_true(exists("plot", envir = result))
})

test_that("agepyramid handles synthetic data correctly", {
  # Create synthetic dataset for testing
  set.seed(123)
  synthetic_data <- data.frame(
    age = sample(18:80, 100, replace = TRUE),
    gender = sample(c("F", "M"), 100, replace = TRUE, prob = c(0.6, 0.4))
  )
  
  expect_error({
    result <- agepyramid(
      data = synthetic_data,
      age = "age",
      gender = "gender",
      female = "F",
      male = "M",
      bin_width = 10,
      plot_title = "Synthetic Data Age Pyramid"
    )
  }, NA)
})

test_that("agepyramid handles different age ranges", {
  # Test with different age data ranges
  # Young population
  young_data <- data.frame(
    age = sample(18:35, 50, replace = TRUE),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )
  
  expect_error({
    result <- agepyramid(
      data = young_data,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male",
      bin_width = 5
    )
  }, NA)
  
  # Older population
  older_data <- data.frame(
    age = sample(50:90, 50, replace = TRUE),
    gender = sample(c("Female", "Male"), 50, replace = TRUE)
  )
  
  expect_error({
    result <- agepyramid(
      data = older_data,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male",
      bin_width = 10
    )
  }, NA)
})

test_that("agepyramid default values work correctly", {
  # Test that default values are applied correctly
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male"
    # Using all default values for optional parameters
  )
  
  expect_s3_class(result, "ResultsElement")
})

test_that("agepyramid handles unbalanced gender data", {
  # Create dataset with unbalanced gender distribution
  unbalanced_data <- data.frame(
    age = c(sample(25:65, 80, replace = TRUE), sample(30:70, 20, replace = TRUE)),
    gender = c(rep("Female", 80), rep("Male", 20))
  )
  
  expect_error({
    result <- agepyramid(
      data = unbalanced_data,
      age = "age",
      gender = "gender",
      female = "Female",
      male = "Male",
      bin_width = 10,
      plot_title = "Unbalanced Gender Distribution"
    )
  }, NA)
})

test_that("agepyramid handles special characters in plot title", {
  # Test plot titles with special characters
  special_titles <- c(
    "Age Pyramid: 2024 Analysis",
    "Population (N=250)",
    "Age Distribution - Clinical Study",
    "Pyramid Chart: α=0.05"
  )
  
  for (title in special_titles) {
    expect_error({
      result <- agepyramid(
        data = histopathology,
        age = "Age",
        gender = "Sex",
        female = "Female",
        male = "Male",
        plot_title = title
      )
    }, NA, info = paste("special title:", title))
  }
})

test_that("agepyramid dependency handling", {
  # Test function behavior when required packages are available
  required_packages <- c("ggplot2", "dplyr", "tidyr", "tibble")
  
  for (pkg in required_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      expect_error({
        result <- agepyramid(
          data = histopathology,
          age = "Age",
          gender = "Sex",
          female = "Female",
          male = "Male"
        )
      }, NA, info = paste("package:", pkg))
    } else {
      skip(paste("Package", pkg, "not available"))
    }
  }
})

test_that("agepyramid vs CLAUDE.md parameter usage", {
  # Test the specific parameter pattern mentioned in CLAUDE.md
  # "agepyramid: Use gender = "Sex", female = "Female" (not sex = "Sex")"
  
  # Correct usage according to CLAUDE.md
  expect_error({
    result <- agepyramid(
      data = histopathology,
      age = "Age",
      gender = "Sex",      # Use 'gender' parameter
      female = "Female",    # Use 'female' level
      male = "Male"
    )
  }, NA)
  
  # Test that results object has expected structure
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male"
  )
  
  expect_true(exists("pyramidTable", envir = result))
  expect_true(exists("plot", envir = result))
})

test_that("agepyramid output is correct and stable", {
  # 1. Snapshot test for the pyramidTable
  result <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male",
    bin_width = 10
  )
  
  # To update snapshots, run testthat::snapshot_review()
  expect_snapshot(result$pyramidTable$asDF)
  
  # 2. Test dataInfo HTML content for a standard run
  expect_true(grepl("Data Summary", result$dataInfo$content))
  expect_true(grepl("Initial observations", result$dataInfo$content))
  
  # 3. Test single-gender cohort functionality
  female_only_data <- histopathology[histopathology$Sex == "Female", ]
  result_single_gender <- agepyramid(
    data = female_only_data,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = NULL
  )
  
  # Check for single-gender message in dataInfo
  expect_true(grepl("Single-gender", result_single_gender$dataInfo$content))
  
  # Check that the plot is a simple bar chart (not a pyramid)
  # We can't directly test the plot object, but we can check the plot's state
  plot_state_data <- result_single_gender$plot$state
  expect_equal(ncol(plot_state_data), 3) # Should have Pop, Gender, n
  expect_equal(as.character(unique(plot_state_data$Gender)), "Female")
  
  # 4. Test correctness of age bin labels
  result_geriatric <- agepyramid(
    data = histopathology,
    age = "Age",
    gender = "Sex",
    female = "Female",
    male = "Male",
    age_groups = "geriatric"
  )
  
  table_df <- result_geriatric$pyramidTable$asDF
  # The geriatric preset's first band is now [0, 65) - left-closed, so a
  # 65-year-old starts the "65-69" band instead of falling into the band below.
  expect_true("0-64" %in% as.character(table_df$Pop))
  expect_false("1-65" %in% as.character(table_df$Pop))
})

# REGRESSION TESTS for critical fixes (2025-01-18)
# These tests prevent the reintroduction of two serious bugs that undermined
# statistical accuracy and user trust

test_that("REGRESSION: age bands are left-closed and labels match them", {
  # HISTORY - this assertion was REVERSED during the release review.
  #
  # The original bug was a label/interval mismatch: bins were (lower, upper]
  # while labels read "0-4", "5-9". The earlier fix kept the right-closed bins
  # and moved the LABELS to match them ("1-5", "6-10"). This review moved the
  # BINS to match the labels instead, i.e. cut(right = FALSE). Both resolve the
  # mismatch; the bins were changed because the right-closed version is wrong
  # in ways relabelling cannot repair:
  #
  #   1. It distorts the pyramid. cut(0:19, c(0,5,10,15,20), include.lowest =
  #      TRUE, right = TRUE) puts 6, 5, 5, 4 single-year ages into the four
  #      bands. On a uniform population the bottom bar is 20% too tall and the
  #      top bar 20% too short - and the bottom bar of an age pyramid is the
  #      birth cohort. right = FALSE gives 5, 5, 5, 5.
  #   2. It contradicts the module's own presets. "Geriatric (65+)" put a
  #      65-year-old in the band labelled "1-65" (not geriatric at all);
  #      "Reproductive (15-50)" put a 15-year-old in "1-15"; "Pediatric (<18)"
  #      counted an 18-year-old as paediatric.
  #   3. WHO/UN standard age groups (0-4, 5-9, ...) are left-closed, so this is
  #      what a demographer reading the pyramid expects.
  #
  # The invariant the original test protected - labels must describe the bins
  # they name - is preserved below and now checked directly.

  # a uniform population must produce equal bars
  uniform <- data.frame(
    age = rep(0:19, each = 2),
    gender = rep(c("Female", "Male"), 20)
  )
  res_uniform <- agepyramid(data = uniform, age = "age", gender = "gender",
                            female = "Female", male = "Male", bin_width = 5)
  df_u <- res_uniform$pyramidTable$asDF
  df_u <- df_u[df_u$Pop != "Total", ]
  expect_equal(unique(df_u$Female), 5)
  expect_equal(unique(df_u$Male), 5)

  test_data <- data.frame(
    age = c(5, 10, 15, 20, 25, 30),   # all on bin boundaries
    gender = rep(c("Female", "Male"), 3)
  )
  result <- agepyramid(data = test_data, age = "age", gender = "gender",
                       female = "Female", male = "Male", bin_width = 5)
  age_groups <- as.character(result$pyramidTable$asDF$Pop)
  age_groups <- age_groups[age_groups != "Total"]

  # [0,5) covers ages 0-4; [5,10) covers 5-9; a boundary age starts a new band.
  # (No observation falls in [0,5) for this data, so that band is absent.)
  expect_true(any(grepl("^5-9$", age_groups)))
  expect_true(any(grepl("^10-14$", age_groups)))
  # the right-closed labels must NOT come back
  expect_false(any(grepl("^1-5$", age_groups)))
  expect_false(any(grepl("^6-10$", age_groups)))

  # LABEL/BIN AGREEMENT: every labelled band must contain exactly the ages it names
  labelled <- agepyramid(
    data = data.frame(age = rep(0:24, each = 2),
                      gender = rep(c("Female", "Male"), 25)),
    age = "age", gender = "gender", female = "Female", male = "Male",
    bin_width = 5)$pyramidTable$asDF
  labelled <- labelled[labelled$Pop != "Total", ]
  for (i in seq_len(nrow(labelled))) {
    lab <- as.character(labelled$Pop[i])
    if (!grepl("^[0-9]+-[0-9]+$", lab)) next
    bounds <- as.numeric(strsplit(lab, "-")[[1]])
    n_ages <- bounds[2] - bounds[1] + 1
    expect_equal(labelled$Female[i], n_ages, info = lab)
  }

  # Preset boundary ages must fall in the band the preset is named for
  presets <- list(
    list(preset = "geriatric",    boundary = 65, must_match = "^65-"),
    list(preset = "reproductive", boundary = 15, must_match = "^15-"),
    list(preset = "pediatric",    boundary = 18, must_match = "^18\\+$")
  )
  for (p in presets) {
    d <- data.frame(age = c(p$boundary, p$boundary),
                    gender = c("Female", "Male"))
    tb <- agepyramid(data = d, age = "age", gender = "gender",
                     female = "Female", male = "Male",
                     age_groups = p$preset)$pyramidTable$asDF
    occupied <- as.character(tb$Pop[tb$Pop != "Total" & (tb$Female + tb$Male) > 0])
    expect_true(any(grepl(p$must_match, occupied)),
                info = sprintf("%s: age %d landed in %s", p$preset, p$boundary,
                               paste(occupied, collapse = ", ")))
  }

  # Open-ended final band: [95, Inf) is "95+", not "96+"
  result_geriatric <- agepyramid(
    data = data.frame(age = c(65, 70, 75, 80, 85, 90, 95, 100),
                      gender = rep(c("Female", "Male"), 4)),
    age = "age", gender = "gender", female = "Female", male = "Male",
    age_groups = "geriatric")
  gg <- as.character(result_geriatric$pyramidTable$asDF$Pop)
  expect_true(any(grepl("^95\\+$", gg)),
              info = "[95, Inf) should be labelled '95+'")
  expect_false(any(grepl("^96\\+$", gg)),
               info = "'96+' belonged to the right-closed convention")
})

test_that("REGRESSION: reported sample size matches aggregated table data", {
  # Issue: Rows with non-numeric ages stayed in mydata during n_final calculation
  #        but disappeared during cut()/aggregation, causing N mismatch
  # Fix: Explicitly filter NA ages before calculating n_final

  # Create test data with non-numeric age values
  test_data <- data.frame(
    age = c("25", "30", "35", "abc", "xyz", "40", "45", "50", "invalid", "55"),
    gender = c("Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Male"),
    stringsAsFactors = FALSE
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 10
  )

  # Extract reported counts from data summary HTML
  data_info <- result$dataInfo$content

  # Parse n_initial and n_final from HTML (they're in table rows)
  n_initial_match <- regmatches(data_info, regexpr("Initial observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))
  n_final_match <- regmatches(data_info, regexpr("Final observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))

  n_initial <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_initial_match))
  n_final <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_final_match))

  # Get actual counts from table (sum of Female + Male, excluding Total row)
  table_df <- result$pyramidTable$asDF
  table_data <- table_df[table_df$Pop != "Total", ]
  actual_female <- sum(table_data$Female, na.rm = TRUE)
  actual_male <- sum(table_data$Male, na.rm = TRUE)
  actual_total <- actual_female + actual_male

  # CRITICAL TEST: Reported n_final must equal sum of table counts
  expect_equal(n_final, actual_total,
               info = paste("Reported N (", n_final, ") must match table sum (", actual_total, ")",
                          "to avoid user confusion about analyzed sample size"))

  # Verify that invalid ages were actually excluded
  # We had 3 invalid values: "abc", "xyz", "invalid"
  # So n_final should be n_initial - 3
  expect_equal(n_final, n_initial - 3,
               info = "Should have excluded exactly 3 non-numeric age values")

  # Check that exclusion is properly reported in HTML
  expect_true(grepl("Non-numeric ages", data_info) || grepl("non-numeric age values", data_info),
              info = "Data quality message should mention non-numeric age exclusions")
})

test_that("REGRESSION: exclusion breakdown in summary is accurate and complete", {
  # Test that all exclusion sources are tracked and reported correctly

  # Create data with multiple exclusion types
  test_data <- data.frame(
    age = c("25", "30", "NA", "35", "abc", "40", "45", "50"),
    gender = c("Female", "Male", "Unknown", NA, "Female", "Male", "Female", "Other"),
    stringsAsFactors = FALSE
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 10
  )

  data_info <- result$dataInfo$content

  # Should report both age and gender exclusions
  expect_true(grepl("Non-numeric ages|non-numeric age values", data_info),
              info = "Should report age conversion failures")
  expect_true(grepl("Missing/unrecognized gender|Gender Exclusions", data_info),
              info = "Should report gender exclusions")

  # Parse exclusion counts from HTML table
  # Look for "- Non-numeric ages:" and "- Missing/unrecognized gender:" rows
  age_excl_match <- regmatches(data_info,
                                regexpr("Non-numeric ages.*?<td[^>]*color: #d32f2f[^>]*>(\\d+)</td>",
                                       data_info, perl = TRUE))
  gender_excl_match <- regmatches(data_info,
                                   regexpr("Missing/unrecognized gender.*?<td[^>]*color: #d32f2f[^>]*>(\\d+)</td>",
                                          data_info, perl = TRUE))

  # Verify exclusion counts are shown in breakdown
  if (length(age_excl_match) > 0) {
    expect_true(grepl("\\d+", age_excl_match),
                info = "Age exclusion count should be displayed")
  }

  if (length(gender_excl_match) > 0) {
    expect_true(grepl("\\d+", gender_excl_match),
                info = "Gender exclusion count should be displayed")
  }

  # Verify table sum still matches reported n_final
  table_df <- result$pyramidTable$asDF
  table_data <- table_df[table_df$Pop != "Total", ]
  actual_total <- sum(table_data$Female, na.rm = TRUE) + sum(table_data$Male, na.rm = TRUE)

  # Extract n_final from HTML
  n_final_match <- regmatches(data_info, regexpr("Final observations.*?<td[^>]*>(\\d+)</td>", data_info, perl = TRUE))
  n_final <- as.numeric(sub(".*?(\\d+)</td>", "\\1", n_final_match))

  expect_equal(n_final, actual_total,
               info = "Even with multiple exclusion types, reported N must match table sum")
})

test_that("REGRESSION: ggcharts gender mapping is deterministic", {
  # ggcharts::pyramid_chart() assigns sides/colors from unique(Gender), so a
  # male-only youngest bin must not make Male the first group.
  test_data <- data.frame(
    age = c(1, 1, 6, 6),
    gender = c("Male", "Male", "Female", "Female")
  )

  result <- agepyramid(
    data = test_data,
    age = "age",
    gender = "gender",
    female = "Female",
    male = "Male",
    bin_width = 5,
    enableGGCharts = TRUE
  )

  # The state deliberately holds the RAW grid - .plotGGCharts calls
  # .prepare_ggcharts_data() at render time so the work is not done twice - so
  # asserting on the state tested something ggcharts never sees. Prepare it the
  # same way the renderer does and assert on that.
  ns <- asNamespace("ClinicoPath")
  helper <- get("agepyramidClass", ns)$new(
    options = get("agepyramidOptions", ns)$new(
      age = "age", gender = "gender", female = "Female", male = "Male"),
    data = test_data)
  ggcharts_state <- helper$.__enclos_env__$private$.prepare_ggcharts_data(
    result$plotGGCharts$state)

  # every Gender x Pop cell must be present, zero-filled, so ggcharts's
  # unique(Gender) is deterministic and Female always takes the first side
  expect_equal(nrow(ggcharts_state), 4L)
  expect_equal(unique(ggcharts_state$Gender), c("Female", "Male"))
  # bin_width 5 over ages 1..6 gives breaks 0, 5, 6; left-closed bands are
  # [0,5) -> "0-4" and the final [5,6] -> "5-6" (closed at the top by
  # include.lowest so the oldest observation is kept).
  expect_equal(as.character(ggcharts_state$Pop), rep(c("0-4", "5-6"), 2))

  female_young <- ggcharts_state$n[
    ggcharts_state$Gender == "Female" & as.character(ggcharts_state$Pop) == "0-4"
  ]
  male_older <- ggcharts_state$n[
    ggcharts_state$Gender == "Male" & as.character(ggcharts_state$Pop) == "5-6"
  ]

  expect_equal(female_young, 0L)
  expect_equal(male_older, 0L)
})
