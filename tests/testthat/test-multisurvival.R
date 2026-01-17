context("multisurvival")

test_that("multisurvival works with basic Cox regression", {

  # Load test data
  data("colon", package = "survival")
  
  # Basic test with continuous and categorical variables
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status", 
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "obstruct", "perfor"),
    contexpl = c("age", "nodes"),
    timetypeoutput = "days"
  )
  
  # Check that results are created
  expect_true(inherits(result, "multisurvivalResults"))
  
  # Check that text output exists
  expect_true(!is.null(result$text))
  
  # Verify the analysis ran without errors
  expect_false(result$todo$visible)
})

test_that("multisurvival handles date-based time calculation", {
  # Create test data with dates
  set.seed(123)
  n <- 100
  test_data <- data.frame(
    dx_date = seq(as.Date("2020-01-01"), by = "day", length.out = n),
    fu_date = seq(as.Date("2020-01-01"), by = "day", length.out = n) + 
              sample(30:365, n, replace = TRUE),
    status = sample(0:1, n, replace = TRUE),
    age = rnorm(n, 60, 10),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    stage = factor(sample(c("I", "II", "III", "IV"), n, replace = TRUE))
  )
  
  result <- multisurvival(
    data = test_data,
    tint = TRUE,
    dxdate = "dx_date", 
    fudate = "fu_date",
    timetypedata = "ymd",
    timetypeoutput = "months",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "stage"),
    contexpl = "age"
  )
  
  # Verify calculation occurred
  expect_true(inherits(result, "multisurvivalResults"))
  expect_false(result$todo$visible)
})

test_that("multisurvival risk score calculation works", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL, 
    explanatory = c("sex", "obstruct"),
    contexpl = "age",
    calculateRiskScore = TRUE,
    numRiskGroups = "three",
    plotRiskGroups = TRUE
  )
  
  # Check risk score outputs
  expect_true(!is.null(result$risk_score_analysis))
  expect_true(!is.null(result$riskScoreTable))
})

test_that("multisurvival handles stratification", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "obstruct",
    contexpl = "age", 
    use_stratify = TRUE,
    stratvar = "sex"
  )
  
  # Verify stratification was applied
  expect_true(inherits(result, "multisurvivalResults"))
  expect_true(!is.null(result$stratificationExplanation))
})

test_that("multisurvival model selection works", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "obstruct", "perfor"),
    contexpl = c("age", "nodes"),
    use_modelSelection = TRUE,
    modelSelection = "backward",
    selectionCriteria = "aic"
  )
  
  # Check model selection outputs
  expect_true(!is.null(result$text_model_selection))
  expect_true(!is.null(result$selection_method))
})

test_that("multisurvival handles missing data gracefully", {
  # Create data with missing values
  data("colon", package = "survival")
  test_data <- colon
  test_data$age[1:10] <- NA
  
  expect_warning(
    result <- multisurvival(
      data = test_data,
      elapsedtime = "time",
      outcome = "status",
      outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
      explanatory = "sex",
      contexpl = "age"
    ),
    NA  # Expect no specific warning, function should handle NAs
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
})

test_that("multisurvival produces plots when requested", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time", 
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "sex",
    contexpl = "age",
    hr = TRUE,
    km = TRUE,
    sty = "t1"
  )
  
  # Check that plots are generated
  expect_true(!is.null(result$plot))
  expect_true(!is.null(result$plotKM))
})

test_that("multisurvival person-time analysis works", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "sex",
    contexpl = "age",
    person_time = TRUE,
    time_intervals = "180, 365, 730",
    rate_multiplier = 1000
  )
  
  # Check person-time outputs
  expect_true(!is.null(result$personTimeTable))
  expect_true(!is.null(result$personTimeSummary))
})

test_that("multisurvival handles landmark analysis", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status", 
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "sex",
    contexpl = "age",
    uselandmark = TRUE,
    landmark = 180
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
})

test_that("multisurvival adjusted survival curves work", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("obstruct", "perfor"),
    contexpl = "age",
    ac = TRUE,
    adjexplanatory = "sex",
    ac_method = "average"
  )
  
  # Check adjusted curve outputs
  expect_true(!is.null(result$plot_adj))
})

test_that("multisurvival works with wide format time-dependent covariates", {
  # Load test data for wide format
  load(file.path(system.file(package = "ClinicoPath"), "..", "..", "data", "test_wide_time_dependent.rda"))
  
  result <- multisurvival(
    data = test_wide_time_dependent,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "stage"),
    contexpl = "age",
    use_time_dependent = TRUE,
    td_format = "wide",
    time_dep_vars = c("treatment_baseline"),
    change_times = "6, 12, 18",
    td_suffix_pattern = "_t{time}",
    timetypeoutput = "months"
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
  expect_false(result$todo$visible)
})

test_that("multisurvival works with long format time-dependent covariates", {
  # Load test data for long format  
  load(file.path(system.file(package = "ClinicoPath"), "..", "..", "data", "test_long_time_dependent.rda"))
  
  result <- multisurvival(
    data = test_long_time_dependent,
    use_time_dependent = TRUE,
    td_format = "long",
    start_time_var = "tstart",
    stop_time_var = "tstop",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    time_dep_vars = c("treatment"),
    explanatory = c("sex", "stage"),
    contexpl = "age",
    timetypeoutput = "months"
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
  expect_false(result$todo$visible)
})

test_that("multisurvival frailty models work", {
  data("colon", package = "survival")
  
  # Add clustering variable for frailty
  colon$hospital <- factor(sample(1:5, nrow(colon), replace = TRUE))
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "obstruct"),
    contexpl = "age",
    use_frailty = TRUE,
    frailty_var = "hospital",
    frailty_distribution = "gamma"
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
  expect_false(result$todo$visible)
})

test_that("multisurvival splines for non-proportional hazards work", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("obstruct", "perfor"),
    contexpl = "age",
    use_splines = TRUE,
    spline_vars = "age",
    spline_df = 3,
    spline_type = "pspline"
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
  expect_false(result$todo$visible)
})

test_that("multisurvival decision tree analysis works", {
  data("colon", package = "survival")
  
  result <- multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "obstruct"),
    contexpl = "age",
    use_tree = TRUE,
    min_node = 20,
    complexity = 0.01,
    max_depth = 3,
    show_terminal_nodes = TRUE
  )
  
  expect_true(inherits(result, "multisurvivalResults"))
  expect_true(!is.null(result$tree_summary))
  expect_true(!is.null(result$tree_plot))
})
