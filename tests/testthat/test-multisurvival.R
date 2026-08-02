test_that("multisurvival works with basic Cox regression", {

  # Load test data
  colon <- survival::colon
  
  # Basic test with continuous and categorical variables
  result <- .run_multisurvival(
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
  
  result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
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

test_that("multisurvival handles missing data gracefully", {
  # Create data with missing values
  colon <- survival::colon
  test_data <- colon
  test_data$age[1:10] <- NA
  
  expect_warning(
    result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
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
  colon <- survival::colon
  
  result <- .run_multisurvival(
    data = colon,
    elapsedtime = "time",
    outcome = "status",
    outcomeLevel = "1", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = c("sex", "obstruct", "perfor"),
    contexpl = "age",
    ac = TRUE,
    adjexplanatory = "sex",
    ac_method = "average"
  )
  
  # Check adjusted curve outputs
  expect_true(!is.null(result$plot_adj))
})
