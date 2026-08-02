# ═══════════════════════════════════════════════════════════
# Edge Case Tests: multisurvival
# ═══════════════════════════════════════════════════════════
#
# Tests edge cases, error handling, and boundary conditions
# for the multisurvival jamovi function

library(testthat)

# Load test data
data(multisurvival_test, package = "ClinicoPath")
data(multisurvival_small, package = "ClinicoPath")

test_that("multisurvival handles missing values in time variable", {
  test_data_na <- multisurvival_test
  test_data_na$elapsedtime[1:5] <- NA
  
  result <- .run_multisurvival(
    data = test_data_na,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalResults")
  expect_match(as.character(result$infoMessages$content), "excluded from the model")
})

test_that("multisurvival handles missing values in outcome variable", {
  test_data_na <- multisurvival_test
  test_data_na$outcome[1:5] <- NA
  
  result <- .run_multisurvival(
    data = test_data_na,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalResults")
  expect_match(as.character(result$infoMessages$content), "excluded from the model")
})

test_that("multisurvival handles missing values in explanatory variables", {
  test_data_na <- multisurvival_test
  test_data_na$treatment[1:10] <- NA
  test_data_na$age[11:20] <- NA
  
  result <- .run_multisurvival(
    data = test_data_na,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    contexpl = "age"
  )
  
  # Should complete with warning about missing data
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles all censored data", {
  test_data_censored <- multisurvival_test
  test_data_censored$outcome <- 0
  
  result <- .run_multisurvival(
    data = test_data_censored,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_true(result$errors$visible)
  expect_match(as.character(result$errors$content), "No events observed")
})

test_that("multisurvival handles all events (no censoring)", {
  test_data_events <- multisurvival_test
  test_data_events$outcome <- 1
  
  result <- .run_multisurvival(
    data = test_data_events,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  # Should complete but may warn
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles very small sample size", {
  # Single group with minimal events
  small_data <- multisurvival_small[1:10, ]
  
  result <- .run_multisurvival(
    data = small_data,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  # Should complete but may have convergence issues
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles zero survival time", {
  test_data_zero <- multisurvival_test
  test_data_zero$elapsedtime[1:5] <- 0
  
  result <- .run_multisurvival(
    data = test_data_zero,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalResults")
  expect_true(result$warnings$visible)
  expect_match(as.character(result$warnings$content), "Zero survival times")
})

test_that("multisurvival handles negative survival time", {
  test_data_neg <- multisurvival_test
  test_data_neg$elapsedtime[1:5] <- -10
  
  result <- .run_multisurvival(
    data = test_data_neg,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )

  expect_true(result$todo$visible)
  expect_match(as.character(result$todo$content), "Negative survival times")
})

test_that("multisurvival handles extremely long follow-up times", {
  test_data_long <- multisurvival_test
  test_data_long$elapsedtime[1:10] <- 10000  # Very long follow-up
  
  result <- .run_multisurvival(
    data = test_data_long,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles constant explanatory variable", {
  test_data_const <- multisurvival_test
  test_data_const$constant_var <- "Same"
  
  result <- suppressWarnings(.run_multisurvival(
    data = test_data_const,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "constant_var"
  ))

  expect_true(result$todo$visible)
  expect_match(
    as.character(result$todo$content),
    "2 or more levels|appropriate types and variation"
  )
})

test_that("multisurvival handles perfect separation", {
  # Create data where one group has no events
  test_data_sep <- multisurvival_test[1:50, ]
  test_data_sep$outcome <- ifelse(test_data_sep$treatment == "Control", 0, 
                                  test_data_sep$outcome)
  
  result <- suppressWarnings(.run_multisurvival(
    data = test_data_sep,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  ))

  expect_s3_class(result, "multisurvivalResults")
  expect_false(result$todo$visible)
  expect_true(result$warnings$visible)
  expect_match(
    as.character(result$warnings$content),
    "Moderate event count|events per variable"
  )
})

test_that("multisurvival handles single event in group", {
  # One group with only one event
  test_data_one <- multisurvival_small
  test_data_one$outcome <- 0
  test_data_one$outcome[test_data_one$treatment == "A"][1] <- 1
  
  result <- .run_multisurvival(
    data = test_data_one,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  # Should complete but may be unstable
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles duplicate time-event pairs", {
  # Multiple patients with same time and event
  test_data_dup <- multisurvival_test
  test_data_dup$elapsedtime[1:20] <- 12
  test_data_dup$outcome[1:20] <- 1
  
  result <- .run_multisurvival(
    data = test_data_dup,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  # Should handle ties appropriately
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles very early events", {
  # Most events occur very early
  test_data_early <- multisurvival_test
  test_data_early$outcome[test_data_early$elapsedtime < 3] <- 1
  
  result <- .run_multisurvival(
    data = test_data_early,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles rare event outcome", {
  # Very few events
  test_data_rare <- multisurvival_test
  test_data_rare$outcome <- 0
  test_data_rare$outcome[sample(nrow(test_data_rare), 5)] <- 1
  
  result <- .run_multisurvival(
    data = test_data_rare,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles extreme age values", {
  test_data_age <- multisurvival_test
  test_data_age$age <- c(rep(18, 50), rep(95, 150))  # Extreme ages
  
  result <- .run_multisurvival(
    data = test_data_age,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    contexpl = "age"
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles high multicollinearity", {
  # Create highly correlated variables
  test_data_col <- multisurvival_test
  test_data_col$nodes2 <- test_data_col$nodes * 1.1 + rnorm(nrow(test_data_col), 0, 0.1)
  
  # Should warn about collinearity
  result <- .run_multisurvival(
    data = test_data_col,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    contexpl = c("nodes", "nodes2")
  )
  
  expect_s3_class(result, "multisurvivalResults")
})

test_that("multisurvival handles landmark time beyond maximum follow-up", {
  data(multisurvival_landmark, package = "ClinicoPath")
  
  result <- .run_multisurvival(
    data = multisurvival_landmark,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    uselandmark = TRUE,
    landmark = 100,
    explanatory = "treatment"
  )

  expect_true(result$errors$visible)
  expect_match(as.character(result$errors$content), "No events observed|landmark")
})

test_that("multisurvival handles invalid date formats", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  test_data_dates <- multisurvival_dates
  test_data_dates$dxdate[1:5] <- "invalid-date"
  
  result <- .run_multisurvival(
    data = test_data_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    outcome = "outcome",
    outcomeLevel = "Dead",
    explanatory = "treatment"
  )

  expect_s3_class(result, "multisurvivalResults")
  expect_true(result$todo$visible || result$infoMessages$visible)
  expect_match(
    paste(result$todo$content, result$infoMessages$content),
    "(?i)invalid|missing|excluded|date"
  )
})

test_that("multisurvival handles follow-up date before diagnosis date", {
  data(multisurvival_dates, package = "ClinicoPath")
  
  test_data_dates <- multisurvival_dates
  # Swap dates for some patients
  test_data_dates$fudate[1:5] <- "2017-01-01"  # Before diagnosis dates
  
  result <- .run_multisurvival(
    data = test_data_dates,
    tint = TRUE,
    dxdate = "dxdate",
    fudate = "fudate",
    timetypedata = "ymd",
    outcome = "outcome",
    outcomeLevel = "Dead",
    explanatory = "treatment"
  )

  expect_true(result$todo$visible)
  expect_match(as.character(result$todo$content), "Negative Survival Times")
})

test_that("multisurvival handles risk score with insufficient events", {
  # Too few events for stable risk score calculation
  test_data_few <- multisurvival_small
  test_data_few$outcome <- 0
  test_data_few$outcome[1:3] <- 1  # Only 3 events
  
  result <- .run_multisurvival(
    data = test_data_few,
    elapsedtime = "elapsedtime",
    outcome = "outcome",
    explanatory = "treatment",
    calculateRiskScore = TRUE,
    numRiskGroups = "four"  # Trying to create 4 groups with only 3 events
  )
  
  # May warn or have unstable results
  expect_s3_class(result, "multisurvivalResults")
})
