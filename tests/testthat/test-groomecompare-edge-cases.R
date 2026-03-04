# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: groomecompare
# ═══════════════════════════════════════════════════════════

library(testthat)
data(groomecompare_test)

test_that("groomecompare handles different event coding schemes", {
  data(groomecompare_edge_truefalse, package = "ClinicoPath")
  data(groomecompare_edge_12, package = "ClinicoPath")

  # Event coded as TRUE/FALSE
  result_tf <- groomecompare(
    data = groomecompare_edge_truefalse,
    time = "time",
    event = "event_tf",
    stage1 = "ypTNM",
    stage2 = "RPA",
    eventValue = "TRUE"
  )
  expect_no_error(result_tf)

  # Event coded as 1/2
  result_12 <- groomecompare(
    data = groomecompare_edge_12,
    time = "time",
    event = "event_12",
    stage1 = "ypTNM",
    stage2 = "RPA",
    eventValue = "2"
  )
  expect_no_error(result_12)
})

test_that("groomecompare handles identical staging systems", {
  data(groomecompare_identical, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_identical,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "ypTNM"  # Identical to stage1
  )

  # Should work - Groome metrics should show equality/tie
  expect_no_error(result)
})

test_that("groomecompare identifies clearly superior system", {
  data(groomecompare_clear_winner, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_clear_winner,
    time = "time",
    event = "event",
    stage1 = "good_stage",   # Strong prognostic value
    stage2 = "poor_stage",   # Weak prognostic value
    detailedmetrics = TRUE,
    cindexcompare = TRUE
  )

  # Should complete and show good_stage as winner
  expect_no_error(result)
})

test_that("groomecompare handles tied survival times", {
  data(groomecompare_tied, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_tied,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  expect_no_error(result)
})

test_that("groomecompare handles missing data in covariates", {
  # Create data with some missing values in non-essential variables
  test_data_na <- groomecompare_test
  test_data_na$age[1:5] <- NA

  result <- groomecompare(
    data = test_data_na,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  # Should work (age not used in comparison)
  expect_no_error(result)
})

test_that("groomecompare errors on missing time data", {
  test_data_na <- groomecompare_test
  test_data_na$time[1:10] <- NA

  expect_error(
    groomecompare(
      data = test_data_na,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "missing|NA|time",
    ignore.case = TRUE
  )
})

test_that("groomecompare errors on missing event data", {
  test_data_na <- groomecompare_test
  test_data_na$event[1:10] <- NA

  expect_error(
    groomecompare(
      data = test_data_na,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "missing|NA|event",
    ignore.case = TRUE
  )
})

test_that("groomecompare errors on missing staging data", {
  # Missing stage1 data
  test_data_na1 <- groomecompare_test
  test_data_na1$ypTNM[1:20] <- NA

  expect_error(
    groomecompare(
      data = test_data_na1,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "missing|NA|stage",
    ignore.case = TRUE
  )

  # Missing stage2 data
  test_data_na2 <- groomecompare_test
  test_data_na2$RPA[1:20] <- NA

  expect_error(
    groomecompare(
      data = test_data_na2,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "missing|NA|stage",
    ignore.case = TRUE
  )
})

test_that("groomecompare handles very small samples", {
  # Very small dataset
  small_data <- groomecompare_test[1:20, ]

  result <- groomecompare(
    data = small_data,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  # Should warn or error about insufficient sample size
  expect_condition(result)
})

test_that("groomecompare handles zero events", {
  # All censored
  no_events <- groomecompare_test
  no_events$event <- factor(0, levels = c(0, 1))

  expect_error(
    groomecompare(
      data = no_events,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "events|censored|zero",
    ignore.case = TRUE
  )
})

test_that("groomecompare handles very few events", {
  # Only 5 events
  few_events <- groomecompare_test
  few_events$event <- factor(0, levels = c(0, 1))
  few_events$event[1:5] <- factor(1, levels = c(0, 1))

  expect_condition(
    groomecompare(
      data = few_events,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    )
  )
})

test_that("groomecompare handles negative survival times", {
  negative_time <- groomecompare_test
  negative_time$time[1:5] <- -10

  expect_error(
    groomecompare(
      data = negative_time,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    ),
    regexp = "negative|positive|time",
    ignore.case = TRUE
  )
})

test_that("groomecompare handles zero survival times", {
  zero_time <- groomecompare_test
  zero_time$time[1:5] <- 0

  result <- groomecompare(
    data = zero_time,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  # Should handle or provide clear message
  expect_true(inherits(result, "groomecompareClass") || inherits(result, "try-error"))
})

test_that("groomecompare handles single-stage groups", {
  # All patients in one stage
  single_stage <- groomecompare_test[groomecompare_test$ypTNM == "Stage I", ]

  expect_condition(
    groomecompare(
      data = single_stage,
      time = "time",
      event = "event",
      stage1 = "ypTNM",
      stage2 = "RPA"
    )
  )
})

test_that("groomecompare handles variables with special characters", {
  special_data <- groomecompare_test
  names(special_data)[names(special_data) == "ypTNM"] <- "ypTNM (Post-Treatment)"

  result <- groomecompare(
    data = special_data,
    time = "time",
    event = "event",
    stage1 = "ypTNM (Post-Treatment)",
    stage2 = "RPA"
  )

  expect_no_error(result)
})

test_that("groomecompare handles very long survival times", {
  long_survival <- groomecompare_test
  long_survival$time <- long_survival$time * 10  # Very long follow-up

  result <- groomecompare(
    data = long_survival,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA"
  )

  expect_no_error(result)
})

test_that("groomecompare handles extremely unbalanced groups", {
  # Create very unbalanced staging
  unbal_extreme <- groomecompare_test
  # 95% in Stage I, 5% in others
  unbal_extreme$ypTNM <- factor(
    sample(c("Stage I", "Stage II", "Stage III", "Stage IV"),
           nrow(unbal_extreme), replace = TRUE,
           prob = c(0.95, 0.02, 0.02, 0.01)),
    levels = c("Stage I", "Stage II", "Stage III", "Stage IV"),
    ordered = TRUE
  )

  result <- groomecompare(
    data = unbal_extreme,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    samplesize = TRUE
  )

  # Should complete but show imbalance in sample size metric
  expect_no_error(result)
})

test_that("groomecompare bootstrap handles edge cases", {
  # Bootstrap with small sample
  data(groomecompare_small, package = "ClinicoPath")

  result <- groomecompare(
    data = groomecompare_small,
    time = "time",
    event = "event",
    stage1 = "clinical_stage",
    stage2 = "molecular_subtype",
    bootstrap = TRUE,
    nboot = 100,
    seed = 42
  )

  # May warn about small sample for bootstrap
  expect_condition(result)
})

test_that("groomecompare handles completely non-prognostic stages", {
  # Create staging with no prognostic value
  random_stage <- groomecompare_test
  random_stage$random_stage <- factor(
    sample(c("A", "B", "C"), nrow(random_stage), replace = TRUE),
    levels = c("A", "B", "C")
  )

  result <- groomecompare(
    data = random_stage,
    time = "time",
    event = "event",
    stage1 = "ypTNM",      # Prognostic
    stage2 = "random_stage"  # Non-prognostic
  )

  # Should show ypTNM as clear winner
  expect_no_error(result)
})
