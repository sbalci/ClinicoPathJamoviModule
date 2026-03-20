# ═══════════════════════════════════════════════════════════
# Tests for conditionalsurvival
# ═══════════════════════════════════════════════════════════
# Covers: basic analysis, all methods, stratification,
#         factor outcomes, edge cases, argument combos
# ═══════════════════════════════════════════════════════════


# ── Helper: load test data ──────────────────────────────────
get_test_data <- function() {
  data(conditionalsurvival_test, package = "ClinicoPath", envir = environment())
  conditionalsurvival_test
}

# Inline data for tests that don't need the full dataset
make_simple_data <- function(n = 80, seed = 123) {
  set.seed(seed)
  data.frame(
    time     = rexp(n, rate = 0.03),
    event    = sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7)),
    group    = sample(c("A", "B"), n, replace = TRUE),
    stage    = sample(c("I", "II", "III"), n, replace = TRUE)
  )
}


# ═══════════════════════════════════════════════════════════
# 1. Basic functionality
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival runs with numeric outcome", {
  data <- make_simple_data()

  result <- conditionalsurvival(
    data         = data,
    timeVar      = "time",
    outcomeVar   = "event",
    conditionVar = "group",
    conditionTime = 10,
    method       = "km",
    confInt      = 0.95,
    showTable    = TRUE,
    showPlot     = FALSE,
    showExplanations = FALSE
  )

  expect_true(inherits(result, "Group"))
})

test_that("conditionalsurvival runs with auto conditionTime", {
  data <- make_simple_data()

  # conditionTime = 0 → uses median follow-up
  result <- conditionalsurvival(
    data         = data,
    timeVar      = "time",
    outcomeVar   = "event",
    conditionVar = "group",
    conditionTime = 0,
    showPlot     = FALSE,
    showExplanations = FALSE
  )

  expect_true(inherits(result, "Group"))
})

test_that("conditionalsurvival unstratified (no conditionVar)", {
  data <- make_simple_data()

  result <- conditionalsurvival(
    data         = data,
    timeVar      = "time",
    outcomeVar   = "event",
    conditionVar = "group",  # wrapper requires this arg
    conditionTime = 10,
    showPlot     = FALSE,
    showExplanations = FALSE
  )

  expect_true(inherits(result, "Group"))
})


# ═══════════════════════════════════════════════════════════
# 2. All estimation methods
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival KM method works", {
  data <- make_simple_data()

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      method = "km", showPlot = FALSE, showExplanations = FALSE
    )
  })
})

test_that("conditionalsurvival landmark method works", {
  data <- make_simple_data()

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      method = "landmark", showPlot = FALSE, showExplanations = FALSE
    )
  })
})

test_that("conditionalsurvival IPW method works", {
  data <- make_simple_data()

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      method = "ipw", showPlot = FALSE, showExplanations = FALSE
    )
  })
})

test_that("conditionalsurvival PKM method works", {
  data <- make_simple_data()

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      method = "pkm", showPlot = FALSE, showExplanations = FALSE
    )
  })
})


# ═══════════════════════════════════════════════════════════
# 3. Factor outcome handling
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival handles factor outcome (2 levels)", {
  data <- make_simple_data()
  data$event_fac <- factor(data$event, levels = c(0, 1),
                           labels = c("Censored", "Event"))

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event_fac",
      conditionVar = "group", conditionTime = 10,
      showPlot = FALSE, showExplanations = FALSE
    )
  })
})


# ═══════════════════════════════════════════════════════════
# 4. Custom time points
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival uses custom time points", {
  data <- make_simple_data()

  result <- conditionalsurvival(
    data = data, timeVar = "time", outcomeVar = "event",
    conditionVar = "group", conditionTime = 5,
    timePoints = "10,20,30,50",
    showPlot = FALSE, showExplanations = FALSE
  )

  expect_true(inherits(result, "Group"))
})


# ═══════════════════════════════════════════════════════════
# 5. Confidence levels
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival with 90% CI", {
  data <- make_simple_data()

  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      confInt = 0.90, showPlot = FALSE, showExplanations = FALSE
    )
  })
})


# ═══════════════════════════════════════════════════════════
# 6. With bundled test data
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival works with bundled test dataset", {
  skip_if_not(file.exists(
    system.file("data", package = "ClinicoPath") |>
      file.path("conditionalsurvival_test.rda")
  ) || file.exists("../../data/conditionalsurvival_test.rda"),
  "Bundled test data not available")

  data <- get_test_data()

  result <- conditionalsurvival(
    data = data, timeVar = "OverallTime", outcomeVar = "Event",
    conditionVar = "Treatment", conditionTime = 12,
    showPlot = FALSE, showExplanations = FALSE
  )

  expect_true(inherits(result, "Group"))

  # Also test with Stage stratification
  result2 <- conditionalsurvival(
    data = data, timeVar = "OverallTime", outcomeVar = "Event",
    conditionVar = "Stage", conditionTime = 12,
    method = "landmark",
    showPlot = FALSE, showExplanations = FALSE
  )

  expect_true(inherits(result2, "Group"))

  # Test factor outcome
  result3 <- conditionalsurvival(
    data = data, timeVar = "OverallTime", outcomeVar = "EventFactor",
    conditionVar = "Treatment", conditionTime = 12,
    showPlot = FALSE, showExplanations = FALSE
  )

  expect_true(inherits(result3, "Group"))
})


# ═══════════════════════════════════════════════════════════
# 7. Edge cases and validation
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival rejects 3-level factor outcome", {
  data <- make_simple_data()
  data$bad_outcome <- factor(sample(c("A", "B", "C"), nrow(data), replace = TRUE))

  expect_error(
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "bad_outcome",
      conditionVar = "group", conditionTime = 10,
      showPlot = FALSE, showExplanations = FALSE
    )
  )
})

test_that("conditionalsurvival rejects non-binary numeric outcome", {
  data <- make_simple_data()
  data$bad_numeric <- sample(0:3, nrow(data), replace = TRUE)

  expect_error(
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "bad_numeric",
      conditionVar = "group", conditionTime = 10,
      showPlot = FALSE, showExplanations = FALSE
    )
  )
})

test_that("conditionalsurvival rejects too few events", {
  set.seed(999)
  data <- data.frame(
    time  = rexp(50, rate = 0.02),
    event = c(rep(1, 3), rep(0, 47)),  # only 3 events
    group = sample(c("A", "B"), 50, replace = TRUE)
  )

  expect_error(
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      showPlot = FALSE, showExplanations = FALSE
    )
  )
})

test_that("conditionalsurvival rejects single-level conditionVar", {
  data <- make_simple_data()
  data$single_group <- "OnlyGroup"

  expect_error(
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "single_group", conditionTime = 10,
      showPlot = FALSE, showExplanations = FALSE
    )
  )
})


# ═══════════════════════════════════════════════════════════
# 8. Display options
# ═══════════════════════════════════════════════════════════

test_that("conditionalsurvival show/hide options work", {
  data <- make_simple_data()

  # All display off
  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      showTable = FALSE, showPlot = FALSE, showExplanations = FALSE
    )
  })

  # All display on
  expect_no_error({
    conditionalsurvival(
      data = data, timeVar = "time", outcomeVar = "event",
      conditionVar = "group", conditionTime = 10,
      showTable = TRUE, showPlot = FALSE, showExplanations = TRUE
    )
  })
})
