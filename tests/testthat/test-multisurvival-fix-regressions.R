library(testthat)

# Regressions for the 2026-09-02 /fix-function pass on multisurvival. Each test
# targets a defect the existing suite could not see: its synthetic columns are
# lowercase, every model has two or more predictors, and the competing-risk
# risk-group plot was never rendered.

msfx_df <- function(n = 160, seed = 2026) {
  set.seed(seed)
  data.frame(
    time = round(stats::rexp(n, 0.05) + 0.5, 2),
    status = stats::rbinom(n, 1, 0.65),
    # Capitalised on purpose: janitor cleans it to `sex` inside the analysis.
    Sex = factor(sample(c("Female", "Male"), n, TRUE)),
    Age = round(stats::rnorm(n, 60, 10), 1),
    cause = factor(sample(c("AWOD", "AWD", "DOD", "DOOC"), n, TRUE,
                          prob = c(0.35, 0.15, 0.30, 0.20)))
  )
}

render_ok <- function(image) {
  grDevices::png(tempfile(fileext = ".png"))
  on.exit(grDevices::dev.off(), add = TRUE)
  isTRUE(image$.render())
}

test_that("person-time group rows index the cleaned column, not the option label", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex", person_time = TRUE)
  tab <- as.data.frame(res$personTimeTable)
  grp <- tab[grepl("^Sex: ", tab$interval), , drop = FALSE]
  expect_equal(nrow(grp), 2L)

  ref <- stats::aggregate(cbind(events = status, pt = time) ~ Sex, d, sum)
  for (i in seq_len(nrow(ref))) {
    row <- grp[grp$interval == paste0("Sex: ", ref$Sex[i]), , drop = FALSE]
    expect_equal(row$events, ref$events[i])
    expect_equal(row$person_time, round(ref$pt[i], 2))
  }
})

test_that("a single-predictor nomogram is drawn and its scoring guide is not blank", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            contexpl = "Age", showNomogram = TRUE, cutp = "6, 12")
  guide <- paste(as.character(res$nomogram_display$content), collapse = "")
  expect_gt(nchar(guide), 100)
  expect_true(render_ok(res$plot_nomogram))
})

test_that("the competing-risk risk-group plot renders instead of crashing", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "cause",
                            multievent = TRUE, analysistype = "compete",
                            dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                            explanatory = "Sex", contexpl = "Age",
                            calculateRiskScore = TRUE, plotRiskGroups = TRUE,
                            numRiskGroups = "two")
  expect_true(render_ok(res$riskGroupPlot))
})

test_that("model performance metrics carry no fixed-cut-off verdict words", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex", contexpl = "Age",
                            show_survmetrics = TRUE, survmetrics_timepoints = "6, 12")
  tab <- as.data.frame(res$survMetricsTable)
  expect_gt(nrow(tab), 1)
  expect_false(any(grepl("Good|Acceptable|Limited|Excellent|Fair|Poor",
                         tab$interpretation)))
})

test_that("the Cox table header names the user's variables, not mytime/myoutcome", {
  d <- msfx_df()
  res <- .run_multisurvival(data = d, elapsedtime = "time", outcome = "status",
                            explanatory = "Sex")
  txt <- paste(as.character(res$text$content), collapse = " ")
  expect_false(grepl("Surv(mytime, myoutcome)", txt, fixed = TRUE))
  expect_true(grepl("Surv(time, status)", txt, fixed = TRUE))
})
