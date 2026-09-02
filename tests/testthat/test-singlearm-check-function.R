# Regression tests from /check-function singlearm (2026-09-02).
#
#   * KMunicate::KMunicate(.theme = ) asserts a bare ggplot2 theme, but jamovi
#     hands a renderFun a LIST wrapping one. The assertion failed on every
#     render, so the KMunicate option produced a blank white panel in every
#     configuration.
#   * ggsurvplot's scale replacement and KMunicate's "Ignoring unknown labels"
#     leaked into jamovi's undifferentiated "Analysis Notes" tray on each run.

library(testthat)

singlearm_cf_args <- function(...) {
  defaults <- list(
    data = as.data.frame(get("singlearm_test")),
    elapsedtime = "time_months", outcome = "outcome", outcomeLevel = "Dead",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL
  )
  supplied <- list(...)
  defaults[names(supplied)] <- supplied
  defaults
}

render_bytes <- function(element) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 700, height = 500)
  on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)
  invisible(try(element$.render(width = 700, height = 500, ppi = 72), silent = TRUE))
  grDevices::dev.off()
  sz <- file.info(f)$size
  if (is.na(sz)) 0L else sz
}

test_that("every enabled plot draws something (KMunicate included)", {
  r <- do.call(ClinicoPath::singlearm, singlearm_cf_args(
    sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE,
    baseline_hazard = TRUE, hazard_smoothing = TRUE))
  for (el in c("plot", "plot2", "plot3", "plot6",
               "baselineHazardPlot", "smoothedHazardPlot")) {
    expect_gt(render_bytes(r[[el]]), 5000)
  }
})

test_that("KMunicate renders with the risk table and CI variants too", {
  r <- do.call(ClinicoPath::singlearm, singlearm_cf_args(
    kmunicate = TRUE, risktable = TRUE, ci95 = TRUE))
  expect_gt(render_bytes(r$plot6), 5000)
})

test_that("rendering leaks no messages or warnings into Analysis Notes", {
  r <- do.call(ClinicoPath::singlearm, singlearm_cf_args(
    sc = TRUE, ce = TRUE, ch = TRUE, kmunicate = TRUE, risktable = TRUE,
    ci95 = TRUE, baseline_hazard = TRUE, hazard_smoothing = TRUE))
  noise <- character(0)
  for (el in c("plot", "plot2", "plot3", "plot6",
               "baselineHazardPlot", "smoothedHazardPlot")) {
    grDevices::png(tempfile(fileext = ".png"), width = 700, height = 500)
    withCallingHandlers(
      invisible(try(r[[el]]$.render(width = 700, height = 500, ppi = 72), silent = TRUE)),
      warning = function(w) noise <<- c(noise, conditionMessage(w)),
      message = function(m) noise <<- c(noise, conditionMessage(m)))
    grDevices::dev.off()
  }
  expect_identical(noise, character(0))
})

test_that("KMunicate plot still refuses under competing risks", {
  d <- as.data.frame(get("singlearm_compete"))
  r <- ClinicoPath::singlearm(
    data = d, elapsedtime = "time_months", outcome = "outcome",
    outcomeLevel = NULL, multievent = TRUE, analysistype = "compete",
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED",
    sc = TRUE, kmunicate = TRUE)
  # the refusal panel is a drawing, but the KM curve itself must be hidden in
  # favour of the cumulative-incidence plot
  expect_false(isTRUE(r$plot$visible))
  expect_true(isTRUE(r$plot_cif$visible))
})

# --- declarative visibility (the .init() hide/re-show block was removed) -----

test_that("option-driven visibility is unchanged without the .init() block", {
  vis <- function(r, e) isTRUE(r[[e]]$visible)

  off <- do.call(ClinicoPath::singlearm, singlearm_cf_args())
  for (e in c("medianSurvivalExplanation", "personTimeTable", "baselineHazardTable",
              "dataQualityTable", "plot", "plot_cif", "plot6", "medianSummary"))
    expect_false(vis(off, e))

  expect_true(vis(do.call(ClinicoPath::singlearm,
    singlearm_cf_args(showExplanations = TRUE)), "medianSurvivalExplanation"))
  expect_true(vis(do.call(ClinicoPath::singlearm,
    singlearm_cf_args(showSummaries = TRUE)), "medianSummary"))
  expect_true(vis(do.call(ClinicoPath::singlearm,
    singlearm_cf_args(person_time = TRUE)), "personTimeTable"))
  expect_true(vis(do.call(ClinicoPath::singlearm,
    singlearm_cf_args(baseline_hazard = TRUE)), "baselineHazardTable"))
  expect_true(vis(do.call(ClinicoPath::singlearm,
    singlearm_cf_args(advancedDiagnostics = TRUE)), "dataQualityTable"))

  plots <- do.call(ClinicoPath::singlearm, singlearm_cf_args(sc = TRUE, kmunicate = TRUE))
  expect_true(vis(plots, "plot"))
  expect_true(vis(plots, "plot6"))
  expect_false(vis(plots, "plot_cif"))
})

test_that("section headings are gated on the required inputs, not on .init()", {
  full <- do.call(ClinicoPath::singlearm, singlearm_cf_args())
  expect_true(isTRUE(full$medianHeading$visible))
  expect_true(isTRUE(full$survTableHeading$visible))
  expect_false(isTRUE(full$todo$visible))

  # outcome chosen but no time source: welcome panel, no empty section titles
  partial <- do.call(ClinicoPath::singlearm, singlearm_cf_args(elapsedtime = NULL))
  expect_false(isTRUE(partial$medianHeading$visible))
  expect_false(isTRUE(partial$survTableHeading$visible))
  expect_true(isTRUE(partial$todo$visible))
})

test_that("competing risks still switches KM for CIF and suppresses the hazard block", {
  d <- as.data.frame(get("singlearm_compete"))
  r <- ClinicoPath::singlearm(
    data = d, elapsedtime = "time_months", outcome = "outcome",
    outcomeLevel = NULL, multievent = TRUE, analysistype = "compete",
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED",
    sc = TRUE, baseline_hazard = TRUE)
  expect_false(isTRUE(r$plot$visible))
  expect_true(isTRUE(r$plot_cif$visible))
  expect_false(isTRUE(r$baselineHazardTable$visible))
})

test_that("notice panels are theme-safe (translucent tint, inherited foreground)", {
  for (type in c("ERROR", "STRONG_WARNING", "WARNING", "INFO")) {
    html <- ClinicoPath:::.singlearmNoticeHTML("a message", type)
    expect_match(html, "background-color: rgba\\(", fixed = FALSE)
    expect_no_match(html, "background-color: #")
    # every text-bearing element inherits the theme foreground
    expect_equal(length(gregexpr("color: inherit", html)[[1]]), 2L)
    expect_no_match(html, "color: #[0-9a-fA-F]{6};'>")
  }
})
