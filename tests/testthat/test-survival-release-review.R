# Regression tests for the survival release review (2026-09-02).
#
# Each block corresponds to a defect that was confirmed empirically and fixed:
#   * .r.yaml `visible:` expressions starting with "((" never route through
#     jmvcore's binding parser, so the element is always visible.
#   * the two age-based plots rebuilt their own frame from self$data, so they
#     disagreed with every other output (landmark) or drew nothing at all
#     (multievent, date-derived time).
#   * the age-stratified KM plot went through survminer's facet.by, which is
#     broken with the installed ggplot2 and produced a blank white panel.

library(testthat)

survival_rr_data <- function() as.data.frame(get("survival_test"))

survival_rr_args <- function(...) {
  defaults <- list(
    data = survival_rr_data(),
    elapsedtime = "elapsedtime", outcome = "outcome", outcomeLevel = "1",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "treatment"
  )
  supplied <- list(...)
  defaults[names(supplied)] <- supplied
  defaults
}

# Renders to a temporary PNG and returns its size. A renderer that bails out
# writes no file at all, which is exactly the "blank panel" the user sees.
render_bytes <- function(element) {
  f <- tempfile(fileext = ".png")
  grDevices::png(f, width = 700, height = 500)
  on.exit(if (grDevices::dev.cur() > 1) grDevices::dev.off(), add = TRUE)
  invisible(try(element$.render(width = 700, height = 500, ppi = 72), silent = TRUE))
  grDevices::dev.off()
  sz <- file.info(f)$size
  if (is.na(sz)) 0L else sz
}

test_that("no .r.yaml visible: expression starts with '((' (never routes)", {
  spec <- testthat::test_path("..", "..", "jamovi", "survival.r.yaml")
  skip_if_not(file.exists(spec))
  lines <- readLines(spec, warn = FALSE)
  offenders <- grep("visible:\\s*\\(\\(", lines, value = TRUE)
  expect_identical(offenders, character(0))

  # and the rewritten form really does evaluate, both ways
  o <- jmvcore::Options$new()
  for (nm in c("sc", "ce", "ch", "kmunicate", "loglog"))
    o$.addOption(jmvcore::OptionBool$new(nm, FALSE))
  o$.addOption(jmvcore::OptionBool$new("showExplanations", TRUE))
  expr <- "(showExplanations && (sc || ce || ch || kmunicate || loglog))"
  expect_false(isTRUE(o$eval(expr)))

  o2 <- jmvcore::Options$new()
  o2$.addOption(jmvcore::OptionBool$new("sc", TRUE))
  for (nm in c("ce", "ch", "kmunicate", "loglog"))
    o2$.addOption(jmvcore::OptionBool$new(nm, FALSE))
  o2$.addOption(jmvcore::OptionBool$new("showExplanations", TRUE))
  expect_true(isTRUE(o2$eval(expr)))
})

test_that("age-based plots draw something in every supported time/outcome mode", {
  age_opts <- list(age_adjustment = TRUE, age_variable = "age",
                   age_stratified_km = TRUE, adjusted_curves = TRUE)

  # (a) plain elapsed time
  r <- do.call(ClinicoPath::survival, c(survival_rr_args(), age_opts))
  expect_gt(render_bytes(r$ageStratifiedKMPlot), 10000)
  expect_gt(render_bytes(r$adjustedCurvesPlot), 10000)

  # (b) date-derived time: elapsedtime is NULL, so a renderer reading
  #     self$options$elapsedtime drew nothing at all
  dd <- as.data.frame(get("survival_dates"))
  rt <- do.call(ClinicoPath::survival, c(list(
    data = dd, tint = TRUE, dxdate = "dxdate", fudate = "fudate",
    elapsedtime = NULL, outcome = "outcome", outcomeLevel = "1",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
    explanatory = "treatment"), age_opts))
  expect_gt(render_bytes(rt$ageStratifiedKMPlot), 10000)
  expect_gt(render_bytes(rt$adjustedCurvesPlot), 10000)

  # (c) multievent: outcomeLevel is unset by design, so the old fallback
  #     as.numeric(as.character(outcome)) produced an all-NA indicator
  set.seed(1)
  d2 <- survival_rr_data()
  d2$out4 <- factor(ifelse(
    d2$outcome == 1,
    sample(c("Dead of Disease", "Dead of Other"), nrow(d2), TRUE),
    sample(c("Alive with Disease", "Alive without Disease"), nrow(d2), TRUE)))
  r2 <- do.call(ClinicoPath::survival, c(list(
    data = d2, elapsedtime = "elapsedtime", outcome = "out4",
    outcomeLevel = NULL, multievent = TRUE, analysistype = "cause",
    dod = "Dead of Disease", dooc = "Dead of Other",
    awd = "Alive with Disease", awod = "Alive without Disease",
    explanatory = "treatment"), age_opts))
  expect_gt(render_bytes(r2$ageStratifiedKMPlot), 10000)
  expect_gt(render_bytes(r2$adjustedCurvesPlot), 10000)
})

test_that("age-based plots use the same landmarked time as the tables", {
  age_opts <- list(age_adjustment = TRUE, age_variable = "age",
                   age_stratified_km = TRUE, adjusted_curves = TRUE)
  raw <- survival_rr_data()

  plain <- do.call(ClinicoPath::survival, c(survival_rr_args(), age_opts))
  land  <- do.call(ClinicoPath::survival,
                   c(survival_rr_args(uselandmark = TRUE, landmark = 12), age_opts))

  s_plain <- plain$ageStratifiedKMPlot$state
  s_land  <- land$ageStratifiedKMPlot$state
  expect_false(is.null(s_plain$plot_data))
  expect_false(is.null(s_land$plot_data))

  # landmarking drops everyone censored/failed before month 12 and shifts the
  # clock, so both n and max(time) must fall by the landmark
  expect_lt(nrow(s_land$plot_data), nrow(s_plain$plot_data))
  expect_equal(max(s_land$plot_data$time),
               max(raw$elapsedtime) - 12, tolerance = 1e-8)
  expect_equal(nrow(s_land$plot_data), sum(raw$elapsedtime >= 12))

  # and both plots share one frame, so they cannot disagree with each other
  expect_equal(land$adjustedCurvesPlot$state$plot_data, s_land$plot_data)
})

test_that("age-based plots still refuse under competing risks", {
  d3 <- survival_rr_data()
  set.seed(2)
  d3$cr <- factor(
    ifelse(d3$outcome == 1, sample(c("Event", "Competing"), nrow(d3), TRUE), "Censored"),
    levels = c("Censored", "Event", "Competing"))
  r <- do.call(ClinicoPath::survival, c(list(
    data = d3, elapsedtime = "elapsedtime", outcome = "cr", outcomeLevel = "Event",
    dod = NULL, dooc = NULL, awd = NULL, awod = NULL, explanatory = "treatment"),
    list(age_adjustment = TRUE, age_variable = "age",
         age_stratified_km = TRUE, adjusted_curves = TRUE)))
  # the refusal panel is itself a drawing, so it is not blank -- what matters is
  # that the state carries the flag the renderer refuses on
  expect_true(isTRUE(r$ageStratifiedKMPlot$state$has_competing))
  expect_gt(render_bytes(r$ageStratifiedKMPlot), 5000)
})

test_that("age plots are not populated when their options are off", {
  r <- do.call(ClinicoPath::survival, survival_rr_args())
  expect_null(r$ageStratifiedKMPlot$state)
  expect_equal(render_bytes(r$ageStratifiedKMPlot), 0)
})
