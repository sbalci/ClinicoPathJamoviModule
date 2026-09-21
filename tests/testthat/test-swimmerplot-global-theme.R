# SW-10: .plot() accepted `ggtheme` and never used it, so the swimmer plot kept
# the ggswim look whatever the user had chosen in jamovi's preferences - a light
# figure in a dark document, unlike every other module's plot.
#
# These tests read the BUILT plot, not the source text, and use the real global
# theme (jmvcore:::getGlobalTheme). Passing ggplot2::theme_grey() instead cannot
# see this bug.

theme_bg <- function(p) ggplot2::calc_element("panel.background", p$theme)$fill

# Call the renderer the way the fixed code expects, but tolerate the old
# signature so the failure it reports is the WRONG THEME, not "unused argument".
sw_build <- function(priv, st, theme_name, gt) {
  o <- st$options
  o$theme <- theme_name
  args <- list(st$patient_data, st$milestone_data, st$event_data, st$arrow_data,
               o, st$stats, theme = NULL)
  if ("ggtheme" %in% names(formals(priv$.createGgswimPlot)))
    args$ggtheme <- gt
  suppressMessages(do.call(priv$.createGgswimPlot, args))
}

sw_state <- function(...) {
  d <- data.frame(
    id    = paste0("P", 1:8),
    start = 0,
    end   = c(4, 6, 8, 10, 12, 14, 16, 18),
    resp  = rep(c("CR", "PR", "SD", "PD"), 2),
    stringsAsFactors = FALSE)

  a <- ClinicoPath:::swimmerplotClass$new(
    options = ClinicoPath:::swimmerplotOptions$new(
      patientID = "id", startTime = "start", endTime = "end", responseVar = "resp", ...),
    data = d)
  suppressWarnings(a$run())
  list(analysis = a, state = a$results$plot$state)
}

test_that("the new plotTheme choice makes the swimmer plot follow jamovi's global theme", {
  skip_if_not_installed("ggswim")
  gt <- jmvcore:::getGlobalTheme("default", "jmv")$ggtheme
  s  <- sw_state()
  skip_if(is.null(s$state))
  priv <- s$analysis$.__enclos_env__$private
  st   <- s$state

  build <- function(theme_name) sw_build(priv, st, theme_name, gt)

  # "jamovi" must carry the global theme, not theme_minimal / theme_ggswim
  expect_equal(theme_bg(build("jamovi")), theme_bg(ggplot2::ggplot() + gt))

  # the existing default is untouched: the ggswim look is preserved
  expect_equal(theme_bg(build("ggswim")),
               theme_bg(ggplot2::ggplot() + ggswim::theme_ggswim()))
  expect_false(isTRUE(all.equal(theme_bg(build("jamovi")), theme_bg(build("ggswim")))))
  expect_false(isTRUE(all.equal(theme_bg(build("jamovi")), theme_bg(build("minimal")))))
})

test_that("a named accessibility palette still overrides the global theme's scales", {
  # ggtheme carries discrete fill/colour scales, so it is applied BEFORE the
  # palette scales; High Contrast (Okabe-Ito) must survive that.
  skip_if_not_installed("ggswim")
  gt <- jmvcore:::getGlobalTheme("default", "jmv")$ggtheme
  s  <- sw_state(colorPalette = "contrast")
  skip_if(is.null(s$state))
  priv <- s$analysis$.__enclos_env__$private
  st   <- s$state

  p <- sw_build(priv, st, "jamovi", gt)

  cols <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(d) d$colour))
  expect_true(any(toupper(cols) %in% c("#E69F00", "#56B4E9", "#009E73", "#000000")))
})

test_that("plotTheme offers the jamovi (follow global) choice and keeps its default", {
  a <- yaml::read_yaml("../../jamovi/swimmerplot.a.yaml")
  opt <- Filter(function(o) identical(o$name, "plotTheme"), a$options)[[1]]
  expect_true("jamovi" %in% vapply(opt$options, function(o) o$name, ""))
  expect_equal(opt$default, "ggswim")   # additive change only
})
