# DM-11 regression: plot colours and the global jamovi palette
#
# All three diagnosticmeta renderers declared `theme` and none read it, so a
# forest plot dropped into a document alongside any other ClinicoPath plot
# carried a second, unrelated colour system. The new "jamovi (follow global)"
# choice consults theme$palette; the accessibility choices must NOT.
#
# The assertions read the BUILT plot (ggplot_build), not the source text, and
# render with a real global theme - jmvcore:::getGlobalTheme() - because
# ggplot2::theme_grey() cannot see any of this.

dm_studies <- function() data.frame(
  study = paste0("S", 1:10),
  tp = c(40, 90, 20, 60, 15, 55, 33, 71, 25, 48),
  fn = c(10, 10, 30, 40,  5, 15, 17,  9, 25, 12),
  fp = c(10, 10, 30,  5, 40, 20, 12, 25, 18,  9),
  tn = c(40, 90, 20, 95, 10, 60, 48, 55, 42, 71),
  stringsAsFactors = FALSE)

# Renders the forest plot through the private renderer under the given jamovi
# palette name and returns the point colour ggplot2 actually drew.
dm_forest_colour <- function(color_palette, jmv_palette) {
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn",
    forest_plot = TRUE, color_palette = color_palette)
  a <- ClinicoPath:::diagnosticmetaClass$new(options = o, data = dm_studies())
  a$run()

  gt <- jmvcore:::getGlobalTheme("default", jmv_palette)
  tmp <- tempfile(fileext = ".png")
  grDevices::png(tmp); on.exit({grDevices::dev.off(); unlink(tmp)}, add = TRUE)
  drew <- a$.__enclos_env__$private$.forestplot(
    a$results$forestplot, ggtheme = gt$ggtheme, theme = gt$theme)
  expect_true(isTRUE(drew))

  b <- ggplot2::ggplot_build(ggplot2::last_plot())
  unique(b$data[[1]]$colour)[1]
}

dm_palette_levels <- function() {
  o <- ClinicoPath:::diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_positives = "fp",
    false_negatives = "fn", true_negatives = "tn")
  unlist(o$.__enclos_env__$private$..color_palette$.__enclos_env__$private$.options)
}

test_that("the jamovi palette choice follows the document theme", {
  skip_if_not_installed("mada")
  # Genuinely blocked until the header is regenerated: OptionList STORES an uncompiled level at
  # $new(), but run() -> init() -> options$check(checkValues = TRUE) rejects it. The direct
  # accessor test below covers the same branch meanwhile, so the guard is never fully dormant.
  skip_if_not("jamovi" %in% dm_palette_levels(),
    "color_palette level 'jamovi' not compiled yet - run jmvtools::prepare()")

  hadley <- dm_forest_colour("jamovi", "hadley")
  jmv    <- dm_forest_colour("jamovi", "jmv")

  expect_equal(hadley, jmvcore::colorPalette(4, "hadley", "color")[1])
  expect_equal(jmv,    jmvcore::colorPalette(4, "jmv", "color")[1])
  # and the two themes must actually differ, otherwise the test proves nothing
  expect_false(identical(hadley, jmv))
})

test_that("an accessibility palette is never overridden by the document theme", {
  skip_if_not_installed("mada")
  # colorblind_safe is a deliberate override: same colour under every theme
  expect_equal(dm_forest_colour("colorblind_safe", "hadley"), "#0173B2")
  expect_equal(dm_forest_colour("colorblind_safe", "jmv"),    "#0173B2")
  expect_equal(dm_forest_colour("high_contrast",   "hadley"), "#000000")
})

# The end-to-end test above cannot run until jmvtools::prepare() regenerates the header, because
# options$check() rejects an uncompiled level. This one exercises the SAME branch of
# .getColorPalette() directly -- constructing the options object stores the level fine, and the
# accessor never calls check() -- so DM-11 is guarded from the moment the fix lands.
test_that(".getColorPalette resolves the jamovi choice from theme$palette", {
  mk <- function(palette) {
    o <- ClinicoPath:::diagnosticmetaOptions$new(
      study = "study", true_positives = "tp", false_positives = "fp",
      false_negatives = "fn", true_negatives = "tn", color_palette = palette)
    ClinicoPath:::diagnosticmetaClass$new(options = o, data = dm_studies())
  }
  get_pal <- function(a, jmv_palette) {
    gt <- jmvcore:::getGlobalTheme("default", jmv_palette)
    a$.__enclos_env__$private$.getColorPalette(gt$theme)
  }

  jam <- mk("jamovi")
  hadley <- get_pal(jam, "hadley")
  jmv    <- get_pal(jam, "jmv")

  expect_identical(hadley$primary, jmvcore::colorPalette(4, "hadley", "color")[1])
  expect_identical(jmv$primary,    jmvcore::colorPalette(4, "jmv", "color")[1])
  # the two document themes must actually differ, or the assertion proves nothing
  expect_false(identical(hadley$primary, jmv$primary))
  expect_setequal(names(hadley), c("primary", "secondary", "tertiary", "study_points"))

  # accessibility palettes are deliberate overrides: identical under every document theme
  cb <- mk("colorblind_safe")
  expect_identical(get_pal(cb, "hadley")$primary, "#0173B2")
  expect_identical(get_pal(cb, "jmv")$primary,    "#0173B2")
  # and the pre-existing default is untouched by the theme
  std <- mk("standard")
  expect_identical(get_pal(std, "hadley")$primary, "darkblue")
})
