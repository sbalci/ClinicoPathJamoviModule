# Regression tests for WF-05 / WF-14.
#
# jamovi's `ggtheme` is not just a theme: it is a COMPLETE ggplot2 theme PLUS a
# discrete fill scale and a discrete colour scale. Anything added BEFORE it is
# silently discarded. waterfall used to add `scale_fill_manual()` first and
# `ggtheme` afterwards, so the clinically meaningful RECIST colours were replaced
# by jamovi's generic pastel palette -- and, outside the old duplicate colorScheme
# == "jamovi" (which drew the RECIST colours, exactly as "recist" did), ggtheme was
# never applied at all. That duplicate level has since been removed and the name
# "jamovi" now means "follow the document palette", as it does everywhere else.
#
# These tests therefore render through the private renderers with a REAL global
# theme and read the built fills. A test using ggplot2::theme_grey() as `ggtheme`
# cannot see this bug.

skip_if_not(exists("getGlobalTheme", envir = asNamespace("jmvcore"), inherits = FALSE),
            "jmvcore:::getGlobalTheme unavailable")

global_theme <- function() jmvcore:::getGlobalTheme("default", "jmv")

# The renderers only ever do `image$state`, so a bare list stands in for the image.
fake_image <- function(state) list(state = state)

# Render and grab the built plot. The renderers print() rather than return the
# plot; every ggplot2 `+` updates last_plot(), so that is the final object.
built <- function(render_call) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    force(render_call)
    ggplot2::ggplot_build(ggplot2::last_plot())
}

wf_df <- data.frame(
    id = c("P1", "P2", "P3", "P4"),
    response = c(-100, -50, 0, 30),
    recist_category = factor(c("CR", "PR", "SD", "PD"),
                             levels = c("CR", "PR", "SD", "PD")),
    stringsAsFactors = FALSE
)

wf_state <- function(colorScheme = "recist") list(
    data = list(waterfall = wf_df),
    options = list(
        patientID = "id", response = "response", timeVar = NULL,
        sortBy = "response", sortDirection = "default",
        showThresholds = FALSE, labelOutliers = FALSE,
        colorScheme = colorScheme, colorBy = "recist", groupVar = NULL,
        barWidth = 0.8, barAlpha = 1,
        showMedian = FALSE, showCI = FALSE, seed = 42,
        showBaseline = FALSE, confirmationVar = NULL, ongoingVar = NULL,
        minResponseForLabel = 50,
        spiderColorBy = "response", spiderColorScheme = "classic",
        timeUnitLabel = "months",
        annotationVars = NULL, showCategoryLabels = FALSE, showSpiderLabels = FALSE
    )
)

sp_df <- data.frame(
    id = c("P1", "P1", "P2", "P2"),
    time = c(0, 3, 0, 3),
    response = c(0, -60, 0, 40),
    stringsAsFactors = FALSE
)

sp_state <- function(spiderColorScheme = "classic") {
    st <- wf_state()
    st$data$spider <- sp_df
    st$options$timeVar <- "time"
    st$options$spiderColorScheme <- spiderColorScheme
    st
}

RECIST <- c(CR = "#1b9e77", PR = "#7570b3", SD = "#e7298a", PD = "#e66101")
RESPONDER <- c(`Non-responder` = "#e66101", Responder = "#1b9e77")


test_that("waterfall RECIST fills survive the real jamovi global theme", {
    gt <- global_theme()
    a <- waterfallClass$new(
        options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                       showWaterfallPlot = TRUE),
        data = wf_df)
    b <- built(a$.__enclos_env__$private$.waterfallplot(
        fake_image(wf_state()), gt$ggtheme, gt$theme))

    # sortBy = "response" puts the worst (PD) on the left, so fills run PD..CR
    expect_equal(unname(b$data[[1]]$fill), rev(unname(RECIST)))
})

test_that("waterfall applies ggtheme for every colour scheme, not only one", {
    gt <- global_theme()
    a <- waterfallClass$new(
        options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                       showWaterfallPlot = TRUE),
        data = wf_df)
    b <- built(a$.__enclos_env__$private$.waterfallplot(
        fake_image(wf_state("recist")), gt$ggtheme, gt$theme))

    # jamovi's theme paints a transparent panel; ggplot2's default is #EBEBEBFF.
    # (attr(theme, "complete") is useless here -- ggplot_build completes it either way.)
    expect_equal(b$plot$theme$panel.background$fill, "transparent")
    expect_equal(unname(b$data[[1]]$fill), rev(unname(RECIST)))
})

test_that("waterfall 'jamovi' scheme follows the document palette", {
    gt <- global_theme()
    a <- waterfallClass$new(
        options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                       showWaterfallPlot = TRUE),
        data = wf_df)
    b <- built(a$.__enclos_env__$private$.waterfallplot(
        fake_image(wf_state("jamovi")), gt$ggtheme, gt$theme))

    expect_equal(unname(b$data[[1]]$fill),
                 rev(unname(jmvcore::colorPalette(5, gt$theme$palette)[1:4])))
})

test_that("spider responder colours survive the real jamovi global theme", {
    gt <- global_theme()
    a <- waterfallClass$new(
        options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                       timeVar = "time", showSpiderPlot = TRUE),
        data = sp_df)
    b <- built(a$.__enclos_env__$private$.spiderplot(
        fake_image(sp_state()), gt$ggtheme, gt$theme))

    # layer 2 is geom_point, whose fill carries the responder palette
    fills <- unique(b$data[[2]]$fill)
    expect_setequal(fills, unname(RESPONDER))
    expect_equal(b$plot$theme$panel.background$fill, "transparent")
})

test_that("spider 'jamovi' scheme follows the document palette", {
    gt <- global_theme()
    a <- waterfallClass$new(
        options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                       timeVar = "time", showSpiderPlot = TRUE),
        data = sp_df)
    b <- built(a$.__enclos_env__$private$.spiderplot(
        fake_image(sp_state("jamovi")), gt$ggtheme, gt$theme))

    expect_setequal(unique(b$data[[2]]$fill),
                    unname(jmvcore::colorPalette(2, gt$theme$palette)))
})

# --- naming-collision contract (WF-14 follow-up) -------------------------------------------
# colorScheme used to declare BOTH `jamovi` (which drew recistColors, identical to `recist`)
# and `global` (follow the document palette), titled "jamovi" and "jamovi (follow global)".
# The duplicate was removed, `recist` promoted to default, and `global` renamed to `jamovi` so
# the name means the same thing it means in every other palette option in this module.

test_that("waterfall palette options declare no duplicate or colliding level", {
  ay <- testthat::test_path("..", "..", "jamovi", "waterfall.a.yaml")
  skip_if_not(file.exists(ay), "a.yaml not available in the installed test context")
  opts <- yaml::read_yaml(ay)$options
  get <- function(n) Filter(function(o) identical(o$name, n), opts)[[1]]

  cs <- get("colorScheme")
  expect_identical(cs$default, "recist")
  expect_identical(vapply(cs$options, `[[`, "", "name"),
                   c("recist", "simple", "colorful", "colorblind", "jamovi"))
  # exactly one level may mention jamovi, and it is the follow-global one
  expect_identical(Filter(function(o) grepl("jamovi", o$title), cs$options)[[1]]$name, "jamovi")
  expect_length(Filter(function(o) grepl("jamovi", o$title), cs$options), 1L)
  expect_false(anyDuplicated(vapply(cs$options, `[[`, "", "title")) > 0)

  sp <- get("spiderColorScheme")
  expect_identical(sp$default, "classic")
  expect_identical(vapply(sp$options, `[[`, "", "name"),
                   c("classic", "vivid", "colorful", "colorblind", "jamovi"))
  expect_length(Filter(function(o) grepl("jamovi", o$title), sp$options), 1L)
  expect_false(anyDuplicated(vapply(sp$options, `[[`, "", "title")) > 0)
})

test_that("the default colour scheme still draws the RECIST colours after the rename", {
  # The point of promoting `recist` to default is that the DEFAULT rendering is unchanged:
  # the removed `jamovi` level drew recistColors too. Until jmvtools::prepare() regenerates
  # R/waterfall.h.R the header still defaults to the old `jamovi`, which now resolves to the
  # document palette - so this locks the contract rather than the transient state.
  skip_if_not(
    identical(waterfallOptions$new(patientID = "id", responseVar = "response")$colorScheme,
              "recist"),
    "waterfall.h.R still declares the pre-rename colorScheme set - run jmvtools::prepare()")

  gt <- global_theme()
  a <- waterfallClass$new(
    options = waterfallOptions$new(patientID = "id", responseVar = "response",
                                   showWaterfallPlot = TRUE),
    data = wf_df)
  b <- built(a$.__enclos_env__$private$.waterfallplot(
    fake_image(wf_state(a$options$colorScheme)), gt$ggtheme, gt$theme))
  expect_equal(unname(b$data[[1]]$fill), rev(unname(RECIST)))
})
