# Renderers must draw from image$state, not from a private$ field only .run() fills.
#
# On the export path (right-click a plot > Export...) jmvcore builds a FRESH analysis instance,
# restores the serialised results, and calls the render function directly. .run() never executes,
# so every private$ field holds its initial value. A renderer gated on one of those returns
# before drawing and the exported file is blank -- while the plot on screen looks perfect, which
# is why this class of bug reaches users.
#
# These two were HIGH findings in the 2026-09-16 jjstatsplot audit:
#   jjdotchart:plot/:plot2 and jjdotplotstats:plot/:plot2 gated on private$.inputsValid,
#   which is FALSE on that path.
#
# The harness must go through Analysis$.createImage(), the call the engine makes. image$.render()
# takes a different path (.createPlotObject) and CANNOT show this bug -- see
# jamovi-library-audit/2026-09-16 meddecide.md and reference notes on the two render entry points.

quiet <- function(expr) suppressWarnings(suppressMessages(expr))

# Draw one image the way the engine does, and report whether the renderer returned TRUE.
draw_via_engine <- function(analysis, image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    isTRUE(quiet(analysis$.createImage(
        image$.__enclos_env__$private$.renderFun, image)))
}

# Rebuild the analysis the way an export does: a new instance, no .run(), state restored.
reopen_without_run <- function(cls, analysis, data, image_names) {
    fresh <- cls$new(options = analysis$options, data = data)
    quiet(fresh$init())
    for (nm in image_names) {
        live <- analysis$results[[nm]]
        if (!is.null(live) && !is.null(fresh$results[[nm]]))
            fresh$results[[nm]]$setState(live$state)
    }
    fresh
}

make_data <- function(n = 60, seed = 20260921) {
    set.seed(seed)
    data.frame(
        measure = rnorm(n, 10, 2),
        grp = factor(rep(letters[1:4], length.out = n)),
        split = factor(rep(c("x", "y"), length.out = n))
    )
}

test_that("jjdotchart plots still draw when .run() never ran (export path)", {
    skip_if_not(exists("jjdotchartClass"), "namespace-internal class not available")
    d <- make_data()
    ran <- quiet(jjdotchart(data = d, dep = "measure", group = "grp"))
    analysis <- ran$.__enclos_env__$private$.parent

    live_img <- analysis$results$plot
    expect_true(draw_via_engine(analysis, live_img))      # on screen

    fresh <- reopen_without_run(jjdotchartClass, analysis, d, c("plot"))
    # the whole point: .run() has NOT been called on `fresh`
    expect_false(isTRUE(fresh$.__enclos_env__$private$.inputsValid))
    expect_true(draw_via_engine(fresh, fresh$results$plot))
})

test_that("jjdotplotstats plots still draw, with their subtitle, when .run() never ran", {
    skip_if_not(exists("jjdotplotstatsClass"), "namespace-internal class not available")
    d <- make_data()
    ran <- quiet(jjdotplotstats(data = d, dep = "measure", group = "grp",
                                resultssubtitle = TRUE))
    analysis <- ran$.__enclos_env__$private$.parent

    expect_true(draw_via_engine(analysis, analysis$results$plot))

    fresh <- reopen_without_run(jjdotplotstatsClass, analysis, d, c("plot"))
    expect_false(isTRUE(fresh$.__enclos_env__$private$.inputsValid))
    expect_true(draw_via_engine(fresh, fresh$results$plot))

    # the subtitle expression must survive too: it used to live only in
    # private$.subtitleCache, so an exported figure lost its statistics line
    st <- fresh$results$plot$state
    expect_true(is.list(st) && isTRUE(st$valid))
    expect_false(is.null(st$subtitle))
})

# The next three were not blank-image failures: the plot still drew, but a piece of it was
# computed in .run() and read from a private$ field at render time, so the EXPORTED figure
# quietly differed from the one on screen. Asserting on the state payload is what catches a
# regression that drops the field again.

test_that("advancedraincloud carries its p-value annotation in state", {
    skip_if_not(exists("advancedraincloudClass"), "namespace-internal class not available")
    d <- make_data()
    ran <- quiet(advancedraincloud(data = d, y_var = "measure", x_var = "grp",
                                   show_statistics = TRUE, p_value_position = "above"))
    analysis <- ran$.__enclos_env__$private$.parent
    st <- analysis$results$plot$state
    # list(data=, comparison=) - a bare data.frame is the pre-fix shape, where .add_p_values()
    # fell back to private$.comparison_results and got NULL on the export path
    expect_false(is.data.frame(st))
    expect_true(is.list(st) && !is.null(st$data))
    expect_true("comparison" %in% names(st))
})

test_that("jjsegmentedtotalbar carries its preset in state", {
    skip_if_not(exists("jjsegmentedtotalbarClass"), "namespace-internal class not available")
    set.seed(20260921)
    d <- data.frame(
        tp = factor(rep(c("Baseline", "Month 6"), each = 40)),
        status = factor(sample(c("CR", "PR", "PD"), 80, TRUE)),
        n = 1
    )
    ran <- quiet(jjsegmentedtotalbar(data = d, x_var = "tp", y_var = "n",
                                     fill_var = "status"))
    analysis <- ran$.__enclos_env__$private$.parent
    st <- analysis$results$plot$state
    expect_false(is.data.frame(st))          # pre-fix shape
    expect_true(is.list(st) && !is.null(st$data))
    expect_true("preset" %in% names(st))
})

test_that("multisurvival adjusted-curve state carries the competing-risk flag", {
    skip_if_not(exists("multisurvivalClass"), "namespace-internal class not available")
    # .adjustedEstimandNote() used to call .isCompetingRisk() with no state, so on the export
    # path it fell through to the options-only test and a Fine-Gray curve could be labelled as
    # cause-specific survival. The flag now travels in the plot_adj state.
    root <- normalizePath(file.path(testthat::test_path(), "..", ".."), mustWork = FALSE)
    skip_if_not(file.exists(file.path(root, "DESCRIPTION")), "package source tree not available")
    src <- readLines(file.path(root, "R", "multisurvival.b.R"), warn = FALSE)
    body <- paste(src, collapse = "\n")
    expect_true(grepl("has_competing = has_competing", body, fixed = TRUE))
    # and the note helper takes the flag instead of looking it up. (.isCompetingRisk() is
    # still called elsewhere, from .run()-side code where private$.eventRecode is populated --
    # only the note helper had to stop doing it.)
    expect_true(grepl("adjustedEstimandNote = function(method, has_competing", body, fixed = TRUE))
    expect_true(grepl("if (isTRUE(has_competing)) {", body, fixed = TRUE))
    # .plot_adj reads the flag off the state it was handed, not off a private$ field
    expect_true(grepl("isTRUE(plotData$has_competing)", body, fixed = TRUE))
})
