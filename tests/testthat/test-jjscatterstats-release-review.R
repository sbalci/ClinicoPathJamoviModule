# Regression tests from the `jjscatterstats` release review.
#
# The analysis wraps ggstatsplot::ggscatterstats. Its correlations were checked against
# stats::cor.test and WRS2 before these expectations were written.

sc_run <- function(dat, ...) {
    o <- do.call(ClinicoPath:::jjscatterstatsOptions$new,
                 utils::modifyList(list(dep = "y", group = "x"), list(...)))
    a <- ClinicoPath:::jjscatterstatsClass$new(options = o, data = dat)
    a$init()
    tryCatch(a$.__enclos_env__$private$.run(), error = function(e) NULL)
    a
}
warn_of <- function(a) {
    v <- tryCatch(a$results$warnings$content, error = function(e) "")
    if (is.null(v)) "" else v
}
xy <- function(seed = 42, n = 80) {
    set.seed(seed)
    d <- data.frame(x = rnorm(n))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.9)
    d
}


test_that("each typestatistics level runs the correlation its label names", {
    skip_if_not_installed("statsExpressions")
    d <- xy()
    method_of <- function(ty)
        as.character(statsExpressions::corr_test(data = d, x = x, y = y, type = ty)$method)[1]

    expect_match(method_of("parametric"),    "Pearson")
    expect_match(method_of("nonparametric"), "Spearman")
    expect_match(method_of("bayes"),         "Bayes")

    # The robust option used to be labelled "bend correlation". It is not: statsExpressions
    # runs a WINSORIZED Pearson. Verified on these 80 points -- 0.58939258, matching
    # WRS2::wincor exactly, while WRS2::pbcor (percentage bend) gives 0.61524739.
    expect_match(method_of("robust"), "Winsorized")

    a_yaml <- paste(readLines("../../jamovi/jjscatterstats.a.yaml", warn = FALSE), collapse = "\n")
    expect_match(a_yaml, "Robust (Winsorized Pearson)", fixed = TRUE)
    # The LEVEL TITLE must not promise a bend correlation. The description may still mention
    # the phrase -- it explains that this is deliberately NOT a percentage-bend correlation --
    # so assert on the title line only.
    titles <- grep("^\\s*- title: Robust", readLines("../../jamovi/jjscatterstats.a.yaml", warn = FALSE),
                   value = TRUE)
    expect_length(titles, 1L)
    expect_false(grepl("bend", titles, fixed = TRUE))
    expect_match(titles, "Winsorized", fixed = TRUE)
})


test_that("the parametric correlation and its CI reproduce stats::cor.test exactly", {
    skip_if_not_installed("statsExpressions")
    d <- xy()
    se  <- statsExpressions::corr_test(data = d, x = x, y = y, type = "parametric",
                                       conf.level = 0.95)
    ref <- cor.test(d$x, d$y, method = "pearson", conf.level = 0.95)
    expect_equal(as.numeric(se$estimate),  as.numeric(ref$estimate),    tolerance = 1e-10)
    expect_equal(as.numeric(se$conf.low),  as.numeric(ref$conf.int[1]), tolerance = 1e-8)
    expect_equal(as.numeric(se$conf.high), as.numeric(ref$conf.int[2]), tolerance = 1e-8)

    # and the Winsorized branch matches WRS2::wincor, not WRS2::pbcor
    skip_if_not_installed("WRS2")
    rb <- statsExpressions::corr_test(data = d, x = x, y = y, type = "robust")
    expect_equal(as.numeric(rb$estimate), as.numeric(WRS2::wincor(d$x, d$y)$cor), tolerance = 1e-6)
    expect_false(isTRUE(all.equal(as.numeric(rb$estimate),
                                  as.numeric(WRS2::pbcor(d$x, d$y)$cor), tolerance = 1e-6)))
})


test_that("two different correlation coefficients in one output are disclosed", {
    # The ggpubr panels take their method from `ggpubrCorrMethod`, which is independent of
    # `typestatistics`. Measured on the same 80 points: main panel rho = 0.6275, ggpubr panel
    # r = 0.6683. Both correct; the silence was the problem.
    d <- xy()
    fired <- function(...) {
        a <- sc_run(d, addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE, ...)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot), silent = TRUE)
        grDevices::dev.off(); on.exit()
        grepl("Correlation method (ggpubr)", warn_of(a), fixed = TRUE)
    }
    expect_true(fired(typestatistics = "nonparametric", ggpubrCorrMethod = "pearson"))
    expect_true(fired(typestatistics = "parametric",    ggpubrCorrMethod = "spearman"))
    # quiet when they agree, and when robust/bayes have no ggpubr analogue
    expect_false(fired(typestatistics = "parametric",    ggpubrCorrMethod = "pearson"))
    expect_false(fired(typestatistics = "nonparametric", ggpubrCorrMethod = "spearman"))
    expect_false(fired(typestatistics = "robust",        ggpubrCorrMethod = "pearson"))
})


test_that("degenerate data is flagged on the MAIN plot, not only the aesthetics plot", {
    # The check lived in .plot3, which renders only when an aesthetic mapping is set, so the
    # default output was silent: a constant variable produced a scatter plot with no
    # coefficient and no explanation.
    const <- data.frame(x = rnorm(40, 3, 1), y = rep(50, 40))
    a <- sc_run(const)
    expect_match(warn_of(a), "Correlation not computed")
    expect_match(warn_of(a), "'y'", fixed = TRUE)          # names the offending variable
    expect_true(a$results$warnings$visible)

    # too few complete pairs
    few <- data.frame(x = c(1, 2, rep(NA_real_, 20)), y = c(3, 4, rep(NA_real_, 20)))
    expect_match(warn_of(sc_run(few)), "complete pair")

    # ordinary data stays quiet
    expect_false(grepl("Correlation not computed", warn_of(sc_run(xy())), fixed = TRUE))
})


test_that("warnings accumulate instead of overwriting each other", {
    # The append logic read $state while writing setContent(). Those are different slots on an
    # Html item -- $state is never populated by setContent() -- so the "existing" prefix was
    # always "" and each warning silently replaced the one before it.
    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("self$results$warnings$state", b, fixed = TRUE))
    expect_match(b, "self$results$warnings$content", fixed = TRUE)
})


test_that("every visible: expression in the results definition resolves", {
    # Recorded because a module-wide TODO claims leading-`!` expressions are always visible.
    # That is not true of this analysis: all seven resolve correctly, including
    # (!is.null(colorvar) || !is.null(sizevar) || ...) on plot3.
    set.seed(1)
    d <- data.frame(x = rnorm(60), y = rnorm(60), g = factor(rep(c("A", "B"), 30)),
                    col = factor(rep(c("p", "q"), 30)))
    vis <- function(item, ...) sc_run(d, ...)$results[[item]]$visible

    expect_false(vis("plot3"))
    expect_true(vis("plot3", colorvar = "col"))
    expect_false(vis("plot2"))
    expect_true(vis("plot2", grvar = "g"))
    expect_false(vis("ggpubrPlot"))
    expect_true(vis("ggpubrPlot", addGGPubrPlot = TRUE))
    expect_false(vis("ggpubrPlot2", addGGPubrPlot = TRUE))
    expect_true(vis("ggpubrPlot2", addGGPubrPlot = TRUE, grvar = "g"))
})


test_that("smoothMethod actually reaches the plot", {
    # ggscatterstats 1.0.0 has no `method`/`formula` formals, so those arguments were swallowed
    # by `...`. Worse, overwriting `smooth.line.args` with list(linewidth, color) DELETED the
    # package default -- list(linewidth = 1.5, color = "blue", method = "lm", formula = y ~ x)
    # -- leaving geom_smooth() with method = NULL, which falls back to LOESS. So the default
    # "Linear Model (lm)" drew a loess curve and all three options behaved identically.
    skip_if_not_installed("ggstatsplot")
    set.seed(1); d <- data.frame(x = rnorm(60)); d$y <- 0.6 * d$x + rnorm(60)
    method_of <- function(m) {
        a <- sc_run(d, smoothMethod = m)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot(a$results$plot,
                                            ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        p <- ggplot2::last_plot()
        i <- which(vapply(p$layers, function(z) inherits(z$stat, "StatSmooth"), logical(1)))
        mm <- p$layers[[i[1]]]$stat_params$method
        if (is.null(mm)) NA_character_ else as.character(mm)[1]
    }
    expect_equal(method_of("lm"),    "lm")
    expect_equal(method_of("loess"), "loess")
    expect_equal(method_of("gam"),   "gam")

    # The behavioural checks above are the real proof. Structurally, the giveaway is that
    # `formula` no longer sits as a sibling of `data =` in the ggscatterstats call: it now
    # lives inside smooth.line.args alongside `method`.
    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("formula = smooth_formula,            # GAM", b, fixed = TRUE))
    expect_match(b, "smooth.line.args", fixed = TRUE)
})


test_that("decimal places reach the subtitle", {
    # `k` is not a formal of ggscatterstats 1.0.0 either; the argument is `digits`.
    skip_if_not_installed("ggstatsplot")
    set.seed(1); d <- data.frame(x = rnorm(60)); d$y <- 0.6 * d$x + rnorm(60)
    sub_at <- function(k) {
        a <- sc_run(d, k = k, resultssubtitle = TRUE, typestatistics = "parametric")
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot(a$results$plot,
                                            ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        paste(deparse(ggplot2::last_plot()$labels$subtitle), collapse = "")
    }
    expect_false(identical(sub_at(0), sub_at(5)))

    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("k = self$options$k,", b, fixed = TRUE))
    expect_match(b, "digits = self$options$k", fixed = TRUE)
})


test_that("arguments ggscatterstats does not accept are not passed to it", {
    # marginal.type / xfill / yfill are not formals in 1.0.0 (they are xsidehistogram.args and
    # ysidehistogram.args), so they were silently discarded: the marginal type and the two
    # side-fill colour controls did nothing on the main and grouped plots.
    skip_if_not_installed("ggstatsplot")
    fmls <- names(formals(ggstatsplot::ggscatterstats))
    for (a in c("method", "formula", "k", "marginal.type", "xfill", "yfill"))
        expect_false(a %in% fmls, info = a)
    for (a in c("digits", "smooth.line.args", "xsidehistogram.args"))
        expect_true(a %in% fmls, info = a)

    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("marginal.type =", b, fixed = TRUE))
    expect_false(grepl("$xfill <-", b, fixed = TRUE))
    expect_false(grepl("yfill = !!", b, fixed = TRUE))
    expect_match(b, "xsidehistogram.args", fixed = TRUE)
})


test_that("the robust estimator is named consistently everywhere the user can see it", {
    set.seed(1); d <- data.frame(x = rnorm(60)); d$y <- 0.6 * d$x + rnorm(60)
    a <- sc_run(d, typestatistics = "robust", showExplanations = TRUE)
    txt <- gsub("<[^>]*>", "", a$results$explanations$content)
    expect_match(txt, "Winsorized")
    expect_false(grepl("percentage-bend", txt, fixed = TRUE))
    expect_false(grepl("trimmed mean", txt, fixed = TRUE))

    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("trimmed mean correlation", b, fixed = TRUE))
    expect_false(grepl("percentage-bend) correlation", b, fixed = TRUE))
})


test_that("a shape variable with more levels than ggplot2 has shapes does not lose cases", {
    # ggplot2's discrete shape palette carries 6 values. Beyond that it warns to the console
    # -- invisible in jamovi -- and DROPS the surplus levels' points entirely. Measured on the
    # raw geom: 7 levels drew 103 of 120 points, 12 levels drew 60 of 120, while the
    # correlation printed beneath used all 120.
    set.seed(1); n <- 120
    d <- data.frame(x = rnorm(n),
                    s6  = factor(rep(paste0("S", 1:6),  length.out = n)),
                    s12 = factor(rep(paste0("S", 1:12), length.out = n)))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)

    drawn_and_warned <- function(v) {
        a <- sc_run(d, shapevar = v)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot3(a$results$plot3,
                                             ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        b <- ggplot2::ggplot_build(ggplot2::last_plot())$data[[1]]
        list(points = sum(!is.na(b$x)),
             warned = grepl("Shape mapping skipped", warn_of(a), fixed = TRUE))
    }
    six <- drawn_and_warned("s6")
    expect_equal(six$points, n)          # 6 levels is fine, and stays mapped
    expect_false(six$warned)

    twelve <- drawn_and_warned("s12")
    expect_equal(twelve$points, n)       # every case still plotted
    expect_true(twelve$warned)           # and the user is told the mapping was dropped
})


test_that("the grouped ggpubr panel honours the journal palette", {
    # `palette` needs a colour mapping to act on; without one, jco/npg/lancet rendered
    # byte-identical output. The grouped panel now colours by the Split By variable.
    skip_if_not_installed("ggpubr")
    set.seed(1); n <- 120
    d <- data.frame(x = rnorm(n), g = factor(rep(paste0("G", 1:4), length.out = n)))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)
    render_md5 <- function(pal) {
        a <- sc_run(d, grvar = "g", addGGPubrPlot = TRUE, ggpubrPalette = pal)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plotGGPubr2(a$results$ggpubrPlot2), silent = TRUE)
        grDevices::dev.off(); on.exit()
        unname(tools::md5sum(f))
    }
    expect_false(identical(render_md5("jco"), render_md5("npg")))

    # ungrouped there is nothing to colour, and that is now stated rather than left silent
    d2 <- data.frame(x = rnorm(60)); d2$y <- 0.6 * d2$x + rnorm(60)
    a <- sc_run(d2, addGGPubrPlot = TRUE, ggpubrPalette = "npg")
    f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
    try(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot), silent = TRUE)
    grDevices::dev.off()
    expect_match(warn_of(a), "Colour palette not applied")
})


test_that("the grouped plot applies rug and theme to every panel, not just the last", {
    # grouped_ggscatterstats returns a patchwork; `+` adds a layer to the LAST panel only,
    # `&` applies it to all of them. Measured on a 4-level grouping variable: layer counts
    # 4,4,4,5 with `+` against 5,5,5,5 with `&`.
    b <- readLines("../../R/jjscatterstats.b.R", warn = FALSE)
    # slice by method rather than by line number, which drifts with every edit
    starts <- grep("^\\s{8}\\.[A-Za-z0-9_]+ = function", b)
    body_of <- function(name) {
        i <- grep(paste0("^\\s{8}\\.", name, " = function"), b)
        stopifnot(length(i) == 1L)
        nxt <- starts[starts > i]
        b[i:(if (length(nxt)) nxt[1] - 1L else length(b))]
    }
    grouped <- body_of("plot2")
    expect_true(any(grepl("plot <- plot & ggplot2::geom_rug", grouped, fixed = TRUE)))
    expect_true(any(grepl("plot <- plot & ggplot2::theme_bw", grouped, fixed = TRUE)))
    expect_false(any(grepl("plot <- plot + ggplot2::", grouped, fixed = TRUE)))
    # the ungrouped .plot builds a single ggplot, where `+` is correct and must stay
    expect_true(any(grepl("plot <- plot + ggplot2::geom_rug", body_of("plot"), fixed = TRUE)))
})


test_that("a clinical preset does not announce settings it cannot deliver", {
    # `overrides` is runtime R6 state; the ggpubr panel's visibility is `visible:
    # (addGGPubrPlot)` in the .r.yaml, which jamovi evaluates against the OPTIONS object.
    # The override therefore could never reveal the panel, yet the banner claimed it had.
    set.seed(1); d <- data.frame(x = rnorm(60)); d$y <- 0.6 * d$x + rnorm(60)
    a <- sc_run(d, clinicalPreset = "biomarker_correlation")
    banner <- gsub("<[^>]*>", " ", a$results$presetInfo$content)

    expect_match(banner, "Spearman")                       # this one really is applied
    expect_equal(a$.__enclos_env__$private$.option("typestatistics"), "nonparametric")
    expect_false(grepl("ggpubr scatter plot enabled", banner, fixed = TRUE))
    expect_false(grepl("Color palette", banner, fixed = TRUE))
    expect_false(a$results$ggpubrPlot$visible)             # still hidden, as the yaml dictates
})


test_that("the aesthetics-plot subtitle names the right coefficient and reports n", {
    # It printed "r" for every method (so a Spearman result carried Pearson's symbol),
    # hard-coded 3 decimal places regardless of the user's setting, and gave a coefficient
    # and a p-value with no denominator anywhere in the analysis.
    set.seed(1); n <- 120
    d <- data.frame(x = rnorm(n), col = factor(rep(c("a", "b"), length.out = n)))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)
    sub3 <- function(...) {
        a <- sc_run(d, colorvar = "col", ...)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot3(a$results$plot3,
                                             ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        ggplot2::last_plot()$labels$subtitle
    }
    expect_match(sub3(typestatistics = "parametric"),    "r = ",   fixed = TRUE)
    expect_match(sub3(typestatistics = "nonparametric"), "rho = ", fixed = TRUE)
    expect_false(grepl("r = ", sub3(typestatistics = "nonparametric"), fixed = TRUE))

    # n is present, and matches the complete cases
    expect_match(sub3(typestatistics = "parametric"), paste0("n = ", n), fixed = TRUE)
    # and the decimal-places option is honoured rather than hard-coded to 3
    expect_match(sub3(typestatistics = "parametric", k = 1), "r = 0.5,", fixed = TRUE)
})


test_that("a grouped plot discloses that its per-panel p-values are unadjusted", {
    # grouped_ggscatterstats has no p-adjustment argument: each facet is tested at the
    # nominal level. On histopathology (Age x OverallTime by Grade) the raw p-values were
    # 0.3015 / 0.0281 / 0.2069 and Holm gives 0.4138 / 0.0842 / 0.4138.
    set.seed(1); n <- 120
    d <- data.frame(x = rnorm(n), g = factor(rep(paste0("G", 1:4), length.out = n)))
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)
    fired <- function(...) grepl("One test per group",
                                 warn_of(sc_run(d, ...)), fixed = TRUE)

    expect_true(fired(grvar = "g", resultssubtitle = TRUE))
    # silent when no per-panel statistic is shown, and when there is no grouping at all
    expect_false(fired(grvar = "g", resultssubtitle = FALSE))
    expect_false(fired(resultssubtitle = TRUE))

    # the message names the number of tests
    a <- sc_run(d, grvar = "g", resultssubtitle = TRUE)
    expect_match(warn_of(a), "4 panels", fixed = TRUE)
})
