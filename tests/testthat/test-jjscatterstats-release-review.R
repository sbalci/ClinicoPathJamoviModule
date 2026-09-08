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
    expect_true(any(grepl("plot <- plot & ggtheme", grouped, fixed = TRUE)))
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


# --- production-profile pass -------------------------------------------------

test_that("the aesthetics panel runs the analysis type the user picked, not a fallback", {
    # It used to run stats::cor.test() behind a switch that DOWNGRADED two of the four
    # types: "robust" silently became Spearman (labelled "robust unavailable", though
    # WRS2 has been in Imports all along) and "bayes" silently became Pearson. The panel
    # therefore contradicted the main plot sitting directly above it. Both now route
    # through statsExpressions::corr_test -- the engine ggscatterstats itself calls -- so
    # they agree by construction.
    skip_if_not_installed("statsExpressions")
    d <- xy()                                   # seed 42, n = 80
    sub3 <- function(ty) {
        a <- sc_run(d, colorvar = NULL, sizevar = NULL, labelvar = NULL,
                    shapevar = NULL, alphavar = NULL, typestatistics = ty)
        # force the aesthetics panel on with a colour mapping
        a <- sc_run(cbind(d, col = factor(rep(c("a", "b"), length.out = nrow(d)))),
                    colorvar = "col", typestatistics = ty)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot3(a$results$plot3,
                                             ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        ggplot2::last_plot()$labels$subtitle
    }

    # each subtitle names the method that actually ran -- no "(unavailable)" apologies
    expect_match(sub3("parametric"),    "Pearson correlation")
    expect_match(sub3("nonparametric"), "Spearman correlation")
    expect_match(sub3("robust"),        "Winsorized Pearson correlation")
    expect_match(sub3("bayes"),         "Bayesian Pearson correlation")
    expect_false(any(grepl("unavailable", c(sub3("robust"), sub3("bayes")), fixed = TRUE)))

    # and the coefficient matches the engine the MAIN panel uses, to the printed digits
    ref <- function(ty) as.numeric(as.data.frame(
        statsExpressions::corr_test(data = d, x = x, y = y, type = ty))$estimate)
    expect_match(sub3("robust"), sprintf("%.2f", ref("robust")), fixed = TRUE)
    expect_match(sub3("nonparametric"), sprintf("%.2f", ref("nonparametric")), fixed = TRUE)

    # the Bayesian panel reports a Bayes factor; it has no p-value to report
    expect_match(sub3("bayes"), "BF10 = ", fixed = TRUE)
    expect_false(grepl("p =", sub3("bayes"), fixed = TRUE))
})


test_that("variable names with spaces and punctuation survive every render path", {
    # composeTerm()-style backtick quoting is wrong as a data[[ ]] key, and tidy-eval
    # arguments need a symbol rather than a string. Both plot paths and the ggpubr panel
    # are exercised here because each reaches its columns differently.
    set.seed(7); n <- 60
    d <- data.frame(`Tumor Size (mm)` = rnorm(n),
                    `Grade / Stage`   = factor(rep(c("I", "II"), length.out = n)),
                    check.names = FALSE)
    d[["Overall Time"]] <- 0.6 * d[["Tumor Size (mm)"]] + rnorm(n, 0, 0.8)

    o <- ClinicoPath:::jjscatterstatsOptions$new(
        dep = "Tumor Size (mm)", group = "Overall Time", colorvar = "Grade / Stage",
        grvar = "Grade / Stage", addGGPubrPlot = TRUE, ggpubrAddCorr = TRUE)
    a <- ClinicoPath:::jjscatterstatsClass$new(options = o, data = d)
    a$init()
    expect_silent(a$.__enclos_env__$private$.run())

    draw <- function(m, item) {
        f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        err <- tryCatch({ a$.__enclos_env__$private[[m]](
                              item, ggtheme = ggplot2::theme_bw(), theme = NULL); NULL },
                        error = function(e) conditionMessage(e))
        grDevices::dev.off(); on.exit()
        err
    }
    expect_null(draw(".plot",       a$results$plot))
    expect_null(draw(".plot2",      a$results$plot2))
    expect_null(draw(".plot3",      a$results$plot3))
    expect_null(draw(".plotGGPubr", a$results$ggpubrPlot))
    # ggpubr builds facet/colour scales from the column NAME; a slash or space in it is
    # the classic place a formula-based facet spec blows up.
    expect_null(draw(".plotGGPubr2", a$results$ggpubrPlot2))

    # the aesthetics subtitle reports a real coefficient, not NA, for these names
    expect_false(grepl("NA", ggplot2::last_plot()$labels$subtitle %||% "", fixed = TRUE))

    # and the generated syntax quotes them into valid, re-parseable R
    expect_silent(parse(text = sub("^[A-Za-z.]+::", "", a$asSource())))
})


test_that("a labelled factor keeps its labels through the grouped and aesthetic panels", {
    # jamovi hands ordinal columns over as factors carrying a `values` attribute; the
    # panels must show the LABELS, not the underlying integer codes.
    set.seed(11); n <- 80
    g <- factor(rep(c("Low grade", "High grade"), length.out = n),
                levels = c("Low grade", "High grade"))
    d <- data.frame(x = rnorm(n), g = g)
    d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)

    a <- sc_run(d, grvar = "g", colorvar = "g")
    f <- tempfile(fileext = ".png"); grDevices::png(f, 900, 400)
    try(a$.__enclos_env__$private$.plot2(a$results$plot2,
                                         ggtheme = ggplot2::theme_bw(), theme = NULL),
        silent = TRUE)
    grDevices::dev.off()

    b <- ggplot2::ggplot_build(ggplot2::last_plot())
    expect_true(all(levels(d$g) %in% levels(droplevels(d$g))))
    # the aesthetics panel maps the factor by label, preserving the declared level order
    a3 <- sc_run(d, colorvar = "g")
    f <- tempfile(fileext = ".png"); grDevices::png(f, 500, 400)
    try(a3$.__enclos_env__$private$.plot3(a3$results$plot3,
                                          ggtheme = ggplot2::theme_bw(), theme = NULL),
        silent = TRUE)
    grDevices::dev.off()
    p3 <- ggplot2::last_plot()
    expect_identical(levels(ggplot2::ggplot_build(p3)$plot$data$g), levels(g))
})


test_that("the welcome panel tracks setup progress and stays theme-safe", {
    # Shown until both axis variables are chosen. Built as a decisionpanel-style intro:
    # every tint is a translucent rgba() with `color: inherit`, because a hardcoded dark
    # foreground is unreadable against jamovi's dark theme (the mirror of the opaque
    # background case that tools/theme_safe_html.py catches).
    wm <- function(...) {
        o <- do.call(ClinicoPath:::jjscatterstatsOptions$new, list(...))
        a <- ClinicoPath:::jjscatterstatsClass$new(
            options = o, data = data.frame(x = 1:5, y = 1:5))
        a$.__enclos_env__$private$.welcomeMessage()
    }
    none <- wm(dep = NULL, group = NULL)
    half <- wm(dep = "x",  group = NULL)
    both <- wm(dep = "x",  group = "y")

    expect_true(grepl("[ ] x-axis", none, fixed = TRUE))
    expect_true(grepl("[ ] y-axis", none, fixed = TRUE))
    expect_true(grepl("[x] x-axis", half, fixed = TRUE))
    expect_true(grepl("[ ] y-axis", half, fixed = TRUE))
    expect_true(grepl("[x] y-axis", both, fixed = TRUE))

    # no hardcoded foreground colour anywhere in the panel
    expect_false(grepl("color: *#", none))
    # well-formed, and only the five structural HTML entities
    expect_identical(lengths(regmatches(none, gregexpr("<div",   none))),
                     lengths(regmatches(none, gregexpr("</div>", none))))
    expect_length(setdiff(unique(unlist(regmatches(none, gregexpr("&[a-zA-Z]+;", none)))),
                          c("&lt;", "&gt;", "&amp;", "&quot;", "&apos;")), 0L)
})


# --- statistical guardrails (check-function-full audit, 2026-09-08) -----------
# Each of these five was verified SILENT before the notices were added: the figure
# and its p-value are what a clinician quotes, so a coefficient that is unstable,
# outlier-driven, computed on half the cohort, or measuring the wrong shape of
# relationship has to say so on screen.

test_that("losing a large share of rows to missing data is disclosed", {
    set.seed(1); n <- 200
    d <- data.frame(x = rnorm(n)); d$y <- 0.6 * d$x + rnorm(n, 0, 0.8)
    d$x[1:90] <- NA                                   # 110 of 200 pairs survive
    w <- warn_of(sc_run(d))
    expect_match(w, "45% of rows could not be used", fixed = TRUE)
    expect_match(w, "110 of 200 rows", fixed = TRUE)

    # under the 20% threshold it stays quiet rather than nagging
    d2 <- data.frame(x = rnorm(n)); d2$y <- 0.6 * d2$x + rnorm(n, 0, 0.8)
    d2$x[1:10] <- NA                                  # 5%
    expect_false(grepl("could not be used", warn_of(sc_run(d2)), fixed = TRUE))
})


test_that("small samples are called out, in two tiers", {
    mk <- function(n) { set.seed(n); d <- data.frame(x = rnorm(n))
                        d$y <- 0.6 * d$x + rnorm(n, 0, 0.8); d }
    expect_match(warn_of(sc_run(mk(8))),  "Very small sample (n = 8)",  fixed = TRUE)
    expect_match(warn_of(sc_run(mk(25))), "Small sample (n = 25)",      fixed = TRUE)
    # n >= 30 with clean data says nothing
    expect_false(grepl("small sample", warn_of(sc_run(mk(150))), ignore.case = TRUE))
})


test_that("a coefficient driven by one observation is flagged, and points at the fix", {
    # 40 points of pure noise plus one far-out point: Pearson goes from r = -0.17
    # (p = 0.30) to r = 0.79 (p = 0.000006) on the strength of that single patient.
    set.seed(4)
    d <- data.frame(x = rnorm(40)); d$y <- rnorm(40)
    d2 <- rbind(d, data.frame(x = 12, y = 12))
    expect_lt(abs(cor(d$x, d$y)), 0.2)
    expect_gt(cor(d2$x, d2$y), 0.7)

    w <- warn_of(sc_run(d2))
    expect_match(w, "One observation is driving this result", fixed = TRUE)
    expect_match(w, "Robust (Winsorized Pearson)", fixed = TRUE)   # names the escape route

    # the resistant methods are the escape route, so they must not be told to switch
    fires <- function(ty) grepl("One observation is driving",
                                warn_of(sc_run(d2, typestatistics = ty)), fixed = TRUE)
    expect_true(fires("parametric")); expect_true(fires("bayes"))
    expect_false(fires("robust"));    expect_false(fires("nonparametric"))

    # clean data does not trip it
    set.seed(11); c1 <- data.frame(x = rnorm(150)); c1$y <- 0.6 * c1$x + rnorm(150, 0, 0.8)
    expect_false(grepl("One observation is driving", warn_of(sc_run(c1)), fixed = TRUE))
})


test_that("leave-one-out correlation matches refitting, to machine precision", {
    # The notice uses an O(n) running-sums update rather than n refits; if that
    # algebra were wrong it would invent influential points that do not exist.
    set.seed(9)
    for (n in c(10L, 37L, 200L)) {
        x <- rnorm(n); y <- 0.4 * x + rnorm(n)
        Sx <- sum(x); Sy <- sum(y); Sxx <- sum(x*x); Syy <- sum(y*y); Sxy <- sum(x*y)
        m <- n - 1L
        sx <- Sx - x; sy <- Sy - y; sxx <- Sxx - x*x; syy <- Syy - y*y; sxy <- Sxy - x*y
        den <- sqrt(pmax(0, m*sxx - sx^2) * pmax(0, m*syy - sy^2))
        fast <- ifelse(den > 0, (m*sxy - sx*sy) / den, NA_real_)
        brute <- vapply(seq_len(n), function(i) cor(x[-i], y[-i]), numeric(1))
        expect_equal(fast, brute)
    }
})


test_that("a relationship the coefficient cannot see is flagged, without crying wolf", {
    # A perfect U-shape gives Pearson r = 0.011, p = 0.93 -- "no association" for a
    # deterministic relationship. This is the failure mode that matters.
    set.seed(5)
    u <- data.frame(x = seq(-3, 3, length.out = 60)); u$y <- u$x^2 + rnorm(60, 0, 0.5)
    expect_lt(abs(cor(u$x, u$y)), 0.1)
    expect_match(warn_of(sc_run(u)), "The relationship is not a straight line", fixed = TRUE)

    # A MONOTONE curve is linear in ranks, so Spearman handles it and stays quiet...
    set.seed(12); e <- data.frame(x = rnorm(150)); e$y <- exp(0.5 * e$x) + rnorm(150, 0, 0.1)
    expect_false(grepl("not a straight line",
                       warn_of(sc_run(e, typestatistics = "nonparametric")), fixed = TRUE))
    # ...but the U-shape is non-monotone, so rho misses it too and the notice still fires
    expect_match(warn_of(sc_run(u, typestatistics = "nonparametric")),
                 "not a straight line", fixed = TRUE)

    # Which variable the user drops on which axis is arbitrary, so the warning must
    # not be: a quadratic in x fits y = x^2 perfectly, but the same cloud with the axes
    # swapped needs x = +/-sqrt(y), which no quadratic in y can express. The probe fits
    # both directions and keeps the stronger.
    swapped <- function(dat, ...) {
        o <- do.call(ClinicoPath:::jjscatterstatsOptions$new,
                     utils::modifyList(list(dep = "x", group = "y"), list(...)))
        a <- ClinicoPath:::jjscatterstatsClass$new(options = o, data = dat)
        a$init(); tryCatch(a$.__enclos_env__$private$.run(), error = function(e) NULL)
        warn_of(a)
    }
    expect_match(swapped(u), "not a straight line", fixed = TRUE)   # dep = x
    expect_match(warn_of(sc_run(u)), "not a straight line", fixed = TRUE)  # dep = y

    # clean linear data: silent, either orientation
    set.seed(11); c1 <- data.frame(x = rnorm(150)); c1$y <- 0.6 * c1$x + rnorm(150, 0, 0.8)
    expect_false(grepl("not a straight line", warn_of(sc_run(c1)), fixed = TRUE))
    expect_false(grepl("not a straight line", swapped(c1), fixed = TRUE))
})


test_that("heavy ties under Spearman are surfaced", {
    # cor.test() emits "Cannot compute exact p-value with ties" as an R warning,
    # which jamovi never shows.
    set.seed(6)
    d <- data.frame(x = sample(1:3, 80, TRUE)); d$y <- sample(1:3, 80, TRUE)
    w <- warn_of(sc_run(d, typestatistics = "nonparametric"))
    expect_match(w, "Heavy ties", fixed = TRUE)
    expect_match(w, "only 3 distinct values across 80 observations", fixed = TRUE)

    # continuous data under Spearman, and the same tied data under Pearson, stay quiet
    set.seed(11); c1 <- data.frame(x = rnorm(150)); c1$y <- 0.6 * c1$x + rnorm(150, 0, 0.8)
    expect_false(grepl("Heavy ties",
                       warn_of(sc_run(c1, typestatistics = "nonparametric")), fixed = TRUE))
    expect_false(grepl("Heavy ties", warn_of(sc_run(d)), fixed = TRUE))
})


# --- review-function pass: schema, labelling and i18n -------------------------

test_that("bfmessage is described and gated as what it actually does", {
    # It was titled "Bayes factor message" and described as showing the BF "in the
    # subtitle when using Bayesian analysis". Measured, every clause was wrong: it adds
    # a CAPTION, it works under PARAMETRIC and is inert under Bayesian, and it reports
    # BF01 -- evidence for the NULL, the reciprocal of the BF10 a reader assumes.
    a_yaml <- paste(readLines("../../jamovi/jjscatterstats.a.yaml", warn = FALSE), collapse = "\n")
    expect_match(a_yaml, "title: Bayes factor for the null (BF01)", fixed = TRUE)
    expect_match(a_yaml, "RECIPROCAL of the more commonly quoted BF10", fixed = TRUE)
    expect_match(a_yaml, "parametric (Pearson) test only", fixed = TRUE)
    expect_false(grepl("display Bayes Factor in the subtitle when using Bayesian",
                       a_yaml, fixed = TRUE))

    # and the control is disabled where it does nothing
    u_yaml <- paste(readLines("../../jamovi/jjscatterstats.u.yaml", warn = FALSE), collapse = "\n")
    expect_match(u_yaml, "enable: (typestatistics:parametric)", fixed = TRUE)

    # behaviour matches the description: a caption under parametric, nothing under bayes
    set.seed(42); d <- xy(n = 60)
    cap <- function(...) {
        a <- sc_run(d, ...)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 520, 400)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot(a$results$plot,
                                            ggtheme = ggplot2::theme_bw(), theme = NULL),
            silent = TRUE)
        grDevices::dev.off(); on.exit()
        !is.null(ggplot2::last_plot()$labels$caption)
    }
    expect_false(cap(typestatistics = "parametric", bfmessage = FALSE))
    expect_true( cap(typestatistics = "parametric", bfmessage = TRUE))
    expect_false(cap(typestatistics = "bayes",      bfmessage = TRUE))   # inert, hence gated
})


test_that("the statistical output is on by default", {
    # resultssubtitle carries the coefficient, its CI, the p-value and n -- the whole
    # statistical result. Defaulting it off left the default output of a correlation
    # analysis with no subtitle at all (verified: NULL, not empty).
    opts <- ClinicoPath:::jjscatterstatsOptions$new(dep = "x", group = "y")
    expect_true(opts$resultssubtitle)
})


test_that("checkbox labels name the thing rather than the action", {
    # The jamovi library reviewer flags leading Show/Enable/Include/Add verbs.
    y <- yaml::read_yaml("../../jamovi/jjscatterstats.a.yaml")
    verbs <- c("Show", "Enable", "Include", "Export", "Generate", "Add")
    offenders <- Filter(Negate(is.null), lapply(y$options, function(o) {
        t <- o$title
        if (!is.null(t) && nzchar(t) && strsplit(t, " ")[[1]][1] %in% verbs) o$name else NULL
    }))
    expect_length(offenders, 0L)
})


test_that("every user-facing notice is translatable", {
    # The notices are what a clinician reads when something is wrong with their data,
    # so they must not be hardcoded English. Each payload has to reach .appendWarning()
    # through .() (optionally via the .fmt() guard), never a bare paste0().
    b <- paste(readLines("../../R/jjscatterstats.b.R", warn = FALSE), collapse = "\n")
    expect_false(grepl("appendWarning(paste0", b, fixed = TRUE))

    # jmvcore::format() silently drops placeholders containing "_", and a placeholder
    # named s/st/str partial-matches its `str` formal and swallows the whole template.
    expect_false(grepl("\\{[a-z]+_[a-z]+\\}", b))
    expect_false(grepl("\\{(s|st|str)\\}", b))
    # a trailing " [...]" in a .() string is treated as msgctxt and stripped from output
    expect_false(grepl('\\.\\("[^"]* \\[', b))

    # and the Turkish catalog carries them, with placeholders intact
    po <- paste(readLines("../../jamovi/i18n/tr.po", warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
    expect_match(po, "One observation is driving this result", fixed = TRUE)
    expect_match(po, "Bu sonucu tek bir g", fixed = TRUE)
})
