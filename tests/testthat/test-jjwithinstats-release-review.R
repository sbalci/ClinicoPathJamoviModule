# Release-review regression tests for jjwithinstats.
#
# The statistical core of this analysis was already sound - the comparison is
# genuinely paired, the 3+ measurement path is a repeated-measures ANOVA with a
# Greenhouse-Geisser sphericity correction, listwise deletion preserves pairs,
# and exclusions are disclosed. What was broken was the option surface: two
# controls that ggstatsplot 1.0.0 no longer accepts were still being passed and
# so did nothing, and the summary panel called every configuration a "repeated
# measures ANOVA" including a two-measurement paired t-test.
#
# The tests below pin BOTH: the fixes, and the correct behaviour that must not
# regress while fixing them. Every expectation was observed on the unfixed code
# first.

library(testthat)

jw_data <- function(n = 40, seed = 1) {
    set.seed(seed)
    d <- data.frame(pre = rnorm(n, 10, 2))
    d$post <- d$pre + rnorm(n, 2, 1)     # correlated, so paired != unpaired
    d$fup  <- d$pre + rnorm(n, 1, 1)
    d
}

jw_run <- function(data = jw_data(), ...) {
    opts <- do.call(ClinicoPath:::jjwithinstatsOptions$new,
                    utils::modifyList(list(dep1 = "pre", dep2 = "post"), list(...)))
    a <- ClinicoPath:::jjwithinstatsClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
jw_plot <- function(a) {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 550)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    try(a$.__enclos_env__$private$.plot(a$results$plot,
            ggtheme = ggplot2::theme_bw(), theme = NULL), silent = TRUE)
    grDevices::dev.off(); on.exit()
    ggplot2::last_plot()
}
jw_subtitle <- function(a) paste(deparse(jw_plot(a)$labels$subtitle), collapse = "")
jw_geoms <- function(a) vapply(jw_plot(a)$layers, function(l) class(l$geom)[1], character(1))
jw_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))


test_that("the comparison is genuinely paired", {
    # The single most important property of this analysis. subject.id is passed
    # to ggwithinstats as the STRING "rowid"; ggwithinstats does ensym(), which
    # accepts a string, so it binds - but if that ever broke, the analysis would
    # silently become an unpaired test on the same data, which on this fixture
    # differs by eleven orders of magnitude in p.
    d <- jw_data()
    paired   <- stats::t.test(d$pre, d$post, paired = TRUE)
    unpaired <- stats::t.test(d$pre, d$post, paired = FALSE)
    expect_equal(unname(paired$parameter), 39)
    expect_equal(unname(unpaired$parameter), 74.89, tolerance = 1e-2)

    s <- jw_subtitle(jw_run(d, resultssubtitle = TRUE))
    expect_match(s, sprintf("%.2f", unname(paired$statistic)), fixed = TRUE)
    expect_match(s, "39", fixed = TRUE)
    # and definitively NOT the unpaired df
    expect_false(grepl("74.89", s, fixed = TRUE))
})


test_that("three measurements get a repeated-measures ANOVA with a sphericity correction", {
    # A between-groups ANOVA on the same data gives F = 12.26; the correct
    # repeated-measures F is 94.54. The fractional degrees of freedom are the
    # Greenhouse-Geisser correction, which must survive any change to how the
    # subtitle is produced.
    d <- jw_data()
    long <- data.frame(id = factor(rep(seq_len(nrow(d)), 3)),
                       m  = factor(rep(c("pre", "post", "fup"), each = nrow(d))),
                       v  = c(d$pre, d$post, d$fup))
    rm_f <- summary(stats::aov(v ~ m + Error(id / m), data = long))[[2]][[1]][1, "F value"]
    bg_f <- summary(stats::aov(v ~ m, data = long))[[1]][1, "F value"]
    expect_gt(abs(rm_f - bg_f), 50)   # the two really are far apart

    s <- jw_subtitle(jw_run(d, dep3 = "fup", resultssubtitle = TRUE))
    expect_match(s, sprintf("%.2f", rm_f), fixed = TRUE)
    expect_false(grepl(sprintf("%.2f", bg_f), s, fixed = TRUE))
    # Greenhouse-Geisser: fractional df, not the uncorrected (2, 78)
    expect_match(s, "1.79", fixed = TRUE)
    expect_match(s, "69.71", fixed = TRUE)
    expect_false(grepl("(2, 78)", s, fixed = TRUE))
})


test_that("the effect-size selector reaches the effect size", {
    # ggstatsplot 1.0.0 removed effsize.type from ggwithinstats, so it was
    # swallowed by `...` and all four choices produced Hedges' g.
    d <- jw_data()
    eff <- function(...) {
        s <- jw_subtitle(jw_run(d, resultssubtitle = TRUE, ...))
        regmatches(s, regexpr("widehat[^,]*", s))
    }
    # two measurements: d vs g
    expect_match(eff(effsizetype = "biased"),   "Cohen",  fixed = TRUE)
    expect_match(eff(effsizetype = "unbiased"), "Hedges", fixed = TRUE)
    expect_false(identical(eff(effsizetype = "biased"), eff(effsizetype = "unbiased")))
    # the ANOVA-only names error in two_sample_test, so they are remapped
    expect_match(eff(effsizetype = "eta"),   "Cohen",  fixed = TRUE)
    expect_match(eff(effsizetype = "omega"), "Hedges", fixed = TRUE)

    # three measurements: eta-squared vs omega-squared
    expect_match(eff(dep3 = "fup", effsizetype = "eta"),   "eta",   fixed = TRUE)
    expect_match(eff(dep3 = "fup", effsizetype = "omega"), "omega", fixed = TRUE)
    expect_false(identical(eff(dep3 = "fup", effsizetype = "eta"),
                           eff(dep3 = "fup", effsizetype = "omega")))
})


test_that("pairwise comparisons can actually be switched off", {
    # `pairwise.comparisons` was removed in ggstatsplot 1.0.0, so unticking the
    # box left the significance brackets on the plot. pairwise.display = "none"
    # is the surviving control.
    d <- jw_data()
    on_g  <- jw_geoms(jw_run(d, dep3 = "fup", resultssubtitle = TRUE, pairwisecomparisons = TRUE))
    off_g <- jw_geoms(jw_run(d, dep3 = "fup", resultssubtitle = TRUE, pairwisecomparisons = FALSE))
    expect_true("GeomSignif" %in% on_g)
    expect_false("GeomSignif" %in% off_g)
})


test_that("the narrative names the test that was actually run", {
    # Every configuration was described as a "Repeated measures ANOVA",
    # including a two-measurement comparison, which is a paired t-test.
    d <- jw_data()
    label <- function(...) jw_text(jw_run(d, ...)$results$summary$content)

    expect_match(label(typestatistics = "parametric"),    "Paired samples t-test", fixed = TRUE)
    expect_match(label(typestatistics = "nonparametric"), "Wilcoxon signed-rank",  fixed = TRUE)
    expect_false(grepl("ANOVA", label(typestatistics = "parametric"), fixed = TRUE))

    expect_match(label(dep3 = "fup", typestatistics = "parametric"),    "Repeated measures ANOVA", fixed = TRUE)
    expect_match(label(dep3 = "fup", typestatistics = "nonparametric"), "Friedman test",           fixed = TRUE)
})


test_that("missing data is deleted listwise and the exclusion is disclosed", {
    # Pairing only survives if an incomplete SUBJECT is dropped whole. rowid is
    # stamped before naOmit(), so it does - and the count is reported.
    d <- jw_data()
    d$pre[1:3]   <- NA
    d$post[38:40] <- NA
    expect_equal(sum(complete.cases(d[, c("pre", "post")])), 34L)

    a <- jw_run(d)
    long <- a$.__enclos_env__$private$.prepared_data
    expect_equal(nrow(long) / 2, 34)
    # the disclosure lives in `warnings`, not `todo`
    expect_match(jw_text(a$results$warnings$content), "34 subjects retained", fixed = TRUE)
    expect_match(jw_text(a$results$warnings$content), "6 incomplete cases removed", fixed = TRUE)

    # an NA in a column that is not part of the analysis must NOT drop a subject
    d2 <- jw_data(); d2$irrelevant <- c(NA, rep(1, nrow(d2) - 1))
    expect_equal(nrow(jw_run(d2)$.__enclos_env__$private$.prepared_data) / 2, 40)
})


test_that("the visual toggles change the rendered figure", {
    # These set width/alpha to zero rather than dropping the layer, so counting
    # layers proves nothing - compare the rendered pixels.
    skip_if_not_installed("png")
    ink <- function(...) {
        a <- jw_run(jw_data(), ...)
        f <- tempfile(fileext = ".png")
        grDevices::png(f, 700, 550, res = 96)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plot(a$results$plot,
                ggtheme = ggplot2::theme_bw(), theme = NULL), silent = TRUE)
        grDevices::dev.off(); on.exit()
        img <- png::readPNG(f)
        sum(apply(img[, , 1:3], c(1, 2), function(p) any(p < 0.98)))
    }
    base <- ink()
    expect_lt(ink(violin = FALSE),  base)
    expect_lt(ink(boxplot = FALSE), base)
    expect_lt(ink(point = FALSE),   base)
    expect_gt(ink(pointpath = TRUE), base)
    expect_gt(ink(centralityplotting = TRUE), base)
})


test_that("decimal places and confidence level reach the subtitle", {
    d <- jw_data()
    s1 <- jw_subtitle(jw_run(d, resultssubtitle = TRUE, k = 1))
    s4 <- jw_subtitle(jw_run(d, resultssubtitle = TRUE, k = 4))
    expect_match(s4, "-14.4840", fixed = TRUE)
    expect_match(s1, "-14.5", fixed = TRUE)
    expect_false(grepl("-14.4840", s1, fixed = TRUE))

    expect_match(jw_subtitle(jw_run(d, resultssubtitle = TRUE, conflevel = 0.90)), "90%", fixed = TRUE)
    expect_match(jw_subtitle(jw_run(d, resultssubtitle = TRUE, conflevel = 0.99)), "99%", fixed = TRUE)
})


test_that("the clinical presets offer guidance and survive the message reset", {
    # .run() calls resetMessages() and THEN applyClinicalPresets(), which is the
    # correct order - a preset applied only from .init() would be wiped. The
    # presets deliberately do not change any setting; they say so.
    d <- jw_data()
    for (p in c("biomarker", "treatment", "laboratory")) {
        w <- jw_text(jw_run(d, clinicalpreset = p)$results$warnings$content)
        expect_match(w, "preset", ignore.case = TRUE)
    }
    expect_match(jw_text(jw_run(d, clinicalpreset = "biomarker")$results$warnings$content),
                 "Guidance Only", fixed = TRUE)
    # custom adds no preset guidance
    expect_false(grepl("preset:", jw_text(jw_run(d, clinicalpreset = "custom")$results$warnings$content),
                       fixed = TRUE))
})


test_that("no panel makes a causal claim", {
    # A within-subjects design over time is the likeliest place for "treatment
    # improved X" to appear. It must not.
    a <- jw_run(jw_data(), dep3 = "fup", showExplanations = TRUE)
    all_text <- paste(vapply(c("todo", "warnings", "interpretation", "explanations", "summary"),
                             function(p) jw_text(a$results[[p]]$content), character(1)),
                      collapse = " ")
    for (w in c("caused", "causes", "proves", "demonstrates that"))
        expect_false(grepl(w, all_text, ignore.case = TRUE), info = w)
})


test_that("the removed ggstatsplot arguments are gone from the call site", {
    b <- readLines("../../R/jjwithinstats.b.R", warn = FALSE)
    code <- sub("#.*$", "", b)
    expect_false(any(grepl("pairwise.comparisons = opts$pairwisecomparisons", code, fixed = TRUE)))
    expect_false(any(grepl("effsize.type = opts$effsizetype", code, fixed = TRUE)))
    expect_true(any(grepl("pairwise.display = private$.pairwiseDisplay(opts)", code, fixed = TRUE)))
    # confirm against the installed package rather than trusting the comment
    expect_false("pairwise.comparisons" %in% names(formals(ggstatsplot::ggwithinstats)))
    expect_false("effsize.type" %in% names(formals(ggstatsplot::ggwithinstats)))
    expect_true("digits" %in% names(formals(ggstatsplot::ggwithinstats)))
})


test_that("infinite measurements are excluded rather than crashing or poisoning the test", {
    # Two distinct failures, both from is.na() where is.finite() was needed
    # (is.na is TRUE for NaN but FALSE for Inf):
    #   1. the skewness check filtered with !is.na(), so sd() returned NaN and
    #      `if (NaN > 0)` aborted the whole analysis with the unactionable
    #      "missing value where TRUE/FALSE needed";
    #   2. once past that, naOmit() let Inf through into the paired test, which
    #      rendered "t(77) = NA, p = NA" under a panel reassuring the user that
    #      all 78 subjects had been retained.
    d <- jw_data()
    for (bad in list(Inf, -Inf, NaN)) {
        dd <- d; dd$post[1:3] <- bad
        a <- expect_no_error(jw_run(dd, resultssubtitle = TRUE))
        long <- a$.__enclos_env__$private$.prepared_data
        expect_true(all(is.finite(long$value)))
        expect_equal(nrow(long) / 2, 37)          # 40 - 3
        s <- jw_subtitle(a)
        expect_false(grepl('== "NA"', s, fixed = TRUE))
        expect_match(s, "37|36", perl = TRUE)      # df = 36 on 37 pairs
    }
    # Inf is disclosed as a data problem, distinct from ordinary missingness
    dd <- d; dd$post[1:3] <- Inf
    expect_match(jw_text(jw_run(dd)$results$warnings$content),
                 "infinite or undefined measurement", fixed = TRUE)
    # NaN alone is ordinary missingness and needs no such note
    dn <- d; dn$post[1:3] <- NaN
    expect_false(grepl("infinite or undefined measurement",
                       jw_text(jw_run(dn)$results$warnings$content), fixed = TRUE))
    # clean data says nothing about non-finite values
    expect_false(grepl("infinite or undefined measurement",
                       jw_text(jw_run(d)$results$warnings$content), fixed = TRUE))
})


test_that("the ggpubr companion panel does not contradict the main figure", {
    # ggpubr::stat_compare_means has no paired omnibus test, so the panel was
    # annotated with method = "anova" / "kruskal.test" - BETWEEN-subjects tests
    # that discard the subject effect this analysis exists to control for. On a
    # 40-subject fixture that printed "Anova, p = 0.13" (exactly
    # stats::aov(y ~ time)) directly beneath a main panel reporting
    # F_Fisher(1.76, 68.5) = 14.26, p = 1.61e-05: two p-values ~8000x apart in
    # one output window, one of them from an invalid model.
    set.seed(1); n <- 40; subj <- rnorm(n, 0, 5)
    d <- data.frame(pre  = subj + rnorm(n, 10, 2),
                    post = subj + rnorm(n, 12, 2),
                    fup  = subj + rnorm(n, 11, 2))
    long <- data.frame(id = factor(rep(seq_len(n), 3)),
                       t  = factor(rep(c("pre", "post", "fup"), each = n)),
                       y  = c(d$pre, d$post, d$fup))
    p_rm  <- summary(stats::aov(y ~ t + Error(id / t), data = long))[[2]][[1]][1, "Pr(>F)"]
    p_bg  <- summary(stats::aov(y ~ t, data = long))[[1]][1, "Pr(>F)"]
    expect_gt(p_bg / p_rm, 1000)          # the two really are far apart

    gg <- function(ty) {
        a <- jw_run(d, dep3 = "fup", addGGPubrPlot = TRUE, ggpubrAddStats = TRUE,
                    typestatistics = ty)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 700, 550)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot,
                ggtheme = ggplot2::theme_bw(), theme = NULL), silent = TRUE)
        grDevices::dev.off(); on.exit()
        ggplot2::last_plot()
    }
    pp <- gg("parametric")
    expect_match(paste(pp$labels$subtitle, collapse = ""), "Repeated measures ANOVA", fixed = TRUE)
    # the between-groups p must not appear anywhere on the panel
    expect_false(grepl(sprintf("%.2f", p_bg), paste(pp$labels$subtitle, collapse = ""), fixed = TRUE))
    # and no stat_compare_means layer remains
    expect_false("StatCompareMeans" %in% vapply(pp$layers, function(l) class(l$stat)[1], character(1)))

    pn <- gg("nonparametric")
    expect_match(paste(pn$labels$subtitle, collapse = ""), "Friedman test", fixed = TRUE)
})


test_that("the Bayes factor message survives the subtitle takeover", {
    # Taking the subtitle over sets results.subtitle = FALSE, which ALSO removes
    # ggstatsplot's bf.message caption - silently disabling the "Bayes factor
    # message" checkbox. The takeover now stands down when that box is ticked.
    d <- jw_data()
    cap <- function(bm) {
        p <- jw_plot(jw_run(d, typestatistics = "parametric",
                            bfmessage = bm, resultssubtitle = TRUE))
        paste(deparse(p$labels$caption), collapse = "")
    }
    expect_match(cap(TRUE), "BF", fixed = TRUE)
    expect_equal(cap(FALSE), "NULL")
    # and the user is told which options stood down with it
    a <- jw_run(d, typestatistics = "parametric", bfmessage = TRUE, resultssubtitle = TRUE)
    expect_match(jw_text(a$results$warnings$content), "uses the package default", fixed = TRUE)
    # and stays silent when the takeover succeeds
    b <- jw_run(d, typestatistics = "parametric", bfmessage = FALSE, resultssubtitle = TRUE)
    expect_false(grepl("uses the package default",
                       jw_text(b$results$warnings$content), fixed = TRUE))
})


test_that("the ggpubr panel and the main analysis use the same subjects", {
    # The companion rebuilds its own frame from self$data and only naOmit()s it,
    # so infinite rows survived there after being screened from the primary
    # analysis - the two figures were drawn from different subject counts.
    d <- jw_data(); d$post[1:3] <- Inf
    a <- jw_run(d, dep3 = "fup", addGGPubrPlot = TRUE, ggpubrAddStats = TRUE)
    n_primary <- nrow(a$.__enclos_env__$private$.prepared_data) / 3

    f <- tempfile(fileext = ".png"); grDevices::png(f, 700, 550)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    try(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot,
            ggtheme = ggplot2::theme_bw(), theme = NULL), silent = TRUE)
    grDevices::dev.off(); on.exit()
    n_ggpubr <- length(unique(ggplot2::last_plot()$data$Subject_ID))

    expect_equal(n_primary, 37)
    expect_equal(n_ggpubr, n_primary)
})


test_that("every ggpubr plot type BUILDS, not merely prints", {
    # ggpubr's `add = "mean_se"` builds stat_summary(fun.data = "mean_se_") and
    # resolves that name from the SEARCH PATH at draw time. It works in a session
    # that has done library(ggpubr) and fails wherever ggpubr is only
    # namespace-loaded - the module, and jamovi:
    #   Error in stat_summary(): object 'mean_se_' of mode 'function' was not found
    # It fails inside ggplot_build(), so a print()-based check reports success
    # while the panel renders nothing. Build every type, and assert the row
    # counts, or this comes back.
    d <- jw_data()
    build <- function(ty) {
        a <- jw_run(d, dep3 = "fup", addGGPubrPlot = TRUE, ggpubrPlotType = ty)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 700, 550, res = 96)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(suppressWarnings(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot,
                ggtheme = ggplot2::theme_bw(), theme = NULL)), silent = TRUE)
        grDevices::dev.off(); on.exit()
        suppressWarnings(ggplot2::ggplot_build(ggplot2::last_plot()))
    }
    for (ty in c("boxplot", "violin", "paired", "line")) {
        b <- expect_no_error(build(ty))
        rows <- vapply(b$data, nrow, integer(1))
        expect_true(all(rows > 0), info = ty)      # nothing renders empty
    }
    # the line plot is a MEAN trajectory: one point per measurement, with error
    # bars - not 120 raw subject-by-time observations
    rows_line <- vapply(build("line")$data, nrow, integer(1))
    expect_true(all(rows_line == 3))
})


test_that("the ggpubr palette applies where it can and is documented where it cannot", {
    skip_if_not_installed("png")
    d <- jw_data()
    md5 <- function(ty, pal) {
        a <- jw_run(d, dep3 = "fup", addGGPubrPlot = TRUE,
                    ggpubrPlotType = ty, ggpubrPalette = pal)
        f <- tempfile(fileext = ".png"); grDevices::png(f, 700, 550, res = 96)
        on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
        try(suppressWarnings(a$.__enclos_env__$private$.plotGGPubr(a$results$ggpubrPlot,
                ggtheme = ggplot2::theme_bw(), theme = NULL)), silent = TRUE)
        grDevices::dev.off(); on.exit()
        unname(tools::md5sum(f))
    }
    # `palette` needs a fill/colour aesthetic to act on; without one every choice
    # rendered a byte-identical figure.
    for (ty in c("boxplot", "violin", "paired"))
        expect_false(md5(ty, "jco") == md5(ty, "lancet"), info = ty)
    # the line plot draws ONE mean trajectory, so a categorical palette has
    # nothing to colour - stated in the option description rather than faked
    expect_true(md5("line", "jco") == md5("line", "lancet"))
    y <- readLines("../../jamovi/jjwithinstats.a.yaml", warn = FALSE)
    expect_true(any(grepl("the line plot draws a single mean trajectory", y, fixed = TRUE)))
})


test_that("data-quality warnings reach the panel and degenerate input is rejected", {
    d <- jw_data()

    # >50% missing was written with a direct setContent() that the accumulator
    # overwrote moments later, so the user never saw it
    dm <- d; dm$post[1:25] <- NA
    expect_match(jw_text(jw_run(dm)$results$warnings$content), "High Missing Data Rate")
    expect_match(jw_text(jw_run(dm)$results$warnings$content), "62.5%", fixed = TRUE)

    # the same variable twice crashed inside pivot_longer with
    # "factor level [2] is duplicated", uncaught, straight from .init()
    expect_error(jw_run(d, dep2 = "pre"), "different variable")
    expect_error(jw_run(d, dep2 = "pre"), "pre", fixed = TRUE)
})


test_that("sampling-based output is reproducible across renders", {
    # BayesFactor MCMC and the robust bootstrap made the same analysis report
    # different numbers on every refresh.
    d <- jw_data()
    for (ty in c("bayes", "robust")) {
        s1 <- jw_subtitle(jw_run(d, typestatistics = ty, resultssubtitle = TRUE))
        s2 <- jw_subtitle(jw_run(d, typestatistics = ty, resultssubtitle = TRUE))
        expect_identical(s1, s2, info = ty)
    }
})


test_that("individual trajectories are drawn at every number of measurements", {
    # ggwithinstats draws point.path only for two measurements, so the checkbox
    # did nothing with 3 or 4 - exactly where following a subject matters most.
    d <- jw_data()
    expect_true("GeomPath" %in% jw_geoms(jw_run(d, pointpath = TRUE)))
    expect_true("GeomPath" %in% jw_geoms(jw_run(d, dep3 = "fup", pointpath = TRUE)))
    expect_false("GeomPath" %in% jw_geoms(jw_run(d, dep3 = "fup", pointpath = FALSE)))
})
