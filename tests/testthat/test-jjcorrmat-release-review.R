# Release-review regression tests for jjcorrmat.
#
# The theme of this file is a single defect class: the correlation TABLE and the
# clinical INTERPRETATION were computed by a hand-rolled stats::cor.test loop,
# while the PLOT was drawn by ggstatsplot::ggcorrmat. The two engines disagreed
# on the robust method, on Bayesian analysis, on partial correlations and on
# p-value adjustment, so the same output panel could show a coefficient of -0.41
# beside a plotted cell of -0.10 for the same pair of variables.
#
# Every expectation below was checked against ggcorrmat's own internal call
# before being written down.

library(testthat)

cm_data <- function(n = 60, seed = 42) {
    set.seed(seed)
    d <- data.frame(a = rnorm(n))
    d$b <- 0.6 * d$a + rnorm(n, 0, 0.8)
    d$c <- -0.4 * d$a + rnorm(n, 0, 0.9)
    d$e <- rnorm(n)
    d$g <- factor(rep(c("X", "Y"), length.out = n))
    d
}
cm_vars <- c("a", "b", "c", "e")

# Build and run the analysis object directly so the private engine is reachable.
cm_run <- function(data = cm_data(), ...) {
    opts <- do.call(ClinicoPath:::jjcorrmatOptions$new,
                    utils::modifyList(list(dep = cm_vars), list(...)))
    a <- ClinicoPath:::jjcorrmatClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
cm_engine <- function(a) {
    p <- a$.__enclos_env__$private
    p$.computeCorrelations(p$.prepareData(), p$.prepareOptions())
}
# The exact call ggstatsplot::ggcorrmat 1.0.0 makes internally.
cm_ggcorrmat_ref <- function(d, type = "parametric", partial = FALSE,
                             p_adjust = "holm", ci = 0.95) {
    # The Bayesian branch samples, so the reference is drawn under the same
    # fixed seed the backend uses for both the table and the plot.
    seed <- ClinicoPath:::jjcorrmatClass$private_fields$.BAYES_SEED
    withr::with_seed(seed, as.data.frame(correlation::correlation(
        data             = d[cm_vars],
        method           = if (type == "nonparametric") "spearman" else "pearson",
        p_adjust         = p_adjust,
        ci               = ci,
        bayesian         = type == "bayes",
        bayesian_prior   = 0.707,
        partial          = partial,
        partial_bayesian = type == "bayes" && partial,
        winsorize        = if (type == "robust") 0.2 else FALSE
    )))
}


test_that("the robust method is Winsorized Pearson, and the table reports it", {
    # ggstatsplot 1.0.0 maps type = "robust" to correlation::correlation(
    # winsorize = 0.2), i.e. WRS2::wincor(tr = 0.2). Older versions used the
    # percentage-bend coefficient (WRS2::pbcor), which this module's option
    # description, interpretation text and assumptions panel all still claimed.
    # On the fixture the three estimates for a~b are:
    #   Winsorized  0.558003   <- what the plot draws
    #   pct. bend   0.654951
    #   Pearson     0.693412   <- what the table used to print, labelled "robust"
    d <- cm_data()
    a <- cm_run(d, typestatistics = "robust")
    got <- cm_engine(a)
    ref <- cm_ggcorrmat_ref(d, type = "robust")

    expect_equal(got$r, ref$r, tolerance = 1e-12)
    expect_false(isTRUE(all.equal(got$r[1], cor(d$a, d$b))))
    expect_equal(got$r[1], WRS2::wincor(d$a, d$b, tr = 0.2)$cor, tolerance = 1e-10)

    tab <- as.data.frame(a$results$table)
    expect_equal(tab$r, ref$r, tolerance = 1e-12)
    expect_true(all(tab$method == "Winsorized Pearson correlation"))
})


test_that("partial correlations reach the table and the interpretation", {
    # `partial` was passed to ggcorrmat but never to the table's own cor.test
    # loop, so the panel showed zero-order values next to a partial plot. For
    # b~c the two differ by a factor of four (-0.4105 vs -0.1012).
    d <- cm_data()
    a <- cm_run(d, partial = TRUE)
    ref <- cm_ggcorrmat_ref(d, partial = TRUE)

    expect_equal(cm_engine(a)$r, ref$r, tolerance = 1e-12)
    expect_equal(as.data.frame(a$results$table)$r, ref$r, tolerance = 1e-12)

    zero_order <- cm_ggcorrmat_ref(d, partial = FALSE)
    expect_false(isTRUE(all.equal(ref$r, zero_order$r)))

    # The interpretation used to bail out entirely on every partial analysis.
    a2 <- cm_run(d, partial = TRUE, showexplanations = TRUE)
    expect_false(grepl("Unable to calculate correlations",
                       a2$results$interpretation$content, fixed = TRUE))
    expect_match(a2$results$interpretation$content, "Partial")
})


test_that("Bayesian analysis reports a Bayes factor, not a Pearson p-value", {
    d <- cm_data()
    a <- cm_run(d, typestatistics = "bayes")
    got <- cm_engine(a)
    ref <- cm_ggcorrmat_ref(d, type = "bayes")

    expect_equal(got$r, ref$rho, tolerance = 1e-12)
    expect_equal(got$bf, ref$BF, tolerance = 1e-8)
    # There is no p-value in a Bayesian analysis; the table used to print
    # Pearson's under the label "bayes".
    expect_true(all(is.na(got$p)))
    expect_true(all(is.na(got$p_adj)))
    expect_true(all(as.data.frame(a$results$table)$method == "Bayesian Pearson correlation"))
})


test_that("the table carries the adjusted p-value the plot marks cells with", {
    # ggcorrmat crosses out cells using correlation::correlation's ADJUSTED p.
    # The table reported the raw p with no adjustment and no note, so a cell
    # crossed out in the figure could sit beside a "significant" table row.
    d <- cm_data()
    for (m in c("holm", "bonferroni", "BH")) {
        a <- cm_run(d, padjustmethod = m)
        got <- cm_engine(a)
        ref <- cm_ggcorrmat_ref(d, p_adjust = m)
        expect_equal(got$p_adj, ref$p, tolerance = 1e-12)
        expect_equal(got$p_adj, stats::p.adjust(got$p, method = m), tolerance = 1e-12)
    }
    # bonferroni on this fixture: a~e goes 0.47611 -> 1.00000, b~c 0.00112 -> 0.00674
    a <- cm_run(d, padjustmethod = "bonferroni")
    got <- cm_engine(a)
    expect_true(all(got$p_adj >= got$p - 1e-12))

    # and the table says which correction was used
    expect_match(a$results$table$notes$padj$note, "Bonferroni")
    a_none <- cm_run(d, padjustmethod = "none")
    expect_match(a_none$results$table$notes$padj$note, "No correction")
})


test_that("confidence intervals are present for every method", {
    # stats::cor.test returns no conf.int for Spearman, so the CI columns were
    # blank for the nonparametric method and showed Pearson intervals - for a
    # coefficient that was never Pearson - under robust and bayes.
    d <- cm_data()
    for (ty in c("parametric", "nonparametric", "robust", "bayes")) {
        got <- cm_engine(cm_run(d, typestatistics = ty))
        expect_false(any(is.na(got$conf_low)), info = ty)
        expect_false(any(is.na(got$conf_high)), info = ty)
        expect_true(all(got$conf_low <= got$r & got$r <= got$conf_high), info = ty)
        expect_equal(got$conf_low, cm_ggcorrmat_ref(d, type = ty)$CI_low,
                     tolerance = 1e-12)
    }
    # and the interval honours the user's confidence level
    wide   <- cm_engine(cm_run(d, conflevel = 0.99))
    narrow <- cm_engine(cm_run(d, conflevel = 0.80))
    expect_true(all(wide$conf_low < narrow$conf_low))
    expect_true(all(wide$conf_high > narrow$conf_high))
})


test_that("the table reports N per pair", {
    # Under pairwise handling each pair uses a different number of rows. With
    # no N column the reader could not tell a correlation on 50 observations
    # from one on 60.
    d <- cm_data()
    d$b[1:10] <- NA
    got <- cm_engine(cm_run(d, naHandling = "pairwise"))
    n_by_pair <- stats::setNames(got$n, paste(got$var1, got$var2))
    expect_equal(unname(n_by_pair[["a b"]]), 50L)
    expect_equal(unname(n_by_pair[["a c"]]), 60L)
    expect_equal(unname(n_by_pair[["c e"]]), 60L)

    # listwise drops the rows once, so every pair shares the same N
    got_lw <- cm_engine(cm_run(d, naHandling = "listwise"))
    expect_true(all(got_lw$n == 50L))
})


test_that("a missing grouping value does not become a group of NA correlations", {
    # unique() on the grouping column included NA; `x == NA` is all-NA, and
    # NA-index subsetting produced a data frame of NA rows, so the table gained
    # a phantom "NA" block whose every coefficient was NA. Only reachable under
    # pairwise handling, since listwise drops those rows in .prepareData.
    d <- cm_data()
    d$g[c(1, 5, 9)] <- NA
    a <- cm_run(d, grvar = "g", naHandling = "pairwise")
    tab <- as.data.frame(a$results$table)

    expect_setequal(unique(tab$group), c("X", "Y"))
    expect_false(any(is.na(tab$r)))
    expect_match(a$results$warnings$content, "no value for the grouping variable")
})


test_that("decimal places do not destroy the table", {
    # r was written to the table as round(r, k). At k = 0 every coefficient
    # collapsed to 1, -1 or 0. k governs the labels drawn inside the plot; the
    # table is formatted by jamovi.
    d <- cm_data()
    ref <- cm_ggcorrmat_ref(d)$r
    for (k in c(0L, 2L, 5L))
        expect_equal(as.data.frame(cm_run(d, k = k)$results$table)$r, ref,
                     tolerance = 1e-12, info = paste("k =", k))
})


test_that("the completion notice actually reaches the user", {
    # It was appended to private$.warnings from .plot(), which runs after
    # .displayWarnings() has already consumed the list, so it never rendered.
    a <- cm_run()
    expect_true(a$results$warnings$visible)
    expect_match(a$results$warnings$content, "Computed 6 zero-order Pearson correlations")

    a_partial <- cm_run(partial = TRUE)
    expect_match(a_partial$results$warnings$content, "partial")
})


test_that("the interpretation names the coefficient it is reporting", {
    d <- cm_data()
    labels <- c(parametric    = "Pearson correlation",
                nonparametric = "Spearman correlation",
                robust        = "Winsorized Pearson correlation",
                bayes         = "Bayesian Pearson correlation")
    for (ty in names(labels)) {
        txt <- cm_run(d, typestatistics = ty, showexplanations = TRUE)$results$interpretation$content
        expect_match(txt, labels[[ty]], fixed = TRUE, info = ty)
    }
    # "r =" was printed whatever the method
    spearman <- cm_run(d, typestatistics = "nonparametric", showexplanations = TRUE)
    expect_match(spearman$results$interpretation$content, "rho", fixed = TRUE)
})


test_that("the interpretation counts significance at the chosen level, on adjusted p", {
    # It compared raw p against a hard-coded 0.05 while the plot marked cells
    # using the adjusted p at the user's significance level.
    d <- cm_data()
    n_sig <- function(a) {
        txt <- gsub("<[^>]+>", " ", a$results$interpretation$content)
        m <- regmatches(txt, regexpr("Significant correlations[^:]*: *[0-9]+", txt))
        as.integer(sub(".*: *", "", m))
    }
    loose  <- cm_run(d, siglevel = 0.05,  padjustmethod = "none",       showexplanations = TRUE)
    strict <- cm_run(d, siglevel = 0.001, padjustmethod = "bonferroni", showexplanations = TRUE)
    expect_gt(n_sig(loose), n_sig(strict))

    # the text states the threshold and the correction actually in force
    expect_match(strict$results$interpretation$content, "0.001", fixed = TRUE)
    expect_match(strict$results$interpretation$content, "Bonferroni", fixed = TRUE)
    expect_match(loose$results$interpretation$content, "unadjusted", fixed = TRUE)
})


test_that("the grouped plot is sized from the resolved grouping column", {
    # .init indexed self$data with the raw option name while every other code
    # path resolved it through .resolveName. When jamovi B64-encodes the name
    # the lookup returns NULL, nlevels(as.factor(NULL)) is 0, and the grouped
    # plot was sized to zero width.
    d <- cm_data()
    a <- cm_run(d, grvar = "g", plotwidth = 500L, plotheight = 400L)
    expect_equal(a$results$plot2$width, 2L * 500L)
    expect_equal(a$results$plot2$height, 400L)
    expect_gt(a$results$plot2$width, 0L)
})


test_that("the documented robust method matches the implemented one", {
    # These three strings all said "percentage bend"; the source of truth is
    # correlation::correlation's own Method label.
    b <- readLines("../../R/jjcorrmat.b.R", warn = FALSE)
    expect_false(any(grepl("percentage bend", b, fixed = TRUE)))
    expect_true(any(grepl("Winsorized Pearson", b, fixed = TRUE)))
    # `beta` was the bending constant of the percentage-bend coefficient and is
    # silently discarded by ggstatsplot 1.0.0, which has no such formal.
    code <- sub("#.*$", "", b)   # the string survives only in an explanatory comment
    expect_false(any(grepl("beta = 0.1", code, fixed = TRUE)))
    expect_false("beta" %in% names(formals(ggstatsplot::ggcorrmat)))

    y <- readLines("../../jamovi/jjcorrmat.a.yaml", warn = FALSE)
    expect_false(any(grepl("^\\s+percentage bend correlation", y)))
})


test_that("a Bayesian correlation matrix is reproducible", {
    # correlation::correlation(bayesian = TRUE) draws posterior samples through
    # BayesFactor. Unseeded, two calls on the same data disagree in the third
    # decimal - so the table and the plot were independent draws, and simply
    # re-running the analysis changed the reported estimate and its interval.
    d <- cm_data()
    r1 <- cm_engine(cm_run(d, typestatistics = "bayes"))
    r2 <- cm_engine(cm_run(d, typestatistics = "bayes"))
    expect_identical(r1$r, r2$r)
    expect_identical(r1$conf_low, r2$conf_low)
    expect_identical(r1$bf, r2$bf)

    # unseeded, the same call is NOT reproducible - this is what is being fixed
    u1 <- correlation::correlation(d[cm_vars], bayesian = TRUE)$rho
    u2 <- correlation::correlation(d[cm_vars], bayesian = TRUE)$rho
    expect_false(identical(u1, u2))

    # non-Bayesian methods are deterministic and must not have their RNG touched
    set.seed(7); before <- runif(1)
    set.seed(7); invisible(cm_engine(cm_run(d, typestatistics = "parametric")))
    expect_equal(runif(1), before)
})


test_that("the missing-data exclusion count survives to the user", {
    # .prepareData() wrote "N rows excluded due to missing values" into the
    # `todo` panel, and .prepareOptions() overwrote it with "Preparing
    # correlation analysis options..." on the very next line of .run(). The
    # count never reached the screen and the panel was left showing a progress
    # string. It is now a notice, emitted from .run() so a cache hit in the
    # memoised .prepareData() cannot swallow it.
    d <- cm_data()
    d$b[1:15] <- NA
    a <- cm_run(d, naHandling = "listwise")
    expect_match(a$results$warnings$content, "15 of 60 rows (25%) were excluded", fixed = TRUE)
    expect_match(a$results$warnings$content, "45 rows were analysed", fixed = TRUE)
    expect_false(grepl("Preparing correlation analysis options",
                       a$results$todo$content, fixed = TRUE))

    # a second run on the same object hits the .prepareData cache; the message
    # must still be there
    a$.__enclos_env__$private$.run()
    expect_match(a$results$warnings$content, "15 of 60 rows", fixed = TRUE)

    # pairwise keeps every row, so there is nothing to disclose
    b <- cm_run(d, naHandling = "pairwise")
    expect_false(grepl("were excluded because they had a missing value",
                       b$results$warnings$content, fixed = TRUE))
})
