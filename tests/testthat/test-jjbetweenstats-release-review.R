# Release-review regression tests for jjbetweenstats.
#
# The dominant defect class here was inert controls. ggstatsplot 1.0.0 removed
# `var.equal`, `effsize.type`, `pairwise.comparisons` and `k` from
# ggbetweenstats; they are silently swallowed by `...`. Four options therefore
# did nothing - while the module's own narrative panels went on switching the
# reported TEST NAME on one of them, so the Results Summary could say
# "Student's t-test" over a figure showing Welch's df.
#
# Every expectation below was observed failing on the unfixed code first.
#
# NOTE ON THE DEV HARNESS: devtools::load_all() attaches package:ClinicoPath
# ahead of package:stats on the search path, and ClinicoPath re-exports `aov`
# (from DoE.base). statsExpressions' ANOVA path then resolves the wrong `aov`
# and errors with "a two-sided formula is required". That is a dev/`library()`
# artefact - jamovi loads the namespace without attaching it - and the analysis
# degrades gracefully to ggstatsplot's own subtitle when it happens. The 3+
# group subtitle assertions are therefore skipped when the masking is active;
# the two-group path is unaffected and is asserted in full.

library(testthat)

jb_2group <- function(seed = 42) {
    set.seed(seed)
    data.frame(y = c(rnorm(30, 0, 1), rnorm(30, 0.8, 2.2)),
               g = factor(rep(c("A", "B"), each = 30)))
}
jb_3group <- function(seed = 7) {
    set.seed(seed)
    data.frame(y = c(rnorm(25, 0, 1), rnorm(25, 1.2, 1), rnorm(25, 2.0, 3)),
               g = factor(rep(c("A", "B", "C"), each = 25)),
               s = factor(rep(c("s1", "s2"), length.out = 75)))
}

jb_run <- function(data = jb_2group(), ...) {
    opts <- do.call(ClinicoPath:::jjbetweenstatsOptions$new,
                    utils::modifyList(list(dep = "y", group = "g"), list(...)))
    a <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = data)
    a$init()
    a$.__enclos_env__$private$.run()
    a
}
jb_plot <- function(a, method = ".plot") {
    f <- tempfile(fileext = ".png")
    grDevices::png(f, 700, 550)
    on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
    try(a$.__enclos_env__$private[[method]](a$results$plot,
            ggtheme = ggplot2::theme_bw(), theme = NULL), silent = TRUE)
    grDevices::dev.off(); on.exit()
    ggplot2::last_plot()
}
jb_subtitle <- function(a) paste(deparse(jb_plot(a)$labels$subtitle), collapse = "")
jb_geoms <- function(a) vapply(jb_plot(a)$layers, function(l) class(l$geom)[1], character(1))
jb_text <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", html))

# TRUE when the attached ClinicoPath has shadowed the stats ANOVA machinery.
# An attached ClinicoPath masks aov, oneway.test, terms, model.frame, formula
# and t.test, so probe the capability itself rather than one symbol.
# TRUE when the RAW stats::oneway.test path is broken by formula.tools. This is
# now expected to be TRUE in any ClinicoPath session and is NO LONGER a reason to
# skip: the analysis wraps its calls in withBaseFormulaChar(), so it works anyway.
# Kept only to document the environment the tests run in.
jb_anova_broken <- function() {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9), g = factor(rep(1:3, 3)))
    inherits(try(stats::oneway.test(y ~ g, data = d), silent = TRUE), "try-error")
}

# Reference values must be computed with base behaviour restored, or they hit the
# very bug the module now shields against. Same mechanism as R/utils-ggstatsplot.R.
jb_base_formula <- function(expr) {
    tbl <- get(".__S3MethodsTable__.", envir = asNamespace("base"))
    if (exists("as.character.formula", envir = tbl, inherits = FALSE)) {
        old <- get("as.character.formula", envir = tbl, inherits = FALSE)
        assign("as.character.formula", function(x, ...) as.character(unclass(x)), envir = tbl)
        on.exit(assign("as.character.formula", old, envir = tbl), add = TRUE)
    }
    force(expr)
}


test_that("Equal variances reaches the test instead of only the prose", {
    # ggbetweenstats 1.0.0 has no `var.equal` formal, so the plot was ALWAYS
    # Welch while .generateClinicalSummary switched between "Student's t-test"
    # and "Welch's t-test" on the checkbox. The subtitle is now computed by
    # statsExpressions, which still honours it.
    d <- jb_2group()
    s_student <- jb_subtitle(jb_run(d, varequal = TRUE,  resultssubtitle = TRUE))
    s_welch   <- jb_subtitle(jb_run(d, varequal = FALSE, resultssubtitle = TRUE))

    expect_false(identical(s_student, s_welch))
    expect_match(s_student, "Student", fixed = TRUE)
    expect_match(s_welch,   "Welch",   fixed = TRUE)

    # and the df match stats::t.test exactly
    tt <- stats::t.test(y ~ g, data = d, var.equal = TRUE)
    tw <- stats::t.test(y ~ g, data = d, var.equal = FALSE)
    expect_equal(unname(tt$parameter), 58)
    expect_match(s_student, "58", fixed = TRUE)
    # NB sprintf, not format(): an attached ClinicoPath masks base::format with
    # jmvcore::format, which returns a number rather than a string.
    expect_match(s_welch, sprintf("%.2f", unname(tw$parameter)), fixed = TRUE)
})


test_that("Effect size type reaches the effect size", {
    # All four choices produced a byte-identical Hedges' g subtitle.
    d <- jb_2group()
    eff <- function(v) {
        s <- jb_subtitle(jb_run(d, effsizetype = v, resultssubtitle = TRUE))
        regmatches(s, regexpr("widehat[^,]*", s))
    }
    expect_match(eff("biased"),   "Cohen",  fixed = TRUE)
    expect_match(eff("unbiased"), "Hedges", fixed = TRUE)
    expect_false(identical(eff("biased"), eff("unbiased")))
    # two_sample_test rejects the ANOVA-only names, so they are mapped onto the
    # equivalent two-group family rather than erroring
    expect_match(eff("eta"),   "Cohen",  fixed = TRUE)
    expect_match(eff("omega"), "Hedges", fixed = TRUE)
})


test_that("Decimal places changes the printed precision", {
    d <- jb_2group()
    s1 <- jb_subtitle(jb_run(d, k = 1, resultssubtitle = TRUE))
    s4 <- jb_subtitle(jb_run(d, k = 4, resultssubtitle = TRUE))
    expect_false(identical(s1, s4))
    # 4 decimal places on the Welch df, 1 on the same quantity
    expect_match(s4, "44.7467", fixed = TRUE)
    expect_match(s1, "44.7",    fixed = TRUE)
    expect_false(grepl("44.7467", s1, fixed = TRUE))
})


test_that("Pairwise comparisons can actually be switched off", {
    # `pairwise.comparisons` was removed in ggstatsplot 1.0.0, so unticking the
    # box left the significance brackets on the plot. The surviving control is
    # pairwise.display, which accepts "none".
    d <- jb_3group()
    on_geoms  <- jb_geoms(jb_run(d, pairwisecomparisons = TRUE,  resultssubtitle = TRUE))
    off_geoms <- jb_geoms(jb_run(d, pairwisecomparisons = FALSE, resultssubtitle = TRUE))
    expect_true("GeomSignif" %in% on_geoms)
    expect_false("GeomSignif" %in% off_geoms)
})


test_that("Levene's warning reaches the user who ticked Equal variances", {
    # The guard was `levene_p < 0.05 && !varequal`, which hid the warning from
    # the only person it was for: the one about to report Student's test on
    # heteroscedastic data.
    d <- jb_2group()
    levene_p <- car::leveneTest(d$y ~ d$g, center = median)$`Pr(>F)`[1]
    expect_lt(levene_p, 0.05)   # the fixture really is heteroscedastic

    for (ve in c(FALSE, TRUE)) {
        diag <- jb_text(jb_run(d, varequal = ve, typestatistics = "parametric")$results$diagnostics$content)
        expect_match(diag, "Variances differ significantly", fixed = TRUE)
    }
    # and ticking the box adds the consequence, not silence
    ticked <- jb_text(jb_run(d, varequal = TRUE)$results$diagnostics$content)
    expect_match(ticked, "'Equal variances' is ticked", fixed = TRUE)
    expect_false(grepl("'Equal variances' is ticked",
                       jb_text(jb_run(d, varequal = FALSE)$results$diagnostics$content), fixed = TRUE))
})


test_that("an empty grouping level does not switch assumption checking off", {
    # min(table(group)) was 0 for a level with no rows - which is what happens
    # when one arm has no outcome data - and `min_group_size >= 3` then skipped
    # Levene's and Shapiro-Wilk entirely, so the panel fell silent exactly when
    # the data most needed checking.
    d <- jb_2group()
    d$g <- factor(d$g, levels = c("A", "B", "C"))   # C is empty
    expect_equal(unname(table(d$g)[["C"]]), 0L)

    diag <- jb_text(jb_run(d, typestatistics = "parametric")$results$diagnostics$content)
    expect_match(diag, "Variances differ significantly", fixed = TRUE)

    # the empty level must not be counted as a group in the narrative either
    expect_match(jb_text(jb_run(d)$results$clinicalSummary$content),
                 "across 2 group", fixed = TRUE)
})


test_that("the outlier count is a count, not a row index", {
    # .detectOutliers returns a COUNT above 5000 rows and ROW INDICES below it.
    # A single index is a length-1 numeric, so it was reported as the count:
    # one outlier sitting at row 40 was announced as "40 potential outlier(s)".
    set.seed(11)
    d <- data.frame(y = c(rnorm(39), 40), g = factor(rep(c("A", "B"), each = 20)))
    q <- stats::quantile(d$y, c(0.25, 0.75)); iqr <- q[2] - q[1]
    n_true <- sum(d$y < q[1] - 1.5 * iqr | d$y > q[2] + 1.5 * iqr)

    diag <- jb_text(jb_run(d)$results$diagnostics$content)
    expect_match(diag, paste0("y has ", n_true, " potential outlier"), fixed = TRUE)
    # the planted value sits at row 40; that number must not appear as a count
    expect_false(grepl("y has 40 potential outlier", diag, fixed = TRUE))
})


test_that("the narrative names the test that was actually run", {
    d <- jb_2group()
    # a two-group comparison is not an ANOVA - both panels used to say so
    for (panel in c("clinicalSummary", "summary")) {
        txt <- jb_text(jb_run(d, showexplanations = TRUE)$results[[panel]]$content)
        expect_false(grepl("ANOVA", txt, fixed = TRUE), info = panel)
        expect_match(txt, "t-test", fixed = TRUE)
    }
    # and it tracks the equal-variances choice, which is now real
    expect_match(jb_text(jb_run(d, varequal = TRUE)$results$clinicalSummary$content),
                 "Student's t-test", fixed = TRUE)
    expect_match(jb_text(jb_run(d, varequal = FALSE)$results$clinicalSummary$content),
                 "Welch's t-test", fixed = TRUE)
    # three groups do get the ANOVA family
    expect_match(jb_text(jb_run(jb_3group())$results$clinicalSummary$content),
                 "ANOVA", fixed = TRUE)
})


test_that("the summary does not point at a subtitle that is switched off", {
    # `resultssubtitle` defaults to FALSE, so the default output carries no
    # statistic anywhere - yet the Results Summary said "See the plot subtitle
    # for the test statistic, p-value, and effect size."
    d <- jb_2group()
    expect_false(ClinicoPath:::jjbetweenstatsOptions$new(dep = "y", group = "g")$resultssubtitle)

    off <- jb_text(jb_run(d, resultssubtitle = FALSE)$results$clinicalSummary$content)
    expect_false(grepl("See the plot subtitle", off, fixed = TRUE))
    expect_match(off, "No test statistic is displayed", fixed = TRUE)

    on <- jb_text(jb_run(d, resultssubtitle = TRUE)$results$clinicalSummary$content)
    expect_match(on, "See the plot subtitle", fixed = TRUE)
})


test_that("the Split By figure discloses which options it cannot honour", {
    # grouped_ggbetweenstats computes its own per-panel subtitle, so the
    # statsExpressions subtitle that carries varequal/effsizetype cannot be
    # applied there. Say so rather than let the two figures disagree in silence.
    d <- jb_3group()
    note <- "apply to the main figure only"
    expect_match(jb_text(jb_run(d, grvar = "s", varequal = TRUE)$results$diagnostics$content),
                 note, fixed = TRUE)
    expect_match(jb_text(jb_run(d, grvar = "s", effsizetype = "omega")$results$diagnostics$content),
                 note, fixed = TRUE)
    # nothing to disclose without a Split By variable, or at the defaults
    expect_false(grepl(note, jb_text(jb_run(d, varequal = TRUE)$results$diagnostics$content), fixed = TRUE))
    expect_false(grepl(note, jb_text(jb_run(d, grvar = "s")$results$diagnostics$content), fixed = TRUE))
})


test_that("the ggpubr panel's visibility expression can be resolved", {
    # `visible: (addGGPubrPlot && grvar)` evaluated `TRUE && "site"`, which is an
    # R error ("invalid 'y' type in 'x && y'"). jamovi surfaced it as
    # "Could not resolve 'addGGPubrPlot && grvar'" and ABORTED THE WHOLE
    # ANALYSIS - so ticking "Add ggpubr plot variant" produced no output at all.
    y <- readLines("../../jamovi/jjbetweenstats.r.yaml", warn = FALSE)
    expect_false(any(grepl("visible: (addGGPubrPlot && grvar)", y, fixed = TRUE)))
    expect_true(any(grepl("visible: (length(grvar) > 0 && addGGPubrPlot)", y, fixed = TRUE)))

    # the replacement is a valid R expression for every combination of values
    for (gv in list(NULL, "site")) for (add in c(TRUE, FALSE)) {
        grvar <- gv; addGGPubrPlot <- add
        expect_type(length(grvar) > 0 && addGGPubrPlot, "logical")
    }
    # and the old form really was an error, not merely ugly
    grvar <- "site"; addGGPubrPlot <- TRUE
    expect_error(addGGPubrPlot && grvar, "invalid 'y' type")
})


test_that("the subtitle falls back gracefully when statsExpressions cannot run", {
    # .subtitleExpr returns NULL rather than propagating an error, and the
    # caller then leaves ggstatsplot to produce its own subtitle. Bayesian is
    # the permanent case (statsExpressions errors on that combination).
    d <- jb_2group()
    a <- jb_run(d, typestatistics = "bayes", resultssubtitle = TRUE)
    p <- a$.__enclos_env__$private
    expect_null(p$.subtitleExpr(p$.prepareData(), "g", "y", p$.prepareOptions()))
    expect_match(jb_subtitle(a), "log", fixed = TRUE)   # ggstatsplot's own BF subtitle

    # subtitles switched off -> nothing computed
    b <- jb_run(d, resultssubtitle = FALSE)
    pb <- b$.__enclos_env__$private
    expect_null(pb$.subtitleExpr(pb$.prepareData(), "g", "y", pb$.prepareOptions()))
})


test_that("three or more groups get the same honoured options", {
    # No longer skipped. This used to be gated on jb_anova_broken() with a reason
    # blaming namespace shadowing - the real cause was formula.tools breaking
    # stats::oneway.test, and the analysis now shields against it, so the
    # three-or-more-group path is expected to work in every session.
    d <- jb_3group()
    s_fisher <- jb_subtitle(jb_run(d, varequal = TRUE,  resultssubtitle = TRUE))
    s_welch  <- jb_subtitle(jb_run(d, varequal = FALSE, resultssubtitle = TRUE))
    expect_match(s_fisher, "Fisher", fixed = TRUE)
    expect_match(s_welch,  "Welch",  fixed = TRUE)

    ref_f <- jb_base_formula(stats::oneway.test(y ~ g, data = d, var.equal = TRUE))
    ref_w <- jb_base_formula(stats::oneway.test(y ~ g, data = d, var.equal = FALSE))
    expect_match(s_fisher, sprintf("%.2f", unname(ref_f$statistic)), fixed = TRUE)
    expect_match(s_welch,  sprintf("%.2f", unname(ref_w$statistic)), fixed = TRUE)
})


test_that("the dead ggstatsplot arguments are gone from the call sites", {
    # These four were passed to every ggbetweenstats call and silently absorbed
    # by `...`; keeping them invites the next reader to believe they work.
    b <- readLines("../../R/jjbetweenstats.b.R", warn = FALSE)
    code <- sub("#.*$", "", b)
    expect_false(any(grepl("pairwise.comparisons = opts$pairwisecomparisons", code, fixed = TRUE)))
    expect_true(any(grepl("pairwise.display = private$.pairwiseDisplay(opts)", code, fixed = TRUE)))
    # `digits` is the surviving name for k
    expect_true(any(grepl("digits = opts$k", code, fixed = TRUE)))
    expect_false("var.equal" %in% names(formals(ggstatsplot::ggbetweenstats)))
    expect_false("effsize.type" %in% names(formals(ggstatsplot::ggbetweenstats)))
    expect_true("digits" %in% names(formals(ggstatsplot::ggbetweenstats)))
})


test_that("the shield removes the fallback instead of merely disclosing it", {
    # This test used to assert the OPPOSITE: that when statsExpressions' ANOVA
    # path died, a notice fired saying the Equal variances / Effect size type /
    # Decimal places choices had been ignored. The cause was formula.tools
    # breaking stats::oneway.test, and R/utils-ggstatsplot.R now shields every
    # call, so there is no fallback left to disclose - the options are simply
    # honoured. Assert the better contract.
    fired <- function(...) grepl("fell back to the package default",
                                 jb_text(jb_run(...)$results$diagnostics$content), fixed = TRUE)

    expect_false(fired(jb_3group(), resultssubtitle = TRUE, varequal = TRUE))
    expect_false(fired(jb_2group(), resultssubtitle = TRUE, varequal = TRUE))
    expect_false(fired(jb_3group(), resultssubtitle = FALSE))
    expect_false(fired(jb_3group(), resultssubtitle = TRUE, typestatistics = "bayes"))

    # ...and the three options it used to apologise for now actually take effect
    # on three groups, which is what the apology was standing in for.
    d <- jb_3group()
    expect_match(jb_subtitle(jb_run(d, resultssubtitle = TRUE, varequal = TRUE)),  "Fisher", fixed = TRUE)
    expect_match(jb_subtitle(jb_run(d, resultssubtitle = TRUE, varequal = FALSE)), "Welch",  fixed = TRUE)
    expect_false(identical(
        jb_subtitle(jb_run(d, resultssubtitle = TRUE, effsizetype = "eta")),
        jb_subtitle(jb_run(d, resultssubtitle = TRUE, effsizetype = "omega"))))
    expect_false(identical(
        jb_subtitle(jb_run(d, resultssubtitle = TRUE, k = 0)),
        jb_subtitle(jb_run(d, resultssubtitle = TRUE, k = 4))))
})

test_that("a stale fallback marker cannot leak into a later run", {
    # The fallback path is now unreachable via formula.tools, but the disclosure
    # machinery remains for any other engine failure - and it must not carry a
    # stale message into the next run. jb_run() calls private$.run() directly, so
    # drive that same entry point twice with a marker planted in between.
    a <- jb_run(jb_3group(), resultssubtitle = TRUE)
    pr <- a$.__enclos_env__$private
    expect_null(pr$.subtitleFallback)          # healthy run leaves nothing behind

    pr$.subtitleFallback <- "a simulated engine failure"
    pr$.run()
    expect_null(pr$.subtitleFallback)          # .run() resets it at the top
    expect_false(grepl("fell back to the package default",
                       jb_text(a$results$diagnostics$content), fixed = TRUE))
})


test_that("only oneway.test is collateral damage from as.character.formula", {
    # Establishes the blast radius, so the next person does not go looking for a
    # wider problem: the sibling formula methods are unaffected.
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
                    g = factor(rep(1:3, 3)), h = factor(rep(1:2, length.out = 9)))
    ok <- function(e) !inherits(try(e, silent = TRUE), "try-error")
    expect_true(ok(stats::t.test(y ~ h, data = d)))
    expect_true(ok(stats::kruskal.test(y ~ g, data = d)))
    expect_true(ok(stats::bartlett.test(y ~ g, data = d)))

    if (jb_anova_broken()) {
        # the method really is the cause, not something about this package
        expect_equal(length(as.character(y ~ g)), 1L)
        expect_false(is.null(utils::getS3method("as.character", "formula", optional = TRUE)))
    } else {
        expect_equal(length(as.character(y ~ g)), 3L)
    }
})


test_that("Bayesian report distinguishes 2-group posterior difference (ETI) from 3+ group Bayesian R-squared (HDI)", {
    a2 <- jb_run(jb_2group(), typestatistics = "bayes", resultssubtitle = TRUE, showexplanations = TRUE)
    rep2 <- jb_text(a2$results$report$content)
    expect_match(rep2, "posterior difference", fixed = TRUE)
    expect_match(rep2, "ETI", fixed = TRUE)
    expect_false(grepl("posterior Bayesian R", rep2, fixed = TRUE))

    a3 <- jb_run(jb_3group(), typestatistics = "bayes", resultssubtitle = TRUE, showexplanations = TRUE)
    rep3 <- jb_text(a3$results$report$content)
    expect_match(rep3, "posterior Bayesian R\u00b2", fixed = TRUE)
    expect_match(rep3, "HDI", fixed = TRUE)
    expect_false(grepl("posterior difference", rep3, fixed = TRUE))
    expect_false(grepl("ETI", rep3, fixed = TRUE))

    a3_tpl <- jb_run(jb_3group(), typestatistics = "bayes", resultssubtitle = FALSE, showexplanations = TRUE)
    rep3_tpl <- jb_text(a3_tpl$results$report$content)
    expect_match(rep3_tpl, "posterior Bayesian R\u00b2", fixed = TRUE)
    expect_match(rep3_tpl, "HDI", fixed = TRUE)
    expect_false(grepl("posterior difference", rep3_tpl, fixed = TRUE))
})


test_that("degenerate constant or singleton-arm variables do not crash multi-variable or pairwise plots", {
    set.seed(101)
    d_deg <- data.frame(
        g = factor(rep(c("A", "B", "C"), each = 10)),
        s = factor(rep(rep(c("S1", "S2"), each = 5), 3)),
        valid_var = c(rnorm(10, 10, 2), rnorm(10, 14, 2), rnorm(10, 18, 2)),
        const_var = rep(5.0, 30),
        n1_arm_var = c(10.5, rep(NA, 9), rnorm(10, 14, 2), rnorm(10, 18, 2))
    )

    render_ok <- function(res_img) {
        tf <- tempfile(fileext = ".png")
        grDevices::png(tf, width = 700, height = 550)
        on.exit({
            try(grDevices::dev.off(), silent = TRUE)
            unlink(tf)
        }, add = TRUE)
        !inherits(try(res_img$.render(), silent = TRUE), "try-error")
    }

    for (ts in c("parametric", "nonparametric", "robust", "bayes")) {
        for (bad_col in c("const_var", "n1_arm_var")) {
            a <- jb_run(
                d_deg,
                dep = c("valid_var", bad_col),
                grvar = "s",
                typestatistics = ts,
                resultssubtitle = TRUE,
                pairwisecomparisons = TRUE,
                addGGPubrPlot = TRUE,
                ggpubrAddStats = TRUE
            )
            expect_true(render_ok(a$results$plot), info = paste(ts, bad_col, "plot"))
            expect_true(render_ok(a$results$plot2), info = paste(ts, bad_col, "plot2"))
            expect_true(render_ok(a$results$ggpubrPlot), info = paste(ts, bad_col, "ggpubrPlot"))
            expect_true(render_ok(a$results$ggpubrPlot2), info = paste(ts, bad_col, "ggpubrPlot2"))
        }
    }

    # Single constant variable with resultssubtitle = TRUE must carry no 'NA' subtitle expression
    a_const <- jb_run(d_deg, dep = "const_var", typestatistics = "parametric",
                      resultssubtitle = TRUE, pairwisecomparisons = TRUE)
    expect_null(jb_plot(a_const)$labels$subtitle)
})


test_that("assumption p-values and companion Welch labels never emit p = 0.000 or p = <", {
    d_skew <- data.frame(
        g = factor(rep(c("A", "B", "C"), each = 20)),
        y = c(c(rep(1.0, 18), 50, 60), seq(10, 14, length.out = 20), seq(40, 340, length.out = 20))
    )
    a <- jb_run(d_skew, typestatistics = "parametric", varequal = TRUE, showexplanations = TRUE)
    diag <- jb_text(a$results$diagnostics$content)
    assm <- jb_text(a$results$assumptions$content)
    expect_false(grepl("0.000", diag, fixed = TRUE))
    expect_false(grepl("0.000", assm, fixed = TRUE))
    expect_match(diag, "p &lt; 0.001", fixed = TRUE)
    expect_match(a$results$items[[1]]$content, "p < 0.001", fixed = TRUE)

    a_welch <- jb_run(d_skew, typestatistics = "parametric", varequal = FALSE,
                      addGGPubrPlot = TRUE, ggpubrAddStats = TRUE)
    tf <- tempfile(fileext = ".png")
    grDevices::png(tf, 700, 550)
    a_welch$results$ggpubrPlot$.render()
    p_ggp <- ggplot2::last_plot()
    grDevices::dev.off()
    unlink(tf)
    gb <- ggplot2::ggplot_build(p_ggp)
    lbl_idx <- which(vapply(gb$data, function(df) "label" %in% names(df), logical(1)))
    expect_true(length(lbl_idx) >= 1L)
    lbl <- gb$data[[lbl_idx[1]]]$label[1]
    expect_match(lbl, "^Welch ANOVA, p (<|=) ")
    expect_false(grepl("p = <", lbl, fixed = TRUE))
})


test_that("reverse-order plot rendering preserves sibling HTML and guide names exact UI control", {
    d <- jb_3group()
    a <- jb_run(d, grvar = "s", addGGPubrPlot = TRUE, ggpubrAddStats = TRUE,
                resultssubtitle = FALSE, showexplanations = TRUE)
    guide_txt <- jb_text(a$results$interpretation$content)
    expect_match(guide_txt, "'Statistical results'", fixed = TRUE)
    expect_false(grepl("'Statistical results subtitle'", guide_txt, fixed = TRUE))

    before <- list(
        diag = a$results$diagnostics$content,
        clin = a$results$clinicalSummary$content,
        summ = a$results$summary$content,
        assm = a$results$assumptions$content,
        rep  = a$results$report$content
    )
    for (nm in c("ggpubrPlot2", "ggpubrPlot", "plot2", "plot")) {
        tf <- tempfile(fileext = ".png")
        grDevices::png(tf, 700, 550)
        expect_true(a$results[[nm]]$.render())
        grDevices::dev.off()
        unlink(tf)
    }
    after <- list(
        diag = a$results$diagnostics$content,
        clin = a$results$clinicalSummary$content,
        summ = a$results$summary$content,
        assm = a$results$assumptions$content,
        rep  = a$results$report$content
    )
    expect_identical(before, after)
})


test_that("ggpubr companion plots do not evaluate expression-shaped or punctuated column names", {
    set.seed(20261115)
    d_adv <- data.frame(
        rnorm(24),
        factor(rep(c("Arm A", "Arm B"), 12)),
        factor(rep(c("Stratum 1", "Stratum 2"), each = 12)),
        check.names = FALSE
    )
    dep_col <- 'stop("CANARY_DEP_EVALUATED")'
    grp_col <- "score (mg/dL)"
    grv_col <- 'I(stop("CANARY_FACET_EVALUATED"))'
    names(d_adv) <- c(dep_col, grp_col, grv_col)

    a <- jb_run(
        d_adv,
        dep = dep_col,
        group = grp_col,
        grvar = grv_col,
        typestatistics = "parametric",
        varequal = TRUE,
        resultssubtitle = TRUE,
        addGGPubrPlot = TRUE,
        ggpubrAddStats = TRUE,
        showexplanations = TRUE
    )

    expect_false(grepl("onclick=", a$results$report$content, fixed = TRUE))

    for (nm in c("plot", "plot2", "ggpubrPlot", "ggpubrPlot2")) {
        tf <- tempfile(fileext = ".png")
        grDevices::png(tf, 700, 550)
        expect_no_error(expect_true(a$results[[nm]]$.render()))
        grDevices::dev.off()
        unlink(tf)
    }
})


test_that("asSource() strips names and ignores non-atomic or undeclared option payloads", {
    d <- jb_2group()
    opts <- ClinicoPath:::jjbetweenstatsOptions$new(dep = "y", group = "g")
    an <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d)

    o_title <- an$options$option("mytitle")
    for (bad_val in list(
        stats::setNames(list("hello"), 'x`, k = {stop("CANARY_LIST"); 2L}, `y'),
        stats::setNames("hello", 'x`, k = {stop("CANARY_CHR"); 2L}, `y'),
        quote(stop("CANARY_CALL"))
    )) {
        o_title$value <- bad_val
        src <- an$asSource()
        expect_false(grepl("CANARY_", src, fixed = TRUE))
        parsed <- parse(text = src)[[1]]
        mc <- match.call(ClinicoPath::jjbetweenstats, parsed)
        for (arg_expr in as.list(mc)[-1]) {
            expect_true(is.symbol(arg_expr) || is.atomic(arg_expr))
        }
    }

    an$options$.addOption(jmvcore:::Option$new('injected = {stop("CANARY_UNDECL"); 1}, z', "a"))
    expect_false(grepl("CANARY_UNDECL", an$asSource(), fixed = TRUE))
})


test_that(".init() clamps malformed plot dimensions and withBaseFormulaChar() short-circuits re-entry", {
    d <- jb_3group()
    for (bad_pw in list(NA_integer_, "not_a_number", list(1), 99999L, -50L)) {
        opts <- ClinicoPath:::jjbetweenstatsOptions$new(dep = c("y", "y2"), group = "g", grvar = "s")
        d2 <- d
        d2$y2 <- d2$y * 2
        opw <- opts$option("plotwidth")
        opw$value <- bad_pw
        an <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d2)
        expect_no_error(an$.__enclos_env__$private$.init())
        expect_lte(an$results$plot2$width, 4096L)
        expect_gte(an$results$plot$width, 300L)
    }

    res_nested <- ClinicoPath:::withBaseFormulaChar(
        ClinicoPath:::withBaseFormulaChar(as.character(y ~ x))
    )
    expect_identical(res_nested, c("~", "y", "x"))
})


test_that("overlapping variable roles and missing grvar column reject cleanly in .run()", {
    d <- jb_3group()
    d$num_grp <- as.numeric(d$g)

    expect_error(
        jb_run(d, dep = "y", group = "g", grvar = "g"),
        "The 'Grouping Variable' and 'Split By' variable must be different columns"
    )
    expect_error(
        jb_run(d, dep = "num_grp", group = "num_grp"),
        "The grouping variable 'num_grp' cannot also be selected in 'Dependent Variables'"
    )
    expect_error(
        jb_run(d, dep = "num_grp", group = "s", grvar = "num_grp"),
        "The 'Split By' variable 'num_grp' cannot also be selected in 'Dependent Variables'"
    )

    opts_missing_grv <- ClinicoPath:::jjbetweenstatsOptions$new(
        dep = "y", group = "g", grvar = "nonexistent_stratum"
    )
    an_missing_grv <- ClinicoPath:::jjbetweenstatsClass$new(options = opts_missing_grv, data = d)
    expect_error(
        an_missing_grv$run(),
        'Split-by variable "nonexistent_stratum" not found in data'
    )
})


test_that("duplicate entries in dep are deduplicated across FWER guidance, summaries, plots, and asSource()", {
    d <- jb_2group()
    a_dup <- jb_run(
        d,
        dep = c("y", "y"),
        group = "g",
        multiEndpointCorrection = "bonferroni",
        showexplanations = TRUE,
        resultssubtitle = TRUE
    )
    a_one <- jb_run(
        d,
        dep = "y",
        group = "g",
        multiEndpointCorrection = "bonferroni",
        showexplanations = TRUE,
        resultssubtitle = TRUE
    )

    expect_identical(a_dup$results$mecGuidance$content, "")
    expect_identical(a_dup$results$clinicalSummary$content, a_one$results$clinicalSummary$content)
    expect_identical(a_dup$results$summary$content, a_one$results$summary$content)
    expect_identical(a_dup$results$assumptions$content, a_one$results$assumptions$content)
    expect_identical(a_dup$results$report$content, a_one$results$report$content)
    expect_identical(a_dup$asSource(), a_one$asSource())
    expect_identical(a_dup$results$plot$height, a_one$results$plot$height)
})


test_that("disabling showexplanations clears stale explanatory HTML blocks and resultssubtitle preserves data cache", {
    d <- jb_2group()
    opts <- ClinicoPath:::jjbetweenstatsOptions$new(
        dep = "y",
        group = "g",
        showexplanations = TRUE,
        resultssubtitle = FALSE
    )
    an <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d)
    an$run()
    expect_true(nzchar(an$results$about$content))
    expect_true(nzchar(an$results$summary$content))
    expect_true(nzchar(an$results$assumptions$content))
    expect_true(nzchar(an$results$interpretation$content))
    expect_true(nzchar(an$results$report$content))

    hash_before <- an$.__enclos_env__$private$.data_hash
    opts$option("resultssubtitle")$value <- TRUE
    opts$option("showexplanations")$value <- FALSE
    an$run()

    expect_identical(an$results$about$content, "")
    expect_identical(an$results$summary$content, "")
    expect_identical(an$results$assumptions$content, "")
    expect_identical(an$results$interpretation$content, "")
    expect_identical(an$results$report$content, "")
    expect_identical(an$.__enclos_env__$private$.data_hash, hash_before)
})


test_that("jjbetweenstatsResults schema declares consistent 650px default plot widths", {
    d <- jb_3group()
    opts <- ClinicoPath:::jjbetweenstatsOptions$new(dep = "y", group = "g", grvar = "s", addGGPubrPlot = TRUE)
    an <- ClinicoPath:::jjbetweenstatsClass$new(options = opts, data = d)
    expect_equal(an$results$plot$width, 650)
    expect_equal(an$results$plot2$width, 650)
    expect_equal(an$results$ggpubrPlot$width, 650)
    expect_equal(an$results$ggpubrPlot2$width, 650)
})
