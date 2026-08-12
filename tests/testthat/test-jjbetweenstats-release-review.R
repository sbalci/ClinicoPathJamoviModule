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
jb_anova_broken <- function() {
    d <- data.frame(y = c(1, 2, 3, 4, 5, 6, 7, 8, 9), g = factor(rep(1:3, 3)))
    inherits(try(stats::oneway.test(y ~ g, data = d), silent = TRUE), "try-error")
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
    skip_if(jb_anova_broken(),
            "an attached ClinicoPath shadows stats::aov/oneway.test, so statsExpressions' ANOVA path cannot run here; verified working with the namespace unattached (Fisher F(2,72)=9.34 p=2.48e-04, Welch F(2,41.71)=7.47 p=1.69e-03)")
    d <- jb_3group()
    s_fisher <- jb_subtitle(jb_run(d, varequal = TRUE,  resultssubtitle = TRUE))
    s_welch  <- jb_subtitle(jb_run(d, varequal = FALSE, resultssubtitle = TRUE))
    expect_match(s_fisher, "Fisher", fixed = TRUE)
    expect_match(s_welch,  "Welch",  fixed = TRUE)

    ref_f <- stats::oneway.test(y ~ g, data = d, var.equal = TRUE)
    ref_w <- stats::oneway.test(y ~ g, data = d, var.equal = FALSE)
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


test_that("a silent fallback to the package default is disclosed", {
    # `formula.tools` registers an as.character.formula method returning one
    # deparsed string where base R returns c("~","y","g"), so
    # stats::oneway.test - the engine behind Welch's ANOVA - rejects every valid
    # formula once that package is loaded. It arrives transitively via logistf,
    # which firthregression loads on demand, so a jamovi session in which Firth
    # regression has been run loses the 3+ group takeover for the rest of its
    # life. The result is safe (ggstatsplot's own subtitle) but it silently
    # ignores three of the user's choices, so it must be stated.
    skip_if(!jb_anova_broken(), "statsExpressions' ANOVA path is healthy here, so there is no fallback to disclose")

    fired <- function(...) grepl("fell back to the package default",
                                 jb_text(jb_run(...)$results$diagnostics$content), fixed = TRUE)

    expect_true(fired(jb_3group(), resultssubtitle = TRUE, varequal = TRUE))
    # nothing to disclose when the takeover succeeds, is not attempted, or the
    # subtitle is not shown at all
    expect_false(fired(jb_2group(), resultssubtitle = TRUE, varequal = TRUE))
    expect_false(fired(jb_3group(), resultssubtitle = FALSE))
    expect_false(fired(jb_3group(), resultssubtitle = TRUE, typestatistics = "bayes"))

    # and the message names the three options that did not apply
    msg <- jb_text(jb_run(jb_3group(), resultssubtitle = TRUE)$results$diagnostics$content)
    for (opt in c("Equal variances", "Effect size type", "Decimal places"))
        expect_match(msg, opt, fixed = TRUE)
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
