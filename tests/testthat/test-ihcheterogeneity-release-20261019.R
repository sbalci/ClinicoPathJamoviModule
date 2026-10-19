# ═══════════════════════════════════════════════════════════
# ihcheterogeneity: final pre-release review 2026-10-19 (IH-1 to IH-5)
# ═══════════════════════════════════════════════════════════
#
# IH-1: with a reference, the proportional-bias slope on the pair mean carries
#   (s_region^2 - s_ref^2) / (2 var L) from unequal errors alone, so an unbiased core
#   "changed with the level" against a precise reference (64% at n = 300), and on a
#   noisy reference the other way. A difference now changes with the level only when
#   the slopes on the reference, the region and their mean agree in direction.
# IH-2: "AGREEMENT THRESHOLDS MET ... correlation >= 0.8" with one core at r = 0.74.
# IH-3: an ordinal HER2 0-3+ score graded INADEQUATE by its CV.
# IH-4: near-zero Ki67 levels inflate the root-mean-square CV without a word.
# IH-5: a level difference between compartments read as a heterogeneity difference.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(data = d, ...)
run_cls <- function(d, ...) {
    o <- ClinicoPath:::ihcheterogeneityOptions$new(...)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run(); an
}
first_row <- function(an) an$.__enclos_env__$private$.bias_stats$rows[[1]]

# Independent oracle: HC3 slope p of d on L by the sandwich formula, no module code.
hc3_p <- function(d, L) {
    fit <- lm(d ~ L); X <- model.matrix(fit); e <- resid(fit); h <- hatvalues(fit)
    bread <- solve(crossprod(X)); V <- bread %*% crossprod(X * (e / (1 - h))) %*% bread
    b <- unname(coef(fit)[2]); c(slope = b, p = 2 * pt(-abs(b / sqrt(V[2, 2])), length(d) - 2))
}
gen <- function(seed, n, sd_ref, sd_reg, scale = 1) {
    set.seed(seed); t <- runif(n, 5, 60)
    data.frame(ref = t + rnorm(n, 0, sd_ref), b1 = scale * t + rnorm(n, 0, sd_reg))
}
# The first seed whose pair-mean slope shows the unequal-error artefact (p < 0.01):
# the old rule called every such dataset "changes with the level".
artefact_data <- function(sd_ref, sd_reg) {
    for (seed in 1:60) {
        d <- gen(seed, 300, sd_ref, sd_reg)
        if (hc3_p(d$b1 - d$ref, (d$b1 + d$ref) / 2)[["p"]] < 0.01) return(d)
    }
    skip("no artefact dataset found")
}

test_that("an unbiased core against a precise reference is not level-dependent (IH-1)", {
    d <- artefact_data(sd_ref = 1, sd_reg = 5)
    an <- run_cls(d, wholesection = "ref", biopsy1 = "b1", bias_margin = 5)
    b <- an$results$samplingbiastable$asDF
    # the displayed pair-mean slope is still the artefact ...
    expect_lt(b$slope_p[1], 0.01)
    expect_gt(b$slope[1], 0)
    # ... but it is not confirmed on the reference value, so it is not a finding
    expect_false(isTRUE(first_row(an)$prop_shown))
    expect_false(grepl("Changes with the level", b$clinical_impact[1], fixed = TRUE))
})

test_that("an unbiased precise core against a noisy reference is not level-dependent (IH-1)", {
    d <- artefact_data(sd_ref = 5, sd_reg = 1)
    an <- run_cls(d, wholesection = "ref", biopsy1 = "b1", bias_margin = 5)
    b <- an$results$samplingbiastable$asDF
    expect_lt(b$slope_p[1], 0.01)
    expect_lt(b$slope[1], 0)
    # the slope on the reference agrees in sign; the one on the region (about
    # +s_region^2 / var(region), here near 0) does not confirm it
    expect_lt(hc3_p(d$b1 - d$ref, d$ref)[["slope"]], 0)
    expect_false(isTRUE(first_row(an)$prop_shown))
    expect_false(grepl("Changes with the level", b$clinical_impact[1], fixed = TRUE))
})

test_that("a true proportional bias is still found against a precise reference (IH-1)", {
    an <- run_cls(gen(2, 300, 1, 5, scale = 1.1), wholesection = "ref", biopsy1 = "b1", bias_margin = 5)
    expect_true(first_row(an)$prop_shown)
    # a compressing core with no average offset is named at both ends
    set.seed(3); t <- runif(300, 40, 90)
    d <- data.frame(ref = t + rnorm(300, 0, 1), b1 = mean(t) + 0.8 * (t - mean(t)) + rnorm(300, 0, 1))
    imp <- ihc(d, wholesection = "ref", biopsy1 = "b1")$samplingbiastable$asDF$clinical_impact[1]
    expect_match(imp, "Changes with the level (the mean of the two measurements)", fixed = TRUE)
})

test_that("the release-review H-score core is no longer called level-dependent (IH-1)", {
    set.seed(7); n <- 40; t <- runif(n, 200, 280)
    d <- data.frame(ref = t + rnorm(n, 0, 4), b1 = t + rnorm(n, 0, 5), b2 = t + rnorm(n, 0, 5), b3 = t + rnorm(n, 0, 28))
    res <- ihc(d, wholesection = "ref", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3",
               showInterpretation = TRUE, showReportSentences = TRUE)
    b <- res$samplingbiastable$asDF
    # the artefact is still the displayed slope (0.41, p = 0.0006 in the review)
    expect_lt(b$slope_p[3], 0.01)
    expect_false(any(grepl("Changes with the level", b$clinical_impact, fixed = TRUE)))
    for (panel in list(res$interpretation$content, res$report_sentences$content))
        expect_false(grepl("changes with the level, an estimated", txt(panel), fixed = TRUE))
})

test_that("every text says the three slopes must agree (IH-1)", {
    res <- ihc(gen(4, 100, 1, 5), wholesection = "ref", biopsy1 = "b1", showReportSentences = TRUE,
               showAssumptions = TRUE, showGlossary = TRUE)
    notes <- paste(vapply(res$samplingbiastable$notes, function(n) n$note, ""), collapse = " ")
    expect_match(notes, "in the same direction on all three: the reference value, the region value and their mean", fixed = TRUE)
    expect_match(txt(res$report_sentences$content), "regressing it on the reference value, on the region value and on the mean of the two", fixed = TRUE)
    expect_match(txt(res$assumptions$content), "Krouwer JS. Stat Med 2008", fixed = TRUE)
    expect_match(txt(res$glossary$content), "against the reference, against the region and against their mean", fixed = TRUE)
})

ih2_data <- function() {
    set.seed(1); n <- 30; t <- runif(n, 200, 280)
    data.frame(ref = t + rnorm(n, 0, 4), b1 = t + rnorm(n, 0, 4), b2 = t + rnorm(n, 0, 4), b3 = t + rnorm(n, 0, 26))
}

test_that("a core below the correlation threshold as a point estimate stops 'met' (IH-2)", {
    d <- ih2_data()
    res <- ihc(d, wholesection = "ref", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3",
               showInterpretation = TRUE, showReportSentences = TRUE, showSummary = TRUE)
    r_b3 <- cor(d$ref, d$b3, method = "spearman")
    expect_lt(r_b3, 0.8)
    a <- txt(res$interpretation$content)
    expect_false(grepl("AGREEMENT THRESHOLDS MET:", a, fixed = TRUE))
    expect_match(a, "AGREEMENT THRESHOLDS MET, NOT CONFIRMED:", fixed = TRUE)
    expect_match(a, "The correlation of region(s) 'b3' with the reference is below the 0.8 threshold as a point estimate", fixed = TRUE)
    expect_match(txt(res$report_sentences$content), "or a correlation was below the threshold as a point estimate", fixed = TRUE)
    expect_match(txt(res$summary$content), "met your correlation and variability thresholds on average, but agreement was not confirmed", fixed = TRUE)
})

test_that("an inter-regional pair below the threshold also stops 'met' (IH-2)", {
    set.seed(5); n <- 100; t <- runif(n, 30, 80)
    d <- data.frame(a = t + rnorm(n, 0, 2), b = t + rnorm(n, 0, 2), c = t + rnorm(n, 0, 12))
    res <- ihc(d, biopsy1 = "a", biopsy2 = "b", biopsy3 = "c", bias_margin = 20, cv_threshold = 40,
               showInterpretation = TRUE)
    pairs <- c(cor(d$a, d$c, method = "spearman"), cor(d$b, d$c, method = "spearman"))
    expect_true(any(pairs < 0.8))
    expect_gte(mean(c(pairs, cor(d$a, d$b, method = "spearman"))), 0.8)
    a <- txt(res$interpretation$content)
    expect_false(grepl("AGREEMENT THRESHOLDS MET:", a, fixed = TRUE))
    expect_match(a, "The correlation of the region pair(s) 'a / c', 'b / c' is below the 0.8 threshold", fixed = TRUE)
})

test_that("an ordinal HER2 score is not graded by its CV (IH-3)", {
    set.seed(3); n <- 40; true <- sample(0:3, n, TRUE, prob = c(.3, .3, .2, .2))
    jit <- function() pmin(3, pmax(0, true + sample(c(-1, 0, 0, 0, 0, 1), n, TRUE)))
    d <- data.frame(ws = true, c1 = jit(), c2 = jit(), c3 = jit())
    res <- ihc(d, wholesection = "ws", biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3",
               showInterpretation = TRUE, showReportSentences = TRUE, showSummary = TRUE)
    expect_match(txt(res$notices$content), "Ordinal score", fixed = TRUE)
    expect_false(grepl("Very high variability (CV > 50%)", txt(res$notices$content), fixed = TRUE))
    a <- txt(res$interpretation$content)
    expect_match(a, "NOT GRADED:", fixed = TRUE)
    expect_false(grepl("INADEQUATE SAMPLING", a, fixed = TRUE))
    expect_match(txt(res$report_sentences$content), "No sampling verdict was given", fixed = TRUE)
    expect_match(txt(res$summary$content), "Overall: no verdict", fixed = TRUE)
    rt <- res$reproducibilitytable$asDF
    expect_equal(rt$interpretation[grepl("^Within-case CV", rt$metric)], "Not graded: ordinal score")
    # the mean of ordinal cores has many values, but gets no slope
    b <- res$samplingbiastable$asDF
    expect_true(all(is.na(b$slope) | is.nan(b$slope)))
})

test_that("a continuous marker is not taken for an ordinal score (IH-3)", {
    res <- ihc(gen(4, 60, 1, 5), wholesection = "ref", biopsy1 = "b1")
    expect_false(grepl("Ordinal score", txt(res$notices$content), fixed = TRUE))
    # whole-number percentages with many values are not ordinal either
    d <- round(gen(4, 60, 1, 5))
    expect_false(grepl("Ordinal score", txt(ihc(d, wholesection = "ref", biopsy1 = "b1")$notices$content), fixed = TRUE))
})

test_that("near-zero levels that dominate the CV are disclosed (IH-4)", {
    set.seed(6); t <- runif(40, 0, 6); d <- data.frame(ws = t, b1 = pmax(0, t + rnorm(40, 0, .8)))
    res <- ihc(d, wholesection = "ws", biopsy1 = "b1")
    expect_match(txt(res$notices$content), "Near-zero levels inflate the CV", fixed = TRUE)
    # not on an ordinary Ki67 range
    res2 <- ihc(gen(4, 100, 1, 5), wholesection = "ref", biopsy1 = "b1")
    expect_false(grepl("Near-zero levels", txt(res2$notices$content), fixed = TRUE))
})

compartments <- function(seed, levels, sds) {
    set.seed(seed); n <- 30
    lev <- rep(levels, each = n); s <- rep(sds, each = n); tt <- lev + rnorm(2 * n, 0, 2)
    data.frame(ws = tt + rnorm(2 * n, 0, s), b1 = tt + rnorm(2 * n, 0, s), b2 = tt + rnorm(2 * n, 0, s),
               sp = factor(rep(c("Central", "Invasive"), each = n)))
}

test_that("a CV difference with a level difference is not called heterogeneity (IH-5)", {
    dd <- compartments(9, c(10, 40), c(3, 3))
    ct <- ihc(dd, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", spatial_id = "sp",
              compartmentTests = TRUE)$compartmentTests
    df <- ct$asDF
    kw <- df[df$test_type == "Kruskal-Wallis test (per-case CV)", ]
    expect_lt(kw$p_value, 0.05)
    expect_false(grepl("more heterogeneous than another", kw$interpretation, fixed = TRUE))
    expect_match(kw$interpretation, "but so does the mean level", fixed = TRUE)
    notes <- paste(vapply(ct$notes, function(n) n$note, ""), collapse = " ")
    expect_match(notes, "not adjusted for multiplicity", fixed = TRUE)
    # same level, different scatter: still a heterogeneity difference
    df2 <- ihc(compartments(9, c(40, 40), c(1, 8)), wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2",
               spatial_id = "sp", compartmentTests = TRUE)$compartmentTests$asDF
    kw2 <- df2[df2$test_type == "Kruskal-Wallis test (per-case CV)", ]
    expect_lt(kw2$p_value, 0.05)
    expect_match(kw2$interpretation, "more heterogeneous than another", fixed = TRUE)
})
