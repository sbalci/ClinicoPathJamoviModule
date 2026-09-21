# ═══════════════════════════════════════════════════════════
# ihcheterogeneity: proportional bias and the within-case CV (review 2026-09-19)
# ═══════════════════════════════════════════════════════════
#
# P1: a region that compresses the scale (+6 points at 45%, -6 at 85%) has a mean
# difference of ~0 and used to get "AGREEMENT THRESHOLDS MET". P2: the plain mean of
# per-case CVs read a true 15% as 11.8% with two values per case. Every number is
# checked against an independent computation (lm, sandwich), never the module's helpers.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
verdict_of <- function(res) {
    t <- txt(res$interpretation$content)
    regmatches(t, regexpr(
        "(AGREEMENT THRESHOLDS MET, NOT CONFIRMED|AGREEMENT THRESHOLDS MET|MODERATE SAMPLING|INADEQUATE SAMPLING|NOT ADEQUATE FOR SUBSTITUTION|INSUFFICIENT DATA)", t))
}
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(data = d, ...)
note_text <- function(tbl) paste(vapply(tbl$notes, function(n) n$note, ""), collapse = " | ")

# One region that compresses the scale around the mean.
compressed <- function(n = 60, seed = 1) {
    set.seed(seed); ref <- runif(n, 40, 90)
    data.frame(ref = ref, reg = mean(ref) + 0.7 * (ref - mean(ref)) + rnorm(n, 0, 1))
}

test_that("slope and p use HC3 standard errors (sandwich::vcovHC)", {
    skip_if_not_installed("sandwich")
    d <- compressed()
    b <- ihc(d, wholesection = "ref", biopsy1 = "reg")$samplingbiastable$asDF
    fit <- lm(I(reg - ref) ~ ref, data = d)
    se <- sqrt(diag(sandwich::vcovHC(fit, type = "HC3")))[2]
    p <- 2 * pt(-abs(coef(fit)[2] / se), df.residual(fit))
    expect_equal(b$slope[1], unname(coef(fit)[2]), tolerance = 1e-10)
    # p is ~1e-43, so compare on the log scale (an absolute tolerance would pass
    # any tiny p), with a fixture where HC3 and plain OLS differ
    expect_gt(abs(log(summary(fit)$coefficients[2, 4] / p)), 1)
    expect_equal(log(b$slope_p[1]), log(unname(p)), tolerance = 1e-8)
})

test_that("a compressing single region is no longer 'met' and both ends are named (P1)", {
    d <- compressed()
    res <- ihc(d, wholesection = "ref", biopsy1 = "reg", showReportSentences = TRUE)
    # independent: the mean difference is inside the 5% margin, the ends are not
    dd <- d$reg - d$ref
    expect_lt(abs(mean(dd)) / mean(d$ref) * 100, 1)
    fit <- lm(I(reg - ref) ~ ref, data = d)
    ends <- quantile(d$ref, c(0.05, 0.95), names = FALSE)
    fitted_ends <- predict(fit, data.frame(ref = ends))
    expect_true(all(abs(fitted_ends) > 0.05 * mean(d$ref)))
    # a single region is judged against the reference itself, whose own error can
    # make such a slope: the level can stop "ruled out" but never make it material
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    imp <- res$samplingbiastable$asDF$clinical_impact[1]
    expect_match(imp, sprintf("%+.1f at %.1f", fitted_ends[1], ends[1]), fixed = TRUE)
    expect_match(imp, sprintf("%+.1f at %.1f", fitted_ends[2], ends[2]), fixed = TRUE)
    expect_match(imp, sprintf("inconclusive at the \u00b1%.1f margin", 0.05 * mean(d$ref)), fixed = TRUE)
    expect_match(txt(res$interpretation$content), "changes with the level", fixed = TRUE)
    expect_match(txt(res$report_sentences$content), "proportional bias", fixed = TRUE)
})

test_that("with two regions a compressing region is not confirmed, never material, and the other is ruled out (P1b)", {
    set.seed(2); n <- 60; t <- runif(n, 40, 90)
    d <- data.frame(ws = t + rnorm(n, 0, 1), b1 = mean(t) + 0.7 * (t - mean(t)) + rnorm(n, 0, 1),
                    b2 = t + rnorm(n, 0, 2))
    res <- ihc(d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2", generate_recommendations = TRUE)
    # a slope only stops 'ruled out': with one reading per method it cannot be told
    # from a regression-to-the-mean artefact (release review 2026-09-19)
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    b <- res$samplingbiastable$asDF
    # every reference row, the mean of all regions included, is regressed on the reference
    expect_equal(b$slope[1], unname(coef(lm(I(b1 - ws) ~ ws, data = d))[2]), tolerance = 1e-10)
    expect_equal(b$slope[3], unname(coef(lm(I((b1 + b2) / 2 - ws) ~ ws, data = d))[2]), tolerance = 1e-10)
    expect_match(b$clinical_impact[1], "inconclusive at the \u00b1", fixed = TRUE)
    expect_match(b$clinical_impact[2], "ruled out", fixed = TRUE)
    it <- txt(res$interpretation$content)
    expect_match(it, "the difference between region 'b1' and its comparison changes with the level", fixed = TRUE)
    expect_false(grepl("slope as well as an offset", it, fixed = TRUE))
    expect_false(grepl("Material systematic difference", txt(res$notices$content), fixed = TRUE))
})

test_that("a slope that reference noise alone produces never makes a region material", {
    # Regressing on the reference builds in a slope of -s^2 / var(reference): with a
    # noisy (hotspot-like) reference an unbiased region shows it, and it may only
    # stop 'ruled out'.
    set.seed(3); n <- 150; t <- runif(n, 10, 60)
    d <- data.frame(ws = t + rnorm(n, 0, 6), b1 = t + rnorm(n, 0, 2), b2 = t + rnorm(n, 0, 2))
    res <- ihc(d, wholesection = "ws", biopsy1 = "b1", biopsy2 = "b2")
    b <- res$samplingbiastable$asDF
    expect_length(b$slope_p, 3)
    expect_equal(b$slope[1], unname(coef(lm(I(b1 - ws) ~ ws, data = d))[2]), tolerance = 1e-10)
    expect_lt(b$slope[1], -0.08)                     # the artefact is there
    expect_false(verdict_of(res) == "NOT ADEQUATE FOR SUBSTITUTION")
    expect_false(any(grepl("shown to exceed", b$clinical_impact, fixed = TRUE)))
})

test_that("regions sharing a site effect are never made material by the slope (A2-F1)", {
    # r1, r2 = T + u + e with u shared by both regions but not the reference: no
    # region is biased. As the other regions' level, u built in a slope of
    # var(u) / var(level): this dataset (release review a2_n2_repro.R, seed 1) read
    # NOT ADEQUATE FOR SUBSTITUTION with slope-calibration advice.
    set.seed(1); n <- 600
    T <- 100 * rbeta(n, 1.5, 3); s <- 1 + 0.10 * T; u <- rnorm(n, 0, 0.15 * T)
    d <- data.frame(row.names = seq_len(n))
    d$r1 <- mean(T) + 1 * (T - mean(T)) + u + rnorm(n, 0, s)
    d$r2 <- T + u + rnorm(n, 0, s)
    d$ref <- T + rnorm(n, 0, 0.5 * s)
    res <- ihc(d, wholesection = "ref", biopsy1 = "r1", biopsy2 = "r2")
    expect_false(verdict_of(res) == "NOT ADEQUATE FOR SUBSTITUTION")
    expect_false(grepl("Material systematic difference", txt(res$notices$content), fixed = TRUE))
})

test_that("a sparse second region cannot make the level check weaker (A3-P6)", {
    # r1 changes with the level; r2 overlaps r1 in 7 mid-range cases (release review
    # refute_a3p6.R). The other regions' level shrank r1's check to those 7 cases and
    # turned the withheld verdict of r1 alone into 'met'. The reference is known for
    # every case of r1.
    set.seed(61); n <- 70
    t <- runif(n, 3, 90); t[34:40] <- seq(38, 52, length.out = 7)
    d <- data.frame(ref = t + rnorm(n, 0, 0.8), r1 = NA_real_, r2 = NA_real_)
    d$r1[1:40] <- (t + 0.08 * (t - 45) + rnorm(n, 0, 0.8))[1:40]
    d$r2 <- t + rnorm(n, 0, 0.8)
    d$r2[1:33] <- NA
    alone <- ihc(d[, c("ref", "r1")], wholesection = "ref", biopsy1 = "r1")
    both <- ihc(d, wholesection = "ref", biopsy1 = "r1", biopsy2 = "r2")
    expect_equal(verdict_of(alone), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
    expect_false(verdict_of(both) == "AGREEMENT THRESHOLDS MET")
    expect_equal(both$samplingbiastable$asDF$slope[1], alone$samplingbiastable$asDF$slope[1], tolerance = 1e-10)
})

test_that("levels differing only in rounding residue give a blank slope, not a crash (A3-P1)", {
    priv <- ClinicoPath:::ihcheterogeneityClass$new(
        options = ClinicoPath:::ihcheterogeneityOptions$new(biopsy1 = "a"),
        data = data.frame(a = 1:5))$.__enclos_env__$private
    # 0.45 written four ways (5 distinct doubles, 1 value) plus one case at 0.9
    L <- c(rep(c(0.45, (0.84 + 0.06) / 2, (0.56 + 0.34) / 2, (0.68 + 0.56 + 0.11) / 3), 5), 0.9)
    expect_equal(length(unique(L)), 5)
    d <- c(-0.03, 0.01, 0.05, -0.03, 0, 0, 0.02, -0.01, 0.06, 0, 0.01, 0.03, -0.01, -0.03, 0.05, -0.07, 0.03, 0, 0.03, 0.01, -0.3)
    expect_null(priv$.levelFit(d, L))
})

test_that("comparisons without a level check are named beside 'not detected' (A3-P4)", {
    # b3 is measured only in 10 cases whose reference is 30 or 40: 2 distinct levels
    set.seed(4); w <- c(runif(50, 20, 80), rep(c(30, 40), each = 5)); n <- length(w)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 1), b2 = w + rnorm(n, 0, 1),
                    b3 = c(rep(NA, 50), w[51:60] + rnorm(10, 0, 0.3)))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3")
    b <- res$samplingbiastable$asDF
    expect_true(is.na(b$slope[3]))
    expect_false(anyNA(b$slope[c(1, 2, 4)]))
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET")
    expect_match(txt(res$interpretation$content),
                 "Not checked for a difference that changes with the level (too few cases or distinct values): 'b3'.", fixed = TRUE)
})

test_that("the mean of all regions gets its own sentence when its difference changes with the level", {
    set.seed(9); n <- 80; t <- runif(n, 20, 90)
    ws <- t + rnorm(n, 0, 0.5)
    d <- data.frame(ws = ws, a = mean(t) + 0.85 * (t - mean(t)) + rnorm(n, 0, 0.5),
                    b = mean(t) + 0.85 * (t - mean(t)) + rnorm(n, 0, 0.5))
    it <- txt(ihc(d, wholesection = "ws", biopsy1 = "a", biopsy2 = "b")$interpretation$content)
    expect_false(grepl("region 'NA'", it, fixed = TRUE))
})

test_that("the slope test holds its size when the error grows with the level (HC3)", {
    priv <- ClinicoPath:::ihcheterogeneityClass$new(
        options = ClinicoPath:::ihcheterogeneityOptions$new(biopsy1 = "a"),
        data = data.frame(a = 1:5))$.__enclos_env__$private
    set.seed(9)
    p <- replicate(400, {
        t <- rlnorm(60, log(20), 0.6)
        x <- t * (1 + rnorm(60, 0, 0.2)); y <- t * (1 + rnorm(60, 0, 0.2)); z <- t * (1 + rnorm(60, 0, 0.2))
        priv$.levelFit(x - y, z)$p        # level = an independent third reading
    })
    # HC3 is not exact with skewed, heteroscedastic data (about 11% here), but plain
    # OLS errors rejected in a third of such studies; the gate is 0.05 / m besides
    expect_lt(mean(p < 0.05), 0.15)
})

test_that("an ordinal score gets no slope, and the blank is explained", {
    set.seed(6); n <- 40; w <- sample(0:3, n, replace = TRUE)
    d <- data.frame(w = w, b1 = pmin(3, pmax(0, w + sample(c(-1, 0, 0, 0, 1), n, replace = TRUE))))
    b <- ihc(d, wholesection = "w", biopsy1 = "b1")$samplingbiastable
    expect_true(is.na(b$asDF$slope[1]))
    expect_match(note_text(b), "fewer than 5 cases or 5 distinct levels", fixed = TRUE)
})

test_that("'met' claims only what was tested", {
    set.seed(42); n <- 60; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 1), b2 = w + rnorm(n, 0, 1))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", showReportSentences = TRUE,
               showAssumptions = TRUE, showGlossary = TRUE)
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET")
    it <- txt(res$interpretation$content)
    expect_match(it, "the average difference of every comparison with the reference was shown to lie within", fixed = TRUE)
    expect_match(it, "No difference that changes with the level was detected; this does not show that there is none.", fixed = TRUE)
    expect_false(grepl("every systematic difference was shown", it, fixed = TRUE))
    expect_match(txt(res$assumptions$content), "Proportional bias:", fixed = TRUE)
    expect_match(txt(res$glossary$content), "Proportional bias:", fixed = TRUE)
})

test_that("the within-case CV is the root mean square of the per-case CVs (P2)", {
    set.seed(2); n <- 1000; mu <- runif(n, 20, 80)
    two <- data.frame(a = mu * (1 + rnorm(n, 0, 0.15)), b = mu * (1 + rnorm(n, 0, 0.15)))
    cv <- function(d, ...) { t <- ihc(d, ...)$reproducibilitytable$asDF
                             t$value[grepl("Within-case CV", t$metric)] }
    m <- as.matrix(two); per_case <- apply(m, 1, sd) / rowMeans(m) * 100
    expect_equal(cv(two, wholesection = "a", biopsy1 = "b"), sqrt(mean(per_case^2)), tolerance = 1e-10)
    # the true CV is 15%; the plain mean read it about 20% low with two values per case
    expect_lt(mean(per_case), 13)
    expect_equal(cv(two, wholesection = "a", biopsy1 = "b"), 15, tolerance = 0.05)
    four <- data.frame(a = mu * (1 + rnorm(n, 0, 0.15)), b = mu * (1 + rnorm(n, 0, 0.15)),
                       c = mu * (1 + rnorm(n, 0, 0.15)), d = mu * (1 + rnorm(n, 0, 0.15)))
    # and it does not depend on how many regions were measured
    expect_equal(cv(four, wholesection = "a", biopsy1 = "b", biopsy2 = "c", biopsy3 = "d"), 15, tolerance = 0.05)
})

test_that("between regions a compressing region blocks 'ruled out' but is never material", {
    set.seed(2); t <- runif(60, 40, 90)
    d <- data.frame(c1 = t + rnorm(60), c2 = t + rnorm(60), c3 = mean(t) + 0.7 * (t - mean(t)) + rnorm(60))
    res <- ihc(d, biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3")
    b <- res$samplingbiastable$asDF
    # level = the case mean of all regions
    fit <- lm(I(c3 - (c1 + c2) / 2) ~ I((c1 + c2 + c3) / 3), data = d)
    expect_equal(b$slope[3], unname(coef(fit)[2]), tolerance = 1e-10)
    expect_match(b$clinical_impact[3], "inconclusive at the \u00b1", fixed = TRUE)
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET, NOT CONFIRMED")
})

test_that("a weak scaling shown by the slope can still be ruled out at both ends", {
    set.seed(8); n <- 400; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w * 1.02 + rnorm(n, 0, 0.5), b2 = w + rnorm(n, 0, 0.5))
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2")
    b <- res$samplingbiastable$asDF
    expect_lt(b$slope_p[1], 0.05 / 3)
    expect_match(b$clinical_impact[1], "ruled out, both 90% CIs within the margin", fixed = TRUE)
    expect_equal(verdict_of(res), "AGREEMENT THRESHOLDS MET")
    expect_false(grepl("No difference that changes with the level was detected", txt(res$interpretation$content), fixed = TRUE))
})

test_that("a region that reads the same value in every case is never green and is named", {
    set.seed(24); n <- 60; w <- runif(n, 20, 80)
    d <- data.frame(whole = w, b1 = w + rnorm(n, 0, 1), b2 = w + rnorm(n, 0, 1), b3 = 50)
    res <- ihc(d, wholesection = "whole", biopsy1 = "b1", biopsy2 = "b2", biopsy3 = "b3")
    expect_false(verdict_of(res) %in% c("AGREEMENT THRESHOLDS MET", "AGREEMENT THRESHOLDS MET, NOT CONFIRMED"))
    expect_match(txt(res$interpretation$content), "'b3'", fixed = TRUE)
    expect_match(res$samplingbiastable$asDF$clinical_impact[3], "Changes with the level", fixed = TRUE)
})

test_that("a single region with a plain offset against a noisy reference gets no slope advice", {
    # the slope on the reference is the regression-to-the-mean artefact; the row is
    # material by its average, so the advice is an offset calibration
    set.seed(1); n <- 100; t <- runif(n, 10, 60)
    d <- data.frame(ws = t + rnorm(n, 0, 6), b1 = t + 0.10 * mean(t) + rnorm(n, 0, 2))
    res <- ihc(d, wholesection = "ws", biopsy1 = "b1", showReportSentences = TRUE, generate_recommendations = TRUE)
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    expect_lt(res$samplingbiastable$asDF$slope_p[1], 0.05)          # the artefact is shown
    # no claim that THIS region's difference changes with the level (the general
    # advice that averaging removes neither kind of difference may stay)
    claims <- c("slope as well as an offset", "compresses the scale", "stretches the scale",
                "and its comparison changes with the level", "by an amount that changes with the level",
                "from the reference changes with the level")
    for (panel in list(res$notices$content, res$interpretation$content, res$report_sentences$content))
        for (claim in claims) expect_false(grepl(claim, txt(panel), fixed = TRUE), info = claim)
    expect_false(grepl("Changes with the level", res$samplingbiastable$asDF$clinical_impact[1], fixed = TRUE))
})

test_that("an offset region is material and a level-dependent region is not described as offset", {
    set.seed(12); n <- 80; t <- runif(n, 30, 90)
    d <- data.frame(ws = t + rnorm(n, 0, 1), a = t + 0.10 * mean(t) + rnorm(n, 0, 1),
                    b = mean(t) + 0.7 * (t - mean(t)) + rnorm(n, 0, 1), c = t + rnorm(n, 0, 1))
    res <- ihc(d, wholesection = "ws", biopsy1 = "a", biopsy2 = "b", biopsy3 = "c",
               showReportSentences = TRUE, generate_recommendations = TRUE)
    expect_equal(verdict_of(res), "NOT ADEQUATE FOR SUBSTITUTION")
    notes <- txt(res$notices$content)
    expect_match(notes, "Region(s) 'a' are offset from the reference", fixed = TRUE)
    expect_false(grepl("'b'", notes, fixed = TRUE))
    rs <- txt(res$report_sentences$content)
    expect_match(rs, "region(s) 'a' are systematically offset", fixed = TRUE)
    expect_false(grepl("'b' are systematically offset", rs, fixed = TRUE))
    expect_match(res$samplingbiastable$asDF$clinical_impact[2], "Changes with the level", fixed = TRUE)
})
