# ═══════════════════════════════════════════════════════════
# ihcheterogeneity: pre-submission review 2026-11-07 (IHC-1 to IHC-7)
# ═══════════════════════════════════════════════════════════
#
# IHC-1: without a reference, a precise and a noisy region "changed with the level"
#   from unequal measurement error alone (70% of studies at n = 300): the three-slope
#   rule of the reference design was not applied to pairs of regions.
# IHC-2: Allred scores (0, 2-8) passed as a continuous scale and were graded
#   INADEQUATE by the CV of their codes beside a weighted kappa of 0.98.
# IHC-3: without a reference no CI was computed for pair correlations, so a pair whose
#   whole CI lay below the threshold was "not shown to be below it".
# IHC-5: the within-case CV row of the reference design is the CV over the reference
#   and every region, not "region vs reference".
# IHC-6: a margin finer than the recorded values got no word.

txt <- function(html) gsub("\\s+", " ", gsub("<[^>]+>", " ", paste(html, collapse = " ")))
ihc <- function(d, ...) ClinicoPath::ihcheterogeneity(data = d, ...)
run_cls <- function(d, ...) {
    o <- ClinicoPath:::ihcheterogeneityOptions$new(...)
    an <- ClinicoPath:::ihcheterogeneityClass$new(options = o, data = d); an$run(); an
}
priv <- function(an) an$.__enclos_env__$private

# Independent oracle: HC3 slope p of d on L by the sandwich formula, no module code.
hc3_p <- function(d, L) {
    fit <- lm(d ~ L); X <- model.matrix(fit); e <- resid(fit); h <- hatvalues(fit)
    bread <- solve(crossprod(X)); V <- bread %*% crossprod(X * (e / (1 - h))) %*% bread
    b <- unname(coef(fit)[2]); c(slope = b, p = 2 * pt(-abs(b / sqrt(V[2, 2])), length(d) - 2))
}
# Spearman CI with the Bonett-Wright variance, by hand.
bw_ci <- function(x, y) {
    ok <- !is.na(x) & !is.na(y); n <- sum(ok); r <- cor(x[ok], y[ok], method = "spearman")
    c(r = r, tanh(atanh(r) + c(-1, 1) * qnorm(0.975) * sqrt((1 + r^2 / 2) / (n - 3))))
}

test_that("IHC-1: unequal precision between two regions is not read as proportional bias", {
    # No bias of any kind; region A SD 1, region B SD 5. Datasets whose pair-mean slope
    # is significant (p < 0.01) - each one used to read "Changes with the level".
    hits <- 0
    for (seed in 1:40) {
        set.seed(seed); n <- 300; t <- runif(n, 5, 60)
        d <- data.frame(A = t + rnorm(n, 0, 1), B = t + rnorm(n, 0, 5))
        if (hc3_p(d$B - d$A, (d$A + d$B) / 2)[["p"]] >= 0.01) next
        hits <- hits + 1
        an <- run_cls(d, biopsy1 = "A", biopsy2 = "B", analysis_type = "reproducibility", showReportSentences = TRUE)
        row <- priv(an)$.bias_stats$rows[[1]]
        expect_false(isTRUE(row$prop_shown))
        expect_false(grepl("Changes with the level", an$results$samplingbiastable$asDF$clinical_impact[1]))
        expect_false(grepl("changes with the level, an estimated", txt(an$results$report_sentences$content)))
        # the slope on each region is fitted for the pair too
        expect_false(is.null(row$prop_ref))
        expect_false(is.null(row$prop_reg))
        if (hits >= 3) break
    }
    expect_gte(hits, 1)
})

test_that("IHC-1: a real proportional difference between two regions is still found", {
    set.seed(5); n <- 100; t <- runif(n, 5, 60)
    d <- data.frame(A = t + rnorm(n, 0, 2), B = 32.5 + 1.15 * (t - 32.5) + rnorm(n, 0, 2))
    r <- ihc(d, biopsy1 = "A", biopsy2 = "B", analysis_type = "reproducibility")
    expect_match(r$samplingbiastable$asDF$clinical_impact[1], "Changes with the level")
    # the method note describes the three-slope rule for pairs
    expect_match(txt(r$samplingbiastable$notes$slope$note), "each of the two regions and their mean")
})

allred_data <- function() {
    set.seed(4); n <- 40
    lev <- c(0, 2:8)
    ws <- c(rep(0, 10), rep(3, 4), sample(6:8, 26, TRUE))
    core <- function() {
        step <- sample(c(-1, 0, 1), n, TRUE, prob = c(.15, .7, .15)); idx <- match(ws, lev) + step
        lev[pmin(pmax(idx, 1), length(lev))]
    }
    data.frame(ws = ws, c1 = core(), c2 = core(), c3 = core())
}

test_that("IHC-2: an Allred score is not graded by the CV of its codes", {
    d <- allred_data()
    expect_lte(length(unique(unlist(d))), 9)
    r <- ihc(d, wholesection = "ws", biopsy1 = "c1", biopsy2 = "c2", biopsy3 = "c3", showInterpretation = TRUE)
    rep <- r$reproducibilitytable$asDF
    expect_equal(rep$interpretation[rep$metric == "Within-case CV (%) - reference and regions"], "Not graded: ordinal or binned values")
    a <- txt(r$interpretation$content)
    expect_match(a, "Not graded:")
    expect_false(grepl("Inadequate sampling", a))
    expect_match(txt(r$notices$content), "Allred")
    expect_match(txt(r$notices$content), "coarsely binned values")
})

test_that("IHC-2: continuous scales and wide integer scales are still graded", {
    set.seed(9); n <- 40; t <- runif(n, 50, 280)
    # H-score recorded as whole numbers
    d <- data.frame(ws = round(t + rnorm(n, 0, 3)), b1 = round(t + rnorm(n, 0, 8)))
    r <- ihc(d, wholesection = "ws", biopsy1 = "b1", showInterpretation = TRUE)
    expect_false(grepl("Not graded", txt(r$interpretation$content)))
    # Ki-67 with decimals in 0-12
    set.seed(10); t <- runif(n, 1, 12)
    d2 <- data.frame(ws = round(t + rnorm(n, 0, .3), 1), b1 = round(t + rnorm(n, 0, .8), 1))
    r2 <- ihc(d2, wholesection = "ws", biopsy1 = "b1", showInterpretation = TRUE)
    expect_false(grepl("Not graded", txt(r2$interpretation$content)))
})

pair_data <- function() {
    set.seed(21); n <- 100; t <- runif(n, 10, 60)
    d <- as.data.frame(sapply(paste0("G", 1:6), function(i) t + rnorm(n, 0, 1.5)))
    d$P <- t + rnorm(n, 0, 12)
    d
}

test_that("IHC-3: pair correlations carry CIs and a pair shown below the threshold sets the verdict", {
    d <- pair_data()
    r <- ihc(d, biopsy1 = "G1", biopsy2 = "G2", biopsy3 = "G3", biopsy4 = "G4", biopsies = c("G5", "G6", "P"),
             bias_margin = 10, correlation_threshold = 0.90, cv_threshold = 40,
             showInterpretation = TRUE, showReportSentences = TRUE, showSummary = TRUE)
    rep <- r$reproducibilitytable$asDF
    row <- rep[rep$metric == "Spearman correlation: G1 / P", ]
    expect_equal(nrow(row), 1)
    o <- bw_ci(d$G1, d$P)
    expect_equal(row$value, o[["r"]], tolerance = 1e-10)
    expect_equal(c(row$ci_lower, row$ci_upper), unname(o[2:3]), tolerance = 1e-10)
    expect_lt(o[3], 0.90)
    a <- txt(r$interpretation$content)
    expect_false(grepl("although not shown to be below it", a))
    expect_match(a, "Pairs of regions shown to be below the threshold on their own")
    expect_false(grepl("Agreement thresholds met", a))
    expect_match(a, "Moderate sampling")
    expect_match(txt(r$report_sentences$content), "was shown to be below the threshold")
    expect_match(txt(r$summary$content), "The weakest pair of regions")
    # the mean inter-regional correlation carries a CI without a reference
    m <- rep[rep$metric == "Mean Inter-Regional Correlation", ]
    expect_false(is.na(m$ci_lower))
})

test_that("IHC-3: two regions - the single pair's CI is the CI of the graded correlation", {
    set.seed(3); n <- 30; t <- runif(n, 10, 60)
    d <- data.frame(A = t + rnorm(n, 0, 3), B = t + rnorm(n, 0, 3))
    r <- ihc(d, biopsy1 = "A", biopsy2 = "B")
    m <- r$reproducibilitytable$asDF
    m <- m[m$metric == "Mean Inter-Regional Correlation", ]
    o <- bw_ci(d$A, d$B)
    expect_equal(c(m$value, m$ci_lower, m$ci_upper), unname(o), tolerance = 1e-10)
})

test_that("IHC-5: the reference-design CV row is named for what it pools", {
    data("ihcheterogeneity_test", package = "ClinicoPath", envir = environment())
    r <- ihc(ihcheterogeneity_test, wholesection = "wholesection", biopsy1 = "biopsy1", biopsy2 = "biopsy2")
    expect_true("Within-case CV (%) - reference and regions" %in% r$reproducibilitytable$asDF$metric)
})

test_that("IHC-6: a margin finer than the recorded values is pointed out", {
    set.seed(8); n <- 40; t <- runif(n, 0.5, 6)
    h <- function(x) pmax(0, round(2 * x) / 2)
    d <- data.frame(ws = h(t + rnorm(n, 0, .2)), b1 = h(t + rnorm(n, 0, .6)))
    r <- ihc(d, wholesection = "ws", biopsy1 = "b1")
    expect_match(txt(r$notices$content), "Margin finer than the recorded values")
    expect_match(txt(r$notices$content), "smallest non-zero difference recorded between paired measurements \\(0.5\\)")
    set.seed(9); t <- runif(n, 50, 280)
    d2 <- data.frame(ws = t + rnorm(n, 0, 3), b1 = t + rnorm(n, 0, 8))
    r2 <- ihc(d2, wholesection = "ws", biopsy1 = "b1")
    expect_false(grepl("Margin finer", txt(r2$notices$content)))
})
