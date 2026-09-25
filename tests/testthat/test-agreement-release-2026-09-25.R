# Regression tests for the 2026-09-25 /release-review-function pass. Finding ids (L321,
# L476, R1, ...) are those of the lead triage of
# development-ideas/agreement-check-function-release-2026-09-24.md (L = line of the lead) and
# of the verifier reports (R). Every expectation is checked against an independent oracle -
# base R counts, irrCAC, or Krippendorff's coincidence-matrix formula written out below -
# never against agreement itself. Each test failed on the pre-fix file.

rr_object <- function(d, ..., outputs = character()) {
    ns <- asNamespace("ClinicoPath")
    options <- ns$agreementOptions$new(...)
    for (nm in outputs) {
        opt <- options$option(nm)
        opt$value <- list(value = TRUE, synced = FALSE)
    }
    ns$agreementClass$new(options = options, data = d)
}
rr_rerun <- function(a, d) {
    a$.__enclos_env__$private$.data <- d
    suppressWarnings(suppressMessages(a$run()))
    a
}

# Krippendorff (2011): coincidence matrix, units in rows, pairs weighted 1 / (m_u - 1).
kripp_oracle <- function(m, metric = c("nominal", "interval", "ratio")) {
    metric <- match.arg(metric)
    vals <- sort(unique(m[!is.na(m)]))
    o <- matrix(0, length(vals), length(vals))
    for (u in seq_len(nrow(m))) {
        x <- m[u, !is.na(m[u, ])]
        if (length(x) < 2) next
        for (a in seq_along(x)) for (b in seq_along(x)) if (a != b) {
            i <- match(x[a], vals); j <- match(x[b], vals)
            o[i, j] <- o[i, j] + 1 / (length(x) - 1)
        }
    }
    nc <- rowSums(o); n <- sum(nc)
    d <- switch(metric,
        nominal = outer(vals, vals, "!=") * 1,
        interval = outer(vals, vals, function(a, b) (a - b)^2),
        ratio = outer(vals, vals, function(a, b) ifelse(a == b, 0, ((a - b) / (a + b))^2)))
    1 - (n - 1) * sum(o * d) / sum(outer(nc, nc) * d)
}

# --- Stale output: rows: 1 tables and notes (L321, L340, L361, L414) --------------------

test_that("L321 a rerun that refuses mean Spearman leaves no rho from the previous data", {
    lv <- c("G1", "G2", "G3")
    withr::local_seed(21)
    b <- sample(lv, 30, TRUE)
    jit <- function() ifelse(runif(30) < .85, b, sample(lv, 30, TRUE))
    d_ord <- data.frame(R1 = factor(jit(), lv, ordered = TRUE), R2 = factor(jit(), lv, ordered = TRUE), R3 = factor(jit(), lv, ordered = TRUE))
    d_nom <- as.data.frame(lapply(d_ord, function(x) factor(as.character(x), lv)))
    a <- rr_object(d_ord, vars = c("R1", "R2", "R3"), meanSpearman = TRUE)
    rr_rerun(a, d_ord)
    expect_true(is.finite(a$results$meanSpearmanTable$asDF$meanRho))   # control: it ran
    rr_rerun(a, d_nom)
    expect_true(is.na(a$results$meanSpearmanTable$asDF$meanRho))
    expect_true("error" %in% names(agreement_notes(a$results$meanSpearmanTable)))
})

test_that("L361 a continuous rerun blanks the headline kappa, interval and p", {
    lv <- c("Low", "Mid", "High")
    withr::local_seed(7)
    t0 <- sample(lv, 40, TRUE)
    cat_d <- data.frame(R1 = factor(t0, lv, ordered = TRUE),
                        R2 = factor(ifelse(runif(40) < .8, t0, sample(lv, 40, TRUE)), lv, ordered = TRUE),
                        R3 = factor(ifelse(runif(40) < .8, t0, sample(lv, 40, TRUE)), lv, ordered = TRUE))
    cont_d <- data.frame(R1 = rnorm(40, 50, 10)); cont_d$R2 <- cont_d$R1 + rnorm(40); cont_d$R3 <- cont_d$R1 + rnorm(40)
    a <- rr_object(cat_d, vars = c("R1", "R2", "R3"), lightKappa = TRUE, allPairsKappa = TRUE)
    rr_rerun(a, cat_d)
    expect_true(is.finite(a$results$irrtable$asDF$kappa))
    rr_rerun(a, cont_d)
    h <- a$results$irrtable$asDF
    expect_true(all(is.na(c(h$kappa, h$ci_lower, h$ci_upper, h$z, h$p))))
    expect_true(is.na(a$results$lightKappaTable$asDF$kappa))
})

test_that("L340 a categorical rerun drops the continuous-data refusal notes", {
    lv <- c("A", "B", "C")
    withr::local_seed(8)
    t0 <- sample(lv, 40, TRUE)
    cat_d <- data.frame(R1 = factor(t0, lv), R2 = factor(ifelse(runif(40) < .8, t0, sample(lv, 40, TRUE)), lv),
                        R3 = factor(ifelse(runif(40) < .8, t0, sample(lv, 40, TRUE)), lv))
    cont_d <- data.frame(R1 = rnorm(40)); cont_d$R2 <- cont_d$R1 + rnorm(40, 0, .2); cont_d$R3 <- cont_d$R1 + rnorm(40, 0, .2)
    a <- rr_object(cont_d, vars = c("R1", "R2", "R3"), allPairsKappa = TRUE, itemModalCategoryAgreement = TRUE)
    rr_rerun(a, cont_d)
    expect_true("type_error" %in% names(agreement_notes(a$results$allPairsKappaTable)))   # control
    rr_rerun(a, cat_d)
    expect_equal(nrow(a$results$allPairsKappaTable$asDF), 3L)
    expect_false("type_error" %in% names(agreement_notes(a$results$allPairsKappaTable)))
    expect_false("type_error" %in% names(agreement_notes(a$results$itemModalAgreementTable)))
})

# --- Frequency tables (L476, L477) ------------------------------------------------------

test_that("L476 the two-rater frequency table keeps ER- and ER+ apart", {
    lv <- c("ER-", "ER+")
    d <- data.frame(R1 = factor(c(rep("ER-", 6), rep("ER+", 4)), lv),
                    R2 = factor(c("ER-", "ER-", "ER-", "ER-", "ER+", "ER+", "ER+", "ER+", "ER+", "ER-"), lv))
    tb <- agreement_run(d, vars = c("R1", "R2"), sft = TRUE)$results$contingencyTable
    oracle <- table(d$R1, d$R2)
    df <- tb$asDF
    titles <- vapply(tb$columns, function(cl) cl$title, character(1))
    names(titles) <- vapply(tb$columns, function(cl) cl$name, character(1))
    for (j in seq_along(lv)) {
        id <- names(titles)[titles == lv[j]]
        expect_length(id, 1L)                                   # was: one column for both
        expect_equal(as.integer(df[[id]][1:2]), as.integer(oracle[, j]))
    }
})

test_that("L477 the combination table lists the most frequent patterns first", {
    lv <- c("A", "B", "C", "D", "E")
    withr::local_seed(3)
    unanimous <- data.frame(R1 = rep("E", 60), R2 = rep("E", 60), R3 = rep("E", 60))
    noise <- data.frame(R1 = sample(lv, 150, TRUE), R2 = sample(lv, 150, TRUE), R3 = sample(lv, 150, TRUE))
    d <- as.data.frame(lapply(rbind(noise, unanimous), factor, levels = lv))
    tb <- agreement_run(d, vars = c("R1", "R2", "R3"), sft = TRUE)$results$ratingCombinationsTable
    patterns <- table(paste(d$R1, d$R2, d$R3))
    df <- tb$asDF
    expect_equal(as.integer(df$count[1]), as.integer(max(patterns)))    # was: EEE (60) not shown
    expect_false(is.unsorted(rev(as.integer(df$count))))
    expect_equal(paste(df[1, 1], df[1, 2], df[1, 3]), "E E E")
})

# --- Krippendorff (L482, R1, R5, irr complete-data bias) --------------------------------

test_that("L482 alpha is not reported as 1 when every rating is one category", {
    d <- data.frame(R1 = factor(rep("Neg", 20), c("Neg", "Pos")),
                    R2 = factor(rep("Neg", 20), c("Neg", "Pos")),
                    R3 = factor(rep("Neg", 20), c("Neg", "Pos")))
    kt <- agreement_run(d, vars = c("R1", "R2", "R3"), kripp = TRUE)$results$krippTable
    expect_true(is.na(kt$asDF$alpha))                               # was 1.000 "Reliable"
    expect_true("single_category" %in% names(agreement_notes(kt)))
})

test_that("L482 bootstrap resamples holding one category no longer pull the upper limit to 1", {
    lv <- c("Neg", "Pos")
    r <- rep("Neg", 40)
    d <- data.frame(R1 = r, R2 = r, R3 = r, stringsAsFactors = FALSE)
    d$R1[c(1, 2)] <- "Pos"; d$R2[1] <- "Pos"                        # the rare call sits in 2 cases
    d <- as.data.frame(lapply(d, factor, levels = lv))
    kt <- agreement_run(d, vars = c("R1", "R2", "R3"), kripp = TRUE, bootstrap = TRUE,
                        nBoot = 400, seed = 42)$results$krippTable
    # Oracle: redo the resampling with the formula, dropping resamples with one category.
    m <- sapply(d, as.integer)
    withr::local_seed(42)
    boots <- vapply(seq_len(400), function(i) {
        bm <- m[sample(seq_len(nrow(m)), replace = TRUE), , drop = FALSE]
        if (length(unique(bm[!is.na(bm)])) < 2) NA_real_ else kripp_oracle(bm, "nominal")
    }, numeric(1))
    expect_gt(sum(is.na(boots)), 0)                                 # the fixture does hit the case
    expect_equal(kt$asDF$ci_upper, unname(quantile(boots, .975, na.rm = TRUE)), tolerance = 1e-6)
    expect_lt(kt$asDF$ci_upper, 1)
    expect_true("boot_undefined" %in% names(agreement_notes(kt)))
})

test_that("irr complete-data bias: alpha with 3 raters matches the coincidence-matrix formula", {
    withr::local_seed(3)
    t0 <- sample(0:3, 50, TRUE)
    m <- cbind(t0, pmin(3, pmax(0, t0 + sample(c(-1, 0, 0, 0, 1), 50, TRUE))),
               pmin(3, pmax(0, t0 + sample(c(-1, 0, 0, 1), 50, TRUE))))
    lv <- as.character(0:3)
    d <- as.data.frame(lapply(as.data.frame(m), function(x) factor(x, lv)))
    names(d) <- c("R1", "R2", "R3")
    a <- agreement_run(d, vars = names(d), kripp = TRUE, krippMethod = "nominal")
    expect_equal(a$results$krippTable$asDF$alpha, kripp_oracle(m, "nominal"), tolerance = 1e-8)
    expect_equal(kripp_oracle(m, "nominal"),
                 irrCAC::krippen.alpha.raw(as.data.frame(m), categ.labels = 0:3)$est$coeff.val,
                 tolerance = 1e-4)                               # the oracle agrees with irrCAC
})

test_that("R1 the bootstrap Krippendorff row is the headline statistic", {
    skip_if_not_installed("boot")
    withr::local_seed(3)
    t0 <- sample(0:3, 50, TRUE)
    num <- data.frame(R1 = t0, R2 = pmin(3, pmax(0, t0 + sample(c(-1, 0, 0, 0, 1), 50, TRUE))),
                      R3 = pmin(3, pmax(0, t0 + sample(c(-1, 0, 0, 1), 50, TRUE))))
    a <- agreement_run(num, vars = names(num), kripp = TRUE, krippMethod = "ratio",
                       bootstrapCI = TRUE, nBoot = 100)
    oracle <- kripp_oracle(as.matrix(num), "ratio")
    expect_equal(a$results$krippTable$asDF$alpha, oracle, tolerance = 1e-8)
    bt <- a$results$bootstrapCITable$asDF
    expect_equal(bt$estimate[grepl("Krippendorff", bt$metric)], oracle, tolerance = 1e-8)   # was 0.844
})

test_that("R5 interval alpha scores numerically labelled categories by their values", {
    withr::local_seed(5)
    t0 <- sample(c(1, 2, 5, 6), 40, TRUE)
    num <- data.frame(R1 = t0, R2 = ifelse(runif(40) < .8, t0, sample(c(1, 2, 5, 6), 40, TRUE)),
                      R3 = ifelse(runif(40) < .8, t0, sample(c(1, 2, 5, 6), 40, TRUE)))
    fac <- as.data.frame(lapply(num, function(x) factor(x, levels = c(1, 2, 5, 6), ordered = TRUE)))
    oracle <- kripp_oracle(as.matrix(num), "interval")
    kt <- agreement_run(fac, vars = names(fac), kripp = TRUE, krippMethod = "interval")$results$krippTable
    expect_equal(kt$asDF$alpha, oracle, tolerance = 1e-8)        # was scored on positions 1..4
    expect_false("positions" %in% names(agreement_notes(kt)))
    txt <- data.frame(lapply(fac, function(x) factor(c("a", "b", "c", "d")[as.integer(x)], c("a", "b", "c", "d"), ordered = TRUE)))
    kt2 <- agreement_run(txt, vars = names(txt), kripp = TRUE, krippMethod = "interval")$results$krippTable
    expect_true("positions" %in% names(agreement_notes(kt2)))    # positions are disclosed
})

# --- Tie-break scale (L458, L437) and the plurality label (L459) ----------------------

tie_fixture <- function() {
    lv <- c("Low", "Moderate", "High")
    # A1 never used Moderate, so its declared levels are {Low, High}; the two tied
    # cases (High vs Moderate) resolve to High under "highest" on the real scale.
    data.frame(P  = factor(c("High", "High", "Low", "Low", "Moderate", "High", "Low"), lv, ordered = TRUE),
               A1 = factor(c("High", "High", "Low", "Low", "Low", "High", "Low"), c("Low", "High"), ordered = TRUE),
               A2 = factor(c("Moderate", "Moderate", "Low", "Low", "Low", "High", "Low"), lv, ordered = TRUE))
}

test_that("L458 strict accuracy does not depend on the order of the annotator variables", {
    d <- tie_fixture()
    run <- function(vars) {
        tb <- agreement_run(d, vars = vars, multiAnnotatorConcordance = TRUE, predictionColumn = 1,
                            tieBreaker = "highest")$results$concordanceF1Table$asDF
        tb$value[tb$metric == tb$metric[2]]
    }
    # Oracle: plurality with ties broken to the higher grade on Low < Moderate < High.
    ref <- c("High", "High", "Low", "Low", "Low", "High", "Low")
    expect_equal(run(c("P", "A1", "A2")), mean(as.character(d$P) == ref))   # was 0.714 in one order
    expect_equal(run(c("P", "A2", "A1")), mean(as.character(d$P) == ref))
})

test_that("L437 the modal rating's tie-break does not depend on variable order", {
    d <- tie_fixture()[, c("A1", "A2")]
    modal <- function(vars) agreement_run(d, vars = vars, loaVariable = TRUE, detailLevel = "detailed",
                                          tieBreaker = "highest")$results$loaDetailTable$asDF$modal_rating[1:2]
    expect_equal(modal(c("A1", "A2")), c("High", "High"))
    expect_equal(modal(c("A2", "A1")), c("High", "High"))
})

test_that("L459 the strict reference is labelled a plurality, not a majority", {
    tb <- agreement_run(tie_fixture(), vars = c("P", "A1", "A2"), multiAnnotatorConcordance = TRUE,
                        predictionColumn = 1)$results$concordanceF1Table$asDF
    expect_true(any(grepl("Plurality", tb$comparison, fixed = TRUE)))
    expect_false(any(grepl("Majority consensus", tb$comparison, fixed = TRUE)))
})

# --- Level-of-agreement bands (L349, L439, L440) --------------------------------------

loa_fixture <- function() {
    withr::local_seed(1)
    lv <- c("A", "B", "C")
    truth <- sample(lv, 40, TRUE)
    m <- sapply(1:6, function(j) ifelse(runif(40) < 0.85, truth, sample(lv, 40, TRUE)))
    d <- as.data.frame(lapply(as.data.frame(m, stringsAsFactors = FALSE), factor, levels = lv))
    stats::setNames(d, paste0("R", 1:6))
}
loa_oracle <- function(d, probs, tertiles = FALSE) {
    pct <- apply(d, 1, function(r) max(table(r)) / length(r))
    q <- quantile(pct[pct < 1], probs, names = FALSE)
    hi <- q[length(q)]; lo <- q[1]; mid <- if (tertiles) hi else q[2]
    cat <- ifelse(pct == 1, "Absolute", ifelse(pct >= hi, "High", ifelse(pct >= mid, "Moderate",
                  ifelse(pct >= lo, "Low", "Poor"))))
    vapply(c("Absolute", "High", "Moderate", "Low", "Poor"), function(k) sum(cat == k), integer(1))
}

test_that("L349 quartile bands are cut over the cases that are not unanimous", {
    d <- loa_fixture()
    tb <- agreement_run(d, vars = names(d), loaVariable = TRUE, detailLevel = "detailed",
                        loaThresholds = "quartiles", showLoaTable = TRUE)$results$loaTable$asDF
    oracle <- loa_oracle(d, c(.25, .5, .75))
    expect_equal(as.integer(tb$count), unname(oracle))
    expect_gt(tb$count[tb$loa_category == "High"], 0)               # was 0, with "High >= 100%"
})

test_that("L349 tertiles leave the Moderate band empty, as the note says", {
    d <- loa_fixture()
    tb <- agreement_run(d, vars = names(d), loaVariable = TRUE, detailLevel = "detailed",
                        loaThresholds = "tertiles", showLoaTable = TRUE)$results$loaTable$asDF
    oracle <- loa_oracle(d, c(1 / 3, 2 / 3), tertiles = TRUE)
    expect_equal(as.integer(tb$count), unname(oracle))
    expect_equal(tb$count[tb$loa_category == "Moderate"], 0)        # was Low that stayed empty
})

test_that("L439 L440 the level-of-agreement notes sit where the reader needs them", {
    d <- data.frame(R1 = factor(c("A", "B", "A", "B", "A", NA), c("A", "B")),
                    R2 = factor(c("A", "B", "B", "B", NA, NA), c("A", "B")),
                    R3 = factor(c("A", "A", "B", "B", NA, "A"), c("A", "B")))
    a <- agreement_run(d, vars = names(d), loaVariable = TRUE, detailLevel = "detailed",
                       loaThresholds = "custom", loaHighThreshold = 60, loaLowThreshold = 70,
                       showLoaTable = TRUE)
    expect_true("no_level" %in% names(agreement_notes(a$results$loaTable)))          # 2 cases, < 100%
    expect_true("threshold_order" %in% names(agreement_notes(a$results$loaDetailTable)))
})

# --- Value parity for upstream-derived cells (TODO.md, L500) and the review's #5/#6 -----

tab_to_raters <- function(tab, lv) {
    idx <- which(tab > 0, arr.ind = TRUE)
    reps <- tab[idx]
    data.frame(R1 = factor(rep(lv[idx[, 1]], reps), lv), R2 = factor(rep(lv[idx[, 2]], reps), lv))
}

test_that("#6 Stuart-Maxwell and Bhapkar run when the categories form disconnected groups", {
    lv <- c("G1", "G2", "G3", "G4")
    tab <- matrix(c(5, 3, 0, 0, 2, 6, 0, 0, 0, 0, 7, 4, 0, 0, 1, 8), 4, byrow = TRUE)
    d <- tab_to_raters(tab, lv)
    r <- agreement_run(d, vars = c("R1", "R2"), stuartMaxwell = TRUE, bhapkar = TRUE)$results
    # Oracle 1: the groups never exchange cases, so SM is the sum of their McNemar tests.
    mc <- function(b, c) (b - c)^2 / (b + c)
    sm_oracle <- mc(3, 2) + mc(4, 1)
    sm <- r$stuartMaxwellTable$asDF
    expect_equal(sm$chisq, sm_oracle, tolerance = 1e-10)               # was refused (singular)
    expect_equal(sm$df, 2)
    expect_equal(sm$p, pchisq(sm_oracle, 2, lower.tail = FALSE), tolerance = 1e-10)
    # Oracle 2: Bhapkar's n d' S^+ d with the unrestricted covariance of the marginal differences.
    n <- sum(tab); p <- tab / n; dd <- rowSums(p) - colSums(p)
    S <- -(p + t(p)) - outer(dd, dd)
    diag(S) <- rowSums(p) + colSums(p) - 2 * diag(p) - dd^2
    expect_equal(r$bhapkarTable$asDF$chisq, n * drop(t(dd) %*% MASS::ginv(S) %*% dd), tolerance = 1e-8)
})

test_that("L500 Bhapkar on a connected table matches DescTools", {
    skip_if_not_installed("DescTools")
    lv <- c("G1", "G2", "G3")
    tab <- matrix(c(14, 1, 4, 4, 12, 2, 0, 5, 12), 3, byrow = TRUE)
    d <- tab_to_raters(tab, lv)
    b <- agreement_run(d, vars = c("R1", "R2"), bhapkar = TRUE)$results$bhapkarTable$asDF
    o <- DescTools::BhapkarTest(d$R1, d$R2)
    expect_equal(b$chisq, as.numeric(o$statistic), tolerance = 1e-6)
    expect_equal(b$df, as.numeric(o$parameter))
})

test_that("L500 Light's kappa equals the mean of the pairwise Cohen kappas", {
    skip_if_not_installed("vcd")
    withr::local_seed(11)
    lv <- c("A", "B", "C")
    t0 <- sample(lv, 60, TRUE)
    d <- data.frame(R1 = factor(ifelse(runif(60) < .8, t0, sample(lv, 60, TRUE)), lv),
                    R2 = factor(ifelse(runif(60) < .7, t0, sample(lv, 60, TRUE)), lv),
                    R3 = factor(ifelse(runif(60) < .6, t0, sample(lv, 60, TRUE)), lv))
    lk <- agreement_run(d, vars = names(d), lightKappa = TRUE)$results$lightKappaTable$asDF$kappa
    oracle <- mean(combn(3, 2, function(ix)
        vcd::Kappa(table(d[[ix[1]]], d[[ix[2]]]))$Unweighted[["value"]]))
    expect_equal(lk, oracle, tolerance = 1e-8)
})

test_that("L500 the kappa sample size is kappaSize's N", {
    skip_if_not_installed("kappaSize")
    d <- data.frame(A = factor(rep(c("x", "y"), 10)), B = factor(rep(c("x", "y"), 10)))
    for (cfg in list(list(m = "kappa", r = 2, q = 2, f = kappaSize::PowerBinary),
                     list(m = "kappa", r = 2, q = 3, f = kappaSize::Power3Cats),
                     list(m = "fleiss", r = 4, q = 4, f = kappaSize::Power4Cats),
                     list(m = "fleiss", r = 5, q = 5, f = kappaSize::Power5Cats))) {
        tb <- agreement_run(d, vars = c("A", "B"), agreementSampleSize = TRUE, ssMetric = cfg$m,
                            ssKappaNull = 0.4, ssKappaAlt = 0.7, ssNRaters = cfg$r,
                            ssNCategories = cfg$q)$results$agreementSampleSizeTable$asDF
        oracle <- max(ceiling(cfg$f(kappa0 = .4, kappa1 = .7, props = rep(1 / cfg$q, cfg$q),
                                    raters = cfg$r, alpha = .05, power = .8)$N[1]), 2 * cfg$r)
        expect_equal(as.numeric(tb$value[2]), oracle)
    }
    # ICC: Walter, Eliasziw & Donner (1998) eq. 2 written out, two-sided alpha.
    tb <- agreement_run(d, vars = c("A", "B"), agreementSampleSize = TRUE, ssMetric = "icc",
                        ssKappaNull = 0.7, ssKappaAlt = 0.9, ssNRaters = 2)$results$agreementSampleSizeTable$asDF
    C <- function(rho) 1 + 2 * rho / (1 - rho)
    expect_equal(as.numeric(tb$value[2]),
                 ceiling(1 + 2 * 2 * (qnorm(.975) + qnorm(.8))^2 / ((2 - 1) * log(C(.9) / C(.7))^2)))
})

test_that("#5 a case rated by only some raters is not described as all raters concurring", {
    lv <- c("A", "B", "C")
    d <- data.frame(R1 = factor(c("A", "A", "A", "B", "A"), lv), R2 = factor(c("A", "A", "B", "B", "A"), lv),
                    R3 = factor(c(NA, "A", "C", "B", "A"), lv))
    a <- agreement_run(d, vars = names(d), loaVariable = TRUE, detailLevel = "simple", showLoaTable = TRUE)
    expect_true("partial_full" %in% names(agreement_notes(a$results$loaTable)))   # case 1: 2 of 3 rated
    expect_false(any(grepl("across all raters", a$results$loaTable$asDF$interpretation, fixed = TRUE)))
})

test_that("concordance per-class rows follow the declared order", {
    lv <- c("Negative", "Weak", "Strong")
    withr::local_seed(4)
    t0 <- sample(lv, 40, TRUE)
    d <- data.frame(P = factor(ifelse(runif(40) < .8, t0, sample(lv, 40, TRUE)), lv, ordered = TRUE),
                    A1 = factor(t0, lv, ordered = TRUE),
                    A2 = factor(ifelse(runif(40) < .9, t0, sample(lv, 40, TRUE)), lv, ordered = TRUE))
    pc <- agreement_run(d, vars = names(d), multiAnnotatorConcordance = TRUE,
                        predictionColumn = 1)$results$concordanceF1PerClassTable$asDF
    expect_equal(as.character(pc$class_label), lv)                    # was Negative, Strong, Weak
})

test_that("#4 one pair has one weighted kappa: headline, subgroup and All-Pairs agree", {
    skip_if_not_installed("vcd")
    lv <- c("G1", "G2", "G3", "G4")                              # G3 declared, never used
    withr::local_seed(12)
    x <- sample(c(1, 2, 4), 80, TRUE, prob = c(.4, .35, .25)); y <- x
    f <- sample(80, 25); y[f] <- sample(c(1, 2, 4), 25, TRUE)
    d <- data.frame(R1 = factor(lv[x], lv, ordered = TRUE), R2 = factor(lv[y], lv, ordered = TRUE),
                    R3 = factor(lv[y], lv, ordered = TRUE), site = factor(rep("S1", 80)))
    # Oracle: vcd on the categories the raters used (the headline's scale).
    used <- c("G1", "G2", "G4")
    oracle <- vcd::Kappa(table(factor(lv[x], used), factor(lv[y], used)),
                         weights = "Fleiss-Cohen")$Weighted[["value"]]
    a <- agreement_run(d, vars = c("R1", "R2"), wght = "squared", agreementBySubgroup = TRUE,
                       subgroupVariable = "site", subgroupMinCases = 5)
    expect_equal(a$results$irrtable$asDF$kappa, oracle, tolerance = 1e-8)
    expect_equal(a$results$subgroupAgreementTable$asDF$agreement_stat, oracle, tolerance = 1e-8)   # was 0.705
    ap <- agreement_run(d, vars = c("R1", "R2", "R3"), wght = "squared",
                        allPairsKappa = TRUE)$results$allPairsKappaTable$asDF
    expect_equal(ap$kappa[ap$rater_a == "R1" & ap$rater_b == "R2"], oracle, tolerance = 1e-8)
})

test_that("L427 identical measurements on the ICC table: ICC 1, no F/p/CI, no sentinel advice", {
    withr::local_seed(9)
    for (x in list(c(12, 18, 25, 31, 40, 47, 55, 63, 70, 85), round(runif(25, 5, 95), 1))) {
        d <- data.frame(R1 = x, R2 = x, R3 = x)
        tb <- agreement_run(d, vars = names(d), icc = TRUE, iccType = "icc21")$results$iccTable
        v <- tb$asDF
        expect_equal(v$icc_value, 1)
        expect_true(all(is.na(c(v$ci_lower, v$ci_upper, v$f_value, v$p))))
        notes <- paste(agreement_notes(tb), collapse = " ")
        expect_true(grepl("identical ratings", notes, fixed = TRUE))
        expect_false(grepl("999", notes, fixed = TRUE))            # was: "check for sentinel codes"
    }
})
