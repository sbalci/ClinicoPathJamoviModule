# Headline Fleiss'/Conger's kappa for 3+ raters uses EVERY observed rating
# (irrCAC, Gwet 2014) instead of irr::kappam.fleiss on complete cases.
# Oracles: irr on complete data, and Gwet's missing-data formula written out by
# hand on a 6-case x 3-rater example. Helpers are in helper-agreement.R.

agr_fl_complete <- function() {
    withr::local_seed(11)
    n <- 60; lv <- c("Low", "Moderate", "High")
    base <- sample(lv, n, TRUE)
    jit <- function() ifelse(stats::runif(n) < 0.7, base, sample(lv, n, TRUE))
    data.frame(r1 = factor(base, levels = lv), r2 = factor(jit(), levels = lv),
               r3 = factor(jit(), levels = lv), r4 = factor(jit(), levels = lv))
}

test_that("complete data: Fleiss equals irr::kappam.fleiss, Conger equals exact = TRUE", {
    skip_if_not_installed("irr"); skip_if_not_installed("irrCAC")
    d <- agr_fl_complete()
    vars <- c("r1", "r2", "r3", "r4")
    chr <- as.data.frame(lapply(d, as.character))
    fl <- agreement_run(d, vars = vars)$results$irrtable$asDF
    co <- agreement_run(d, vars = vars, exct = TRUE)$results$irrtable$asDF
    expect_equal(fl$kappa[1], irr::kappam.fleiss(chr, exact = FALSE)$value, tolerance = 1e-10)
    expect_equal(co$kappa[1], irr::kappam.fleiss(chr, exact = TRUE)$value, tolerance = 1e-10)
    expect_equal(fl$subjects[1], nrow(d))
})

test_that("incomplete data: kappa equals Gwet's generalisation computed by hand", {
    skip_if_not_installed("irrCAC")
    # Case 4 has ONE rating: it enters the category prevalences, not observed
    # agreement. Cases 2 and 6 have two ratings each.
    d <- data.frame(
        r1 = factor(c("A", "A", "B", "A",  "B", NA),  levels = c("A", "B")),
        r2 = factor(c("A", "B", "B", NA,   "B", "A"), levels = c("A", "B")),
        r3 = factor(c("A", NA,  "B", NA,   "A", "A"), levels = c("A", "B")))
    # Per case: r_i ratings, r_ik in category k.
    #   pa = mean over cases with r_i >= 2 of sum_k r_ik (r_ik - 1) / (r_i (r_i - 1))
    #      = (1 + 0 + 1 + 1/3 + 1) / 5 = 2/3
    #   pi_k = mean over ALL 6 cases of r_ik / r_i
    #      pi_A = (1 + 1/2 + 0 + 1 + 1/3 + 1) / 6 = 23/36, pi_B = 13/36
    #   pe = pi_A^2 + pi_B^2 = 698/1296;  kappa = (pa - pe) / (1 - pe) = 166/598
    hand <- function(d) {
        m <- as.matrix(as.data.frame(lapply(d, as.character)))
        cats <- sort(unique(m[!is.na(m)]))
        rik <- sapply(cats, function(k) rowSums(m == k, na.rm = TRUE))
        ri <- rowSums(rik)
        two <- ri >= 2
        pa <- mean(rowSums(rik[two, ] * (rik[two, ] - 1)) / (ri[two] * (ri[two] - 1)))
        pe <- sum(colMeans(rik / ri)^2)
        (pa - pe) / (1 - pe)
    }
    expect_equal(hand(d), 166 / 598, tolerance = 1e-12)

    r <- agreement_run(d, vars = c("r1", "r2", "r3"))$results$irrtable
    got <- r$asDF
    expect_equal(got$kappa[1], 166 / 598, tolerance = 1e-10)
    # listwise deletion would have kept only cases 1, 3, 5
    expect_equal(got$subjects[1], 6)
    notes <- agreement_notes(r)
    expect_true(grepl("every observed rating", notes[["missing"]], fixed = TRUE))
    expect_true(grepl("3 complete cases", notes[["missing"]], fixed = TRUE))
})

test_that("non-alphabetical labels do not change the value", {
    skip_if_not_installed("irr"); skip_if_not_installed("irrCAC")
    d <- agr_fl_complete()
    d$r2[c(3, 9)] <- NA; d$r4[c(5, 20, 33)] <- NA
    vars <- c("r1", "r2", "r3", "r4")
    # Same data, labels whose alphabetical order differs from the declared one
    # and which collide under upper-casing (irrCAC upper-cases character labels).
    relab <- as.data.frame(lapply(d, function(x) factor(
        c(Low = "z", Moderate = "Z", High = "a")[as.character(x)], levels = c("z", "Z", "a"))))
    a <- agreement_run(d, vars = vars)$results$irrtable$asDF
    b <- agreement_run(relab, vars = vars)$results$irrtable$asDF
    expect_equal(b$kappa[1], a$kappa[1], tolerance = 1e-12)
    expect_equal(b$ci_lower[1], a$ci_lower[1], tolerance = 1e-12)
    # and irrCAC called directly on the original labels agrees
    ref <- irrCAC::fleiss.kappa.raw(as.matrix(as.data.frame(lapply(d, as.character))))$est
    expect_equal(a$kappa[1], (ref$pa - ref$pe) / (1 - ref$pe), tolerance = 1e-10)
})

test_that("the interval, z and two-sided p are reported and match irrCAC", {
    skip_if_not_installed("irrCAC")
    d <- agr_fl_complete()
    d$r3[c(2, 7, 40)] <- NA
    r <- agreement_run(d, vars = c("r1", "r2", "r3", "r4"), confLevel = 0.9)$results$irrtable
    got <- r$asDF
    ref <- irrCAC::fleiss.kappa.raw(as.matrix(as.data.frame(lapply(d, as.character))),
                                    conflev = 0.9)$est
    k <- (ref$pa - ref$pe) / (1 - ref$pe)
    n <- nrow(d)
    expect_true(is.finite(got$ci_lower[1]) && is.finite(got$ci_upper[1]))
    expect_equal(got$ci_lower[1], k - stats::qt(0.95, n - 1) * ref$coeff.se, tolerance = 1e-10)
    expect_equal(got$ci_upper[1], min(1, k + stats::qt(0.95, n - 1) * ref$coeff.se), tolerance = 1e-10)
    # irrCAC's own interval string (3 dp) agrees
    lims <- as.numeric(strsplit(gsub("[()]", "", ref$conf.int), ",")[[1]])
    expect_equal(c(got$ci_lower[1], got$ci_upper[1]), lims, tolerance = 1e-3)
    expect_equal(got$z[1], k / ref$coeff.se, tolerance = 1e-10)
    # irrCAC 1.4 reports ONE-sided p = 1 - pt(t, n - 1); the table is two-sided
    expect_equal(got$p[1], 2 * stats::pt(-abs(k / ref$coeff.se), n - 1), tolerance = 1e-10)
    notes <- agreement_notes(r)
    expect_true(grepl("subject-sampling standard error", notes[["ci_method"]], fixed = TRUE))
    expect_false("ci_multi" %in% names(notes))

    # Conger (exact) now carries a test and an interval too
    co <- agreement_run(d, vars = c("r1", "r2", "r3", "r4"), exct = TRUE)$results$irrtable
    expect_true(is.finite(co$asDF$p[1]) && is.finite(co$asDF$ci_lower[1]))
    expect_false("exact_note" %in% names(agreement_notes(co)))
})

test_that("one category in use stays undefined, not kappa = 1", {
    # irrCAC substitutes pe = 1e-15 when only one label exists and returns 1.
    d <- data.frame(A = factor(rep("pos", 20)), B = factor(rep("pos", 20)),
                    C = factor(c(rep("pos", 19), NA)))
    r <- agreement_run(d, vars = c("A", "B", "C"))$results$irrtable
    expect_true(is.na(r$asDF$kappa[1]))
    expect_true("undefined_kappa" %in% names(agreement_notes(r)))
})

test_that("an empty rater column: Conger's kappa is computed without it, Agreement % is blanked with a note", {
    skip_if_not_installed("irrCAC")
    d <- agr_fl_complete()
    d$r5 <- factor(NA, levels = levels(d$r1))
    vars <- c("r1", "r2", "r3", "r4")
    for (ex in c(FALSE, TRUE)) {
        with_empty <- agreement_run(d, vars = c(vars, "r5"), exct = ex)$results$irrtable
        without <- agreement_run(d, vars = vars, exct = ex)$results$irrtable
        expect_true(is.finite(with_empty$asDF$kappa[1]), info = paste("exct", ex))
        expect_equal(with_empty$asDF$kappa[1], without$asDF$kappa[1], tolerance = 1e-10)
        expect_true(is.na(with_empty$asDF$peragree[1]))
        expect_true(grepl("no case was", agreement_notes(with_empty)[["peragree_empty"]], fixed = TRUE))
        expect_false("undefined_kappa" %in% names(agreement_notes(with_empty)))
    }
})
