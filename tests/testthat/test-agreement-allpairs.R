# Tests for the All-Pairs Cohen's Kappa table in `agreement`.
#
# Focus: C(k,2) row counts, multiple-testing correction equivalence to
# stats::p.adjust, weighted-kappa parity with irr::kappa2, and -- the key
# regression test -- that per-pair 95% CIs match psych::cohen.kappa() (i.e. use
# the non-null asymptotic SE, not the irr::kappa2 null-hypothesis SE).

testthat::skip_if_not_installed("vcd")
testthat::skip_if_not_installed("psych")
testthat::skip_if_not_installed("irr")

# Synthetic k-rater ordinal data with controllable agreement.
make_raters <- function(n, k, C = 4, agree = 0.7, seed = 1) {
    set.seed(seed)
    truth <- sample(seq_len(C), n, replace = TRUE)
    df <- as.data.frame(lapply(seq_len(k), function(j) {
        r <- ifelse(stats::runif(n) < agree, truth,
                    sample(seq_len(C), n, replace = TRUE))
        factor(r, levels = seq_len(C), ordered = TRUE)
    }))
    names(df) <- paste0("R", seq_len(k))
    df
}

test_that("allPairsKappa yields choose(k, 2) rows for k = 3..6", {
    for (k in 3:6) {
        df <- make_raters(150, k, seed = 100 + k)
        res <- agreement(data = df, vars = names(df), allPairsKappa = TRUE)
        tab <- res$allPairsKappaTable$asDF
        expect_equal(nrow(tab), choose(k, 2),
                     info = paste("k =", k))
    }
})

test_that("Bonferroni / BH adjusted p-values match stats::p.adjust", {
    df <- make_raters(150, 4, agree = 0.35, seed = 21)  # weak agreement -> p varies

    res_b <- agreement(data = df, vars = names(df),
                       allPairsKappa = TRUE, multipleTestCorrection = "bonferroni")
    tb <- res_b$allPairsKappaTable$asDF
    expect_equal(tb$p_adj, stats::p.adjust(tb$p, method = "bonferroni"))
    expect_true(all(tb$p_adj >= tb$p, na.rm = TRUE))

    res_bh <- agreement(data = df, vars = names(df),
                        allPairsKappa = TRUE, multipleTestCorrection = "bh")
    tbh <- res_bh$allPairsKappaTable$asDF
    expect_equal(tbh$p_adj, stats::p.adjust(tbh$p, method = "BH"))
})

test_that("unweighted per-pair CI matches psych::cohen.kappa (non-null ASE)", {
    df <- make_raters(120, 4, agree = 0.6, seed = 7)
    res <- agreement(data = df, vars = names(df), allPairsKappa = TRUE)
    tab <- res$allPairsKappaTable$asDF

    sub <- df[, c("R1", "R2")]
    sub <- sub[stats::complete.cases(sub), ]
    ck <- psych::cohen.kappa(data.frame(lapply(sub, as.integer)))

    row <- tab[tab$rater_a == "R1" & tab$rater_b == "R2", ]
    expect_equal(unname(row$kappa), unname(ck$kappa), tolerance = 1e-3)
    expect_equal(row$ci_lower, unname(ck$confid["unweighted kappa", "lower"]),
                 tolerance = 1e-2)
    expect_equal(row$ci_upper, unname(ck$confid["unweighted kappa", "upper"]),
                 tolerance = 1e-2)
    # CI must bracket the estimate and the SE must be positive.
    expect_true(row$ci_lower < row$kappa && row$kappa < row$ci_upper)
    expect_true(row$se > 0)
})

test_that("weighted (squared) per-pair kappa matches irr::kappa2", {
    df <- make_raters(120, 4, agree = 0.6, seed = 9)
    res <- agreement(data = df, vars = names(df),
                     allPairsKappa = TRUE, wght = "squared")
    tab <- res$allPairsKappaTable$asDF

    sub <- df[, c("R1", "R2")]
    sub <- sub[stats::complete.cases(sub), ]
    expected <- irr::kappa2(sub, weight = "squared")$value

    row <- tab[tab$rater_a == "R1" & tab$rater_b == "R2", ]
    expect_equal(unname(row$kappa), unname(expected), tolerance = 1e-3)
})

test_that("pairs with < 5 complete cases are flagged without crashing", {
    df <- make_raters(20, 3, seed = 3)
    df$R3[1:18] <- NA  # only 2 complete cases in any pair involving R3
    expect_error({
        res <- agreement(data = df, vars = names(df), allPairsKappa = TRUE)
        tab <- res$allPairsKappaTable$asDF
        # R1-R3 and R2-R3 should be NA kappa; R1-R2 should be finite
        r13 <- tab[tab$rater_a == "R1" & tab$rater_b == "R3", ]
        expect_true(is.na(r13$kappa))
    }, NA)
})

test_that("perfect-agreement pairs do not crash (vcd -> irr fallback)", {
    n <- 30
    set.seed(4)
    v <- factor(sample(1:3, n, replace = TRUE), levels = 1:3, ordered = TRUE)
    df <- data.frame(R1 = v, R2 = v, R3 = v)  # all three identical
    expect_error({
        res <- agreement(data = df, vars = names(df), allPairsKappa = TRUE)
        tab <- res$allPairsKappaTable$asDF
        expect_true(all(tab$peragree == 1 | is.na(tab$peragree)))
    }, NA)
})

test_that("fewer than 3 raters produces no all-pairs rows", {
    df <- make_raters(40, 2, seed = 6)
    res <- agreement(data = df, vars = names(df), allPairsKappa = TRUE)
    tab <- res$allPairsKappaTable$asDF
    expect_equal(nrow(tab), 0L)
})

test_that("a low-prevalence category triggers the kappa-paradox advisory", {
    set.seed(1)
    n <- 60
    common <- sample(c("Benign", "Atypical"), n, replace = TRUE, prob = c(.8, .2))
    df <- data.frame(R1 = common, R2 = common, R3 = common)
    df$R3[1:2] <- "Rare"                      # rare category, n = 2 (< 5)
    df[] <- lapply(df, factor)
    res <- agreement(data = df, vars = names(df))
    note <- res$irrtable$notes$prevalence$note
    expect_true(!is.null(note))
    expect_match(note, "Low-prevalence")
    expect_match(note, "Rare")
})

test_that("a well-balanced design raises no prevalence advisory", {
    df <- make_raters(150, 3, C = 3, agree = 0.7, seed = 31)
    res <- agreement(data = df, vars = names(df))
    expect_null(res$irrtable$notes$prevalence)
})
