# Validation regression tests for kappaSizeCI (/validate-function depth=exhaustive, 2026-10-15).
# Report: development-ideas/kappaSizeCI-validation-2026-10-15.md
# Evidence script: development-scripts/validate_kappaSizeCI.R
#
# No expected value below comes from kappaSize or from R/kappaSizeCI.b.R / R/utils-kappasize.R:
#   * hand arithmetic, written out next to each value;
#   * the sample sizes printed in Rotondi & Donner 2012 (J Clin Epidemiol 65:778-84), Tables 2 and 4;
#   * an own goodness-of-fit chi-square from the published models - binary: Altaye, Donner & Klar
#     2001 eq. (4) (Biometrics 57:584-8, p.586) with the disagreement cells pooled; 3-5 categories:
#     the general formula printed in RD2012 p.779 - and N = the smallest N whose expected X^2
#     reaches qchisq at every requested limit (RD2012 p.780);
#   * an own GOF interval inverter pinned to the Donner & Eliasziw 1992 worked example (p.1519).
# VAL-kappaSizeCI-01..07 were written skip-guarded with the correct expectation, checked to FAIL on
# the unfixed code (all 7 did), and made live by the fix of 2026-10-15: the analysis now reports the
# closed-form minimum n, judges the pooled cells, and carries the corrected Notes and refs.
#
# Independent review, 2026-10-15 (development-ideas/kappaSizeCI-independent-review-2026-10-15.md):
# except for a binary outcome with two raters, the reported n is now the larger of the fixed-proportion
# minimum and the restricted-ML minimum - the proportions re-estimated by ML at each kappa from the pooled cells
# (Altaye, Donner & Eliasziw 2001 eq. (8)-(9)), the interval the Notes recommend there. vci_N_gov()
# is that contract; its restricted fit is an own golden-section / BFGS fit on the cells below, not
# the module's. IR-kappaSizeCI-01..05 are at the end of the file.

skip_if_not_installed("kappaSize")

vci_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", citype = "two_sided", kappa0 = 0.60, kappaL = 0.40, kappaU = 0.80,
             props = "0.20, 0.80", raters = "2", alpha = 0.05),
        list(...))
    suppressWarnings(do.call(ClinicoPath::kappaSizeCI, args))
}
vci_n <- function(res) {
    l <- strsplit(res$text1$content, "\n")[[1]][1]
    as.numeric(gsub(",", "", regmatches(l, regexpr("[0-9][0-9,]*", l)), fixed = TRUE))
}
vci_lambda <- function(k, p, rho) p^k + rho * (1 - p) * sum(p^(k - seq_len(k - 1)))   # ADK eq. (4)
vci_cells_bin <- function(R, p, rho) {
    pos <- vci_lambda(R, p, rho); neg <- vci_lambda(R, 1 - p, rho)
    c(neg, 1 - pos - neg, pos)
}
vci_agree <- function(n, pj, k)                                                     # RD2012 p.779
    pj^n * prod(1 + k * (seq_len(n - 1) - pj) / pj) / prod(1 + (0:(n - 2)) * k)
vci_cells_cat <- function(R, p, rho) {
    a <- vapply(p, function(pj) vci_agree(R, pj, rho), numeric(1))
    c(1 - sum(a), a)
}
vci_cells <- function(K, R, p, rho) if (K == 2) vci_cells_bin(R, p[1], rho) else vci_cells_cat(R, p, rho)
vci_slope <- function(K, R, p, k0, rho) {
    a <- vci_cells(K, R, p, k0); b <- vci_cells(K, R, p, rho); sum((a - b)^2 / b)
}
vci_min_slope <- function(K, R, p, k0, kL, kU = NA)
    min(vci_slope(K, R, p, k0, kL), if (is.na(kU)) Inf else vci_slope(K, R, p, k0, kU))
# the defined minimum: smallest N with expected X^2 >= the critical value at every requested limit
vci_N <- function(K, R, p, k0, kL, kU = NA, alpha = 0.05) {
    crit <- if (is.na(kU)) qchisq(1 - 2 * alpha, 1) else qchisq(1 - alpha, 1)
    ceiling(crit / vci_min_slope(K, R, p, k0, kL, kU))
}
# Expected chi-square per subject with the proportions RE-ESTIMATED by ML at rho from the pooled cells
# (ADE 2001 eq. (9)): binary by golden-section search on p, 3-5 categories by BFGS on K - 1 logits.
vci_rml_slope <- function(K, R, p, k0, rho) {
    q <- vci_cells(K, R, p, k0)
    if (K == 2) {
        ph <- optimize(function(pp) -sum(q * log(vci_cells_bin(R, pp, rho))), c(1e-6, 1 - 1e-6),
                       tol = 1e-12)$minimum
        b <- vci_cells_bin(R, ph, rho)
    } else {
        nll <- function(th) {
            pp <- exp(c(th, 0)); b <- vci_cells_cat(R, pp / sum(pp), rho)
            if (any(b <= 0)) 1e10 else -sum(q * log(b))
        }
        o <- optim(log(p[-K] / p[K]), nll, method = "BFGS", control = list(reltol = 1e-14, maxit = 1000))
        pp <- exp(c(o$par, 0)); b <- vci_cells_cat(R, pp / sum(pp), rho)
    }
    sum((q - b)^2 / b)
}
# the reported n: the fixed-proportion minimum, and - except for a binary outcome with two raters,
# where Donner & Eliasziw's plug-in interval is calibrated - also the restricted-ML minimum
vci_N_gov <- function(K, R, p, k0, kL, kU = NA, alpha = 0.05) {
    n <- vci_N(K, R, p, k0, kL, kU, alpha)
    if (K == 2 && R == 2) return(n)
    crit <- if (is.na(kU)) qchisq(1 - 2 * alpha, 1) else qchisq(1 - alpha, 1)
    s <- min(vci_rml_slope(K, R, p, k0, kL), if (is.na(kU)) Inf else vci_rml_slope(K, R, p, k0, kU))
    max(n, ceiling(crit / s))
}
vci_limits <- function(K, R, p, k0, n, crit) {
    f <- function(r) n * vci_slope(K, R, p, k0, r) - crit
    c(lo = if (f(1e-6) > 0) uniroot(f, c(1e-6, k0), tol = 1e-12)$root else -Inf,
      hi = if (f(1 - 1e-9) > 0) uniroot(f, c(k0, 1 - 1e-9), tol = 1e-12)$root else 1)
}
vci_pstr <- function(p) paste(base::format(p, scientific = FALSE, trim = TRUE), collapse = ", ")

# RD2012 Tables 2 (p.780) and 4 (p.781), raters 2..6
vci_rd <- list(
    list(.44, c(.11, .70, .19), .39, .49, c(1209, 696, 547, 478, 437)),
    list(.44, c(.11, .70, .19), .34, .54, c( 306, 171, 135, 119, 110)),
    list(.66, c(.01, .12, .33, .36, .18), .61, .71, c(600, 372, 295, 256, 232)),
    list(.66, c(.01, .12, .33, .36, .18), .56, .76, c(161,  97,  76,  65,  58)),
    list(.66, c(.13, .33, .36, .18),      .56, .76, c(162,  98,  76,  65,  59)))


test_that("C01 the oracle reproduces every RD2012 Table 2/4 value with the paper's own 3.84", {
    # RD2012 p.780 prints the critical value as 3.84. With it (and kappaSize's 0.001 stopping
    # offset) the oracle gives all 25 printed values; this pins the oracle to the paper before it is
    # used as the reference below.
    for (d in vci_rd) for (i in 1:5) {
        s <- vci_min_slope(length(d[[2]]), i + 1, d[[2]], d[[1]], d[[3]], d[[4]])
        expect_equal(ceiling((3.84 + 0.001) / s), d[[5]][i],
                     label = sprintf("k0 %s [%s,%s] raters %d", d[[1]], d[[3]], d[[4]], i + 1))
    }
})


test_that("C01 hand-calculated sample sizes", {
    # 2 raters, p = .5, [.4, .8]: slope_L = 1/70 + 1/30 = 1/21 -> N* = 21 x 3.841459 = 80.67
    expect_equal(vci_n(vci_run(props = "0.5, 0.5")), 81)
    # default: slope_L = .032^2/.104 + .064^2/.192 + .032^2/.704 = .032634 -> N* = 117.71
    expect_equal(vci_n(vci_run()), 118)
    # one-sided, p = .5, kappa0 .6, kappaL .2: slope 1/6, crit qchisq(.9, 1) -> N* = 16.23
    expect_equal(vci_n(vci_run(citype = "one_sided", props = "0.5", kappaL = 0.2)), 17)
    # 3 raters binary, p = .5: pooled cells .125 + .375k, .75(1 - k): slope_L = 1/11 -> N* = 42.26
    expect_equal(vci_n(vci_run(props = "0.5", raters = "3")), 43)
    # 3 categories (.2, .3, .5), 2 raters: slope_L = .0684603 -> N* = 56.11
    expect_equal(vci_n(vci_run(outcome = "3", props = "0.2, 0.3, 0.5")), 57)
})


test_that("C01 the n equals the governing minimum on every RD2012 Table 2 and 4 cell", {
    # Two raters: the fixed-proportion minimum, which is the printed value except Table 4's 6-rater
    # cells (the paper rounded the critical value to 3.84). Three or more raters: the larger of that
    # and the restricted-ML minimum (IR-kappaSizeCI-01).
    for (d in vci_rd) for (i in 1:5) {
        K <- length(d[[2]])
        expect_equal(vci_n(vci_run(outcome = as.character(K), props = vci_pstr(d[[2]]), kappa0 = d[[1]],
                                   kappaL = d[[3]], kappaU = d[[4]], raters = as.character(i + 1))),
                     vci_N_gov(K, i + 1, d[[2]], d[[1]], d[[3]], d[[4]]),
                     label = sprintf("k0 %s [%s,%s] raters %d", d[[1]], d[[3]], d[[4]], i + 1))
    }
    # the paper's own worked sentence (p.781): three raters and 171 films with the proportions
    # fixed; re-estimating them, as its own interval does (p.779), needs 190
    expect_equal(vci_N(3, 3, c(.11, .70, .19), .44, .34, .54), 171)
    expect_equal(vci_n(vci_run(outcome = "3", props = "0.11, 0.70, 0.19", raters = "3",
                               kappa0 = .44, kappaL = .34, kappaU = .54)), 190)
})


test_that("VAL-kappaSizeCI-01 the reported n is the minimum, not one or two above it", {
    # kappaSize stops at X^2 >= crit + 0.001, so its own n can exceed the minimum by 1-2 subjects.
    # Binary, two raters (the fixed-proportion n governs there): [.55, .65] is 1624, kappaSize 1625.
    r <- vci_run(kappaL = .55, kappaU = .65)
    expect_equal(vci_n(r), 1624)
    # the Summary pane prints kappaSize's 1625 and says why, beside it
    expect_match(r$text_summary$content, "A minimum of 1625 subjects", fixed = TRUE)
    expect_match(r$text_summary$content, "reports 1,625 subjects; the required\nsample size is 1,624.",
                 fixed = TRUE)
    # RD2012 Table 2's two-rater cell: the fixed-proportion minimum is 1209 (kappaSize 1210), but
    # with 3 categories the proportions are re-estimated (IR-kappaSizeCI-01), so 1239 is reported
    # and the pane gives THAT reason instead of the stopping margin
    r <- vci_run(outcome = "3", props = "0.11, 0.70, 0.19", kappa0 = .44, kappaL = .39, kappaU = .49,
                 raters = "2")
    expect_equal(vci_N(3, 2, c(.11, .70, .19), .44, .39, .49), 1209)
    expect_equal(vci_n(r), vci_N_gov(3, 2, c(.11, .70, .19), .44, .39, .49))
    expect_equal(vci_n(r), 1239)
    expect_match(r$text_summary$content, "A minimum of 1210 subjects", fixed = TRUE)
    expect_match(r$text_summary$content, "reports 1,210 subjects, holding the", fixed = TRUE)
    expect_false(grepl("stops only\nonce the chi-square", r$text_summary$content, fixed = TRUE))
    # and says nothing when the two agree
    expect_false(grepl("own search", vci_run()$text_summary$content, fixed = TRUE))
    # four raters: the fixed-proportion minimum is the printed 547; the reported n also covers
    # re-estimating the proportions (IR-kappaSizeCI-01)
    expect_equal(vci_N(3, 4, c(.11, .70, .19), .44, .39, .49), 547)
    expect_equal(vci_n(vci_run(outcome = "3", props = "0.11, 0.70, 0.19", kappa0 = .44,
                               kappaL = .39, kappaU = .49, raters = "4")),
                 vci_N_gov(3, 4, c(.11, .70, .19), .44, .39, .49))
    # binary, p = .01, [.5, .7], alpha .10: defined minimum 4578, kappaSize 4580
    expect_equal(vci_n(vci_run(props = "0.01", kappaL = .5, kappaU = .7, alpha = 0.10)), 4578)
})


test_that("C01 the reported n is the ADK eq. (4) governing minimum for binary designs (raters 2-6, p .5 and .05)", {
    for (R in 2:6) for (p in c(0.5, 0.05)) {
        expect_equal(vci_n(vci_run(props = vci_pstr(c(p, 1 - p)), raters = as.character(R))),
                     vci_N_gov(2, R, p, .6, .4, .8), label = sprintf("raters %d p %s", R, p))
    }
})


test_that("C02 at the reported n the expected confidence limits lie inside the requested ones", {
    # RD2012 p.780: N "will ensure that a two-sided 95% CI for kappa has an expected lower bound of
    # kL and an expected upper bound of kU". Inverting the expected X^2 at n must give limits inside.
    designs <- list(list(2, 2, .2, .6, .4, .8), list(2, 4, .2, .6, .4, .8),
                    list(3, 3, c(.11, .70, .19), .44, .34, .54),
                    list(5, 4, c(.01, .12, .33, .36, .18), .66, .56, .76))
    for (d in designs) {
        K <- d[[1]]; R <- d[[2]]; p <- d[[3]]
        n <- vci_n(vci_run(outcome = as.character(K), props = vci_pstr(if (K == 2) c(p, 1 - p) else p),
                           raters = as.character(R), kappa0 = d[[4]], kappaL = d[[5]], kappaU = d[[6]]))
        lim <- vci_limits(K, R, p, d[[4]], n, qchisq(.95, 1))
        expect_gte(lim[["lo"]], d[[5]] - 1e-9)
        expect_lte(lim[["hi"]], d[[6]] + 1e-9)
    }
    # default design: limits 0.4003 / 0.7489 at n = 118
    lim <- vci_limits(2, 2, .2, .6, 118, qchisq(.95, 1))
    expect_equal(unname(round(lim, 4)), c(0.4003, 0.7489))
})


test_that("C01 metamorphic: relabelling categories never changes n", {
    for (pp in c("0.05, 0.95", "0.2, 0.8"))
        expect_equal(vci_n(vci_run(props = pp, raters = "5")),
                     vci_n(vci_run(props = paste(rev(strsplit(pp, ", ")[[1]]), collapse = ", "),
                                   raters = "5")))
    base <- vci_n(vci_run(outcome = "5", props = "0.01, 0.12, 0.33, 0.36, 0.18", raters = "4",
                          kappa0 = .66, kappaL = .56, kappaU = .76))
    for (q in c("0.36, 0.01, 0.18, 0.12, 0.33", "0.18, 0.36, 0.33, 0.12, 0.01"))
        expect_equal(vci_n(vci_run(outcome = "5", props = q, raters = "4",
                                   kappa0 = .66, kappaL = .56, kappaU = .76)), base, label = q)
})


test_that("C03/C04/C06/C07 the confidence level and n agree across panes", {
    for (al in c(0.025, 0.001)) {
        r <- vci_run(alpha = al)
        pct <- base::format(100 * (1 - al), trim = TRUE)
        expect_match(r$text2$content, paste0("Confidence level: ", pct, "%"), fixed = TRUE)
        expect_match(r$notices$content, paste0("the ", pct, "% confidence interval (two-sided)"), fixed = TRUE)
    }
    # [.55, .65]: slope_L = .002365690, N* = 3.841459/.002365690 = 1623.82 -> 1624 (kappaSize: 1625)
    r <- vci_run(kappaL = 0.55, kappaU = 0.65)
    expect_equal(vci_n(r), vci_N(2, 2, .2, .6, .55, .65))
    expect_equal(vci_n(r), 1624)
    expect_match(r$text_summary$content, "A minimum of 1625 subjects", fixed = TRUE)
    expect_match(r$text_summary$content, "reports 1,625 subjects", fixed = TRUE)
    expect_match(r$notices$content, "(1,624)", fixed = TRUE)
    # one-sided uses qchisq(1 - 2 alpha, 1) (RD2012 p.780: 2.71 at 5%)
    # slope_L = .032634, crit = qchisq(.80, 1) = 1.642374 -> N* = 50.33 -> 51
    expect_equal(vci_n(vci_run(citype = "one_sided", alpha = 0.10)), vci_N(2, 2, .2, .6, .4, NA, 0.10))
    expect_equal(vci_N(2, 2, .2, .6, .4, NA, 0.10), 51)
})


test_that("C10 the large-sample warning starts above 1,000 exactly", {
    # binary, 2 raters, p = .5, kappa0 .6, kappaU .8; kappaL found by search with the oracle
    r1000 <- vci_run(props = "0.5", kappaL = 0.54814855209)
    r1001 <- vci_run(props = "0.5", kappaL = 0.54817556852)
    expect_equal(c(vci_n(r1000), vci_n(r1001)), c(1000, 1001))
    expect_false(grepl("very large", r1000$notices$content, fixed = TRUE))
    expect_match(r1001$notices$content, "very large", fixed = TRUE)
})


test_that("C11 the 100,000 ceiling: 100,000 is computed, 100,001 is refused with its number", {
    expect_equal(vci_n(vci_run(props = "0.5", kappaL = 0.59501862103)), 100000)
    msg <- tryCatch(vci_run(props = "0.5", kappaL = 0.59501864605), error = conditionMessage)
    expect_match(msg, "about 100,001 subjects", fixed = TRUE)
})


test_that("C14 proportion-sum tolerance: 1.0009 and 1.001 accepted, 1.0011 refused", {
    expect_true(is.finite(vci_n(vci_run(outcome = "3", props = "0.2, 0.3, 0.5009"))))
    expect_true(is.finite(vci_n(vci_run(outcome = "3", props = "0.2, 0.3, 0.501"))))
    expect_error(vci_run(outcome = "3", props = "0.2, 0.3, 0.5011"), "should sum to 1.0")
    # accepted vectors are renormalised before sizing
    expect_equal(vci_n(vci_run(outcome = "3", props = "0.2, 0.3, 0.5009")),
                 vci_N(3, 2, c(.2, .3, .5009) / 1.0009, .6, .4, .8))
})


test_that("C09 the sparse notice's numbers match the oracle where its cells are the pooled ones", {
    # 3-5 categories already use k all-agree cells + one disagreement cell, as the test does.
    r <- vci_run(outcome = "3", props = "0.20, 0.60, 0.20", raters = "3")
    n <- vci_n(r)
    per <- lapply(c(.4, .8), function(rho) n * vci_cells_cat(3, c(.2, .6, .2), rho))
    fired <- Filter(function(e) any(e < 1) || mean(e < 5) > 0.2, per)
    worst <- fired[[which.min(vapply(fired, min, 0))]]
    expect_match(r$notices$content,
                 sprintf("smallest expected count is %s, and the number of cells below 5 is %d of %d",
                         base::format(signif(min(worst), 2), scientific = FALSE, trim = TRUE),
                         sum(worst < 5), length(worst)), fixed = TRUE)
    # binary, 6 raters, default prevalence: the POOLED cells are sparse too (1 of 3 below 5)
    expect_match(vci_run(raters = "6")$notices$content, "Sparse categories", fixed = TRUE)
})


test_that("VAL-kappaSizeCI-02 binary 3+-rater sparse notice judges the pooled cells of the 1-df test", {
    # p = .05, 3 raters, [.5, .7]: fixed-proportion n = 976 (smallest pooled count > 20 already);
    # reported n 1,149 with the proportions re-estimated (IR-kappaSizeCI-01), counts larger still
    r <- vci_run(props = "0.05", raters = "3", kappaL = 0.5, kappaU = 0.7)
    expect_equal(vci_N(2, 3, .05, .6, .5, .7), 976)
    expect_equal(vci_n(r), vci_N_gov(2, 3, .05, .6, .5, .7))
    expect_false(grepl("Sparse categories", r$notices$content, fixed = TRUE))
    # p = .5, 3 raters: n = 43, pooled counts at kappaL .4: 11.8, 19.4, 11.8
    expect_false(grepl("Sparse categories", vci_run(props = "0.5", raters = "3")$notices$content,
                       fixed = TRUE))
})


test_that("VAL-kappaSizeCI-03 a design whose minimum is below 11 reports that minimum", {
    # one-sided, p = .5, kappa0 .9, kappaL .1: slope = 2(.2^2/.275) + .4^2/.45 = .6465,
    # N* = 2.705543/.6465 = 4.19 -> 5
    expect_equal(vci_n(vci_run(citype = "one_sided", props = "0.5", kappa0 = 0.9, kappaL = 0.1)), 5)
})


test_that("O4 the interval inverter reproduces the Donner & Eliasziw 1992 example", {
    # 63 both positive, 11 discordant, 44 both negative (N = 118): kappa 0.81, GOF 95% CI (0.67, 0.89)
    m <- c(44, 11, 63); N <- 118; ph <- (2 * 63 + 11) / (2 * N)
    X2 <- function(r) { e <- N * vci_cells_bin(2, ph, r); sum((m - e)^2 / e) }
    kh <- 1 - 11 / (2 * N * ph * (1 - ph))
    f <- function(r) X2(r) - qchisq(.95, 1)
    expect_equal(round(c(kh, uniroot(f, c(0, kh))$root, uniroot(f, c(kh, 1 - 1e-9))$root), 2),
                 c(0.81, 0.67, 0.89))
})


test_that("C08 'roughly half' of studies of the planned size finish short of kappaL (simulation)", {
    # Default design, n = 118, true kappa = 0.6; each simulated study analysed with the DE1992 GOF
    # interval. Pre-registered band 0.40-0.60; R = 1000 (MC SE ~ 0.016). Also checks coverage.
    set.seed(20261015)
    crit <- qchisq(.95, 1); N <- 118
    res <- t(replicate(1000, {
        shared <- runif(N) < 0.6
        x <- ifelse(shared, 2 * rbinom(N, 1, 0.2), rbinom(N, 2, 0.2))
        m <- c(sum(x == 0), sum(x == 1), sum(x == 2)); ph <- sum(x) / (2 * N)
        kh <- 1 - m[2] / (2 * N * ph * (1 - ph))
        X2 <- function(r) { e <- N * vci_cells_bin(2, ph, r); t <- (m - e)^2 / e; sum(t[is.finite(t)]) }
        f <- function(r) X2(r) - crit
        adm <- -min(ph / (1 - ph), (1 - ph) / ph) + 1e-9
        lo <- if (f(adm) > 0) uniroot(f, c(adm, kh), tol = 1e-10)$root else adm
        hi <- if (kh >= 1 - 1e-9) 1 else if (f(1 - 1e-10) > 0) uniroot(f, c(kh, 1 - 1e-10), tol = 1e-10)$root else 1
        c(lo, hi)
    }))
    short <- mean(res[, 1] < 0.40)
    expect_gte(short, 0.40); expect_lte(short, 0.60)
    cover <- mean(res[, 1] <= 0.6 & res[, 2] >= 0.6)
    expect_lt(abs(cover - 0.95), 3 * sqrt(.95 * .05 / 1000))
    expect_match(vci_run()$notices$content, "Roughly half of such studies observe less", fixed = TRUE)
})


test_that("VAL-kappaSizeCI-04 the text does not say the method reaches a requested 'width'", {
    # n is identical for widths .25 and .40 when only the non-binding limit moves
    expect_equal(vci_n(vci_run(kappaL = .55, kappaU = .80)), vci_n(vci_run(kappaL = .55, kappaU = .95)))
    r <- vci_run()
    expect_false(grepl("requested width", r$text2$content, fixed = TRUE))
    expect_false(grepl("width approach", r$notices$content, fixed = TRUE))
})


test_that("VAL-kappaSizeCI-05 the estimand sentence does not say more raters break the Cohen equivalence", {
    expect_false(grepl("with more raters, or unequal marginals, it does not",
                       vci_run(raters = "3")$notices$content, fixed = TRUE))
})


test_that("VAL-kappaSizeCI-06 a planned n below 25 carries the documented validity caveat", {
    r <- vci_run(citype = "one_sided", props = "0.5", kappaL = 0.2)
    expect_equal(vci_n(r), 17)
    expect_match(r$notices$content, "25 subjects", fixed = TRUE)
})


test_that("VAL-kappaSizeCI-07 the models the Notes rely on are citable from the output", {
    r <- yaml::yaml.load_file(test_path("..", "..", "jamovi", "kappaSizeCI.r.yaml"))
    expect_true(all(c("altayeDonnerKlarKappaRaters", "altayeDonnerEliasziwKappaGOF") %in% r$refs))
})


# --- Independent review, 2026-10-15 (IR-kappaSizeCI-01..05) -------------------------------------

test_that("IR-kappaSizeCI-01 with 3+ raters the n also reaches the limits when the proportions are re-estimated", {
    # ADE 2001 eq. (8)-(9): the interval with nominal coverage re-estimates the proportions at each
    # kappa. Its expected limits at the reported n must lie inside the requested ones, as the
    # fixed-proportion ones do (C02), and one subject fewer must miss one of the four.
    crit <- qchisq(.95, 1)
    designs <- list(list(2, 4, .2, .6, .4, .8), list(2, 6, .2, .6, .4, .8),
                    list(3, 3, c(.11, .70, .19), .44, .34, .54), list(3, 6, c(.11, .70, .19), .44, .34, .54))
    for (d in designs) {
        K <- d[[1]]; R <- d[[2]]; p <- d[[3]]; k0 <- d[[4]]
        n <- vci_n(vci_run(outcome = as.character(K), props = vci_pstr(if (K == 2) c(p, 1 - p) else p),
                           raters = as.character(R), kappa0 = k0, kappaL = d[[5]], kappaU = d[[6]]))
        f <- function(r) n * vci_rml_slope(K, R, p, k0, r) - crit
        lab <- sprintf("K %d, %d raters", K, R)
        expect_gte(uniroot(f, c(.005, k0 - 1e-6), tol = 1e-10)$root, d[[5]] - 1e-6, label = lab)
        expect_lte(uniroot(f, c(k0 + 1e-6, .995), tol = 1e-10)$root, d[[6]] + 1e-6, label = lab)
        slopes <- c(vci_slope(K, R, p, k0, d[[5]]), vci_slope(K, R, p, k0, d[[6]]),
                    vci_rml_slope(K, R, p, k0, d[[5]]), vci_rml_slope(K, R, p, k0, d[[6]]))
        expect_lt((n - 1) * min(slopes), crit, label = lab)
    }
    # RD2012's worked example: 171 with the proportions fixed, 190 re-estimated; both shown and explained
    r <- vci_run(outcome = "3", props = "0.11, 0.70, 0.19", raters = "3", kappa0 = .44, kappaL = .34, kappaU = .54)
    expect_equal(vci_n(r), 190)
    expect_match(r$text2$content, "would need 171; re-estimating them", fixed = TRUE)
    expect_match(r$text2$content, "drives the sample size is 0.54,", fixed = TRUE)
    expect_match(r$text_summary$content, "A minimum of 171 subjects", fixed = TRUE)
    expect_match(r$text_summary$content, "holding the\ncategory proportions", fixed = TRUE)
    expect_match(r$notices$content, "re-estimates them by maximum likelihood", fixed = TRUE)
    # two raters keep the published fixed-proportion n (the plug-in interval is calibrated there)
    expect_equal(vci_n(vci_run(kappaL = .55, kappaU = .65)), 1624)
    expect_false(grepl("re-estimates them", vci_run()$notices$content, fixed = TRUE))
    # equal prevalences: the two analyses agree, so n is the fixed-proportion one
    expect_equal(vci_n(vci_run(props = "0.5", raters = "4")), vci_N(2, 4, .5, .6, .4, .8))
})


test_that("IR-kappaSizeCI-02 binary 3+-rater designs disclose that the multi-rater model sets n", {
    expect_match(vci_run(raters = "4")$notices$content, "beta-binomial", fixed = TRUE)
    expect_false(grepl("beta-binomial", vci_run()$notices$content, fixed = TRUE))
    expect_false(grepl("beta-binomial", vci_run(outcome = "3", props = "0.2, 0.3, 0.5",
                                                raters = "4")$notices$content, fixed = TRUE))
    # what it claims: the 3-category (Dirichlet-multinomial) model at K -> 2 needs fewer subjects here
    expect_lt(vci_n(vci_run(outcome = "3", props = "0.2, 0.7999, 0.0001", raters = "6")),
              vci_n(vci_run(raters = "6")))
})


test_that("IR-kappaSizeCI-03 the one-sided objective does not promise the limit", {
    txt <- vci_run(citype = "one_sided")$text2$content
    expect_false(grepl("ensuring", txt, fixed = TRUE))
    expect_match(txt, "so that the expected lower confidence limit reaches 0.4", fixed = TRUE)
})


test_that("IR-kappaSizeCI-04 the small-sample caveat keeps its sources' qualifiers", {
    txt <- vci_run(citype = "one_sided", props = "0.5", kappaL = 0.2)$notices$content
    expect_match(txt, "For a binary outcome", fixed = TRUE)
    expect_match(txt, "about a fifth of the ratings", fixed = TRUE)
    expect_match(txt, "for three to five categories no such figure was given", fixed = TRUE)
    expect_false(grepl("has been shown to keep its stated coverage from about 25", txt, fixed = TRUE))
})


test_that("IR-kappaSizeCI-05 the quoted plug-in coverage matches the simulations", {
    expect_match(vci_run(raters = "3")$notices$content, "about 0.91 to 0.93 instead of 0.95", fixed = TRUE)
    expect_false(grepl("0.91 to 0.93", vci_run()$notices$content, fixed = TRUE))
})
