# Validation regression tests for kappaSizePower (/validate-function depth=exhaustive, 2026-10-09).
# Report: development-ideas/kappaSizePower-validation-2026-10-09.md
# Evidence script: development-scripts/validate_kappaSizePower.R
#
# Every expected value comes from an oracle that does not touch kappaSize:
#   * lambda: for a 1-df chi-square test, P(reject) = pnorm(d - z) + pnorm(-d - z) with
#     d = sqrt(lambda), z = qnorm(1 - alpha/2); solved with uniroot.
#   * binary cells: every one of the 2^k rating patterns of the common-correlation model (with
#     probability kappa all raters copy one Bernoulli(p) draw, otherwise they are independent).
#   * multi-category cells: Dirichlet moments E[pi_j^n] through lbeta().
#   * N = lambda / sum((P(kappa1) - P(kappa0))^2 / P(kappa0)).
# VAL-kappaSizePower-01..07 were fixed on 2026-10-09; their tests below were written skip-guarded
# with the correct expectation first, checked to FAIL on the unfixed code, then made live.

skip_if_not_installed("kappaSize")

vkp_lambda <- function(alpha, power) {
    z <- qnorm(1 - alpha / 2)
    uniroot(function(d) pnorm(d - z) + pnorm(-d - z) - power, c(0, 60), tol = 1e-15)$root^2
}
vkp_cells_bin <- function(k, p, kap) {
    pats <- as.matrix(expand.grid(rep(list(0:1), k)))
    s <- rowSums(pats)
    pr <- (1 - kap) * p^s * (1 - p)^(k - s) + kap * ifelse(s == k, p, ifelse(s == 0, 1 - p, 0))
    vapply(0:k, function(j) sum(pr[s == j]), 0)
}
vkp_cells_dm <- function(n, props, kap) {
    th <- (1 - kap) / kap
    agree <- vapply(props, function(pj) exp(lbeta(pj * th + n, th - pj * th) - lbeta(pj * th, th - pj * th)), 0)
    c(1 - sum(agree), agree)
}
vkp_cells <- function(outcome, raters, props, kap)
    if (outcome == 2) vkp_cells_bin(raters, props[1], kap) else vkp_cells_dm(raters, props, kap)
vkp_N <- function(outcome, raters, props, k0, k1, alpha = 0.05, power = 0.80) {
    P0 <- vkp_cells(outcome, raters, props, k0); P1 <- vkp_cells(outcome, raters, props, k1)
    vkp_lambda(alpha, power) / sum((P1 - P0)^2 / P0)
}
vkp_run <- function(outcome = "2", kappa0 = 0.4, kappa1 = 0.6, props = "0.20, 0.80", raters = "2",
                    alpha = 0.05, power = 0.80)
    ClinicoPath::kappaSizePower(outcome = outcome, kappa0 = kappa0, kappa1 = kappa1, props = props,
                                raters = raters, alpha = alpha, power = power)
vkp_num <- function(txt, pattern) {
    m <- regmatches(txt, regexec(pattern, txt))[[1]]
    if (length(m) < 2) NA_real_ else as.numeric(gsub(",", "", m[2]))
}
vkp_n <- function(res) vkp_num(res$text1$content, "A minimum of (\\S+) subjects")
vkp_has <- function(res, title) grepl(title, res$notices$content, fixed = TRUE)


test_that("VAL C01: hand-calculated sample sizes (A1-A3)", {
    # A1 binary, 2 raters, p .30, kappa .4 -> .6: cells .574/.252/.174 vs .616/.168/.216,
    #    Sigma = .0412111, lambda(.05, .80) = 7.8489 -> N = 190.46 -> 191
    expect_equal(vkp_n(vkp_run(props = "0.30, 0.70")), 191)
    # A2 3 categories, 3 raters, .2/.3/.5, kappa .5 -> .7: Sigma = .1298924 -> N = 60.43 -> 61
    expect_equal(vkp_n(vkp_run(outcome = "3", kappa0 = 0.5, kappa1 = 0.7, props = "0.20, 0.30, 0.50",
                               raters = "3")), 61)
    # A3 binary, 3 raters, p .30, kappa .4 -> .6: cells .4858/.2646/.1134/.1362, Sigma = .074382 -> 106
    expect_equal(vkp_n(vkp_run(props = "0.30", raters = "3")), 106)
    expect_equal(vkp_cells_bin(3, 0.3, 0.4), c(0.4858, 0.2646, 0.1134, 0.1362), tolerance = 1e-12)
})


test_that("VAL C01: the method papers' own published sample sizes are reproduced", {
    # Donner & Eliasziw 1992 p.1518 and Altaye, Donner & Klar 2001 p.587: kappa 0.6 -> 0.9,
    # prevalence 0.3, alpha 0.05, power 0.80 need 66 subjects with 2 raters and 40 with 3.
    expect_equal(vkp_n(vkp_run(kappa0 = 0.6, kappa1 = 0.9, props = "0.3", raters = "2")), 66)
    expect_equal(vkp_n(vkp_run(kappa0 = 0.6, kappa1 = 0.9, props = "0.3", raters = "3")), 40)
    # Altaye, Donner & Eliasziw 2001 Table IV (p.2486), k = 3, pi = 0.1/0.1/0.8, 2-6 raters. The
    # paper rounds to the nearest subject and the module rounds up, so the module shows the
    # printed value or one more - always the ceiling of the unrounded N.
    tab <- list(list(k = c(0.4, 0.6), n = c(205, 113, 83, 68, 59)),
                list(k = c(0.6, 0.8), n = c(172, 102, 77, 65, 57)))
    for (row in tab) for (i in 1:5) {
        exact <- vkp_N(3, i + 1, c(0.1, 0.1, 0.8), row$k[1], row$k[2])
        expect_equal(round(exact), row$n[i])            # the oracle reproduces the printed table
        shown <- vkp_n(vkp_run(outcome = "3", props = "0.1, 0.1, 0.8", raters = as.character(i + 1),
                               kappa0 = row$k[1], kappa1 = row$k[2]))
        expect_equal(shown, ceiling(exact))
        expect_true((shown - row$n[i]) %in% c(0, 1))
    }
})


test_that("VAL C01: displayed N equals ceiling(oracle N) across outcomes and raters", {
    designs <- list(list(2, "0.3"), list(2, "0.1"), list(3, "0.3, 0.6, 0.1"),
                    list(4, "0.1, 0.2, 0.3, 0.4"), list(5, "0.1, 0.2, 0.2, 0.2, 0.3"))
    for (d in designs) for (ra in c(2, 4, 6)) for (kp in list(c(0.4, 0.6), c(0.6, 0.8))) {
        pv <- as.numeric(strsplit(d[[2]], ",\\s*")[[1]])
        expect_equal(vkp_n(vkp_run(outcome = as.character(d[[1]]), kappa0 = kp[1], kappa1 = kp[2],
                                   props = d[[2]], raters = as.character(ra))),
                     ceiling(vkp_N(d[[1]], ra, pv, kp[1], kp[2])),
                     label = sprintf("outcome %d props %s raters %d kappa %.1f->%.1f", d[[1]], d[[2]], ra, kp[1], kp[2]))
    }
})


test_that("VAL C25: alpha and power act through lambda only (two-sided 1-df test)", {
    # the normal-tail root satisfies the noncentral chi-square CDF: same lambda by a second route
    for (ap in list(c(0.05, 0.80), c(0.01, 0.90)))
        expect_equal(pchisq(qchisq(1 - ap[1], 1), 1, vkp_lambda(ap[1], ap[2])), 1 - ap[2], tolerance = 1e-9)
    expect_equal(vkp_n(vkp_run(props = "0.3", alpha = 0.01, power = 0.90)),
                 ceiling(vkp_N(2, 2, 0.3, 0.4, 0.6, 0.01, 0.90)))
})


test_that("VAL C02: the four panes report the same N", {
    for (args in list(list(), list(outcome = "4", props = "0.1, 0.2, 0.3, 0.4", raters = "5"),
                      list(props = "0.01"))) {
        res <- do.call(vkp_run, args)
        n <- vkp_n(res)
        expect_equal(vkp_num(res$text_summary$content, "A minimum of (\\S+) subjects"), n)
        expect_equal(vkp_num(gsub("\\s+", " ", res$text2$content), "The required sample size is ([0-9,]+) subject"), n)
        expect_equal(vkp_num(res$notices$content, "Required sample size: <b>([0-9,]+)</b>"), n)
    }
})


test_that("VAL C18: binary N does not depend on which category is typed first (moderate prevalence)", {
    # The explanation states the N is the same whichever category is called positive.
    for (p in c("0.3", "0.1", "0.01")) for (ra in 2:6) {
        q <- format(1 - as.numeric(p), digits = 15)
        a <- vkp_n(vkp_run(props = paste0(p, ", ", q), raters = as.character(ra)))
        b <- vkp_n(vkp_run(props = paste0(q, ", ", p), raters = as.character(ra)))
        expect_equal(b, a, label = sprintf("p %s raters %d", p, ra))
        expect_equal(a, ceiling(vkp_N(2, ra, as.numeric(p), 0.4, 0.6)))
    }
})


test_that("VAL C18: binary N with a 0.01% finding typed common-category first (fixed: VAL-kappaSizePower-01)", {
    # oracle: 181,129 subjects, the same as the rare-first entry of this design
    expect_equal(vkp_n(vkp_run(props = "0.9999, 0.0001", raters = "5")), ceiling(vkp_N(2, 5, 1e-4, 0.4, 0.6)))
    expect_equal(vkp_n(vkp_run(props = "0.9999, 0.0001", raters = "6", kappa0 = 0.6, kappa1 = 0.8)),
                 ceiling(vkp_N(2, 6, 1e-4, 0.6, 0.8)))
    expect_equal(vkp_n(vkp_run(props = "0.999, 0.001", raters = "6", kappa0 = 0.6, kappa1 = 0.8)),
                 ceiling(vkp_N(2, 6, 1e-3, 0.6, 0.8)))
})


test_that("VAL C08: category order does not change the N (nominal categories)", {
    perms <- list(c(.3, .6, .1), c(.1, .3, .6), c(.6, .1, .3), c(.1, .6, .3))
    for (ra in c(2, 4, 6)) {
        ns <- vapply(perms, function(v) vkp_n(vkp_run(outcome = "3", props = paste(v, collapse = ", "),
                                                      raters = as.character(ra))), 0)
        expect_equal(ns, rep(ceiling(vkp_N(3, ra, c(.3, .6, .1), 0.4, 0.6)), length(perms)))
    }
})


test_that("VAL C09: a sum within 0.001 of 1 is renormalised before sizing", {
    for (ps in c("0.2, 0.3, 0.5005", "0.2, 0.3, 0.4995")) {
        pv <- as.numeric(strsplit(ps, ",\\s*")[[1]])
        expect_equal(vkp_n(vkp_run(outcome = "3", props = ps)), ceiling(vkp_N(3, 2, pv / sum(pv), 0.4, 0.6)))
    }
    expect_equal(vkp_n(vkp_run(props = "0.2, 0.8009")), ceiling(vkp_N(2, 2, 0.2 / 1.0009, 0.4, 0.6)))
})


test_that("VAL C15: sparse-cell notice reports the oracle's expected counts on the pooled test cells", {
    # Binary, 3+ raters: the 1-df test pools every split call into one cell (Altaye, Donner & Klar
    # 2001 p.586), so the notice judges all absent / any disagreement / all present at kappa0 x N.
    pooled <- function(k, p, kap) { P <- vkp_cells_bin(k, p, kap); c(P[1], sum(P[2:k]), P[k + 1]) }
    # pooling leaves N unchanged under their model
    Np <- vkp_lambda(0.05, 0.80) / sum((pooled(6, 0.05, 0.5) - pooled(6, 0.05, 0.2))^2 / pooled(6, 0.05, 0.2))
    expect_equal(Np, vkp_N(2, 6, 0.05, 0.2, 0.5), tolerance = 1e-10)
    res <- vkp_run(props = "0.05", raters = "6", kappa0 = 0.2, kappa1 = 0.5)
    e <- pooled(6, 0.05, 0.2) * vkp_N(2, 6, 0.05, 0.2, 0.5)
    expect_true(vkp_has(res, "Sparse categories."))
    expect_match(res$notices$content, sprintf("smallest expected count is %s and %d of %d cells",
                 base::format(signif(min(e), 2), scientific = FALSE), sum(e < 5), length(e)), fixed = TRUE)
    # 6 raters, 5% finding, kappa 0.4 -> 0.6: unpooled cells reach 0.0003, pooled cells are all >= 5
    e6 <- pooled(6, 0.05, 0.4) * vkp_N(2, 6, 0.05, 0.4, 0.6)
    expect_gte(min(e6), 5)
    expect_false(vkp_has(vkp_run(props = "0.05", raters = "6"), "Sparse categories."))
})


test_that("VAL C12 C13 C14 C16: guard thresholds from both sides", {
    fp <- function(t) uniroot(function(p) vkp_N(2, 2, p, .4, .6) - t, c(0.005, 0.2), tol = 1e-13)$root
    for (tg in c(2000, 2001)) {
        res <- vkp_run(props = sprintf("%.12f", fp(tg - 0.5)))
        expect_equal(vkp_n(res), tg)
        expect_identical(vkp_has(res, "Very large sample size."), tg > 2000)
    }
    fk <- function(t) uniroot(function(k1) vkp_N(2, 2, .5, .02, k1) - t, c(0.6, 0.99), tol = 1e-13)$root
    for (tg in c(9, 10)) {
        res <- vkp_run(props = "0.5", kappa0 = 0.02, kappa1 = fk(tg - 0.5))
        expect_equal(vkp_n(res), tg)
        expect_identical(vkp_has(res, "Very small sample size."), tg < 10)
    }
    for (pw in c(0.49, 0.50)) expect_identical(vkp_has(vkp_run(power = pw), "Low power."), pw < 0.5)
    fm <- function(t) uniroot(function(p) p * vkp_N(2, 2, p, .2, .9) - t, c(0.01, 0.5), tol = 1e-13)$root
    for (tg in c(4.8, 5.2))
        expect_identical(vkp_has(vkp_run(props = sprintf("%.12f", fm(tg)), kappa0 = 0.2, kappa1 = 0.9),
                                 "Rare outcome category."), tg < 5)
})


test_that("VAL C05: two raters, binary - the GOF test at the shown N has nominal size and power (exact)", {
    # Exact enumeration of the 3-cell multinomial at N = 191 (A1). Test: Pearson X2 at kappa0 with the
    # prevalence estimated from all ratings, chi-square(1) critical value (Donner & Eliasziw 1992).
    N <- 191; p <- 0.3
    g <- expand.grid(n0 = 0:N, n1 = 0:N); g <- g[g$n0 + g$n1 <= N, ]; g$n2 <- N - g$n0 - g$n1
    ph <- (g$n1 + 2 * g$n2) / (2 * N)
    E <- sapply(0:2, function(j) N * ((1 - 0.4) * dbinom(j, 2, ph) + 0.4 * (if (j == 2) ph else if (j == 0) 1 - ph else 0)))
    O <- as.matrix(g[, c("n0", "n1", "n2")])
    X2 <- rowSums(ifelse(E > 0, (O - E)^2 / E, 0)); X2[ph <= 0 | ph >= 1] <- 0
    rej <- X2 > qchisq(0.95, 1)
    lprob <- function(kap) { P <- vkp_cells_bin(2, p, kap)
        lfactorial(N) - rowSums(lfactorial(O)) + O %*% log(P) }
    size  <- sum(exp(lprob(0.4))[rej]); power <- sum(exp(lprob(0.6))[rej])
    expect_gte(size, 0.025); expect_lte(size, 0.075)      # Bradley's liberal band
    expect_gte(power, 0.77); expect_lte(power, 0.83)      # nominal 0.80 +/- 0.03
})


test_that("VAL C17: the Methodology names the test the N is valid for when there are 3+ raters (fixed: VAL-kappaSizePower-02)", {
    # The N is for the papers' own 1-df goodness-of-fit test (all-agree cells plus ONE pooled
    # disagreement cell; Altaye, Donner & Klar 2001 p.586, Altaye, Donner & Eliasziw 2001 p.2482).
    res <- vkp_run(props = "0.3", raters = "4")
    expect_match(res$notices$content, "plus one pooled cell for any disagreement", fixed = TRUE)
    expect_match(res$notices$content, "likelihood[- ]ratio", ignore.case = TRUE)
    # binary 3+ raters: the all-counts chi-square has raters - 1 df (ADK p.586)
    expect_match(res$notices$content, "every count of raters calling the finding present", fixed = TRUE)
    # 3+ categories with 3+ raters: prevalences re-estimated by ML at kappa0 (ADE p.2483)
    cat3 <- vkp_run(outcome = "3", props = "0.3, 0.6, 0.1", raters = "4")$notices$content
    expect_match(cat3, "maximum likelihood with kappa held at kappa0", fixed = TRUE)
    expect_false(grepl("every count of raters", cat3, fixed = TRUE))
    # two raters: the published test is already 1 df on three cells, so neither warning shows
    two <- vkp_run(props = "0.3")$notices$content
    expect_false(grepl("With three or more raters", two, fixed = TRUE))
})


test_that("VAL C27: displayed N is the ceiling of the exact N at large N (fixed: VAL-kappaSizePower-03)", {
    # oracle N = 245.0032 -> 246 (shown 245); 5651.0109 -> 5652 (shown 5651)
    expect_equal(vkp_n(vkp_run(outcome = "3", props = "0.3333, 0.3333, 0.3334", raters = "3", kappa0 = 0.8,
                               kappa1 = 0.9, alpha = 0.01, power = 0.90)),
                 ceiling(vkp_N(3, 3, c(.3333, .3333, .3334), 0.8, 0.9, 0.01, 0.90)))
    expect_equal(vkp_n(vkp_run(props = "0.01", kappa0 = 0.6, kappa1 = 0.8, alpha = 0.01, power = 0.90)),
                 ceiling(vkp_N(2, 2, 0.01, 0.6, 0.8, 0.01, 0.90)))
})


test_that("VAL C31: a round N prints as an integer in every pane (fixed: VAL-kappaSizePower-04)", {
    # kappa1 solved against the ORACLE so the exact N is 99,999.5: solving against kappaSize's
    # own (slightly off) N lands on 99,999 once the module computes N exactly, and the test
    # would then pass without ever printing a round number.
    k1 <- uniroot(function(k1) vkp_N(2, 2, 0.3, 0.4, k1) - 99999.5, c(0.4001, 0.45), tol = 1e-13)$root
    res <- vkp_run(props = "0.3", kappa1 = k1)
    expect_equal(vkp_n(res), 1e5)
    expect_match(res$text1$content, "A minimum of 100000 subjects", fixed = TRUE)
    expect_false(grepl("1e+05", res$text1$content, fixed = TRUE))
    expect_false(grepl("1e+05", res$text_summary$content, fixed = TRUE))
})


test_that("VAL C11: a typed kappa gap of exactly 0.05 does not raise the 'small difference' notice (fixed: VAL-kappaSizePower-05)", {
    for (pr in list(c(.40, .45), c(.55, .60), c(.10, .15), c(.20, .25), c(.45, .40), c(.30, .35)))
        expect_false(vkp_has(vkp_run(kappa0 = pr[1], kappa1 = pr[2]), "Small kappa difference."))
    expect_true(vkp_has(vkp_run(kappa0 = .40, kappa1 = .44), "Small kappa difference."))
})


test_that("VAL C21: the small-N notice does not call an over-estimate a lower bound (fixed: VAL-kappaSizePower-06)", {
    k1 <- uniroot(function(k1) vkp_N(2, 2, .5, .02, k1) - 8.5, c(0.6, 0.99), tol = 1e-13)$root
    res <- vkp_run(props = "0.5", kappa0 = 0.02, kappa1 = k1)
    expect_equal(vkp_n(res), 9)
    expect_true(vkp_has(res, "Very small sample size."))
    expect_false(grepl("lower bound", res$notices$content, fixed = TRUE))
})


test_that("VAL C17: the power approach is attributed to its sources and they are citable (fixed: VAL-kappaSizePower-07)", {
    # Rotondi & Donner 2012 is the confidence-interval paper that introduced kappaSize; the power
    # approach is Donner & Eliasziw 1992 (PMID 1410963) and Altaye et al. 2001 (PMIDs 11414588,
    # 11512137), verified on PubMed 2026-10-09.
    notes <- vkp_run()$notices$content
    expect_match(notes, "approach of Donner and Eliasziw (two raters, binary outcome), Altaye, Donner and Klar (binary outcome, more raters) and Altaye, Donner and Eliasziw (three to five categories)", fixed = TRUE)
    # the estimand all four papers share: intraclass kappa, no rater bias (DE1992 p.1512, p.1518)
    expect_match(notes, "unweighted intraclass kappa", fixed = TRUE)
    refs <- yaml::read_yaml(file.path("../../jamovi", "kappaSizePower.r.yaml"))$refs
    expect_true(all(c("donnerEliasziwKappaGOF", "altayeDonnerKlarKappaRaters",
                      "altayeDonnerEliasziwKappaGOF") %in% refs))
    defined <- yaml::read_yaml(file.path("../../jamovi", "00refs.yaml"))$refs
    expect_true(all(refs %in% names(defined)))
    expect_equal(defined$altayeDonnerKlarKappaRaters$doi, "10.1111/j.0006-341x.2001.00584.x")
    expect_equal(defined$altayeDonnerEliasziwKappaGOF$doi, "10.1002/sim.911")
})


test_that("VAL C30: a proportion that underflows the cells is refused, never printed as a number", {
    # p = 1e-200: p^2 underflows, every (P1 - P0)^2 term is 0, N would be Inf.
    expect_error(vkp_run(props = "1e-200"), "could not be computed")
})


test_that("VAL C06: binary designs with 3+ raters disclose the rater-dependence model (fixed: VAL-kappaSizePower-08)", {
    # kappaSize's binary engine assumes a mixture (with probability kappa all raters copy one call);
    # at the same kappa a beta-binomial model needs 0.89 to 12.5 times the subjects, so the
    # assumption is stated, without a direction.
    vkp_bb <- function(k, p, kap) { th <- (1 - kap) / kap; a <- p * th; b <- (1 - p) * th; j <- 0:k
        choose(k, j) * exp(lbeta(a + j, b + k - j) - lbeta(a, b)) }
    nb <- function(cells) vkp_lambda(0.05, 0.80) / sum((cells(0.6) - cells(0.4))^2 / cells(0.4))
    mix <- nb(function(kap) vkp_cells_bin(6, 0.1, kap)); bb <- nb(function(kap) vkp_bb(6, 0.1, kap))
    expect_gt(abs(mix / bb - 1), 0.2)                       # the model matters for this design
    expect_equal(vkp_cells_bin(2, 0.3, 0.4), vkp_bb(2, 0.3, 0.4), tolerance = 1e-12)  # not at 2 raters
    # The mixture is the published model of Altaye, Donner & Klar 2001: their eq. (4),
    # lambda_k = pi^k + rho (1 - pi) sum_{j=1}^{k-1} pi^(k-j) = (1 - rho) pi^k + rho pi.
    lam_k <- function(k, p, r) p^k + r * (1 - p) * sum(p^(k - seq_len(k - 1)))
    expect_equal(lam_k(4, 0.3, 0.4), (1 - 0.4) * 0.3^4 + 0.4 * 0.3, tolerance = 1e-14)
    msg <- "uses the rater-dependence model of Altaye, Donner and Klar"
    expect_match(vkp_run(props = "0.1", raters = "6")$notices$content, msg, fixed = TRUE)
    expect_false(grepl(msg, vkp_run(props = "0.1", raters = "2")$notices$content, fixed = TRUE))
    expect_false(grepl(msg, vkp_run(outcome = "3", props = "0.3, 0.6, 0.1", raters = "4")$notices$content, fixed = TRUE))
})
