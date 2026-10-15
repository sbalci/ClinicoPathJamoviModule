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
# Third pass 2026-10-14 (development-ideas/kappaSizePower-validation-2026-10-14.md) appended at the
# end: live tests for the estimand sentence, the pooled cells, gap monotonicity and the plug-in vs
# ML prevalence claim; VAL-kappaSizePower-09..12 were written skip-guarded, checked to FAIL with the
# skip() removed, and made live by the 2026-10-14 release-review fix.

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
    # counts at the N shown (ceiling), as the notice says "at the required sample size"
    e <- pooled(6, 0.05, 0.2) * ceiling(vkp_N(2, 6, 0.05, 0.2, 0.5))
    expect_true(vkp_has(res, "Sparse categories."))
    expect_match(res$notices$content, sprintf("smallest expected count is %s and %d of %d cells",
                 base::format(signif(min(e), 2), scientific = FALSE), sum(e < 5), length(e)), fixed = TRUE)
    # 6 raters, 5% finding, kappa 0.4 -> 0.6: unpooled cells reach 0.0003, pooled cells are all >= 5
    e6 <- pooled(6, 0.05, 0.4) * ceiling(vkp_N(2, 6, 0.05, 0.4, 0.6))
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


# ---------------------------------------------------------------------------------------------
# Third pass, 2026-10-14 (development-ideas/kappaSizePower-validation-2026-10-14.md).
# Oracles added: population Fleiss' kappa by enumerating every rating composition; the pooled
# goodness-of-fit cells (all-negative, any disagreement, all-positive); and the asymptotic
# noncentrality of that test when the prevalence is estimated by grouped-data maximum likelihood
# (Mitra 1958): lambda_est = N [d'Wd - (d'Wg)^2 / g'Wg], W = 1/P0, g = dP0/dp.
# ---------------------------------------------------------------------------------------------

vkp_pool <- function(P) { n <- length(P) - 1; c(P[1], sum(P[2:n]), P[n + 1]) }
vkp_ratio_bin <- function(n, p, k0, k1) {     # lambda_est / lambda_known, binary pooled cells
    cb <- function(pp, kap) vkp_pool(vkp_cells_bin(n, pp, kap))
    P0 <- cb(p, k0); d <- cb(p, k1) - P0; g <- (cb(p + 1e-6, k0) - cb(p - 1e-6, k0)) / 2e-6
    full <- sum(d^2 / P0); (full - sum(d * g / P0)^2 / sum(g^2 / P0)) / full
}
vkp_fleiss_pop <- function(X, prob) {         # population Fleiss' kappa (Scott's pi at 2 raters)
    n <- sum(X[1, ]); pj <- colSums(X * prob) / n
    Pbar <- sum(prob * (rowSums(X^2) - n)) / (n * (n - 1))
    (Pbar - sum(pj^2)) / (1 - sum(pj^2))
}
vkp_comps <- function(n, K) { g <- as.matrix(expand.grid(rep(list(0:n), K))); g[rowSums(g) == n, , drop = FALSE] }
vkp_pmf_dm <- function(n, props, kap) {
    X <- vkp_comps(n, length(props)); th <- (1 - kap) / kap; a <- props * th
    lp <- lfactorial(n) - rowSums(lfactorial(X)) + lgamma(th) - lgamma(n + th) +
          rowSums(sweep(lgamma(sweep(X, 2, a, "+")), 2, lgamma(a), "-"))
    list(X = X, p = exp(lp))
}


test_that("VAL C35: kappa is Scott's pi (2 raters) / Fleiss' kappa (more) of the models the N is computed under", {
    # The Methodology: "Kappa here is the unweighted intraclass kappa for nominal categories (Scott's pi
    # for two raters, Fleiss' kappa for more)". Oracle: enumerate every rating composition and
    # compute the population value of Fleiss' statistic; it must equal the model's kappa.
    for (n in 2:6) for (p in c(0.5, 0.03)) for (kap in c(0.1, 0.4, 0.8))
        expect_equal(vkp_fleiss_pop(cbind(0:n, n - 0:n), vkp_cells_bin(n, p, kap)), kap, tolerance = 1e-10)
    for (props in list(c(0.1, 0.1, 0.8), c(0.1, 0.2, 0.2, 0.2, 0.3))) for (n in c(2, 4, 6)) for (kap in c(0.1, 0.6)) {
        m <- vkp_pmf_dm(n, props, kap)
        expect_equal(sum(m$p), 1, tolerance = 1e-12)
        expect_equal(vkp_fleiss_pop(m$X, m$p), kap, tolerance = 1e-10)
        # and the composition pmf reproduces the lbeta all-agree cells the N is computed from
        ag <- vapply(seq_along(props), function(j) sum(m$p[m$X[, j] == n]), 0)
        expect_equal(c(1 - sum(ag), ag), vkp_cells_dm(n, props, kap), tolerance = 1e-12)
    }
})


test_that("VAL C36: the pooled cells the Methodology names give the same N as all n+1 cells (binary, 3+ raters)", {
    # Altaye, Donner & Klar 2001 p.586 pool the disagreement cells; under their eq. (4) the middle
    # cells are proportional at every kappa, so pooling leaves the noncentrality unchanged.
    for (n in 3:6) for (p in c(0.5, 0.1, 0.02)) {
        P0 <- vkp_cells_bin(n, p, 0.4); P1 <- vkp_cells_bin(n, p, 0.6)
        expect_equal(sum((vkp_pool(P1) - vkp_pool(P0))^2 / vkp_pool(P0)), sum((P1 - P0)^2 / P0), tolerance = 1e-12)
    }
    shown <- vapply(3:6, function(n) vkp_n(vkp_run(props = "0.1", raters = as.character(n))), 0)
    expect_equal(shown, c(270, 218, 184, 160))
})


test_that("VAL C42: a smaller kappa gap in the same direction never needs fewer subjects", {
    # gaps +.05, +.10, +.20 above kappa0 = .2 (6 raters, 4 categories); -.02, -.05, -.10 below the default .4
    up   <- vapply(c(0.25, 0.3, 0.4), function(k1) vkp_n(vkp_run(outcome = "4", props = "0.1, 0.2, 0.3, 0.4", raters = "6", kappa0 = 0.2, kappa1 = k1)), 0)
    down <- vapply(c(0.38, 0.35, 0.3), function(k1) vkp_n(vkp_run(props = "0.1", raters = "4", kappa1 = k1)), 0)
    expect_true(all(diff(up) < 0)); expect_true(all(diff(down) < 0))
})


test_that("VAL C01: a kappa gap of 1e-6 gives the exact (huge) N with both warnings, never a refusal or NaN", {
    res <- vkp_run(props = "0.3", kappa0 = 0.4, kappa1 = 0.400001)
    shown <- vkp_num(gsub("\\s+", " ", res$text2$content), "The required sample size is ([0-9,]+) subject")
    expect_equal(shown, ceiling(vkp_N(2, 2, 0.3, 0.4, 0.400001)), tolerance = 1e-9)
    expect_true(vkp_has(res, "Small kappa difference.") && vkp_has(res, "Very large sample size."))
})


test_that("VAL C39: with 3+ raters and 3-5 categories, plugged-in proportions over-reject and ML at kappa0 does not", {
    # The Notes: "estimate the category prevalences by maximum likelihood with kappa held at kappa0,
    # as Altaye, Donner and Eliasziw do [p.2483]; plugging in the overall proportions of ratings makes
    # the test reject a true kappa0 too often". Simulated under kappa0 at the shown N (seeded).
    skip_on_cran()
    props <- c(0.1, 0.2, 0.2, 0.2, 0.3); n <- 3; k0 <- 0.4
    res <- vkp_run(outcome = "5", props = "0.1, 0.2, 0.2, 0.2, 0.3", raters = "3")
    expect_match(res$notices$content, "maximum likelihood with kappa held at kappa0", fixed = TRUE)
    N <- vkp_n(res); expect_equal(N, 45)
    m <- vkp_pmf_dm(n, props, k0)
    set.seed(20261014); R <- 1000
    cnt <- rmultinom(R, N, m$p)
    agree <- sapply(seq_along(props), function(j) colSums(cnt[m$X[, j] == n, , drop = FALSE]))
    obs <- cbind(N - rowSums(agree), agree)
    php <- t(crossprod(m$X, cnt)) / (n * N)
    pear <- function(o, E) sum(ifelse(E > 0, (o - E)^2 / E, ifelse(o > 0, Inf, 0)))
    f_ag <- function(pj) prod((pj * (1 - k0) + 0:(n - 1) * k0) / ((1 - k0) + 0:(n - 1) * k0))
    rml <- function(o, st) {
        nll <- function(eta) { pi <- exp(c(eta, 0)); pi <- pi / sum(pi)
            ag <- vapply(pi, f_ag, 0); P <- c(1 - sum(ag), ag); if (any(P <= 0)) 1e300 else -sum(o * log(P)) }
        s0 <- log(pmax(st, 1e-6)); fit <- optim(s0[-5] - s0[5], nll, method = "BFGS", control = list(reltol = 1e-12))
        pi <- exp(c(fit$par, 0)); pi / sum(pi) }
    crit <- qchisq(0.95, 1)
    size_plug <- mean(vapply(seq_len(R), function(r) pear(obs[r, ], N * vkp_cells_dm(n, php[r, ], k0)), 0) > crit)
    size_ml   <- mean(vapply(seq_len(R), function(r) pear(obs[r, ], N * vkp_cells_dm(n, rml(obs[r, ], php[r, ]), k0)), 0) > crit)
    mcse <- sqrt(0.05 * 0.95 / R)
    expect_gt(size_plug, 0.05 + 3 * mcse)                                   # over-rejects, as stated
    expect_gte(size_ml, 0.025 - 3 * mcse); expect_lte(size_ml, 0.075 + 3 * mcse)   # Bradley's band
})


vkp_sim <- function(res) {
    no <- gsub("<[^>]+>", "", res$notices$content)
    list(power = vkp_num(no, "kappa equals kappa1 was about ([0-9.]+), short"),
         n = vkp_num(no, "About ([0-9,]+) subjects gave a simulated power"),
         power_n = vkp_num(no, "gave a simulated power of about ([0-9.]+)"))
}


test_that("VAL C36 / KSP-IR-01: binary - the shortfall note reports the simulated power of the recommended test", {
    # Independent review 2026-10-15 (development-ideas/kappaSizePower-independent-review-2026-10-15.md): the local
    # projection's figures missed held-out simulation by up to 0.14 either way, so a binary outcome is now simulated.
    # Oracle: the audit's independent optimize()-based simulation of the same test (pooled cells, prevalence by
    # ML at kappa0), R = 20000: power 0.590 at N = 316 (6 raters, p .05, kappa .4 -> .6).
    res <- vkp_run(props = "0.05", raters = "6")
    expect_equal(vkp_n(res), 316)                             # headline stays the published N
    expect_true(vkp_has(res, "Simulated power below the stated power."))
    expect_false(vkp_has(res, "Expected proportions treated as known."))
    sim <- vkp_sim(res)
    expect_equal(sim$power, 0.590, tolerance = 0.03 / 0.590)  # 2 decimals + Monte Carlo error (R = 4000 vs 20000)
    expect_gte(sim$power_n, 0.785)                            # the N it reports reaches the target
    expect_gt(sim$n, 316)
    expect_match(gsub("<[^>]+>", "", res$notices$content), "In 4,000 simulated studies of 316 subjects", fixed = TRUE)
    expect_match(res$notices$content, "random seed 20261015", fixed = TRUE)
    t2 <- gsub("\\s+", " ", res$text2$content)
    expect_match(t2, "A simulation of the recommended goodness-of-fit test gives a power of about", fixed = TRUE)
    # same inputs, same output; the caller's random stream is left alone
    expect_identical(vkp_run(props = "0.05", raters = "6")$notices$content, res$notices$content)
    set.seed(99); x1 <- runif(1); set.seed(99); invisible(vkp_run(props = "0.05", raters = "6")); x2 <- runif(1)
    expect_identical(x1, x2)
    # the assumption itself is stated on every design
    for (r in list(res, vkp_run(props = "0.5", raters = "6"), vkp_run(outcome = "3", props = "0.3, 0.3, 0.4")))
        expect_match(r$notices$content, "treats the expected proportions as known", fixed = TRUE)
    # below 10 subjects nothing is simulated
    tiny <- vkp_run(props = "0.5", kappa0 = 0.02, kappa1 = 0.98)
    expect_lt(vkp_n(tiny), 10)
    expect_false(vkp_has(tiny, "Simulated power below the stated power."))
})


test_that("KSP-IR-01: the note follows the simulated power, not a projection (independent review 2026-10-15)", {
    skip_on_cran()
    # Independent simulations (audit blocks F, F2, V): published-N power already at or above target -> no note
    no_note <- list(
        vkp_run(kappa0 = 0.6, kappa1 = 0.8, props = "0.05"),                         # 0.827 (old note: "about 0.76")
        vkp_run(kappa0 = 0.6, kappa1 = 0.8, props = "0.10"),                         # 0.830 (old note: "about 0.77")
        vkp_run(kappa0 = 0.7, kappa1 = 0.9, props = "0.15", raters = "4", power = 0.9))  # 0.917 at a 0.90 target
    for (r in no_note) expect_false(vkp_has(r, "Simulated power below the stated power."))
    # ... and short by more than 0.03 -> note, with an N that reaches the target
    short <- list(
        list(r = vkp_run(kappa0 = 0.2, kappa1 = 0.4, props = "0.05"), truth = 0.751),     # 2 raters: old note said 0.78
        list(r = vkp_run(kappa0 = 0.7, kappa1 = 0.5, props = "0.005", raters = "6", alpha = 0.01), truth = 0.284))
    for (x in short) {
        expect_true(vkp_has(x$r, "Simulated power below the stated power."))
        sim <- vkp_sim(x$r)
        expect_lt(abs(sim$power - x$truth), 0.035)
        expect_gte(sim$power_n, 0.785)
    }
    # 3-5 categories: qualitative note only (it fires only with the Sparse notice in the reviewed grids)
    cat3 <- vkp_run(outcome = "3", props = "0.05, 0.05, 0.9", raters = "4")
    expect_true(vkp_has(cat3, "Expected proportions treated as known."))
    expect_match(cat3$notices$content, "How far short cannot be computed reliably here", fixed = TRUE)
    expect_false(grepl("subjects gave a simulated power", cat3$notices$content, fixed = TRUE))
})


test_that("KSP-IR-01: the Methodology states the approximation in both directions", {
    for (r in list(vkp_run(), vkp_run(props = "0.05", raters = "6"),
                   vkp_run(outcome = "4", props = "0.1, 0.2, 0.3, 0.4", raters = "3"))) {
        notes <- r$notices$content
        expect_match(notes, "can differ from the stated power in either direction", fixed = TRUE)
        expect_match(notes, "For a binary outcome the analysis therefore simulates the recommended test", fixed = TRUE)
        expect_false(grepl("whenever the loss exceeds 0.02", notes, fixed = TRUE))
    }
})


test_that("KSP-IR-02: two raters, binary - the Notes recommend ML for the prevalence (independent review 2026-10-15)", {
    # Simulated, R = 20000, p .01-.05, kappa0 .2-.6, nominal 0.05: the published plug-in (overall proportion of
    # positive ratings, DE 1992 p.1513) rejected a true kappa0 at 0.057-0.066, ML with kappa at kappa0 at 0.043-0.051.
    # The release review's second run (R = 20000, kappa1 = kappa0 + 0.2) gave 0.055-0.066 and 0.046-0.052: the union.
    notes <- vkp_run(props = "0.05")$notices$content
    expect_match(notes, "With two raters, estimate the prevalence for the test by maximum likelihood with kappa held at kappa0", fixed = TRUE)
    expect_match(notes, "rate of 0.055-0.066 and the maximum-likelihood version at 0.043-0.052.", fixed = TRUE)
    expect_false(grepl("from the same pooled cells", notes, fixed = TRUE))
    for (r in list(vkp_run(props = "0.05", raters = "3"), vkp_run(outcome = "3", props = "0.2, 0.3, 0.5")))
        expect_false(grepl("With two raters, estimate the prevalence", r$notices$content, fixed = TRUE))
})


test_that("VAL C36: binary 3+ raters - the Notes say how to estimate the prevalence and that the GOF test tests the model (fixed: VAL-kappaSizePower-10)", {
    # The published binary test plugs in the overall proportion of positive ratings (DE 1992 p.1513,
    # ADK 2001 p.585): simulated size 0.096-0.164 at 4-6 raters and a 1-10% finding; the pooled-cell
    # ML estimate at kappa0 gave 0.046-0.050. Under beta-binomial ratings at the same kappa the
    # mixture-model GOF test rejected a true kappa0 in 9-81% of studies (validation N5, N6).
    notes <- vkp_run(props = "0.05", raters = "6")$notices$content
    expect_match(notes, "maximum likelihood from the same pooled cells, with kappa held at kappa0", fixed = TRUE)
    expect_match(notes, "The goodness-of-fit test also tests that model", fixed = TRUE)
    expect_match(notes, "resamples subjects", fixed = TRUE)
    for (r in list(vkp_run(props = "0.05"), vkp_run(outcome = "3", props = "0.3, 0.6, 0.1", raters = "4"))) {
        expect_false(grepl("from the same pooled cells", r$notices$content, fixed = TRUE))
        expect_false(grepl("also tests that model", r$notices$content, fixed = TRUE))
    }
})


test_that("VAL C43 C16: a count printed in the Notes agrees with its own 'below 5' threshold (fixed: VAL-kappaSizePower-11)", {
    # smallest pooled cell x N = 4.97 (binary, 2 raters, kappa .3 -> .8); rarest marginal x N = 4.97 (kappa .2 -> .9).
    # N is the one shown, ceiling(N), a step function of p: solve with it held, then check it did not move.
    Ne <- 33
    pe <- uniroot(function(p) min(vkp_cells_bin(2, p, 0.3)) * Ne - 4.97, c(0.28, 0.32), tol = 1e-13)$root
    expect_equal(ceiling(vkp_N(2, 2, pe, 0.3, 0.8)), Ne)
    notes <- vkp_run(props = sprintf("%.12f", pe), kappa0 = 0.3, kappa1 = 0.8)$notices$content
    expect_equal(vkp_num(notes, "the smallest expected count is ([0-9.]+) and"), 4.97)
    Nm <- 18
    pm <- 4.97 / Nm
    expect_equal(ceiling(vkp_N(2, 2, pm, 0.2, 0.9)), Nm)
    notes <- vkp_run(props = sprintf("%.12f", pm), kappa0 = 0.2, kappa1 = 0.9)$notices$content
    expect_equal(vkp_num(notes, "rarest outcome category is expected in only ([0-9.]+) subjects"), 4.97)
})


test_that("VAL C44: the Notes count at the N shown and print the stated power to the simulated one's decimals", {
    # 4 categories, 6 raters: N = 25.09 unrounded, 26 shown; the rarest category is 0.1 x 26 = 2.6, not 2.5
    res <- vkp_run(outcome = "4", props = "0.1, 0.2, 0.3, 0.4", raters = "6", kappa0 = 0.3, kappa1 = 0.5)
    expect_equal(vkp_n(res), 26)
    expect_equal(vkp_num(res$notices$content, "rarest outcome category is expected in only ([0-9.]+) subjects"), 2.6)
    # "about 0.59, short of the stated 0.80", never "0.8" beside two decimals; a typed 0.825 is not rounded
    res <- vkp_run(props = "0.05", raters = "6")
    for (txt in c(res$notices$content, res$text2$content))
        expect_match(gsub("\\s+", " ", txt), "short of the stated 0.80", fixed = TRUE)
    expect_match(vkp_run(props = "0.05", raters = "6", power = 0.825)$notices$content,
                 "short of the stated 0.825", fixed = TRUE)
})


test_that("VAL C42: the 'Very large sample size' note claims 'always' only for a gap on the same side (fixed: VAL-kappaSizePower-12)", {
    # the counterexample across sides, through the wrapper (kappa0 .2, 6 raters, 5 categories)
    up   <- vkp_n(vkp_run(outcome = "5", props = "0.1, 0.1, 0.2, 0.3, 0.3", raters = "6", kappa0 = 0.2, kappa1 = 0.3))
    down <- vkp_n(vkp_run(outcome = "5", props = "0.1, 0.1, 0.2, 0.3, 0.3", raters = "6", kappa0 = 0.2, kappa1 = 0.05))
    expect_lt(up, down)
    notes <- vkp_run(props = "0.01")$notices$content
    expect_false(grepl("and a high power target always inflate the sample size", notes, fixed = TRUE))
    expect_match(notes, "Moving kappa1 closer to kappa0 from the same side, or raising the power target, always inflates", fixed = TRUE)
})


test_that("VAL C23: non-finite, negative or unreadable inputs are refused, never turned into a number", {
    # Probed 2026-10-14 through the wrapper. NaN / NA reach the R wrapper only (jamovi's Number
    # boxes cannot send them) and are refused there with a raw R message; every other case is a
    # jmvcore::reject() or an option-bound error.
    for (args in list(list(kappa0 = NaN), list(kappa1 = Inf), list(alpha = NA_real_), list(power = -Inf),
                      list(props = "Inf"), list(props = "NaN, 0.5"), list(props = "0x1"), list(props = "30%"),
                      list(props = "-0.3, 1.3"), list(props = "0.3, 0.7", outcome = "3")))
        expect_error(suppressWarnings(do.call(vkp_run, args)))
    # accepted spellings of a valid input give the oracle N
    expect_equal(vkp_n(vkp_run(props = ".3")), 191)
    expect_equal(vkp_n(vkp_run(props = "0.3\t0.7")), 191)
    expect_equal(vkp_n(vkp_run(props = "1e-1, 9e-1")), ceiling(vkp_N(2, 2, 0.1, 0.4, 0.6)))
})


test_that("review 2026-10-15: the bold sample size, the stated seed and the >1e9 shortfall follow the simulation", {
    # 6 raters, p .05, kappa .4 -> .6: the published 316 simulates to ~0.59. The bold line is what gets copied into a
    # protocol, so it must carry the simulated N too, not 316 alone.
    res  <- vkp_run(props = "0.05", raters = "6")
    sim  <- vkp_sim(res)
    bold <- regmatches(res$notices$content, regexpr("Required sample size: .*?raters\\.", res$notices$content))
    expect_match(bold, "<b>316</b> subjects by the published formula and about <b>", fixed = TRUE)
    expect_equal(vkp_num(bold, "about <b>([0-9,]+)</b> by simulating"), sim$n)
    # "random seed 20261015" must reproduce every simulated figure the notes report, the simulated N's power
    # included (the N search used to step the seed to 20261016-20, so it did not).
    priv <- kappaSizePowerClass$new(options = kappaSizePowerOptions$new(), data = data.frame())$.__enclos_env__$private
    again <- withr::with_seed(20261015L, priv$.simPowerBinary(6L, 0.05, 0.4, 0.6, 0.05, sim$n, 4000L))
    expect_equal(round(again, 2), sim$power_n)
    # The N that reaches the target exceeds rmultinom's 1e9 here (published 685,287,131); the shortfall was measured,
    # so it is still reported, with its seed and without a simulated N. It used to vanish.
    big <- vkp_run(props = "0.005", raters = "6", kappa0 = 0.7, kappa1 = 0.6996, alpha = 0.01)
    expect_equal(vkp_n(big), 685287131)
    expect_true(vkp_has(big, "Simulated power below the stated power."))
    expect_match(big$notices$content, "too large to simulate (random seed 20261015)", fixed = TRUE)
    expect_false(grepl("subjects gave a simulated power", big$notices$content, fixed = TRUE))
    expect_match(big$notices$content, "Required sample size: <b>685,287,131</b> subjects, each rated by all 6 raters.", fixed = TRUE)
    expect_match(gsub("\\s+", " ", big$text2$content), "the sample size that reaches it is too large to simulate", fixed = TRUE)
    # no simulation, no change: the default design keeps the plain line
    expect_match(vkp_run()$notices$content, "Required sample size: <b>241</b> subjects, each rated by all 2 raters.", fixed = TRUE)
})
