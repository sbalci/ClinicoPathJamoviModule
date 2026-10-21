# Validation regression tests for kappaSizeFixedN (/validate-function depth=exhaustive, 2026-10-16).
# Report: development-ideas/kappaSizeFixedN-validation-2026-10-16.md (addendum: the fix of the same day)
# Evidence script: development-scripts/validate_kappaSizeFixedN.R
#
# No expected value below comes from kappaSize or from R/kappaSizeFixedN.b.R / R/utils-kappasize.R:
#   * hand algebra, written out next to each value (two raters, p = .5: closed-form root);
#   * the numbers printed in Rotondi & Donner 2012 (J Clin Epidemiol 65:778-84), Tables 2 and 4,
#     read through the duality "N subjects give an expected two-sided 95% lower limit of kL",
#     i.e. this analysis at alpha = 0.025;
#   * an own goodness-of-fit chi-square from the published models - binary: with probability rho all
#     raters share one call (Altaye, Donner & Klar 2001 eq. (4); Donner & Eliasziw 1992 p.1512 for two
#     raters); 3-5 categories: the formula printed in RD2012 p.779 - and the bound = the minimal
#     admissible root of N * X^2 = qchisq(1 - 2 alpha, 1) (DE1992 p.1514; RD2012 p.780);
#   * an own restricted-ML version (prevalences re-estimated at each rho from the pooled cells,
#     Altaye, Donner & Eliasziw 2001 eqs (8)-(9)), fitted with optimize() / Nelder-Mead - a different
#     optimiser from the module's.
# Contract since the 2026-10-16 fix: the bound shown is the fixed-proportion root (binary, two raters)
# or the lower of the fixed and restricted-ML roots (every other design), a positive bound rounded DOWN
# to 3 decimals, |bound| < 0.001 at 2 significant digits, a negative one to nearest. VAL-kappaSizeFixedN-01
# ..07 were written skip-guarded against the pre-fix code (each failed with its skip removed) and are
# live now.

skip_if_not_installed("kappaSize")

vfn_run <- function(...) {
    args <- utils::modifyList(
        list(outcome = "2", kappa0 = 0.60, props = "0.20, 0.80", raters = "2", alpha = 0.05, n = 100L,
             showExplanations = TRUE, showSummary = TRUE),
        list(...))
    suppressWarnings(do.call(ClinicoPath::kappaSizeFixedN, args))
}
vfn_flat  <- function(x) gsub("\\s+", " ", x)
vfn_bound_str <- function(res)
    sub(".*The expected lower bound for kappa is (\\S+?)\\.(?: |$).*", "\\1", vfn_flat(res$text2$content), perl = TRUE)
vfn_bound <- function(res) as.numeric(vfn_bound_str(res))
# the fixed-proportion bound as the Study Explanation states it ("would be X" when re-estimation is lower)
vfn_fixed_shown <- function(res) {
    x <- vfn_flat(res$text2$content)
    if (grepl("the bound would be ", x, fixed = TRUE))
        as.numeric(sub(".*the bound would be (\\S+?);.*", "\\1", x)) else vfn_bound(res)
}

vfn_binU <- function(R, p, rho) {                       # exactly j of R positive, j = 0..R
    j <- 0:R
    (1 - rho) * stats::dbinom(j, R, p) + rho * ((j == 0) * (1 - p) + (j == R) * p)
}
vfn_binP <- function(R, p, rho) {                       # pooled: all - / any disagreement / all +
    u <- vfn_binU(R, p, rho); if (R == 2) u else c(u[1], sum(u[2:R]), u[R + 1])
}
vfn_agree <- function(R, pj, k)                         # RD2012 p.779
    pj^R * prod(1 + k * (seq_len(R - 1) - pj) / pj) / prod(1 + (0:(R - 2)) * k)
vfn_cells <- function(K, R, p, rho, pooled = TRUE) {
    if (K == 2) return((if (pooled) vfn_binP else vfn_binU)(R, p[1], rho))
    a <- vapply(p, function(pj) vfn_agree(R, pj, rho), numeric(1)); c(1 - sum(a), a)
}
vfn_floor <- function(K, R, p) {
    if (K == 2) { p <- p[1]; return(max(-p^(R - 1) / (1 - p^(R - 1)), -(1 - p)^(R - 1) / (1 - (1 - p)^(R - 1)))) }
    f <- max(-p / (R - 1 - p)); if (R >= 3) max(f, -1 / (R - 2)) else f
}
vfn_crit <- function(alpha) stats::qchisq(1 - 2 * alpha, 1)
vfn_root <- function(K, R, p, k0, n, crit) {           # exact minimal admissible root, fixed proportions
    q <- vfn_cells(K, R, p, k0)
    f <- function(r) { b <- vfn_cells(K, R, p, r); n * sum((q - b)^2 / b) - crit }
    stats::uniroot(f, c(vfn_floor(K, R, p) + 1e-12, k0 - 1e-10), tol = 1e-14)$root
}
# Restricted ML: p-hat(rho) maximises sum q log P(p, rho) over valid p; X2 against the fitted cells.
vfn_rml_fit <- function(K, R, p0, k0, rho) {
    q <- vfn_cells(K, R, p0, k0)
    valid <- function(p) {
        b <- vfn_cells(K, R, p, rho)
        if (!all(is.finite(b)) || any(b <= 0)) return(FALSE)
        if (K == 2) return(TRUE)
        all(outer(p, 0:(R - 1), function(pj, i) pj * (1 - rho) + i * rho) >= 0) && all(1 + (-1:(R - 2)) * rho > 0)
    }
    if (K == 2) {
        nll <- function(pp) { if (!valid(c(pp, 1 - pp))) return(1e10); -sum(q * log(vfn_cells(2, R, pp, rho))) }
        p <- stats::optimize(nll, c(1e-9, 1 - 1e-9), tol = 1e-13)$minimum; p <- c(p, 1 - p)
    } else {
        to_p <- function(th) { e <- exp(c(th, 0)); e / sum(e) }
        nll <- function(th) { pp <- to_p(th); if (!valid(pp)) return(1e10); -sum(q * log(vfn_cells(K, R, pp, rho))) }
        p <- to_p(stats::optim(log(p0[-K] / p0[K]), nll, method = "Nelder-Mead",
                               control = list(reltol = 1e-15, maxit = 20000))$par)
    }
    b <- vfn_cells(K, R, p, rho)
    list(props = p, stat = sum((q - b)^2 / b))
}
# Restricted-ML root, scanned down from the fixed root; Inf when it lies above the fixed root.
vfn_rml_root <- function(K, R, p, k0, n, crit) {
    rf <- vfn_root(K, R, p, k0, n, crit)
    g <- function(r) n * vfn_rml_fit(K, R, p, k0, r)$stat - crit
    if (g(rf) >= 0) return(Inf)
    hi <- rf
    repeat { lo <- hi - 0.005; if (g(lo) >= 0) break; hi <- lo }
    stats::uniroot(g, c(lo, hi), tol = 1e-11)$root
}
vfn_gov <- function(K, R, p, k0, n, alpha) {
    crit <- vfn_crit(alpha); f <- vfn_root(K, R, p, k0, n, crit)
    if (K == 2 && R == 2) return(f)
    min(f, vfn_rml_root(K, R, p, k0, n, crit))
}
# Display contract: positive >= 0.001 rounded down to 3 dp; 0 < x < 0.001 rounded down to 2 s.f. (IR-02);
# negative to nearest (3 dp, or 2 s.f. above -0.001).
vfn_shown_ok <- function(d, g) {
    if (abs(g) < 0.001 && g > 0) {
        u <- 10^(floor(log10(g)) - 1)
        return(g - d >= -1e-12 && g - d < u + 1e-12)
    }
    if (abs(g) < 0.001) return(abs(d - g) <= 0.5 * 10^(floor(log10(abs(g))) - 1) + 1e-12)
    if (g > 0) return(g - d >= -1e-9 && g - d < 0.001 + 1e-9)
    abs(d - g) <= 0.0005 + 1e-9
}
vfn_cochran <- function(e) any(e < 1) || mean(e < 5) > 0.2

test_that("C01 hand closed form: two raters, p = .5, kappa0 .6, n 100, one-sided 95%", {
    # cells .25(1 + k), .5(1 - k), .25(1 + k) (DE1992 p.1512). sum q^2/P = 1 + t, t = crit/n, gives
    # (1 + t) r^2 - 2 k0 r + k0^2 - t = 0, r = (k0 - sqrt(t (1 + t - k0^2))) / (1 + t) = 0.45339.
    rL <- function(k0, t) (k0 - sqrt(t * (1 + t - k0^2))) / (1 + t)
    expect_equal(rL(0.6, vfn_crit(0.05) / 100), 0.4533924, tolerance = 1e-6)
    res <- vfn_run(props = "0.5")
    expect_identical(vfn_bound_str(res), "0.453")
    expect_true(vfn_shown_ok(vfn_bound(res), rL(0.6, vfn_crit(0.05) / 100)))
})

test_that("C01 the bound is the governing root of the independent oracle, every outcome", {
    designs <- list(
        list(K = 2, R = 2, p = 0.2, k0 = 0.6, n = 100L),       # the defaults
        list(K = 2, R = 4, p = 0.1, k0 = 0.7, n = 150L),
        list(K = 2, R = 6, p = 0.3, k0 = 0.5, n = 60L),
        list(K = 3, R = 2, p = c(.2, .3, .5), k0 = 0.6, n = 100L),
        list(K = 3, R = 3, p = c(.11, .70, .19), k0 = 0.44, n = 171L),
        list(K = 4, R = 5, p = c(.1, .2, .3, .4), k0 = 0.8, n = 500L),
        list(K = 5, R = 6, p = c(.1, .2, .2, .25, .25), k0 = 0.4, n = 30L))
    for (d in designs) {
        pv <- if (d$K == 2) c(d$p, 1 - d$p) else d$p
        res <- vfn_run(outcome = as.character(d$K), kappa0 = d$k0, props = paste(d$p, collapse = ", "),
                       raters = as.character(d$R), n = d$n)
        g <- vfn_gov(d$K, d$R, pv, d$k0, d$n, 0.05)
        expect_true(vfn_shown_ok(vfn_bound(res), g), info = paste(d$K, d$R, d$k0, d$n, vfn_bound(res), g))
        # the fixed-proportion figure the Study Explanation states is the fixed root
        expect_true(vfn_shown_ok(vfn_fixed_shown(res), vfn_root(d$K, d$R, pv, d$k0, d$n, vfn_crit(0.05))))
    }
})

test_that("C01 Rotondi & Donner 2012 Tables 2 and 4: the published N reaches kL (fixed proportions, alpha 0.025)", {
    rd <- rbind(
        data.frame(K = 3, k0 = .44, kL = .39, kU = .49, R = 2:6, N = c(1209, 696, 547, 478, 437), p = "0.11, 0.70, 0.19"),
        data.frame(K = 3, k0 = .44, kL = .34, kU = .54, R = 2:6, N = c(306, 171, 135, 119, 110), p = "0.11, 0.70, 0.19"),
        data.frame(K = 5, k0 = .66, kL = .61, kU = .71, R = 2:6, N = c(600, 372, 295, 256, 232), p = "0.01, 0.12, 0.33, 0.36, 0.18"),
        data.frame(K = 5, k0 = .66, kL = .56, kU = .76, R = 2:6, N = c(161, 97, 76, 65, 58), p = "0.01, 0.12, 0.33, 0.36, 0.18"),
        data.frame(K = 4, k0 = .66, kL = .56, kU = .76, R = 2:6, N = c(162, 98, 76, 65, 59), p = "0.13, 0.33, 0.36, 0.18"),
        stringsAsFactors = FALSE)
    for (i in seq_len(nrow(rd))) {
        pv <- as.numeric(strsplit(rd$p[i], ", ")[[1]])
        q <- vfn_cells(rd$K[i], rd$R[i], pv, rd$k0[i])
        slope <- function(r) { b <- vfn_cells(rd$K[i], rd$R[i], pv, r); sum((q - b)^2 / b) }
        res <- vfn_run(outcome = as.character(rd$K[i]), kappa0 = rd$k0[i], props = rd$p[i],
                       raters = as.character(rd$R[i]), alpha = 0.025, n = as.integer(rd$N[i]))
        fx <- vfn_fixed_shown(res)
        # the paper's 3.84 rounding leaves the exact root up to 0.0006 under kL; shown rounded down
        expect_gte(fx, rd$kL[i] - 0.001)
        if (slope(rd$kL[i]) <= slope(rd$kU[i])) expect_lte(fx, rd$kL[i] + 0.001)
        expect_lte(vfn_bound(res), fx)                    # re-estimation can only lower it
    }
    # p.780-781: "a study with three raters and 171 films will provide an expected CI width of 0.20"
    r171 <- vfn_run(outcome = "3", kappa0 = 0.44, props = "0.11, 0.70, 0.19", raters = "3",
                    alpha = 0.025, n = 171L)
    expect_identical(vfn_fixed_shown(r171), 0.340)
    # re-estimated (ADE 2001 eqs 8-9): 0.33877, shown 0.338 -- what kappaSizeCI now plans for (n 190)
    expect_identical(vfn_bound(r171), 0.338)
})

test_that("C14 RD2012 Table 4 footnote a: the row with 'extremely small' expected counts is flagged sparse", {
    for (k in seq_along(2:6)) {
        res <- vfn_run(outcome = "5", kappa0 = 0.66, props = "0.01, 0.12, 0.33, 0.36, 0.18",
                       raters = as.character((2:6)[k]), alpha = 0.025, n = c(161L, 97L, 76L, 65L, 58L)[k])
        expect_match(res$notices$content, "Sparse categories.", fixed = TRUE)
    }
})

test_that("C14 3-5 categories: the sparse notice is Cochran's rule on the cells at the reported bound", {
    for (d in list(list(K = 3, R = 3, p = c(.05, .15, .80), k0 = 0.8, n = 40L),
                   list(K = 4, R = 2, p = c(.1, .2, .3, .4), k0 = 0.6, n = 100L),
                   list(K = 5, R = 4, p = c(.01, .12, .33, .36, .18), k0 = 0.66, n = 300L))) {
        res <- vfn_run(outcome = as.character(d$K), kappa0 = d$k0, props = paste(d$p, collapse = ", "),
                       raters = as.character(d$R), n = d$n)
        crit <- vfn_crit(0.05)
        rf <- vfn_root(d$K, d$R, d$p, d$k0, d$n, crit); rr <- vfn_rml_root(d$K, d$R, d$p, d$k0, d$n, crit)
        cells <- if (rr < rf) vfn_cells(d$K, d$R, vfn_rml_fit(d$K, d$R, d$p, d$k0, rr)$props, rr)
                 else vfn_cells(d$K, d$R, d$p, rf)
        expect_identical(grepl("Sparse categories.", res$notices$content, fixed = TRUE), vfn_cochran(d$n * cells))
    }
})

test_that("C16 kappaSize's walk out of the model is never shown; the in-model root is", {
    oom <- list(list(o = "2", p = "0.02", R = "3", k0 = 0.01, n = 100L, a = 0.2),
                list(o = "2", p = "0.02", R = "2", k0 = 0.01, n = 11L, a = 0.001),
                list(o = "5", p = "0.01, 0.02, 0.02, 0.05, 0.90", R = "3", k0 = 0.01, n = 11L, a = 0.001),
                list(o = "3", p = "0.05, 0.15, 0.80", R = "3", k0 = 0.05, n = 15L, a = 0.001))
    for (d in oom) {
        raw <- as.numeric(strsplit(d$p, ", ")[[1]]); K <- as.integer(d$o); R <- as.integer(d$R)
        pv <- if (K == 2) c(raw, 1 - raw) else raw
        # kappaSize itself lands below the floor (this is what used to be printed, then refused)
        kfun <- switch(d$o, "2" = kappaSize::FixedNBinary, "3" = kappaSize::FixedN3Cats, "5" = kappaSize::FixedN5Cats)
        expect_lt(kfun(kappa0 = d$k0, n = d$n, props = raw, alpha = d$a, raters = R)$kappaL, vfn_floor(K, R, pv))
        res <- vfn_run(outcome = d$o, kappa0 = d$k0, props = d$p, raters = d$R, alpha = d$a, n = d$n)
        b <- vfn_bound(res)
        expect_lte(b, vfn_root(K, R, pv, d$k0, d$n, vfn_crit(d$a)) + 0.0005)   # never above the fixed root
        # never outside the model. Once the proportions are re-estimated they may move off their anticipated
        # values, so the floor is that of the most permissive (equal) proportions; independent multi-start
        # restricted-ML roots (2026-10-17, IR-03): -0.03670 (5 categories) and -0.06190 (3 categories)
        reest <- !(K == 2 && R == 2)
        expect_gte(b, vfn_floor(K, R, if (reest) rep(1 / K, K) else pv) - 0.0005)
        if (K > 2) expect_identical(vfn_bound_str(res), if (K == 5) "-0.037" else "-0.062")
        expect_match(res$notices$content, "This sample size cannot demonstrate agreement.", fixed = TRUE)
        expect_match(vfn_flat(res$text_summary$content), "stepped past the lowest agreement the model allows", fixed = TRUE)
    }
})

test_that("C17 input guards at their thresholds", {
    expect_error(vfn_run(n = 10L)); expect_silent(vfn_run(n = 11L))
    expect_silent(vfn_run(n = 1000000L)); expect_error(vfn_run(n = 1000001L))
    expect_error(vfn_run(kappa0 = 0.0099)); expect_error(vfn_run(kappa0 = 0.9901))
    expect_error(vfn_run(alpha = 0.0009)); expect_error(vfn_run(alpha = 0.2001))
    expect_error(vfn_run(props = "0,20 0,80"), "decimal point, not a decimal comma", fixed = TRUE)
    expect_silent(vfn_run(props = "0.2, 0.8009"))
    expect_error(vfn_run(props = "0.2, 0.801"), "must sum to 1", fixed = TRUE)
    expect_error(vfn_run(props = "0.2, 0.3, 0.5"), "binary outcome", fixed = TRUE)
    expect_error(vfn_run(outcome = "3", props = "0.2, 0.8"), "exactly 3 proportions", fixed = TRUE)
    for (s in c("", "abc", "NaN", "0, 1", "-0.2, 1.2")) expect_error(vfn_run(props = s))
})

test_that("C22 the bound does not depend on the order of the categories", {
    b <- vapply(c("0.2", "0.8", "0.2, 0.8", "0.8, 0.2"), function(s) vfn_bound(vfn_run(props = s, raters = "4", n = 90L)), 0)
    expect_true(all(b == b[1]))
    p <- c(.01, .12, .33, .36, .18)
    b5 <- vapply(list(p, rev(p), p[c(3, 1, 5, 2, 4)]), function(x)
        vfn_bound(vfn_run(outcome = "5", props = paste(x, collapse = ", "), raters = "3", n = 120L)), 0)
    expect_true(all(b5 == b5[1]))
})

test_that("C11 the remedies move the bound up: more subjects, more raters, larger alpha", {
    b_n <- vapply(c(15L, 60L, 500L, 100000L), function(n) vfn_bound(vfn_run(kappa0 = 0.3, props = "0.05", n = n)), 0)
    b_r <- vapply(as.character(2:6), function(R) vfn_bound(vfn_run(kappa0 = 0.2, props = "0.5", raters = R, n = 60L)), 0)
    b_a <- vapply(c(0.001, 0.01, 0.05, 0.2), function(a) vfn_bound(vfn_run(outcome = "3", kappa0 = 0.44,
                  props = "0.11, 0.70, 0.19", raters = "3", alpha = a, n = 60L)), 0)
    expect_true(all(diff(b_n) >= 0)); expect_true(all(diff(b_r) >= 0)); expect_true(all(diff(b_a) >= 0))
})

test_that("C13 with kappa0 <= 0.40 no design reaches 0.40; just above 0.40 one does", {
    best <- function(k0) vfn_bound(vfn_run(kappa0 = k0, props = "0.5", raters = "2", alpha = 0.2, n = 1000000L))
    expect_lt(best(0.40), 0.40); expect_lt(best(0.35), 0.40)
    # the solved bound gets arbitrarily close to kappa0, so with two raters and p .5 kappa0 0.4009 reaches
    # 0.40 (Codex: 0.40013 at n = 1e6); just above 0.40 that is not true of every design (IR-04, tested in
    # the release-review file), which is why the remedy is chosen by .liftable()
    expect_gte(best(0.4009), 0.40); expect_gte(best(0.41), 0.40)
    expect_false(grepl("cannot support a reliability claim", vfn_run(kappa0 = 0.4009, props = "0.5",
                       alpha = 0.2, n = 1000000L)$notices$content, fixed = TRUE))
})

test_that("C02/C03/C04/C19 the panes agree and echo the inputs", {
    res <- vfn_run()
    b <- vfn_bound_str(res)
    expect_match(res$text1$content, paste0("lower limit for kappa of ", b, "."), fixed = TRUE)
    # the Summary pane keeps kappaSize's own figure (0.418 here) and says why it differs (0.419)
    expect_match(res$text_summary$content, "expected lower limit for kappa of 0.418.", fixed = TRUE)
    expect_match(vfn_flat(res$text_summary$content), "reports 0.418; the expected lower bound is 0.419", fixed = TRUE)
    t2 <- vfn_flat(res$text2$content)
    expect_match(t2, "kappa = 0.6 and have access to 100 subjects, each rated by 2 raters", fixed = TRUE)
    expect_match(t2, "the one-sided 95% lower confidence bound", fixed = TRUE)
    conf <- function(a) sub(".*the one-sided (\\S+)% lower.*", "\\1", vfn_flat(vfn_run(alpha = a)$text2$content))
    expect_identical(vapply(c(0.025, 0.001, 0.2, 0.07), conf, ""), c("97.5", "99.9", "80", "93"))
    # grouped by thousands as in kappaSizeCI (F4), and never "1e+05"
    expect_match(vfn_run(n = 100000L)$text1$content, "A total of 100,000 subjects", fixed = TRUE)
})

test_that("C09 two raters, equal marginals: the model's kappa is Cohen's kappa", {
    u <- vfn_binU(2, 0.2, 0.6); Po <- u[1] + u[3]; Pe <- 0.2^2 + 0.8^2
    expect_equal((Po - Pe) / (1 - Pe), 0.6, tolerance = 1e-12)
    a <- vfn_cells(3, 2, c(.11, .7, .19), 0.44)[-1]
    expect_equal((sum(a) - sum(c(.11, .7, .19)^2)) / (1 - sum(c(.11, .7, .19)^2)), 0.44, tolerance = 1e-12)
    expect_match(vfn_run()$methodology$content, "for two raters with equal marginal frequencies this kappa coincides with Cohen's kappa", fixed = TRUE)
})

test_that("C20 the engine's own marginal warning (Summary pane) reads the first proportion only", {
    expect_match(vfn_run(props = "0.03, 0.97")$text_summary$content, "less than five", fixed = TRUE)
    expect_false(grepl("less than five", vfn_run(props = "0.97, 0.03")$text_summary$content, fixed = TRUE))
    expect_identical(vfn_bound(vfn_run(props = "0.03, 0.97")), vfn_bound(vfn_run(props = "0.97, 0.03")))
})

test_that("C23 the exported syntax reproduces the bound", {
    # showExplanations = TRUE: the Study Explanation this reads is off by default, and with it off both
    # sides were "" and the comparison passed without comparing a bound.
    opts <- ClinicoPath:::kappaSizeFixedNOptions$new(outcome = "3", kappa0 = 0.5, props = "0.2, 0.3, 0.5",
                                                     raters = "3", alpha = 0.05, n = 150L,
                                                     showExplanations = TRUE)
    an <- ClinicoPath:::kappaSizeFixedNClass$new(options = opts, data = data.frame())
    an$init(); suppressWarnings(an$run())
    # eval() of the analysis's OWN generated call with fixed, test-defined inputs: that the syntax
    # pane's code runs and reproduces the bound is the claim under test.
    res2 <- suppressWarnings(eval(parse(text = an$asSource())))
    expect_true(is.finite(vfn_bound(an$results)))
    expect_identical(vfn_bound_str(res2), vfn_bound_str(an$results))
    expect_identical(res2$text1$content, an$results$text1$content)
})

test_that("C07 'roughly half' of studies observing the planned design end below the bound (simulation)", {
    skip_on_cran()
    # two raters, p .2, kappa0 .6, n 100 (the defaults); DE1992 one-sided GOF limit, plug-in pi-hat
    set.seed(20261016)
    disp <- vfn_bound(vfn_run()); crit <- vfn_crit(0.05); pu <- vfn_binU(2, 0.2, 0.6)
    lo <- replicate(500, {
        m <- as.vector(stats::rmultinom(1, 100, pu)); ph <- (m[2] + 2 * m[3]) / 200
        X2 <- function(r) { e <- 100 * vfn_binU(2, ph, r); if (any(e <= 0)) return(Inf); sum((m - e)^2 / e) }
        fl <- vfn_floor(2, 2, ph)
        kh <- stats::optimize(X2, c(fl + 1e-9, 1 - 1e-9), tol = 1e-10)$minimum
        if (X2(fl + 1e-12) <= crit) fl else stats::uniroot(function(r) X2(r) - crit, c(fl + 1e-12, kh), tol = 1e-10)$root
    })
    expect_gte(mean(lo < disp), 0.40); expect_lte(mean(lo < disp), 0.60)
})

# ---------------------------------------------------------------------------------------------
# VAL-kappaSizeFixedN-01..07: written skip-guarded against the pre-fix code, live since the fix.
# ---------------------------------------------------------------------------------------------

test_that("VAL-kappaSizeFixedN-01 verdicts use the solved bound, not kappaSize's floating-point residue", {
    # kappa0 .2, p .5, two raters, n 68: hand root (k0 - sqrt(t(1 + t - k0^2)))/(1 + t), t = 2.705543/68,
    # is +0.000532 - chance-level agreement IS ruled out. kappaSize's walk gave -1.6e-16.
    t <- vfn_crit(0.05) / 68
    expect_equal((0.2 - sqrt(t * (1 + t - 0.04))) / (1 + t), 0.000532, tolerance = 1e-3)
    r68 <- vfn_run(kappa0 = 0.2, props = "0.5", n = 68L)
    expect_identical(vfn_bound_str(r68), "0.00053")
    expect_false(grepl("This sample size cannot demonstrate agreement.", r68$notices$content, fixed = TRUE))
    expect_false(grepl("at or below zero", r68$text2$content, fixed = TRUE))
    expect_false(grepl("e-16", paste(r68$text1$content, r68$text2$content, r68$text_summary$content), fixed = TRUE))
    # defaults at n = 83: exact limit 0.40012 - not "a bound under 0.40" (kappaSize walked to 0.3999999999999998)
    expect_gt(vfn_root(2, 2, 0.2, 0.6, 83, vfn_crit(0.05)), 0.40)
    r83 <- vfn_run(n = 83L)
    expect_identical(vfn_bound_str(r83), "0.400")
    expect_false(grepl("This design cannot support a reliability claim.", r83$notices$content, fixed = TRUE))
    # and on the other side of the threshold the amber block still fires (n = 82: exact limit below 0.40)
    expect_lt(vfn_root(2, 2, 0.2, 0.6, 82, vfn_crit(0.05)), 0.40)
    expect_match(vfn_run(n = 82L)$notices$content, "This design cannot support a reliability claim.", fixed = TRUE)
})

test_that("VAL-kappaSizeFixedN-02 binary 3+ raters: the sparse notice judges the pooled cells of the 1-df test", {
    # ADK 2001 p.586 pools every disagreement into one cell; under eq. (4) pooling leaves X^2 unchanged.
    # The unpooled rule fired on both on an interior cell below 1; the pooled cells are all large.
    for (d in list(list(R = 6, p = 0.2, k0 = 0.5, n = 1000L), list(R = 6, p = 0.2, k0 = 0.9, n = 1000L))) {
        res <- vfn_run(kappa0 = d$k0, props = as.character(d$p), raters = as.character(d$R), n = d$n)
        b <- vfn_bound(res)
        expect_false(vfn_cochran(d$n * vfn_binP(d$R, d$p, b)))
        expect_true(vfn_cochran(d$n * vfn_binU(d$R, d$p, b)))       # what the old rule judged
        expect_false(grepl("Sparse categories.", res$notices$content, fixed = TRUE))
    }
})

test_that("VAL-kappaSizeFixedN-03 3+ raters / 3-5 categories: the bound also holds when the prevalences are re-estimated", {
    # Simulated (R = 2000, evidence script): one-sided coverage of the plug-in GOF limit 0.927 (binary,
    # 4 raters, p .1, n 100) and 0.919 (RD2012's 3 categories, 3 raters, n 171); restricted ML 0.938, 0.953.
    # binary, 6 raters, p .05, kappa0 .7, n 50: fixed root 0.4058, restricted-ML root 0.3793
    res <- vfn_run(kappa0 = 0.7, props = "0.05", raters = "6", n = 50L)
    expect_true(vfn_shown_ok(vfn_bound(res), vfn_gov(2, 6, c(.05, .95), 0.7, 50, 0.05)))
    expect_identical(vfn_fixed_shown(res), 0.405)
    expect_lt(vfn_bound(res), 0.40)                                  # crosses 0.40 once re-estimated
    expect_match(res$notices$content, "This design cannot support a reliability claim.", fixed = TRUE)
    for (r in list(vfn_run(props = "0.1", raters = "4"),
                   vfn_run(outcome = "3", kappa0 = 0.44, props = "0.11, 0.70, 0.19", raters = "3", n = 171L)))
        expect_match(r$methodology$content, "maximum likelihood", fixed = TRUE)
    # binary with two raters: Donner & Eliasziw's plug-in interval keeps its coverage (0.953), nothing changes
    expect_false(grepl("maximum likelihood", vfn_run()$methodology$content, fixed = TRUE))
})

test_that("VAL-kappaSizeFixedN-04 the bound is within 0.001 of the exact root and the Notes no longer promise 'at most 0.001' for kappaSize's walk", {
    res <- vfn_run()   # the defaults: exact limit 0.419035, kappaSize's walk 0.418
    gap <- vfn_root(2, 2, 0.2, 0.6, 100, vfn_crit(0.05)) - vfn_bound(res)
    expect_true(gap >= 0 && gap < 0.001)
    expect_false(grepl("conservative by at most 0.001", res$methodology$content, fixed = TRUE))
})

test_that("VAL-kappaSizeFixedN-05 a study below the documented ~25 subjects carries the caveat", {
    res <- vfn_run(props = "0.5", n = 20L)     # dense cells (6.2 / 7.6 / 6.2), so no sparse box
    expect_false(grepl("Sparse categories.", res$notices$content, fixed = TRUE))
    expect_match(res$notices$content, "Small sample.", fixed = TRUE)
    expect_match(res$notices$content, "below 25", fixed = TRUE)
    expect_false(grepl("Small sample.", vfn_run(props = "0.5", n = 25L)$notices$content, fixed = TRUE))
})

test_that("VAL-kappaSizeFixedN-06 the models and the scale the output relies on are citable from it", {
    refs <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "kappaSizeFixedN.r.yaml"))$refs
    expect_true(all(c("altayeDonnerKlarKappaRaters", "altayeDonnerEliasziwKappaGOF", "LandisKoch1977") %in% refs))
    notes <- vfn_run()$methodology$content
    expect_match(notes, "Altaye, Donner and Klar", fixed = TRUE)
    expect_match(notes, "Altaye, Donner and Eliasziw", fixed = TRUE)
})

test_that("VAL-kappaSizeFixedN-07 the sparse remedy does not offer a larger alpha", {
    # two raters, p .05: all-positive cell at the bound is 100 * (.0025 + r * .0475), about 2.4 -> sparse
    res <- vfn_run(props = "0.05", n = 100L)
    expect_true(vfn_cochran(100 * vfn_cells(2, 2, 0.05, vfn_bound(res))))
    res20 <- vfn_run(props = "0.05", n = 100L, alpha = 0.2)
    expect_true(vfn_cochran(100 * vfn_cells(2, 2, 0.05, vfn_bound(res20))))   # still sparse at alpha .20
    expect_match(res$notices$content, "Sparse categories.", fixed = TRUE)
    expect_false(grepl("relaxing the significance level", res$notices$content, fixed = TRUE))
})

# ---------------------------------------------------------------------------------------------
# IR-kappaSizeFixedN-01/02 (development-ideas/kappaSizeFixedN-independent-review-2026-10-16.md),
# fixed by /check-function --profile release, 2026-10-16.
# ---------------------------------------------------------------------------------------------

test_that("IR-kappaSizeFixedN-02 a positive bound below 0.001 is rounded down, never up", {
    # kappa0 .22, p .5, two raters, n 56: hand root (k0 - sqrt(t(1 + t - k0^2)))/(1 + t), t = crit/56,
    # is 0.000197. Rounded to nearest at 2 s.f. it printed "0.0002", above the bound it stands for.
    t <- vfn_crit(0.05) / 56
    g <- (0.22 - sqrt(t * (1 + t - 0.22^2))) / (1 + t)
    expect_equal(g, 0.000197, tolerance = 1e-2)
    res <- vfn_run(kappa0 = 0.22, props = "0.5", n = 56L)
    expect_identical(vfn_bound_str(res), "0.00019")
    expect_lte(vfn_bound(res), g)
    expect_true(vfn_shown_ok(vfn_bound(res), g))
})

test_that("IR-kappaSizeFixedN-01 / FN-02 the plug-in coverage warning covers balanced categories too", {
    # FN-02 (independent review 2026-10-21) reverses IR-01's rare-category qualifier: balanced K4 R4 n 80
    # gave plug-in coverage 0.878, so "without such a category it stayed close to 0.95" was false.
    notes <- vfn_run(outcome = "3", props = "0.3, 0.3, 0.4", raters = "3")$methodology$content
    expect_false(grepl("less than about a fifth of the ratings", notes, fixed = TRUE))
    expect_false(grepl("stayed close to 0.95", notes, fixed = TRUE))
    expect_match(notes, "Balanced categories do not prevent it", fixed = TRUE)
    expect_false(grepl("with three or more raters, or three or more categories, plugging in",
                       notes, fixed = TRUE))
    # two raters, binary: no re-estimation, so no advice at all
    expect_false(grepl("estimate the category prevalences by maximum likelihood", vfn_run()$methodology$content,
                       fixed = TRUE))
})
