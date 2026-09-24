# Oracle tests for the latent ordinal agreement helpers (R/agreement-latent.R).
# Every expectation is checked against an independent oracle: published values
# (Nelson & Edwards 2015), a closed form, Monte Carlo, stats::integrate on the
# original z-integral, lme4::glmer, a simulation with known truth, or a hand count.
# Holmquist (5 categories) is checked by development-scripts/validate_agreement_holmquist.R.

# Absolute-difference check (testthat 3e's tolerance is relative).
expect_near <- function(object, expected, tol, label = NULL) {
    expect_lt(max(abs(object - expected)), tol, label = label)
}

# Oracle: the spec's z-integral, integrated piecewise around every threshold jump.
kappa_m_by_integrate <- function(rho, C) {
    if (rho == 0) return(0)
    q <- stats::qnorm(seq_len(C - 1) / C)
    s <- sqrt(rho)
    w <- sqrt(1 - rho)
    f <- function(z) {
        P <- vapply(c(-Inf, q, Inf), function(a) stats::pnorm((a - z * s) / w), numeric(length(z)))
        P <- matrix(P, nrow = length(z))
        rowSums((P[, -1, drop = FALSE] - P[, -ncol(P), drop = FALSE])^2) * stats::dnorm(z)
    }
    br <- q / s + rep(c(-20, -5, -1, 0, 1, 5, 20), each = C - 1) * w / s
    br <- sort(unique(c(-10, 10, br[br > -10 & br < 10])))
    po <- sum(vapply(seq_len(length(br) - 1), function(i)
        stats::integrate(f, br[i], br[i + 1], rel.tol = 1e-13, abs.tol = 1e-15,
                         subdivisions = 1000L)$value, numeric(1)))
    (C * po - 1) / (C - 1)
}

# Simulated crossed design: y* = u_i + v_j + e, cut at thr.
sim_latent <- function(u, v, thr) {
    n_case <- length(u)
    J <- length(v)
    ystar <- outer(u, v, "+") + matrix(stats::rnorm(n_case * J), n_case, J)
    codes <- matrix(findInterval(ystar, thr) + 1L, n_case, J)
    ratings <- as.data.frame(codes)
    names(ratings) <- paste0("P", seq_len(J))
    agreement_long_frame(ratings, levels = as.character(seq_len(length(thr) + 1)))
}

# --- kappa_m -----------------------------------------------------------------

test_that("kappa_m reproduces the published and derived values", {
    expect_near(agreement_kappa_m(0.7174, 5), 0.2661, 5e-4)  # Holmquist
    expect_near(agreement_kappa_m(0.8725, 4), 0.4836, 5e-4)  # Gleason
    expect_near(agreement_kappa_m(1 / 3, 5), 0.0899, 5e-4)
    expect_near(agreement_kappa_m(5 / 6, 5), 0.3684, 5e-4)
    expect_near(agreement_kappa_m(5 / 7, 5), 0.264, 5e-4)
})

test_that("kappa_m: C = 2 is (2/pi) asin(rho); rho = 0 -> 0; rho >= 1 -> 1; vectorised", {
    rr <- c(seq(0, 0.99, by = 0.01), 0.999, 0.9999)
    expect_near(agreement_kappa_m(rr, 2), 2 / pi * asin(rr), 1e-8)
    expect_identical(agreement_kappa_m(0, 5), 0)
    expect_identical(agreement_kappa_m(c(1, 1.5), 4), c(1, 1))
    expect_true(is.na(agreement_kappa_m(-0.1, 3)))
    expect_length(agreement_kappa_m(rr, 5), length(rr))
    expect_true(all(diff(agreement_kappa_m(rr, 5)) > 0))
})

test_that("kappa_m agrees with a Monte-Carlo oracle within 3 MCSE", {
    withr::local_seed(20260924)
    n <- 2e6
    for (C in 2:6) {
        q <- stats::qnorm(seq_len(C - 1) / C)
        for (rho in c(0.1, 0.5, 0.9)) {
            z <- stats::rnorm(n)
            a <- sqrt(rho) * z + sqrt(1 - rho) * stats::rnorm(n)
            b <- sqrt(rho) * z + sqrt(1 - rho) * stats::rnorm(n)
            same <- findInterval(a, q) == findInterval(b, q)
            po <- mean(same)
            k_mc <- (po - 1 / C) / (1 - 1 / C)
            mcse <- sqrt(po * (1 - po) / n) / (1 - 1 / C)
            expect_lt(abs(agreement_kappa_m(rho, C) - k_mc), 3 * mcse,
                      label = sprintf("C=%d rho=%.1f", C, rho))
        }
    }
})

test_that("kappa_m quadrature matches stats::integrate on the z-integral to 1e-8", {
    for (C in 2:8) for (rho in c(0.01, 0.2, 0.5, 0.8, 0.95, 0.99, 0.999, 0.9999)) {
        expect_near(agreement_kappa_m(rho, C), kappa_m_by_integrate(rho, C), 1e-8,
                    label = sprintf("C=%d rho=%g", C, rho))
    }
})

test_that("kappa_m_deriv matches a central finite difference", {
    h <- 1e-5
    for (C in c(2, 3, 5, 8)) for (rho in c(0.02, 0.3, 0.7174, 0.95)) {
        fd <- (agreement_kappa_m(rho + h, C) - agreement_kappa_m(rho - h, C)) / (2 * h)
        expect_equal(agreement_kappa_m_deriv(rho, C), fd, tolerance = 1e-6)
    }
    expect_equal(agreement_kappa_m_deriv(0.4, 2), 2 / pi / sqrt(1 - 0.16), tolerance = 1e-12)
    expect_true(is.na(agreement_kappa_m_deriv(1, 5)))
})

# --- long frame and design summary --------------------------------------------

test_that("long frame has one row per non-NA cell and keeps the declared level order", {
    ratings <- data.frame(A = c("High", "Low", NA, "Mid"),
                          B = factor(c("Low", NA, NA, "High")),
                          C = c(NA, NA, NA, NA),
                          stringsAsFactors = FALSE)
    lv <- c("Low", "Mid", "High")
    long <- agreement_long_frame(ratings, lv)
    expect_equal(nrow(long), sum(!is.na(ratings)))
    expect_identical(levels(long$y), lv)
    expect_true(is.ordered(long$y))
    expect_identical(levels(long$rater), c("A", "B"))
    expect_identical(levels(long$case), c("1", "2", "4"))
    expect_identical(as.character(long$y[long$case == "4" & long$rater == "B"]), "High")
    expect_false("cluster" %in% names(long))

    long2 <- agreement_long_frame(ratings, lv, cluster = c("x", "x", "y", "y"))
    expect_identical(as.character(long2$cluster), c("x", "x", "y", "x", "y"))

    err <- tryCatch(agreement_long_frame(ratings, c("Low", "Mid")), error = function(e) e)
    expect_s3_class(err, "agreement_unknown_labels")
    expect_identical(err$labels, "High")
    expect_error(agreement_long_frame(ratings, lv, cluster = "x"), class = "agreement_cluster_length")
    expect_error(agreement_long_frame(ratings, lv, cluster = c("x", NA, "y", "y")),
                 class = "agreement_cluster_missing")
})

test_that("design summary counts a hand-built incomplete design with 3 components", {
    # {R1,R2 x c1-3}, {R3,R4 x c4-5}, {R5 x c6}; row 7 unread; R6 never reads; "X" unused.
    ratings <- data.frame(
        R1 = c("a", "b", "a", NA, NA, NA, NA),
        R2 = c("a", "a", "b", NA, NA, NA, NA),
        R3 = c(NA, NA, NA, "b", "c", NA, NA),
        R4 = c(NA, NA, NA, "c", "c", NA, NA),
        R5 = c(NA, NA, NA, NA, NA, "a", NA),
        R6 = NA_character_, stringsAsFactors = FALSE)
    s <- agreement_design_summary(agreement_long_frame(ratings, c("a", "b", "c", "X")))
    expect_equal(s$n_cases, 6)
    expect_equal(s$n_raters, 5)
    expect_equal(s$n_ratings, 11)
    expect_equal(unname(s$per_case), c(1, 2, 2))
    expect_equal(unname(s$per_rater), c(1, 2, 3))
    expect_equal(s$n_cases_lt2, 1)
    expect_false(s$complete)
    expect_false(s$connected)
    expect_equal(s$n_components, 3)
    expect_equal(s$C_declared, 4)
    expect_equal(s$C_used, 3)
    expect_identical(s$categories_unused, "X")

    full <- agreement_design_summary(agreement_long_frame(ratings[1:3, 1:2], c("a", "b")))
    expect_true(full$complete)
    expect_true(full$connected)
    expect_equal(full$n_components, 1)
})

# --- latent fit, rho and kappa_m CI -------------------------------------------

test_that("fit_latent recovers a known latent ICC (I = 300, J = 8, K = 5)", {
    skip_if_not_installed("ordinal")
    withr::local_seed(4242)
    s2u <- 1.5
    s2v <- 0.2
    # Effects rescaled to exactly the target variances, so the truth is the realised
    # rho and the 0.05 window measures the estimator, not 8-rater sampling noise.
    u <- as.vector(scale(stats::rnorm(300))) * sqrt(s2u)
    v <- as.vector(scale(stats::rnorm(8))) * sqrt(s2v)
    long <- sim_latent(u, v, c(-0.5, 0.6, 1.4, 2.2))
    fit <- agreement_fit_latent(long)
    expect_true(fit$ok)
    expect_true(fit$converged)
    expect_false(fit$boundary)
    expect_true(fit$hessian_pd)
    expect_equal(fit$C_used, 5)
    expect_equal(fit$n, 2400)
    expect_equal(nrow(fit$thresholds), 4)
    expect_equal(dim(fit$vcov_sd), c(2L, 2L))
    rc <- agreement_rho_ci(fit)
    truth <- s2u / (s2u + s2v + 1)
    expect_lt(abs(rc$rho - truth), 0.05)
    expect_true(rc$ci_ok)
    expect_true(is.na(rc$reason))
    expect_true(rc$lower < rc$rho && rc$rho < rc$upper)
    kc <- agreement_kappa_m_ci(rc, 5)
    expect_equal(kc$kappa, agreement_kappa_m(rc$rho, 5))
    expect_equal(kc$lower, agreement_kappa_m(rc$lower, 5))
    expect_equal(kc$se, agreement_kappa_m_deriv(rc$rho, 5) * rc$se_rho)
})

test_that("fit_latent on poLCA::carcinoma (binary) matches lme4::glmer probit", {
    skip_if_not_installed("poLCA")
    skip_if_not_installed("lme4")
    skip_if_not_installed("ordinal")
    data("carcinoma", package = "poLCA", envir = environment())
    long <- agreement_long_frame(carcinoma, c("1", "2"))
    fit <- agreement_fit_latent(long)
    expect_true(fit$ok)
    long$yb <- as.integer(long$y == "2")
    g <- suppressWarnings(suppressMessages(lme4::glmer(yb ~ 1 + (1 | case) + (1 | rater), data = long,
                                      family = stats::binomial("probit"))))
    vc <- lme4::VarCorr(g)
    # Not 2e-3 absolute: on these data (s2_case ~ 6.7, a flat ridge) glmer fails its own
    # convergence check, its optimisers scatter s2_case over 6.71-6.83, and glmer's probit
    # Laplace objective differs from clmm's by a NON-constant 5.45-5.56 over a grid, so
    # the two MLEs genuinely differ by ~1-3 %. rho, the reported quantity, agrees closely.
    expect_equal(fit$s2_case, vc$case[1], tolerance = 0.05)
    expect_equal(fit$s2_rater, vc$rater[1], tolerance = 0.05)
    rho_glmer <- vc$case[1] / (vc$case[1] + vc$rater[1] + 1)
    expect_near(agreement_rho_ci(fit)$rho, rho_glmer, 0.01)
})

test_that("fit_latent returns ok = FALSE instead of throwing", {
    skip_if_not_installed("ordinal")
    ratings <- data.frame(A = c("1", "2", "1", "2"), B = c("1", "2", "2", "2"))
    fit <- agreement_fit_latent(agreement_long_frame(ratings, c("1", "2")))  # 2 raters
    expect_false(fit$ok)
    expect_true(is.character(fit$error) && nzchar(fit$error))
    rc <- agreement_rho_ci(fit)
    expect_false(rc$ci_ok)
    expect_identical(rc$reason, "fit_failed")
    one <- agreement_fit_latent(agreement_long_frame(ratings[1, ], c("1", "2")))
    expect_identical(one$error, "agreement_one_category")
})

test_that("rho_ci refuses a CI at the boundary", {
    fake <- list(ok = TRUE, s2_case = 1, s2_rater = 0, boundary = TRUE, hessian_pd = TRUE,
                 converged = TRUE, vcov_sd = matrix(0.01, 2, 2, dimnames = list(c("case", "rater"), c("case", "rater"))))
    rc <- agreement_rho_ci(fake)
    expect_equal(rc$rho, 0.5)
    expect_false(rc$ci_ok)
    expect_identical(rc$reason, "boundary")
    expect_true(is.na(rc$lower))
})

# --- severity -----------------------------------------------------------------

test_that("severity: a rater with v = +1 gets a POSITIVE shift; shifts centred; LRT df = J - 1", {
    skip_if_not_installed("ordinal")
    withr::local_seed(7)
    v <- c(1, 0, 0, 0, 0)
    long <- sim_latent(stats::rnorm(200, 0, sqrt(1.5)), v, c(-0.8, 0.4, 1.4))
    sev <- agreement_fit_severity(long)
    expect_true(attr(sev, "ok"))
    expect_identical(sev$rater, levels(long$rater))
    expect_equal(sum(sev$shift), 0, tolerance = 1e-10)
    target <- v - mean(v)  # 0.8, -0.2, ...
    expect_gt(sev$shift[1], 0)
    expect_lt(max(abs(sev$shift - target)), 0.3)
    expect_true(all(sev$se > 0))
    expect_lt(sev$p_holm[1], 0.001)
    expect_equal(sev$p_holm, stats::p.adjust(sev$p, "holm"))
    lrt <- attr(sev, "lrt")
    expect_equal(lrt[["df"]], 4)
    expect_lt(lrt[["p"]], 0.001)
    expect_equal(attr(sev, "nAGQ"), 10)
})

# --- boundary table (hand-computed) --------------------------------------------

boundary_example <- function() {
    ratings <- data.frame(
        A = c("G1", "G3", "G2", NA),
        B = c("G2", "G3", "G1", "G2"),
        C = c("G2", NA, "G3", "G1"), stringsAsFactors = FALSE)
    agreement_long_frame(ratings, c("G1", "G2", "G3"))
}

test_that("boundary table matches a hand computation (3 raters, 4 cases)", {
    bt <- agreement_boundary_table(boundary_example())
    expect_equal(nrow(bt), 6)
    expect_identical(unique(bt$boundary), c(">= G2", ">= G3"))
    # A, >= G2: cases 1-3, d = (0 - 1, 1 - 1, 1 - 0.5) = (-1, 0, 0.5)
    a2 <- bt[bt$rater == "A" & bt$boundary == ">= G2", ]
    d <- c(-1, 0, 0.5)
    expect_equal(a2$n, 3)
    expect_equal(a2$excess, -1 / 6)
    expect_equal(a2$se, sqrt(21) / 6 / sqrt(3))
    expect_equal(a2$upper, -1 / 6 + stats::qt(0.975, 2) * sqrt(21) / 6 / sqrt(3))
    # A, >= G3: d = (0 - 0, 1 - 1, 0 - 0.5)
    expect_equal(bt$excess[bt$rater == "A" & bt$boundary == ">= G3"], -1 / 6)
    # C, >= G2: cases 1, 3, 4, d = (1 - 0.5, 1 - 0.5, 0 - 1)
    c2 <- bt[bt$rater == "C" & bt$boundary == ">= G2", ]
    expect_equal(c(c2$n, c2$excess, c2$se), c(3, 0, sqrt(0.75) / sqrt(3)))

    nom <- agreement_boundary_table(boundary_example(), ordinal = FALSE)
    expect_equal(nrow(nom), 9)
    # B, = G1: cases 1-4, d = (0 - 0.5, 0 - 0, 1 - 0, 0 - 1) -> mean -1/8
    expect_equal(nom$excess[nom$rater == "B" & nom$boundary == "= G1"], -1 / 8)
    expect_equal(nom$n[nom$rater == "B" & nom$boundary == "= G1"], 4)
})

test_that("pathologist_classical matches a hand computation", {
    pc <- agreement_pathologist_classical(boundary_example())
    expect_identical(pc$rater, c("A", "B", "C"))
    expect_equal(pc$cases_read, c(3, 4, 3))
    # A-B 1/3 agree, A-C 0/2, B-C 1/3
    expect_equal(pc$pct_exact, 100 * c(1 / 5, 2 / 6, 1 / 5))
    # kappa A-B 0, A-C -1/3, B-C 0
    expect_equal(pc$mean_kappa, c(-1 / 6, 0, -1 / 6))
    expect_equal(pc$n_pairs_used, c(2, 2, 2))
})

# --- review fixes (2026-09-24) ------------------------------------------------------

# Multi-centre crossed design: y* = u_i + c_k(i) + v_j + e. Effects rescaled to exact variances.
sim_multicentre <- function(seed, s2u, s2v, s2c, I, J, K, thr = c(-0.5, 0.6, 1.4, 2.2)) {
    withr::local_seed(seed)
    u <- as.vector(scale(stats::rnorm(I))) * sqrt(s2u)
    v <- as.vector(scale(stats::rnorm(J))) * sqrt(s2v)
    cl <- rep(seq_len(K), length.out = I)
    cv <- if (s2c > 0) as.vector(scale(stats::rnorm(K))) * sqrt(s2c) else rep(0, K)
    ystar <- outer(u + cv[cl], v, "+") + matrix(stats::rnorm(I * J), I, J)
    ratings <- as.data.frame(matrix(findInterval(ystar, thr) + 1L, I, J))
    names(ratings) <- paste0("P", seq_len(J))
    agreement_long_frame(ratings, as.character(seq_len(length(thr) + 1)), cluster = paste0("C", cl))
}

# Oracle for se_rho: central-difference gradient of the population rho over the SDs, times vcov_sd.
se_rho_fd <- function(fit, h = 1e-6) {
    comps <- if (is.finite(fit$s2_cluster)) c("case", "rater", "cluster") else c("case", "rater")
    sd <- sqrt(c(case = fit$s2_case, rater = fit$s2_rater, cluster = fit$s2_cluster))[comps]
    rho_of <- function(s) {
        num <- s[["case"]]^2 + (if ("cluster" %in% names(s)) s[["cluster"]]^2 else 0)
        num / (num + s[["rater"]]^2 + 1)
    }
    g <- vapply(comps, function(k) {
        up <- dn <- sd
        up[[k]] <- sd[[k]] + h
        dn[[k]] <- sd[[k]] - h
        (rho_of(up) - rho_of(dn)) / (2 * h)
    }, numeric(1))
    V <- fit$vcov_sd[comps, comps]
    sqrt(as.numeric(t(g) %*% V %*% g))
}

test_that("rho recovery: se_rho equals a finite-difference gradient times vcov", {
    skip_if_not_installed("ordinal")
    withr::local_seed(4242)
    u <- as.vector(scale(stats::rnorm(300))) * sqrt(1.5)
    v <- as.vector(scale(stats::rnorm(8))) * sqrt(0.2)
    fit <- agreement_fit_latent(sim_latent(u, v, c(-0.5, 0.6, 1.4, 2.2)))
    rc <- agreement_rho_ci(fit)
    expect_true(rc$ci_ok)
    expect_near(rc$se_rho, se_rho_fd(fit), 1e-6)
    expect_gt(rc$se_rho, 0)
})

test_that("cluster = TRUE estimates the POPULATION rho; delta method over three SDs; reorder mapping", {
    skip_if_not_installed("ordinal")
    # 300 cases, 8 raters, 10 centres: clmm orders the factors case (300), cluster (10), rater (8),
    # so the formula order (case, rater, cluster) is NOT the ST order.
    long <- sim_multicentre(11, s2u = 1.2, s2v = 0.2, s2c = 0.8, I = 300, J = 8, K = 10)
    f0 <- agreement_fit_latent(long)
    f1 <- agreement_fit_latent(long, cluster = TRUE)
    expect_true(f1$ok)
    expect_false(f1$boundary)
    expect_false(f1$boundary_cluster)
    r0 <- agreement_rho_ci(f0)
    r1 <- agreement_rho_ci(f1)
    expect_near(r1$rho, r0$rho, 0.03)
    expect_equal(r1$rho, (f1$s2_case + f1$s2_cluster) / (f1$s2_case + f1$s2_cluster + f1$s2_rater + 1))
    expect_true(r1$ci_ok)
    expect_equal(dim(f1$vcov_sd), c(3L, 3L))
    expect_near(r1$se_rho, se_rho_fd(f1), 1e-6)

    # Mapping oracle: refit directly and read each component by names(ST) / ordinal::VarCorr.
    m <- ordinal::clmm(y ~ 1 + (1 | case) + (1 | rater) + (1 | cluster), data = long, link = "probit")
    expect_identical(names(m$ST), c("case", "cluster", "rater"))  # the reorder happened
    vc <- ordinal::VarCorr(m)
    expect_equal(c(f1$s2_case, f1$s2_rater, f1$s2_cluster),
                 unname(c(vc$case[1], vc$rater[1], vc$cluster[1])), tolerance = 1e-8)
    V <- stats::vcov(m)
    st <- stats::setNames(paste0("ST", seq_along(m$ST)), names(m$ST))
    for (a in names(st)) for (b in names(st))
        expect_equal(f1$vcov_sd[a, b], V[st[[a]], st[[b]]], tolerance = 1e-8)
})

test_that("a cluster SD at the boundary does not hide the rho CI", {
    skip_if_not_installed("ordinal")
    # No centre effect: the cluster SD is estimated at 0 and clmm drops its ST from the Hessian.
    long <- sim_multicentre(104, s2u = 1.2, s2v = 0.2, s2c = 0, I = 200, J = 8, K = 6)
    fit <- agreement_fit_latent(long, cluster = TRUE)
    expect_true(fit$ok)
    expect_true(fit$boundary_cluster)
    expect_false(fit$boundary)
    expect_true(all(is.finite(fit$vcov_sd)))
    expect_equal(unname(fit$vcov_sd["cluster", ]), c(0, 0, 0))
    expect_true(is.na(fit$se_sd[["cluster"]]))
    rc <- agreement_rho_ci(fit)
    expect_true(rc$ci_ok)
    expect_true(is.na(rc$reason))
    expect_near(rc$se_rho, se_rho_fd(fit), 1e-6)
    # Case and rater SDs still decide: a case/rater SD at the bound refuses the CI.
    fake <- list(ok = TRUE, s2_case = 1, s2_rater = 0.2, s2_cluster = 0, boundary = FALSE,
                 boundary_cluster = TRUE, hessian_pd = TRUE, converged = TRUE,
                 vcov_sd = matrix(c(0.01, 0, 0, 0, 0.02, 0, 0, 0, 0), 3, 3,
                                  dimnames = rep(list(c("case", "rater", "cluster")), 2)))
    expect_true(agreement_rho_ci(fake)$ci_ok)
    fake$boundary <- TRUE
    expect_identical(agreement_rho_ci(fake)$reason, "boundary")
})

test_that("severity: shifts and SEs are invariant to rater order (last-rater SE)", {
    skip_if_not_installed("ordinal")
    withr::local_seed(7)
    long <- sim_latent(stats::rnorm(200, 0, sqrt(1.5)), c(1, 0, -0.4, 0, 0.3), c(-0.8, 0.4, 1.4))
    sev <- agreement_fit_severity(long)
    rev_long <- long
    rev_long$rater <- factor(long$rater, levels = rev(levels(long$rater)))
    sev2 <- agreement_fit_severity(rev_long)
    sev2 <- sev2[match(sev$rater, sev2$rater), ]
    expect_near(sev2$shift, sev$shift, 1e-5)
    expect_near(sev2$se, sev$se, 1e-5)
    expect_true(attr(sev, "se_ok"))
})

# Wrap the real fitter and overwrite the returned fit's gradient (or Hessian).
mock_fits <- function(gradient = NULL, hessian = NULL) {
    real <- .agreement_try_fit
    function(expr) {
        r <- real(expr)
        if (!is.null(r$value)) {
            if (!is.null(gradient)) r$value$gradient[] <- gradient
            if (!is.null(hessian)) r$value$Hessian <- hessian(r$value$Hessian)
        }
        r
    }
}

test_that("severity and fit_latent share the convergence rule; the Newton-step rule decides", {
    skip_if_not_installed("ordinal")
    withr::local_seed(7)
    long <- sim_latent(stats::rnorm(200, 0, sqrt(1.5)), c(1, 0, 0, 0, 0), c(-0.8, 0.4, 1.4))
    # |grad| = 2e-3 fails the gradient rule; the Newton step is far below 1% of every SE.
    local({
        local_mocked_bindings(.agreement_try_fit = mock_fits(gradient = 2e-3))
        sev <- agreement_fit_severity(long)
        expect_true(attr(sev, "converged"))
        fit <- agreement_fit_latent(long)
        expect_gt(fit$max_grad, 1e-3)
        expect_lt(fit$max_rel_step, 0.01)
        expect_true(fit$converged)
    })
    # A large gradient fails both rules.
    local({
        local_mocked_bindings(.agreement_try_fit = mock_fits(gradient = 50))
        expect_false(attr(agreement_fit_severity(long), "converged"))
        expect_false(agreement_fit_latent(long)$converged)
    })
})

test_that("severity: vcov failure keeps the shifts, blanks the SEs, never NaN", {
    skip_if_not_installed("ordinal")
    withr::local_seed(7)
    long <- sim_latent(stats::rnorm(200, 0, sqrt(1.5)), c(1, 0, 0, 0, 0), c(-0.8, 0.4, 1.4))
    ref <- agreement_fit_severity(long)
    for (h in list(function(H) { H[] <- NaN; H }, function(H) -H)) {
        local({
            local_mocked_bindings(.agreement_try_fit = mock_fits(hessian = h))
            sev <- agreement_fit_severity(long)
            expect_true(attr(sev, "ok"))
            expect_false(attr(sev, "se_ok"))
            expect_near(sev$shift, ref$shift, 1e-12)
            for (col in c("se", "lower", "upper", "p", "p_holm")) {
                expect_true(all(is.na(sev[[col]])), label = col)
                expect_false(any(is.nan(sev[[col]])), label = col)
            }
        })
    }
})

# --- gate -----------------------------------------------------------------------------

gate_design <- function(n_cases, n_raters, median_per_case, connected = TRUE) {
    list(n_cases = n_cases, n_raters = n_raters,
         per_case = c(min = 1, median = median_per_case, max = n_raters), connected = connected)
}
good_fit <- list(ok = TRUE, converged = TRUE)
good_ci <- list(ci_ok = TRUE, reason = NA_character_)
good_sev <- structure(data.frame(), ok = TRUE, converged = TRUE, se_ok = TRUE)

test_that("gate constants are the grid v2 values (validate_agreement_ordinal_grid.R, 2026-09-24)", {
    # Changing one of these means rerunning the grid, and re-reading .populateGradingGuide() and
    # .gradingReason() in agreement.b.R (their skewed and binary sentences assume the Inf limits).
    # Each value is where every simulated design passed,
    # except rho_ci_min_raters/rho_ci_min_cases, a recorded maintainer decision (2026-09-24) that departs
    # from the rule's UPPER coverage bound only - see the comment on AGREEMENT_LATENT_GATES.
    expect_identical(AGREEMENT_LATENT_GATES, list(
        skew_share = 0.50,
        kappa_m_min_raters = 3L,
        kappa_m_min_per_case = 3,
        kappa_m_min_cases = 50L,
        kappa_m_min_cases_skewed = Inf,
        rho_min_cases = 50L,
        rho_ci_min_raters = 3L,
        rho_ci_min_cases = 100L,
        rho_min_cases_incomplete = 100L,
        binary_min_per_case = Inf,
        binary_min_cases = 50L,
        severity_min_raters = 3L,
        severity_min_cases = 50L,
        severity_min_raters_binary = 4L,
        severity_min_raters_skewed = 6L))
})

test_that("gate: ordinal kappa_m boundaries (raters, ratings per case, cases, skew)", {
    G <- AGREEMENT_LATENT_GATES
    gate <- function(...) agreement_latent_gate(fit = good_fit, rho_ci = good_ci, severity = good_sev, ...)
    ok <- gate(gate_design(G$kappa_m_min_cases, G$kappa_m_min_raters, G$kappa_m_min_per_case), "ordinal", G$skew_share - 0.1)
    expect_true(ok$show_kappa_m)
    expect_true(is.na(ok$reason_model))
    r <- gate(gate_design(G$kappa_m_min_cases, G$kappa_m_min_raters - 1, G$kappa_m_min_per_case), "ordinal", G$skew_share - 0.1)
    expect_false(r$show_kappa_m)
    expect_identical(r$reason_model, "too_few_raters")
    r <- gate(gate_design(G$kappa_m_min_cases, G$kappa_m_min_raters, G$kappa_m_min_per_case - 0.5), "ordinal", G$skew_share - 0.1)
    expect_identical(r$reason_model, "too_few_ratings_per_case")
    r <- gate(gate_design(G$kappa_m_min_cases - 1, G$kappa_m_min_raters, G$kappa_m_min_per_case), "ordinal", G$skew_share - 0.1)
    expect_identical(r$reason_model, "too_few_cases")
    # skewed: the case minimum rises to kappa_m_min_cases_skewed at max_share >= skew_share
    n <- G$kappa_m_min_cases_skewed
    if (is.finite(n)) {
        expect_true(gate(gate_design(n - 1, 3, 3), "ordinal", G$skew_share - 1e-9)$show_kappa_m)
        r <- gate(gate_design(n - 1, 3, 3), "ordinal", G$skew_share)
        expect_false(r$show_kappa_m)
        expect_identical(r$reason_model, "skewed")
        expect_true(gate(gate_design(n, 3, 3), "ordinal", G$skew_share)$show_kappa_m)
    } else {  # Inf: never shown for skewed data, however many cases
        expect_true(gate(gate_design(1e6, 3, 3), "ordinal", G$skew_share - 1e-9)$show_kappa_m)
        r <- gate(gate_design(1e6, 3, 3), "ordinal", G$skew_share)
        expect_false(r$show_kappa_m)
        expect_identical(r$reason_model, "skewed")
    }
})

test_that("gate: ordinal rho point and CI boundaries", {
    G <- AGREEMENT_LATENT_GATES
    gate <- function(d, share = G$skew_share - 0.1, fit = good_fit, rho_ci = good_ci)
        agreement_latent_gate(d, "ordinal", share, fit = fit, rho_ci = rho_ci)
    n <- max(G$rho_min_cases, G$rho_ci_min_cases)    # passes both the point and the CI limit
    nr <- G$rho_ci_min_raters
    r <- gate(gate_design(n, nr, 3))
    expect_true(r$show_rho && r$show_ci)
    # The CI has its own case minimum: between the two limits the point shows and the CI does not.
    if (G$rho_ci_min_cases > G$rho_min_cases) {
        r <- gate(gate_design(G$rho_ci_min_cases - 1, nr, 3))
        expect_true(r$show_rho)
        expect_false(r$show_ci)
        expect_identical(r$reason_ci, "too_few_cases")
    }
    r <- gate(gate_design(G$rho_min_cases - 1, nr, 3))
    # kappa_m's own case limit decides whether it shows one case below the rho limit.
    expect_identical(r$show_kappa_m, G$rho_min_cases - 1 >= G$kappa_m_min_cases)
    expect_false(r$show_rho)
    expect_identical(r$reason_rho, "too_few_cases")
    expect_false(r$show_ci)
    r <- gate(gate_design(n, nr, 3), share = G$skew_share)
    expect_identical(r$show_kappa_m, n >= G$kappa_m_min_cases_skewed)
    expect_false(r$show_rho)
    expect_identical(r$reason_rho, "skewed")
    expect_true(gate(gate_design(n, nr, 3), share = G$skew_share - 1e-9)$show_rho)
    r <- gate(gate_design(n, nr - 1, 3))
    expect_false(r$show_ci)
    expect_identical(r$reason_ci, "too_few_raters")
    # Only when the CI needs more raters than kappa_m does the point survive a CI rater refusal.
    expect_identical(r$show_rho, nr - 1 >= G$kappa_m_min_raters)
    for (why in c("boundary", "hessian_not_pd", "not_converged")) {
        r <- gate(gate_design(n, nr, 3), rho_ci = list(ci_ok = FALSE, reason = why))
        expect_true(r$show_rho)
        expect_false(r$show_ci)
        expect_identical(r$reason_ci, why)
    }
    expect_identical(gate(gate_design(n, nr, 3), rho_ci = list(ci_ok = FALSE, reason = "se_unavailable"))$reason_ci,
                     "se_unavailable")
    # Incomplete designs need more cases for the rho point (3-of-8 fails at 50 cases).
    inc <- function(nc) { d <- gate_design(nc, 8, 3); d$complete <- FALSE; gate(d) }
    if (G$rho_min_cases_incomplete > G$rho_min_cases) {
        r <- inc(G$rho_min_cases_incomplete - 1)
        expect_false(r$show_rho)
        expect_identical(r$reason_rho, "too_few_cases_incomplete")
    }
    expect_true(inc(G$rho_min_cases_incomplete)$show_rho)
    # No rho_ci yet: CI not shown, no reason (pending).
    r <- gate(gate_design(n, nr, 3), rho_ci = NULL)
    expect_false(r$show_ci)
    expect_true(is.na(r$reason_ci))
})

test_that("gate: fit status refuses the model-based numbers", {
    G <- AGREEMENT_LATENT_GATES
    d <- gate_design(max(G$rho_min_cases, G$rho_ci_min_cases), G$rho_ci_min_raters, G$rho_ci_min_raters)  # passes every design limit
    r <- agreement_latent_gate(d, "ordinal", 0.4, fit = list(ok = FALSE), rho_ci = good_ci)
    expect_false(r$show_kappa_m || r$show_rho || r$show_ci)
    expect_identical(c(r$reason_model, r$reason_rho, r$reason_ci), rep("fit_failed", 3))
    r <- agreement_latent_gate(d, "ordinal", 0.4, fit = list(ok = TRUE, converged = FALSE), rho_ci = good_ci)
    expect_identical(r$reason_model, "not_converged")
    expect_false(r$show_kappa_m)
    r <- agreement_latent_gate(d, "ordinal", 0.4)  # design only: nothing shown, nothing refused
    expect_false(r$show_kappa_m || r$show_rho || r$show_ci || r$show_severity)
    expect_true(all(is.na(c(r$reason_model, r$reason_rho, r$reason_ci, r$reason_severity))))
})

test_that("gate: binary boundaries", {
    G <- AGREEMENT_LATENT_GATES
    gate <- function(d) agreement_latent_gate(d, "binary", 0.5, fit = good_fit, rho_ci = good_ci)
    m <- G$binary_min_per_case
    n <- G$binary_min_cases
    if (is.finite(m)) {
        r <- gate(gate_design(n, m, m))
        expect_true(r$show_kappa_m && r$show_rho && r$show_ci)
        r <- gate(gate_design(n, m + 2, m - 0.5))
        expect_false(r$show_kappa_m || r$show_rho || r$show_ci)
        expect_identical(c(r$reason_model, r$reason_rho, r$reason_ci), rep("binary_needs_more_raters", 3))
        r <- gate(gate_design(n - 1, m, m))
        expect_false(r$show_kappa_m)
        expect_identical(r$reason_model, "too_few_cases")
    } else {  # Inf: no binary design shows a model-based number
        r <- gate(gate_design(1e6, 1000, 1000))
        expect_false(r$show_kappa_m || r$show_rho || r$show_ci)
        expect_identical(c(r$reason_model, r$reason_rho, r$reason_ci), rep("binary_needs_more_raters", 3))
    }
})

test_that("gate: severity boundaries (ordinal and binary) and severity fit status", {
    G <- AGREEMENT_LATENT_GATES
    share <- G$skew_share - 0.1                          # not skewed
    for (sc in c("ordinal", "binary")) {
        gate <- function(d, sev = good_sev) agreement_latent_gate(d, sc, share, severity = sev)
        nr <- agreement_severity_min_raters(sc, share)   # the scale's own limit (binary needs more)
        n <- G$severity_min_cases
        r <- gate(gate_design(n, nr, 2))
        expect_true(r$show_severity && r$show_severity_ci && r$show_severity_labels, label = sc)
        r <- gate(gate_design(n, nr - 1, 2))
        expect_false(r$show_severity || r$show_severity_ci || r$show_severity_labels)
        expect_identical(r$reason_severity, "too_few_raters")
        r <- gate(gate_design(n - 1, nr, 2))
        expect_false(r$show_severity)
        expect_identical(r$reason_severity, "too_few_cases")
        r <- gate(gate_design(n, nr, 2), sev = structure(data.frame(), ok = TRUE, converged = TRUE, se_ok = FALSE))
        expect_true(r$show_severity)
        expect_false(r$show_severity_ci || r$show_severity_labels)
        expect_identical(r$reason_severity, "hessian_not_pd")
        r <- gate(gate_design(n, nr, 2), sev = structure(data.frame(), ok = FALSE))
        expect_identical(r$reason_severity, "fit_failed")
        r <- gate(gate_design(n, nr, 2), sev = structure(data.frame(), ok = TRUE, converged = FALSE, se_ok = TRUE))
        expect_false(r$show_severity)
        expect_identical(r$reason_severity, "not_converged")
    }
})

test_that("gate: severity needs more pathologists for binary and for skewed data (severity grid, 2026-09-24)", {
    G <- AGREEMENT_LATENT_GATES
    expect_identical(agreement_severity_min_raters("ordinal", G$skew_share - 0.1), G$severity_min_raters)
    expect_identical(agreement_severity_min_raters("binary", G$skew_share - 0.1), G$severity_min_raters_binary)
    expect_identical(agreement_severity_min_raters("ordinal", G$skew_share), G$severity_min_raters_skewed)
    n <- G$severity_min_cases
    sev <- function(sc, share, raters)
        agreement_latent_gate(gate_design(n, raters, 2), sc, share, severity = good_sev)
    # binary: refused one below its limit, shown at it
    expect_identical(sev("binary", 0.3, G$severity_min_raters_binary - 1)$reason_severity, "too_few_raters")
    expect_true(sev("binary", 0.3, G$severity_min_raters_binary)$show_severity_labels)
    # skewed ordinal: refused one below its limit, shown at it; the base limit alone is not enough
    expect_identical(sev("ordinal", G$skew_share, G$severity_min_raters_skewed - 1)$reason_severity, "too_few_raters")
    expect_true(sev("ordinal", G$skew_share, G$severity_min_raters_skewed)$show_severity_labels)
    expect_false(sev("ordinal", G$skew_share, G$severity_min_raters)$show_severity)
})

test_that("gate: nominal and disconnected designs refuse everything model-based", {
    big <- gate_design(500, 10, 10)
    for (case in list(list(big, "nominal", "nominal_scale"),
                      list(gate_design(500, 10, 10, connected = FALSE), "ordinal", "disconnected"),
                      list(gate_design(500, 10, 10, connected = FALSE), "binary", "disconnected"))) {
        r <- agreement_latent_gate(case[[1]], case[[2]], 0.3, fit = good_fit, rho_ci = good_ci, severity = good_sev)
        expect_false(any(unlist(r[startsWith(names(r), "show_")])))
        expect_identical(unlist(r[startsWith(names(r), "reason_")], use.names = FALSE), rep(case[[3]], 4))
    }
    expect_error(agreement_latent_gate(big, "ordinal", 1.2))
})

test_that("gate: single-read cases do not count toward the case minimums (runtime review 2026-09-24)", {
    G <- AGREEMENT_LATENT_GATES
    gate <- function(d) agreement_latent_gate(d, "ordinal", G$skew_share - 0.1,
                                              fit = good_fit, rho_ci = good_ci, severity = good_sev)
    d <- gate_design(G$kappa_m_min_cases, 4, 4)
    expect_true(gate(d)$show_kappa_m)
    d$n_cases <- G$kappa_m_min_cases + 25L; d$n_cases_lt2 <- 25L   # 25 extra cases read once
    expect_true(gate(d)$show_kappa_m)
    d$n_cases <- G$kappa_m_min_cases - 1L + 25L                    # one informative case short
    r <- gate(d)
    expect_false(r$show_kappa_m || r$show_severity)
    expect_identical(c(r$reason_model, r$reason_severity), c("too_few_cases", "too_few_cases"))
})

test_that("gate: the grading tendency is refused above the runtime ceiling, the latent model is not", {
    G <- AGREEMENT_LATENT_GATES
    d <- gate_design(500, 20, 20)
    d$n_ratings <- 500 * 20                                        # 10,000 x 20 = 200,000 > ceiling
    expect_gt(d$n_ratings * d$n_raters, AGREEMENT_SEVERITY_MAX_WORK)
    r <- agreement_latent_gate(d, "ordinal", 0.3, fit = good_fit, rho_ci = good_ci, severity = good_sev)
    expect_identical(r$reason_severity, "too_large")
    expect_false(r$show_severity)
    expect_true(r$show_kappa_m && r$show_rho && r$show_ci)
    d$n_ratings <- floor(AGREEMENT_SEVERITY_MAX_WORK / 20)          # exactly at the ceiling: fitted
    expect_true(is.na(agreement_latent_gate(d, "ordinal", 0.3, severity = good_sev)$reason_severity))
})
