# Model-based ordinal agreement for many raters (Nelson & Edwards 2015, Stat Med 34:3116).
#
# Pure helpers for agreement.b.R: they return numbers and codes, never user-facing
# sentences (the R6 caller translates). Model:
#   Phi^-1(P(Y_ij <= c | u_i, v_j)) = alpha_c - (u_i + v_j),  u ~ N(0, s2u), v ~ N(0, s2v)
#   rho = s2u / (s2u + s2v + 1),  kappa_m = g_C(rho).
# With an institution term c_k ~ N(0, s2c) the case's latent value is u_i + c_k, and rho is
# still the POPULATION latent ICC, (s2u + s2c) / (s2u + s2c + s2v + 1): the cluster term
# accounts for clustering, it does not change the quantity reported.
# ordinal::clmm stores each random-effect SD directly (nlminb, lower bound 0), so
# vcov()'s ST rows are already on the SD scale; an SD below 1e-3 is dropped from the
# Hessian (ordinal:::paratBoundary2). Grouping factors are re-ordered by decreasing number
# of levels, and names(fit$ST) gives that order (the names on optRes$par do NOT).


# What the shipping analysis may show, per design. FINAL, from grid v2 of
# development-scripts/validate_agreement_ordinal_grid.R, 2026-09-24: decision_latent.csv,
# decision_severity.csv, regions.csv and summary_*.csv in the run's --out directory
# (scratchpad/grid2/out of that session). Per-cell rule: |bias| <= 0.02 for a point,
# coverage in [0.92, 0.98] for an interval, 200 replicates for every borderline cell; each
# limit is the largest quadrant {cases >= I0, raters >= J0} in which every design passes.
# Inf = never shown (no tested design passed). Read only by agreement_latent_gate().
# Shares are proportions (0-1); per-case counts are medians.
AGREEMENT_LATENT_GATES <- list(
    skew_share = 0.50,             # largest category share at/above which data count as skewed
    kappa_m_min_raters = 3L,       # ordinal kappa_m: passes at >= 50 cases, every J 3-10
    kappa_m_min_per_case = 3,
    kappa_m_min_cases = 50L,       # 30 cases fail at 3 raters (bias +0.020 at rho 0.80)
    kappa_m_min_cases_skewed = Inf, # 70% skew fails at 50 and 100 cases (bias up to +0.069)
    rho_min_cases = 50L,           # the rho point passes from 50 cases at every J 3-10
    # rho / kappa_m CI. The pre-set rule (coverage in [0.92, 0.98] in every cell) passed only
    # {>= 100 cases, >= 10 raters}. MAINTAINER DECISION 2026-09-24, a deliberate, recorded
    # departure from the upper bound only: at >= 100 cases and >= 4 raters (incl. the incomplete
    # 3-of-8 design) all 42 cells, 200 replicates each, covered 0.923-0.995 - none below 0.92;
    # every failure there is OVER-coverage (the interval is wider than it needs to be), so the
    # interval is shown and labelled conservative. Corrected after review the same day: at >= 100
    # cases 3 raters also pass (0.922-0.979, even the strict window); the 0.85-0.91 under-coverage
    # at 3 raters is at 30-50 cases only. Region {>= 100 cases, >= 3 raters}: 54 cells, coverage
    # 0.922-0.995, mean 0.954, none below 0.92.
    rho_ci_min_raters = 3L,
    rho_ci_min_cases = 100L,
    # Incomplete designs (not every rater reads every case): the 3-of-8 design fails the rho point
    # at 50 cases (bias -0.0203) and passes at 100.
    rho_min_cases_incomplete = 100L,
    binary_min_per_case = Inf,     # binary: rho 0.80 biased at every tested design (-0.028 to -0.053)
    binary_min_cases = 50L,
    severity_min_raters = 3L,      # severity shifts, CIs and Holm labels; 30 cases x 3 raters fail
    severity_min_cases = 50L,
    # Severity on binary and skewed data: a second run of the same script (--part for the severity
    # families, 2026-09-24; 44 cells x 200 replicates through agreement_fit_severity). Binary passes at
    # every tested design from 4 raters (3 was not tested, so it gets no allowance). Skewed data
    # (largest share >= skew_share) fail at 4 raters in the HARMFUL direction (shifted coverage
    # 0.908, family-wise false-flag rate 0.082) and pass at 6. 200 further replicates put both
    # within the limits, so the 6 is the pre-set rule applied as written, not a large effect.
    severity_min_raters_binary = 4L,
    severity_min_raters_skewed = 6L
)

# Runtime ceiling for the grading-tendency fit, in ratings x pathologists. Not a validity
# limit: each pathologist adds a fixed effect, and clmm's numerical gradient costs one
# Laplace pass per parameter, so the fit grows with ratings x pathologists. Measured on the
# development Mac (full + null fit): 3,000 ratings x 10 = 14 s, 5,000 x 10 = 31 s, 4,000 x 20
# = 35 s, 7,500 x 15 = 66-77 s, 20,000 x 20 = 439 s. Warm starts, ucminf and quadrature
# (nAGQ 5 / -7) did not help (2026-09-24). The latent model has one rater variance, not one
# parameter per rater, and fits 20,000 x 20 in 11 s, so it has no ceiling.
# ponytail: fixed ceiling; an opt-in "fit anyway" option if large panels ask for it.
AGREEMENT_SEVERITY_MAX_WORK <- 1e5

#' Pathologists needed for the grading tendency on this kind of data
#'
#' The one place the minimum is decided, shared by agreement_latent_gate() and the R6
#' sentence that explains a refusal, so the two cannot disagree.
#' @param scale "ordinal", "binary" or "nominal".
#' @param max_share largest category share (0-1).
#' @return integer minimum number of raters.
#' @noRd
agreement_severity_min_raters <- function(scale, max_share) {
    G <- AGREEMENT_LATENT_GATES
    if (identical(scale, "binary")) G$severity_min_raters_binary
    else if (max_share >= G$skew_share) G$severity_min_raters_skewed
    else G$severity_min_raters
}


#' Wide ratings -> long frame, one row per non-NA cell
#'
#' @param ratings data.frame, one column per rater, cells are category labels or NA.
#' @param levels character, the declared category order.
#' @param cluster optional vector (length nrow(ratings)): the institution of each case.
#' @return data.frame(case, rater, y [ordered], cluster [if given]); case = row position.
#'   Errors are classed conditions whose message is a code: agreement_unknown_labels
#'   (field `labels`), agreement_cluster_length, agreement_cluster_missing.
#' @noRd
agreement_long_frame <- function(ratings, levels, cluster = NULL) {
    ratings <- as.data.frame(ratings)
    levels <- as.character(levels)
    cells <- lapply(ratings, as.character)
    cells <- lapply(cells, function(x) { x[!is.na(x) & !nzchar(trimws(x))] <- NA_character_; x })
    y <- unlist(cells, use.names = FALSE)
    row <- rep(seq_len(nrow(ratings)), times = length(cells))
    col <- rep(seq_along(cells), each = nrow(ratings))
    keep <- !is.na(y)
    bad <- setdiff(unique(y[keep]), levels)
    if (length(bad))
        stop(errorCondition("agreement_unknown_labels", labels = bad,
                            class = "agreement_unknown_labels"))
    rated_rows <- sort(unique(row[keep]))
    rated_cols <- sort(unique(col[keep]))
    long <- data.frame(
        case = factor(row[keep], levels = rated_rows),
        rater = factor(names(ratings)[col[keep]], levels = names(ratings)[rated_cols]),
        y = factor(y[keep], levels = levels, ordered = TRUE)
    )
    if (!is.null(cluster)) {
        if (length(cluster) != nrow(ratings))
            stop(errorCondition("agreement_cluster_length", class = "agreement_cluster_length"))
        cl <- cluster[row[keep]]
        if (anyNA(cl))
            stop(errorCondition("agreement_cluster_missing", class = "agreement_cluster_missing"))
        long$cluster <- droplevels(as.factor(cl))
    }
    long
}


#' Design summary of a long agreement frame
#'
#' @return list(n_cases, n_raters, n_ratings, per_case, per_rater, n_cases_lt2, complete,
#'   connected, n_components, C_declared, C_used, categories_unused).
#' @noRd
agreement_design_summary <- function(long) {
    case <- droplevels(long$case)
    rater <- droplevels(long$rater)
    nc <- nlevels(case)
    nr <- nlevels(rater)
    by_case <- as.vector(table(case))
    by_rater <- as.vector(table(rater))
    three <- function(x) c(min = min(x), median = stats::median(x), max = max(x))

    # Components of the bipartite case-rater graph: frontier BFS over raters.
    ci <- as.integer(case)
    ri <- as.integer(rater)
    raters_of_case <- split(ri, factor(ci, levels = seq_len(nc)))
    cases_of_rater <- split(ci, factor(ri, levels = seq_len(nr)))
    comp_case <- integer(nc)
    comp_rater <- integer(nr)
    k <- 0L
    for (start in seq_len(nr)) {
        if (comp_rater[start] != 0L) next
        k <- k + 1L
        comp_rater[start] <- k
        frontier <- start
        while (length(frontier)) {
            cs <- unique(unlist(cases_of_rater[frontier], use.names = FALSE))
            cs <- cs[comp_case[cs] == 0L]
            comp_case[cs] <- k
            rs <- unique(unlist(raters_of_case[cs], use.names = FALSE))
            rs <- rs[comp_rater[rs] == 0L]
            comp_rater[rs] <- k
            frontier <- rs
        }
    }
    used <- levels(long$y)[levels(long$y) %in% as.character(long$y)]
    list(
        n_cases = nc,
        n_raters = nr,
        n_ratings = nrow(long),
        per_case = three(by_case),
        per_rater = three(by_rater),
        n_cases_lt2 = sum(by_case < 2),
        complete = nrow(long) == nc * nr,
        connected = k == 1L,
        n_components = k,
        C_declared = nlevels(long$y),
        C_used = length(used),
        categories_unused = setdiff(levels(long$y), used)
    )
}


# Gauss-Legendre nodes/weights on [-1, 1] (Golub-Welsch).
.agreement_gauss_legendre <- function(n) {
    k <- seq_len(n - 1)
    J <- matrix(0, n, n)
    J[cbind(k, k + 1)] <- J[cbind(k + 1, k)] <- k / sqrt(4 * k^2 - 1)
    e <- eigen(J, symmetric = TRUE)
    list(x = e$values, w = 2 * e$vectors[1, ]^2)
}

# S(t): the kappa_m integrand after the Sheppard substitution r = sin(t).
# P(both raters in category c) = Phi2 rectangle; d Phi2(h, k; r)/dr = bivariate
# density, and with r = sin(t) the arcsine singularity cancels, leaving
#   kappa_m = C / (2 pi (C - 1)) * integral_0^asin(rho) S(t) dt,
#   S(t) = 2 sum_j E(q_j, q_j) - 2 sum_j E(q_j, q_{j+1}),  q_j = qnorm(j / C),
#   E(h, k) = exp(-(h - k)^2 / (2 cos^2 t) - h k / (1 + sin t))   (stable form).
.agreement_kappa_S <- function(t, C) {
    q <- stats::qnorm(seq_len(C - 1) / C)
    s <- sin(t)
    c2 <- cos(t)^2
    diag_part <- vapply(q, function(h) exp(-h^2 / (1 + s)), numeric(length(t)))
    diag_part <- matrix(diag_part, nrow = length(t))
    out <- 2 * rowSums(diag_part)
    if (C > 2) {
        for (j in seq_len(C - 2)) {
            h <- q[j]
            k <- q[j + 1]
            out <- out - 2 * exp(-(h - k)^2 / (2 * c2) - h * k / (1 + s))
        }
    }
    out
}


#' Nelson-Edwards model-based kappa_m for latent ICC rho and C categories
#'
#' Composite Gauss-Legendre (8 panels x 16 nodes) of the arcsine-substituted
#' bivariate-normal integral; accurate to ~1e-12 for rho in [0, 0.9999].
#' @param rho numeric vector; rho = 0 -> 0, rho >= 1 -> 1, rho < 0 or NA -> NA.
#' @param C integer >= 2.
#' @noRd
agreement_kappa_m <- function(rho, C) {
    C <- as.integer(C)
    stopifnot(length(C) == 1L, C >= 2L)
    gl <- .agreement_gauss_legendre(16L)
    panels <- 8L
    vapply(rho, function(r) {
        if (is.na(r) || r < 0) return(NA_real_)
        if (r == 0) return(0)
        if (r >= 1) return(1)
        a <- asin(r)
        edges <- seq(0, a, length.out = panels + 1L)
        total <- 0
        for (p in seq_len(panels)) {
            half <- (edges[p + 1] - edges[p]) / 2
            mid <- (edges[p + 1] + edges[p]) / 2
            total <- total + half * sum(gl$w * .agreement_kappa_S(mid + half * gl$x, C))
        }
        C / (2 * pi * (C - 1)) * total
    }, numeric(1))
}


#' Analytic derivative d kappa_m / d rho
#'
#' = C / (2 pi (C - 1)) * S(asin rho) / sqrt(1 - rho^2); NA for rho < 0 or rho >= 1.
#' @noRd
agreement_kappa_m_deriv <- function(rho, C) {
    C <- as.integer(C)
    stopifnot(length(C) == 1L, C >= 2L)
    vapply(rho, function(r) {
        if (is.na(r) || r < 0 || r >= 1) return(NA_real_)
        C / (2 * pi * (C - 1)) * .agreement_kappa_S(asin(r), C) / sqrt(1 - r^2)
    }, numeric(1))
}


# Run a model-fitting call, muffling and keeping its warnings. Returns
# list(value = fit or NULL, error = message or NULL, warnings, seconds).
# Only third-party model calls go through here (never jmvcore::reject()).
.agreement_try_fit <- function(expr) {
    warns <- character(0)
    t0 <- proc.time()[["elapsed"]]
    value <- tryCatch(
        withCallingHandlers(expr, warning = function(w) {
            warns <<- c(warns, conditionMessage(w))
            invokeRestart("muffleWarning")
        }),
        error = function(e) e
    )
    secs <- proc.time()[["elapsed"]] - t0
    if (inherits(value, "error"))
        return(list(value = NULL, error = conditionMessage(value), warnings = warns, seconds = secs))
    list(value = value, error = NULL, warnings = warns, seconds = secs)
}


# One convergence rule for every clmm fit: nlminb code 0 AND (max|grad| <= 1e-3 OR the
# remaining Newton step is <= 1% of every SE). max|grad| alone flags ~10-45% of good fits
# (the NLL gradient grows with n; 04_bias_clmm.R).
.agreement_convergence <- function(fit) {
    H <- fit$Hessian
    pd <- !is.null(H) && all(is.finite(H)) &&
        min(eigen(H, symmetric = TRUE, only.values = TRUE)$values) > 0
    max_grad <- max(abs(fit$gradient))
    # A Hessian can pass the eigenvalue test and still be numerically singular; solve() then
    # raises, and an error here would take the whole analysis down (headline table included).
    # Guard the one call: the step is then unknown and convergence rests on the gradient alone.
    step <- if (pd) tryCatch(max(abs(solve(H, fit$gradient)) / sqrt(diag(solve(H)))),
                             error = function(e) NA_real_) else NA_real_
    list(converged = isTRUE(fit$optRes$convergence == 0) &&
             (isTRUE(max_grad <= 1e-3) || isTRUE(step <= 0.01)),
         max_grad = max_grad, max_rel_step = step, hessian_pd = pd)
}


#' Crossed-random-effects cumulative-probit fit (latent ICC model)
#'
#' @param long output of agreement_long_frame().
#' @param cluster logical, add (1|cluster) (needs long$cluster). The cluster term accounts
#'   for cases nested in institutions; agreement_rho_ci() still reports the population rho.
#' @return list(ok, error, warnings, s2_case, s2_rater, s2_cluster, se_sd, vcov_sd,
#'   thresholds = data.frame(threshold, estimate, se), n, C_used, converged, max_grad,
#'   max_rel_step, boundary, boundary_cluster, hessian_pd, seconds). Never throws for a
#'   model failure (ok = FALSE, error = message).
#'   boundary: the case or rater SD is at 0 (the cluster SD never sets it);
#'   boundary_cluster: the cluster SD is at 0 (NA without a cluster term).
#'   vcov_sd: covariance of the SDs, named by component; an SD clmm dropped at the bound
#'   has a zero row/column (held fixed at ~0) and se_sd NA. All NA when vcov() fails.
#' @noRd
agreement_fit_latent <- function(long, cluster = FALSE) {
    long$y <- droplevels(long$y)
    out <- list(ok = FALSE, error = NULL, warnings = character(0),
                s2_case = NA_real_, s2_rater = NA_real_, s2_cluster = NA_real_,
                se_sd = NULL, vcov_sd = NULL, thresholds = NULL,
                n = nrow(long), C_used = nlevels(long$y),
                converged = FALSE, max_grad = NA_real_, max_rel_step = NA_real_,
                boundary = NA, boundary_cluster = NA, hessian_pd = FALSE, seconds = NA_real_)
    if (out$C_used < 2L) {
        out$error <- "agreement_one_category"
        return(out)
    }
    if (cluster && is.null(long$cluster)) {
        out$error <- "agreement_no_cluster_column"
        return(out)
    }
    fml <- if (cluster) y ~ 1 + (1 | case) + (1 | rater) + (1 | cluster)
           else y ~ 1 + (1 | case) + (1 | rater)
    res <- .agreement_try_fit(ordinal::clmm(fml, data = long, link = "probit"))
    out$warnings <- res$warnings
    out$seconds <- res$seconds
    if (is.null(res$value)) {
        out$error <- res$error
        return(out)
    }
    fit <- res$value

    sds <- vapply(fit$ST, function(s) s[1, 1], numeric(1))
    comps <- names(sds)
    out$s2_case <- unname(sds["case"]^2)
    out$s2_rater <- unname(sds["rater"]^2)
    if (cluster) out$s2_cluster <- unname(sds["cluster"]^2)
    st <- paste0("ST", seq_along(sds))  # clmm's names, in names(fit$ST) order
    H <- fit$Hessian
    at_bound <- sds < 1e-4 | (if (is.null(H)) FALSE else !(st %in% colnames(H)))
    names(at_bound) <- comps
    out$boundary <- any(at_bound[c("case", "rater")])
    if (cluster) out$boundary_cluster <- unname(at_bound["cluster"])
    out[c("converged", "max_grad", "max_rel_step", "hessian_pd")] <-
        .agreement_convergence(fit)[c("converged", "max_grad", "max_rel_step", "hessian_pd")]

    V <- tryCatch(suppressWarnings(stats::vcov(fit)), error = function(e) NULL)
    vsd <- matrix(NA_real_, length(sds), length(sds), dimnames = list(comps, comps))
    have <- st %in% colnames(V)
    if (!is.null(V)) {
        vsd[] <- 0  # an SD clmm dropped at the bound is held fixed at ~0
        vsd[have, have] <- V[st[have], st[have]]
    }
    out$vcov_sd <- vsd
    out$se_sd <- sqrt(pmax(diag(vsd), 0))
    out$se_sd[!have] <- NA_real_

    alpha <- fit$alpha
    se_alpha <- if (!is.null(V)) sqrt(pmax(diag(V)[seq_along(alpha)], 0)) else rep(NA_real_, length(alpha))
    out$thresholds <- data.frame(threshold = names(alpha), estimate = unname(alpha),
                                 se = unname(se_alpha), stringsAsFactors = FALSE)
    out$ok <- TRUE
    out
}


#' Population latent ICC rho with a logit-Wald CI
#'
#' rho = (s2u + s2c) / (s2u + s2c + s2v + 1), s2c = 0 without a cluster term, so turning
#' the cluster term on does not change the estimand. logit(rho) = log(s2u + s2c) - log(s2v + 1);
#' gradient w.r.t. (sd_case, sd_rater, sd_cluster) is
#' (2 sd_case / (s2u + s2c), -2 sd_rater / (s2v + 1), 2 sd_cluster / (s2u + s2c)), used
#' with the full SD-scale covariance. Only the case and rater SDs decide "boundary"; a
#' cluster SD at 0 enters as a fixed ~0 (zero row in vcov_sd).
#' @return list(rho, se_rho, lower, upper, ci_ok, reason); reason is NA or one of
#'   fit_failed, boundary, hessian_not_pd, not_converged, se_unavailable.
#' @noRd
agreement_rho_ci <- function(fit, conf = 0.95) {
    res <- list(rho = NA_real_, se_rho = NA_real_, lower = NA_real_, upper = NA_real_,
                ci_ok = FALSE, reason = NA_character_)
    if (!isTRUE(fit$ok)) {
        res$reason <- "fit_failed"
        return(res)
    }
    has_cl <- isTRUE(is.finite(fit$s2_cluster))
    comps <- if (has_cl) c("case", "rater", "cluster") else c("case", "rater")
    s2u <- fit$s2_case
    s2v <- fit$s2_rater
    s2c <- if (has_cl) fit$s2_cluster else 0
    num <- s2u + s2c
    res$rho <- num / (num + s2v + 1)
    V <- fit$vcov_sd[comps, comps]
    g <- c(case = 2 * sqrt(s2u) / num, rater = -2 * sqrt(s2v) / (s2v + 1),
           cluster = 2 * sqrt(s2c) / num)[comps]
    se_logit <- sqrt(as.numeric(t(g) %*% V %*% g))
    reason <- if (isTRUE(fit$boundary)) "boundary"
        else if (!isTRUE(fit$hessian_pd)) "hessian_not_pd"
        else if (!isTRUE(fit$converged)) "not_converged"
        else if (!is.finite(se_logit)) "se_unavailable"
    if (!is.null(reason)) {
        res$reason <- reason
        return(res)
    }
    z <- stats::qnorm(1 - (1 - conf) / 2)
    lg <- stats::qlogis(res$rho)
    res$se_rho <- res$rho * (1 - res$rho) * se_logit
    res$lower <- stats::plogis(lg - z * se_logit)
    res$upper <- stats::plogis(lg + z * se_logit)
    res$ci_ok <- TRUE
    res
}


#' kappa_m with the image of the rho CI under g_C and a delta-method SE
#'
#' @return list(kappa, se, lower, upper, ci_ok, reason).
#' @noRd
agreement_kappa_m_ci <- function(rho_ci, C) {
    k <- agreement_kappa_m(rho_ci$rho, C)
    res <- list(kappa = k, se = NA_real_, lower = NA_real_, upper = NA_real_,
                ci_ok = isTRUE(rho_ci$ci_ok), reason = rho_ci$reason)
    if (!res$ci_ok) return(res)
    res$se <- agreement_kappa_m_deriv(rho_ci$rho, C) * rho_ci$se_rho
    res$lower <- agreement_kappa_m(rho_ci$lower, C)
    res$upper <- agreement_kappa_m(rho_ci$upper, C)
    res
}


#' Per-rater grading tendency (fixed rater effects, random case)
#'
#' clmm P(Y <= c) = Phi(theta_c - eta), so a positive shift = grades HIGHER, in latent
#' residual-SD units, centred on the panel mean (contr.sum; last rater = -sum of the rest).
#' Falls back to nAGQ = 1 when the requested nAGQ fails. converged uses the same rule as
#' agreement_fit_latent() (.agreement_convergence), on both the full and the null fit.
#' @return data.frame(rater, shift, se, lower, upper, p, p_holm) with attributes
#'   ok, error, se_ok, lrt = c(stat, df, p), converged, nAGQ, warnings, seconds.
#'   se_ok = FALSE (vcov failed or gave a non-finite / non-positive variance): the shifts
#'   stand, se/lower/upper/p/p_holm are NA (never NaN).
#' @noRd
agreement_fit_severity <- function(long, nAGQ = 10, conf = 0.95) {
    long$y <- droplevels(long$y)
    long$rater <- droplevels(long$rater)
    raters <- levels(long$rater)
    J <- length(raters)
    tab <- data.frame(rater = raters, shift = NA_real_, se = NA_real_, lower = NA_real_,
                      upper = NA_real_, p = NA_real_, p_holm = NA_real_, stringsAsFactors = FALSE)
    fail <- function(msg, warns = character(0), secs = NA_real_) {
        attr(tab, "ok") <- FALSE
        attr(tab, "error") <- msg
        attr(tab, "se_ok") <- FALSE
        attr(tab, "warnings") <- warns
        attr(tab, "seconds") <- secs
        tab
    }
    if (J < 2L) return(fail("agreement_one_rater"))
    if (nlevels(long$y) < 2L) return(fail("agreement_one_category"))

    fit_pair <- function(q) {
        full <- .agreement_try_fit(ordinal::clmm(y ~ rater + (1 | case), data = long,
            link = "probit", contrasts = list(rater = "contr.sum"), nAGQ = q))
        null <- .agreement_try_fit(ordinal::clmm(y ~ 1 + (1 | case), data = long,
            link = "probit", nAGQ = q))
        list(full = full, null = null, q = q)
    }
    fits <- fit_pair(nAGQ)
    if ((is.null(fits$full$value) || is.null(fits$null$value)) && nAGQ != 1)
        fits <- fit_pair(1)
    warns <- c(fits$full$warnings, fits$null$warnings)
    secs <- fits$full$seconds + fits$null$seconds
    if (is.null(fits$full$value)) return(fail(fits$full$error, warns, secs))
    full <- fits$full$value
    b <- full$coefficients[length(full$alpha) + seq_len(J - 1L)]
    Cm <- rbind(diag(J - 1L), -1)
    tab$shift <- as.vector(Cm %*% b)

    V <- tryCatch(suppressWarnings(stats::vcov(full)), error = function(e) NULL)
    v_shift <- if (!is.null(V) && all(names(b) %in% colnames(V)))
        diag(Cm %*% V[names(b), names(b), drop = FALSE] %*% t(Cm)) else NA_real_
    se_ok <- all(is.finite(v_shift)) && all(v_shift > 0)
    if (se_ok) {
        tab$se <- sqrt(v_shift)
        z <- stats::qnorm(1 - (1 - conf) / 2)
        tab$lower <- tab$shift - z * tab$se
        tab$upper <- tab$shift + z * tab$se
        tab$p <- 2 * stats::pnorm(-abs(tab$shift / tab$se))
        tab$p_holm <- stats::p.adjust(tab$p, method = "holm")
    }

    conv <- function(f) .agreement_convergence(f)$converged
    lrt <- c(stat = NA_real_, df = J - 1L, p = NA_real_)
    if (!is.null(fits$null$value)) {
        lrt[["stat"]] <- max(0, 2 * (full$logLik - fits$null$value$logLik))
        lrt[["p"]] <- stats::pchisq(lrt[["stat"]], df = J - 1L, lower.tail = FALSE)
    }
    attr(tab, "ok") <- TRUE
    attr(tab, "error") <- NULL
    attr(tab, "se_ok") <- se_ok
    attr(tab, "lrt") <- lrt
    attr(tab, "converged") <- conv(full) && !is.null(fits$null$value) && conv(fits$null$value)
    attr(tab, "nAGQ") <- fits$q
    attr(tab, "warnings") <- warns
    attr(tab, "seconds") <- secs
    tab
}


# Case x rater matrix of integer category codes (NA = not read), on USED levels.
.agreement_wide_codes <- function(long) {
    y <- droplevels(long$y)
    case <- droplevels(long$case)
    rater <- droplevels(long$rater)
    Y <- matrix(NA_integer_, nlevels(case), nlevels(rater),
                dimnames = list(levels(case), levels(rater)))
    Y[cbind(as.integer(case), as.integer(rater))] <- as.integer(y)
    list(Y = Y, levels = levels(y))
}


#' Model-free boundary excess per rater
#'
#' For rater j and boundary c: d_i = 1(y_ij >= c) - mean_{k != j reading i} 1(y_ik >= c)
#' over cases j read that at least one other rater read ("= c" when nominal). Boundaries
#' run over the categories actually used (an unused middle level would duplicate the
#' next boundary). No p-values.
#' @return data.frame(rater, boundary, n, excess, se, lower, upper).
#' @noRd
agreement_boundary_table <- function(long, ordinal = TRUE, conf = 0.95) {
    w <- .agreement_wide_codes(long)
    Y <- w$Y
    L <- w$levels
    cuts <- if (ordinal) seq_along(L)[-1] else seq_along(L)
    if (length(L) < 2L) cuts <- integer(0)
    rows <- list()
    for (j in seq_len(ncol(Y))) {
        for (c in cuts) {
            I <- if (ordinal) Y >= c else Y == c
            storage.mode(I) <- "double"
            others <- I[, -j, drop = FALSE]
            n_oth <- rowSums(!is.na(others))
            use <- !is.na(I[, j]) & n_oth > 0
            d <- I[use, j] - rowSums(others[use, , drop = FALSE], na.rm = TRUE) / n_oth[use]
            n <- length(d)
            m <- if (n > 0) mean(d) else NA_real_
            se <- if (n > 1) stats::sd(d) / sqrt(n) else NA_real_
            tq <- if (n > 1) stats::qt(1 - (1 - conf) / 2, n - 1) else NA_real_
            rows[[length(rows) + 1L]] <- data.frame(
                rater = colnames(Y)[j],
                boundary = paste0(if (ordinal) ">= " else "= ", L[c]),
                n = n, excess = m, se = se, lower = m - tq * se, upper = m + tq * se,
                stringsAsFactors = FALSE)
        }
    }
    if (!length(rows))
        return(data.frame(rater = character(0), boundary = character(0), n = integer(0),
                          excess = numeric(0), se = numeric(0), lower = numeric(0),
                          upper = numeric(0)))
    do.call(rbind, rows)
}


#' Classical per-rater agreement: exact agreement and mean pairwise Cohen kappa
#'
#' pct_exact pools every paired reading of rater j (every other rater, every shared case).
#' mean_kappa averages unweighted irr::kappa2 over other raters with >= 2 shared cases and
#' a finite kappa; n_pairs_used counts those raters.
#' @return data.frame(rater, cases_read, pct_exact, mean_kappa, n_pairs_used).
#' @noRd
agreement_pathologist_classical <- function(long) {
    w <- .agreement_wide_codes(long)
    Y <- w$Y
    L <- w$levels
    J <- ncol(Y)
    agree <- n_shared <- matrix(0, J, J)
    kap <- matrix(NA_real_, J, J)
    for (j in seq_len(J - 1L)) {
        for (k in (j + 1L):J) {
            s <- !is.na(Y[, j]) & !is.na(Y[, k])
            n_shared[j, k] <- n_shared[k, j] <- sum(s)
            agree[j, k] <- agree[k, j] <- sum(Y[s, j] == Y[s, k])
            if (sum(s) >= 2L) {
                kv <- irr::kappa2(cbind(L[Y[s, j]], L[Y[s, k]]))$value
                if (is.finite(kv)) kap[j, k] <- kap[k, j] <- kv
            }
        }
    }
    tot <- rowSums(n_shared)
    data.frame(
        rater = colnames(Y),
        cases_read = colSums(!is.na(Y)),
        pct_exact = ifelse(tot > 0, 100 * rowSums(agree) / tot, NA_real_),
        mean_kappa = apply(kap, 1, function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)),
        n_pairs_used = rowSums(!is.na(kap)),
        row.names = NULL, stringsAsFactors = FALSE
    )
}


#' What the model-based section may show (the single decision point)
#'
#' Pure: reads design facts, AGREEMENT_LATENT_GATES and the fit summaries, nothing else.
#' A number that needs a fit is shown only when that fit is supplied and passes: with
#' fit / rho_ci / severity left NULL the matching show_* flags are FALSE and, when the
#' design itself passes, their reason is NA (not yet fitted). So a design-only call
#' (reason_model NA) is how the caller decides whether to fit at all.
#' The CI flag covers both the rho and the kappa_m interval and needs the rho point.
#'
#' @param design agreement_design_summary() output (n_cases, n_raters, per_case, connected,
#'   complete; a missing `complete` counts as complete).
#' @param scale "ordinal", "nominal" or "binary".
#' @param max_share largest category share in the data, 0-1.
#' @param fit agreement_fit_latent() output, or NULL.
#' @param rho_ci agreement_rho_ci() output, or NULL.
#' @param severity agreement_fit_severity() output, or NULL.
#' @return list(show_kappa_m, show_rho, show_ci, show_severity, show_severity_ci,
#'   show_severity_labels, reason_model, reason_rho, reason_ci, reason_severity). Reasons
#'   are untranslated codes or NA: nominal_scale, too_few_raters, too_few_ratings_per_case,
#'   too_few_cases, too_few_cases_incomplete, disconnected, binary_needs_more_raters, skewed,
#'   fit_failed, boundary, hessian_not_pd, se_unavailable, not_converged, too_large
#'   (severity only: over AGREEMENT_SEVERITY_MAX_WORK). Case minimums count only cases
#'   with 2+ ratings (n_cases - n_cases_lt2). reason_severity explains the first severity item hidden.
#' @noRd
agreement_latent_gate <- function(design, scale = c("ordinal", "nominal", "binary"), max_share,
                                  fit = NULL, rho_ci = NULL, severity = NULL) {
    scale <- match.arg(scale)
    stopifnot(is.numeric(max_share), length(max_share) == 1L, !is.na(max_share),
              max_share >= 0, max_share <= 1)
    G <- AGREEMENT_LATENT_GATES
    # Cases read by one pathologist carry no agreement information and were never in the
    # grid (every simulated case had >= 3 ratings), so they do not count toward a minimum.
    nc <- design$n_cases - sum(design$n_cases_lt2)
    nr <- design$n_raters
    med <- design$per_case[["median"]]
    code <- function(x) if (is.null(x)) NA_character_ else x
    out <- list(show_kappa_m = FALSE, show_rho = FALSE, show_ci = FALSE, show_severity = FALSE,
                show_severity_ci = FALSE, show_severity_labels = FALSE,
                reason_model = NA_character_, reason_rho = NA_character_,
                reason_ci = NA_character_, reason_severity = NA_character_)
    whole <- if (scale == "nominal") "nominal_scale" else if (!isTRUE(design$connected)) "disconnected"
    if (!is.null(whole)) {
        out[c("reason_model", "reason_rho", "reason_ci", "reason_severity")] <- whole
        return(out)
    }

    # Latent model: kappa_m, then rho, then the CI; each inherits the earlier refusal.
    model <- if (scale == "binary") {
        if (med < G$binary_min_per_case) "binary_needs_more_raters"
        else if (nc < G$binary_min_cases) "too_few_cases"
    } else {
        if (nr < G$kappa_m_min_raters) "too_few_raters"
        else if (med < G$kappa_m_min_per_case) "too_few_ratings_per_case"
        else if (nc < G$kappa_m_min_cases) "too_few_cases"
        else if (max_share >= G$skew_share && nc < G$kappa_m_min_cases_skewed) "skewed"
    }
    if (is.null(model) && !is.null(fit))
        model <- if (!isTRUE(fit$ok)) "fit_failed" else if (!isTRUE(fit$converged)) "not_converged"
    rho <- if (!is.null(model)) model
        else if (scale == "ordinal" && nc < G$rho_min_cases) "too_few_cases"
        else if (scale == "ordinal" && isFALSE(design$complete) && nc < G$rho_min_cases_incomplete) "too_few_cases_incomplete"
        else if (scale == "ordinal" && max_share >= G$skew_share) "skewed"
    ci <- if (!is.null(rho)) rho
        else if (nr < G$rho_ci_min_raters) "too_few_raters"
        else if (nc < G$rho_ci_min_cases) "too_few_cases"
        else if (!is.null(rho_ci) && !isTRUE(rho_ci$ci_ok))
            switch(code(rho_ci$reason), boundary = "boundary", not_converged = "not_converged",
                   fit_failed = "fit_failed", se_unavailable = "se_unavailable", "hessian_not_pd")
    out$reason_model <- code(model)
    out$reason_rho <- code(rho)
    out$reason_ci <- code(ci)
    out$show_kappa_m <- is.null(model) && !is.null(fit)
    out$show_rho <- is.null(rho) && !is.null(fit)
    out$show_ci <- is.null(ci) && !is.null(fit) && !is.null(rho_ci)

    # Severity (fixed rater effects): shifts, then CIs and Holm labels from the same limits.
    sev <- if (nr < agreement_severity_min_raters(scale, max_share)) "too_few_raters"
        else if (nc < G$severity_min_cases) "too_few_cases"
        else if (isTRUE(design$n_ratings * nr > AGREEMENT_SEVERITY_MAX_WORK)) "too_large"
        else if (!is.null(severity) && !isTRUE(attr(severity, "ok"))) "fit_failed"
        else if (!is.null(severity) && !isTRUE(attr(severity, "converged"))) "not_converged"
    sev_ci <- if (!is.null(sev)) sev
        else if (!is.null(severity) && !isTRUE(attr(severity, "se_ok"))) "hessian_not_pd"
    out$reason_severity <- code(sev_ci)
    out$show_severity <- is.null(sev) && !is.null(severity)
    out$show_severity_ci <- out$show_severity_labels <- is.null(sev_ci) && !is.null(severity)
    out
}
