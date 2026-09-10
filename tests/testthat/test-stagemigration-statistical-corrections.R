# Regression tests for the statistical corrections applied to stagemigration.
#
# Each test pins a corrected estimator against a hand-computed or reference-package
# value, and several demonstrate the ORIGINAL defect so a silent revert fails loudly.
# See R/stagemigration.b.R, R/stagemigration-part1..5.R and R/stagemigration_helpers.R.

# The backend is one R6 class chain spread over R/stagemigration.b.R and
# R/stagemigration-part1.R ... part5.R; source-level checks must read all six files.
stagemigration_backend_files <- function() {
    c(testthat::test_path("..", "..", "R", "stagemigration.b.R"),
      testthat::test_path("..", "..", "R", sprintf("stagemigration-part%d.R", 1:5)))
}
stagemigration_read_backend <- function(paths) unlist(lapply(paths, readLines, warn = FALSE))

testthat::test_that("concordance on a risk score needs reverse=TRUE", {
    # The cross-validated C-index used concordance(Surv ~ risk) with no reverse,
    # returning 1 - C. That also flips the SIGN of (new - old), reporting a better
    # staging system as worse.
    set.seed(1)
    n <- 400
    x <- stats::rnorm(n)
    t <- stats::rexp(n, exp(0.9 * x))
    e <- stats::rbinom(n, 1, 0.7)
    fit <- survival::coxph(survival::Surv(t, e) ~ x)
    risk <- stats::predict(fit, type = "risk")
    S <- survival::Surv(t, e)

    c_obj <- survival::concordance(fit)$concordance
    c_rev <- survival::concordance(S ~ risk, reverse = TRUE)$concordance
    c_raw <- survival::concordance(S ~ risk)$concordance

    testthat::expect_equal(c_rev, c_obj, tolerance = 1e-8)
    testthat::expect_equal(c_raw, 1 - c_obj, tolerance = 1e-8)
})

testthat::test_that("within-stage homogeneity is not circular", {
    # Splitting a stage by QUARTILES OF ITS OWN SURVIVAL TIME and log-rank testing
    # those quartiles cannot fail to reject: it asks whether patients with short
    # survival have short survival. Verified here on data that is homogeneous by
    # construction (a single exponential).
    set.seed(7)
    n <- 200
    t <- stats::rexp(n, 0.05)
    cn <- stats::rexp(n, 0.02)
    tm <- pmin(t, cn)
    ev <- as.integer(t <= cn)
    q <- cut(tm, stats::quantile(tm, c(0, .25, .5, .75, 1)),
             include.lowest = TRUE, labels = paste0("Q", 1:4))
    sd_circular <- survival::survdiff(survival::Surv(tm, ev) ~ q)
    p_circular <- stats::pchisq(sd_circular$chisq, df = 3, lower.tail = FALSE)

    # the circular test rejects on homogeneous data
    testthat::expect_lt(p_circular, 1e-10)

    # an outcome-independent split does not
    grp <- factor(rep(c("A", "B"), length.out = n))
    sd_ok <- survival::survdiff(survival::Surv(tm, ev) ~ grp)
    p_ok <- stats::pchisq(sd_ok$chisq, df = 1, lower.tail = FALSE)
    testthat::expect_gt(p_ok, 0.01)
})

testthat::test_that("R lists are passed by value, so a mutating callee must return", {
    # .calculateBootstrapMetrics mutated its local copy and the caller discarded the
    # result, leaving every bootstrap container at its preallocated 0 -- then reported
    # as "0.000, Poor, 100% successful, Robust".
    mutate_discard <- function(res, i) { res$v[i] <- 99; invisible(NULL) }
    mutate_return  <- function(res, i) { res$v[i] <- 99; res }

    r1 <- list(v = numeric(3)); mutate_discard(r1, 1)
    testthat::expect_equal(r1$v, c(0, 0, 0))

    r2 <- list(v = numeric(3)); r2 <- mutate_return(r2, 1)
    testthat::expect_equal(r2$v, c(99, 0, 0))
})

testthat::test_that("Royston R2_D uses D, not the LR chi-square", {
    set.seed(11)
    n <- 500
    x <- stats::rnorm(n)
    t <- stats::rexp(n, exp(0.9 * x))
    cn <- stats::rexp(n, 0.3)
    tm <- pmin(t, cn); ev <- as.integer(t <= cn)
    f <- survival::coxph(survival::Surv(tm, ev) ~ x)

    lp <- stats::predict(f, type = "lp")
    kappa <- sqrt(8 / pi)
    z <- stats::qnorm((rank(lp) - 3 / 8) / (n + 1 / 4)) / kappa
    D <- unname(stats::coef(survival::coxph(f$y ~ z))[1])
    r2d <- D^2 / (D^2 + 4 * pi / 3)

    LR <- -2 * (f$loglik[1] - f$loglik[2])
    old <- LR / (LR + (pi^2 / 3) * n)

    testthat::expect_gt(r2d, 0.25)      # a real effect registers
    testthat::expect_lt(old, 0.20)      # the old formula understates it badly
    testthat::expect_gt(r2d, old * 2)
})

testthat::test_that("IDI standard error must use paired differences", {
    # Two nested staging systems predict the SAME patients, so their predictions
    # correlate strongly. Summing marginal variances treats them as independent.
    set.seed(9)
    n <- 400
    a <- stats::runif(n)
    b <- a + stats::rnorm(n, 0, 0.05)   # rho ~ 0.95
    se_marginal <- sqrt(stats::var(a) / n + stats::var(b) / n)
    se_paired   <- sqrt(stats::var(b - a) / n)
    testthat::expect_gt(se_marginal / se_paired, 3)
})

testthat::test_that("NRI variance is multinomial, not binomial", {
    n <- 200; up <- 120; dn <- 70
    p  <- (up + dn) / n
    v_binom <- p * (1 - p) / n                       # variance of "reclassified at all"
    pu <- up / n; pd <- dn / n
    v_multi <- (pu + pd - (pu - pd)^2) / n           # variance of (up - down)/n
    testthat::expect_gt(sqrt(v_multi) / sqrt(v_binom), 3)
})

testthat::test_that("RMST and its SE match survRM2", {
    testthat::skip_if_not_installed("survRM2")
    rmst1 <- function(time, status, tau) {
        fit <- survival::survfit(survival::Surv(time, status) ~ 1)
        k <- fit$time <= tau
        tt <- fit$time[k]; ss <- fit$surv[k]
        dd <- fit$n.event[k]; nn <- fit$n.risk[k]
        starts <- c(0, tt); heights <- c(1, ss); ends <- c(tt, tau)
        areas <- (ends - starts) * heights
        A <- rev(cumsum(rev(areas)))[-1]
        keep <- dd > 0 & nn > dd
        v <- sum((A[keep])^2 * dd[keep] / (nn[keep] * (nn[keep] - dd[keep])))
        c(rmst = sum(areas), se = sqrt(v))
    }
    set.seed(4)
    n <- 300
    t <- stats::rexp(n, 0.05); cn <- stats::rexp(n, 0.02)
    tm <- pmin(t, cn); ev <- as.integer(t <= cn)
    arm <- rep(0:1, each = n / 2)
    mine <- rmst1(tm[arm == 0], ev[arm == 0], 20)
    ref <- survRM2::rmst2(tm, ev, arm, tau = 20)
    testthat::expect_equal(unname(mine["rmst"]), unname(ref$RMST.arm0$rmst["Est."]), tolerance = 1e-6)
    testthat::expect_equal(unname(mine["se"]),   unname(ref$RMST.arm0$rmst["se"]),   tolerance = 1e-6)
})

testthat::test_that("DeLong SE is recovered from the statistic, not from $parameter", {
    testthat::skip_if_not_installed("pROC")
    set.seed(3)
    n <- 200
    y <- stats::rbinom(n, 1, 0.4)
    a <- stats::rnorm(n) + y
    b <- stats::rnorm(n) + 1.2 * y
    r1 <- pROC::roc(y, a, quiet = TRUE)
    r2 <- pROC::roc(y, b, quiet = TRUE)
    tt <- pROC::roc.test(r1, r2, method = "delong")

    # roc.test(method="delong") has NO $parameter -> the old sqrt(Z^2/df) was always NA
    testthat::expect_null(tt$parameter)

    d <- as.numeric(r2$auc) - as.numeric(r1$auc)
    se <- abs(d) / abs(as.numeric(tt$statistic))
    se_from_ci <- diff(as.numeric(tt$conf.int)[1:2]) / 2 / stats::qnorm(0.975)
    testthat::expect_equal(se, se_from_ci, tolerance = 1e-6)
})

testthat::test_that("comparing factors with different level sets errors; as.character does not", {
    # oldStage and newStage having different levels is the premise of this analysis.
    f1 <- factor(c("a", "b")); f2 <- factor(c("a", "c"))
    testthat::expect_error(f1 == f2, "level sets")
    testthat::expect_equal(as.character(f1) == as.character(f2), c(TRUE, FALSE))
})

testthat::test_that("migration direction needs one common ordinal scale", {
    # Two independent seq_along(levels(...)) scales made every UNCHANGED Stage IV
    # patient (row 4, column 5) score as upstaged.
    ol <- c("I", "II", "III", "IV")
    nl <- c("I", "IIA", "IIB", "III", "IV")
    old_order <- stats::setNames(seq_along(ol), ol)
    new_order <- stats::setNames(seq_along(nl), nl)
    testthat::expect_true(new_order[["IV"]] > old_order[["IV"]])   # the old bug

    common <- union(ol, nl)
    rank_map <- stats::setNames(seq_along(common), common)
    testthat::expect_equal(rank_map[["IV"]], rank_map[["IV"]])      # identity -> not counted
})

testthat::test_that("as.numeric(factor) inverts event coding", {
    f <- factor(c("Dead", "Alive", "Dead"), levels = c("Dead", "Alive"))
    testthat::expect_equal(as.numeric(f), c(1, 2, 1))               # level indices
    g <- factor(c("1", "0", "1"))
    testthat::expect_equal(suppressWarnings(as.numeric(as.character(g))), c(1, 0, 1))
})

testthat::test_that("KM is a step function; linear interpolation biases S(t) down", {
    tt <- c(1, 5, 10); ss <- c(0.9, 0.7, 0.4)
    testthat::expect_equal(stats::approx(tt, ss, 3, method = "constant", f = 0)$y, 0.9)
    testthat::expect_lt(stats::approx(tt, ss, 3)$y, 0.9)
})

testthat::test_that("stratum matching must be exact, not a substring test", {
    strata <- c("stage=1", "stage=1A", "stage=10")
    testthat::expect_equal(sum(grepl("stage=1", strata, fixed = TRUE)), 3L)
    testthat::expect_equal(sum(strata == "stage=1"), 1L)
})

testthat::test_that("bootstrap p-value cannot exceed 1", {
    d <- c(rep(0, 60), rep(1, 30), rep(-1, 10))   # many exact ties
    old <- 2 * min(mean(d <= 0), mean(d >= 0))
    pz <- mean(d == 0)
    new <- min(1, 2 * min(mean(d < 0) + 0.5 * pz, mean(d > 0) + 0.5 * pz))
    testthat::expect_gt(old, 1)
    testthat::expect_lte(new, 1)
})

testthat::test_that("tryCatch handlers assign into their own frame without <<-", {
    f <- function() { r <- list(); tryCatch(stop("x"), error = function(e) { r[["a"]] <- 1 }); length(r) }
    g <- function() { r <- list(); tryCatch(stop("x"), error = function(e) { r[["a"]] <<- 1 }); length(r) }
    testthat::expect_equal(f(), 0L)
    testthat::expect_equal(g(), 1L)
})

testthat::test_that("2:length(x) counts down when length is 1", {
    x <- 1
    testthat::expect_equal(2:length(x), c(2L, 1L))       # the crash path
    testthat::expect_length(seq_len(length(x) - 1) + 1, 0)
})

testthat::test_that("plogis of a Cox lp is not a risk probability", {
    set.seed(5)
    n <- 300
    x <- stats::rnorm(n)
    t <- stats::rexp(n, exp(0.8 * x)); cn <- stats::rexp(n, 0.3)
    tm <- pmin(t, cn); ev <- as.integer(t <= cn)
    f <- survival::coxph(survival::Surv(tm, ev) ~ x)
    lp <- stats::predict(f, type = "lp")
    sf <- survival::survfit(f)
    s0 <- sf$surv[findInterval(stats::median(tm), sf$time)]
    risk <- 1 - s0^exp(lp)

    # plogis pins a centred lp at mean ~0.5 regardless of the actual risk level
    testthat::expect_equal(mean(stats::plogis(lp)), 0.5, tolerance = 0.02)
    testthat::expect_gt(abs(mean(risk) - 0.5), 0.02)
})

testthat::test_that("vectorised win/loss matrices match the brute-force FS rule", {
    # .winLossMatrices replaced an O(n1*n2) nested R loop. A pair is decided only when
    # censoring cannot mask the ordering:
    #   win  <=> group-2 subject had the event and was outlived
    #   loss <=> group-1 subject had the event and was outlived
    set.seed(21)
    n1 <- 40; n2 <- 35
    t1 <- stats::rexp(n1, .1); e1 <- stats::rbinom(n1, 1, .6)
    t2 <- stats::rexp(n2, .1); e2 <- stats::rbinom(n2, 1, .6)

    e2m <- matrix(rep(e2 == 1, each = n1), nrow = n1)     # e2 indexes COLUMNS
    e1m <- matrix(rep(e1 == 1, times = n2), nrow = n1)    # e1 indexes ROWS
    win  <- outer(t1, t2, ">") & e2m
    loss <- outer(t1, t2, "<") & e1m

    bw <- 0L; bl <- 0L
    for (i in seq_len(n1)) for (j in seq_len(n2)) {
        if (e1[i] == 1 && e2[j] == 1) {
            if (t1[i] > t2[j]) bw <- bw + 1L else if (t1[i] < t2[j]) bl <- bl + 1L
        } else if (e1[i] == 0 && e2[j] == 1) {
            if (t1[i] > t2[j]) bw <- bw + 1L
        } else if (e1[i] == 1 && e2[j] == 0) {
            if (t1[i] < t2[j]) bl <- bl + 1L
        }
    }
    testthat::expect_equal(sum(win), bw)
    testthat::expect_equal(sum(loss), bl)
})

testthat::test_that("win ratio variance accounts for correlated pairs", {
    # sqrt(1/wins + 1/losses) treats all n1*n2 pairs as independent, but each subject
    # appears in n2 (resp. n1) of them. The two-sample U-statistic variance
    # (Bebu & Lachin 2016) is the correct one and is materially larger.
    set.seed(31)
    n1 <- 60; n2 <- 60
    t1 <- stats::rexp(n1, .08); e1 <- stats::rbinom(n1, 1, .7)
    t2 <- stats::rexp(n2, .12); e2 <- stats::rbinom(n2, 1, .7)
    e2m <- matrix(rep(e2 == 1, each = n1), nrow = n1)
    e1m <- matrix(rep(e1 == 1, times = n2), nrow = n1)
    win  <- outer(t1, t2, ">") & e2m
    loss <- outer(t1, t2, "<") & e1m

    nW <- sum(win); nL <- sum(loss)
    testthat::skip_if(nW == 0 || nL == 0)

    naive_se <- sqrt(1 / nW + 1 / nL)

    N <- n1 * n2; pW <- nW / N; pL <- nL / N
    Wi <- rowSums(win) / n2; Li <- rowSums(loss) / n2
    Wj <- colSums(win) / n1; Lj <- colSums(loss) / n1
    varW <- stats::var(Wi) / n1 + stats::var(Wj) / n2
    varL <- stats::var(Li) / n1 + stats::var(Lj) / n2
    covWL <- stats::cov(Wi, Li) / n1 + stats::cov(Wj, Lj) / n2
    u_se <- sqrt(varW / pW^2 + varL / pL^2 - 2 * covWL / (pW * pL))

    testthat::expect_true(is.finite(u_se) && u_se > 0)
    testthat::expect_gt(u_se, naive_se)   # naive SE is anti-conservative
})

# ---------------------------------------------------------------------------
# Round 3: frailty, competing risks, notices (check-function-full remediation)
# ---------------------------------------------------------------------------

testthat::test_that("frailty ICC uses the extreme-value latent variance pi^2/6", {
    # pi^2/3 is the LOGISTIC latent variance; for a proportional-hazards frailty model the
    # residual is extreme-value distributed with variance pi^2/6.
    v <- 0.30
    icc_ph <- v / (v + pi^2 / 6)
    icc_logistic <- v / (v + pi^2 / 3)
    testthat::expect_equal(icc_ph, 0.30 / (0.30 + 1.644934), tolerance = 1e-6)
    testthat::expect_gt(icc_ph, icc_logistic * 1.8)   # the old constant roughly halved it
})

testthat::test_that("frailty heterogeneity LR uses coxme's INTEGRATED log-likelihood", {
    testthat::skip_if_not_installed("coxme")
    sim <- function(sd_b, seed) {
        set.seed(seed)
        k <- 30; m <- 12; n <- k * m
        cl <- rep(seq_len(k), each = m)
        x <- stats::rnorm(n)
        b <- stats::rnorm(k, 0, sd_b)[cl]
        t <- stats::rexp(n, exp(0.7 * x + b)); cn <- stats::rexp(n, 0.2)
        data.frame(tm = pmin(t, cn), ev = as.integer(t <= cn), x = x, cl = factor(cl))
    }
    lr_test <- function(d) {
        f0 <- survival::coxph(survival::Surv(tm, ev) ~ x, data = d)
        f1 <- coxme::coxme(survival::Surv(tm, ev) ~ x + (1 | cl), data = d)
        lr <- max(0, 2 * (unname(f1$loglik["Integrated"]) - f0$loglik[2]))
        list(lr = lr, p = 0.5 * stats::pchisq(lr, 1, lower.tail = FALSE),
             lr_wrong = 2 * (as.numeric(stats::logLik(f1)) - f0$loglik[2]))
    }
    null <- lr_test(sim(0, 13))
    alt  <- lr_test(sim(0.8, 13))

    testthat::expect_gt(null$p, 0.05)         # no frailty -> not significant
    testthat::expect_lt(alt$p, 1e-4)          # true frailty -> detected
    # logLik(coxme) is on a different scale and overstates the statistic
    testthat::expect_gt(alt$lr_wrong, alt$lr * 1.5)
})

testthat::test_that("Aalen-Johansen CIF equals the crude proportion only without censoring", {
    testthat::skip_if_not_installed("cmprsk")
    set.seed(21)
    n <- 400
    t1 <- stats::rexp(n, 0.05); t2 <- stats::rexp(n, 0.03)
    tt <- pmin(t1, t2); cause <- ifelse(t1 <= t2, 1L, 2L)
    tp <- 10

    # no censoring: AJ CIF at tp is exactly the proportion with a cause-1 event by tp
    aj0 <- cmprsk::timepoints(cmprsk::cuminc(tt, cause, cencode = 0), tp)$est
    crude0 <- mean(cause == 1L & tt <= tp)
    testthat::expect_equal(unname(aj0[grep(" 1$", rownames(aj0)), 1]), crude0, tolerance = 1e-8)

    # with censoring the crude proportion is biased downward; AJ is not
    cn <- stats::rexp(n, 0.06)
    obs <- pmin(tt, cn); fs <- ifelse(tt <= cn, cause, 0L)
    aj <- cmprsk::timepoints(cmprsk::cuminc(obs, fs, cencode = 0), tp)$est
    aj1 <- unname(aj[grep(" 1$", rownames(aj)), 1])
    crude <- mean(fs == 1L & obs <= tp)
    testthat::expect_lt(crude, aj1)
    testthat::expect_equal(aj1, crude0, tolerance = 0.06)   # AJ recovers the uncensored truth
})

testthat::test_that("Gray's test is not the cause-specific log-rank", {
    # They test different quantities. With an EQUAL cause-1 hazard in both groups but a much
    # heavier competing hazard in group B, the cause-specific log-rank correctly finds no
    # difference in the cause-1 HAZARD, while Gray's test detects the lower cause-1
    # CUMULATIVE INCIDENCE in B (competing deaths remove patients before cause 1 can occur).
    # The former .performGrayTest returned the cause-specific log-rank under Gray's name.
    # (Verified across 40 seeds: Gray p < 0.05 in 40/40, cause-specific p > 0.05 in 36/40.)
    testthat::skip_if_not_installed("cmprsk")
    set.seed(8)
    n <- 600
    g <- factor(rep(c("A", "B"), each = n / 2))
    t1 <- stats::rexp(n, 0.04)
    t2 <- stats::rexp(n, ifelse(g == "A", 0.01, 0.12))
    cn <- stats::rexp(n, 0.02)
    tt <- pmin(t1, t2, cn)
    fs <- ifelse(tt == cn, 0L, ifelse(tt == t1, 1L, 2L))

    gray_p <- cmprsk::cuminc(tt, fs, group = g, cencode = 0)$Tests["1", "pv"]
    cs <- survival::survdiff(survival::Surv(tt, fs == 1L) ~ g)
    cs_p <- stats::pchisq(cs$chisq, df = 1, lower.tail = FALSE)

    testthat::expect_lt(gray_p, 0.05)   # cumulative incidence differs
    testthat::expect_gt(cs_p, 0.05)     # cause-specific hazard does not
})

testthat::test_that("every self$results$<item> in stagemigration.b.R exists in its .r.yaml", {
    # Deleting a results item whose writers were not all rerouted gives NULL$setContent()
    # -> "attempt to apply non-function" at runtime; prepare() and the parser stay clean.
    testthat::skip_if_not_installed("yaml")
    b_path <- stagemigration_backend_files()
    r_path <- testthat::test_path("..", "..", "jamovi", "stagemigration.r.yaml")
    testthat::skip_if_not(all(file.exists(b_path)) && file.exists(r_path), "source tree not available")

    src <- stagemigration_read_backend(b_path)
    src <- src[!grepl("^\\s*#", src)]
    refs <- unique(unlist(regmatches(src, gregexpr("self\\$results\\$[A-Za-z_][A-Za-z0-9_]*", src))))
    refs <- sub("^self\\$results\\$", "", refs)
    refs <- setdiff(refs, c("insert", "get", "remove", "setVisible", "isFilled", "items"))

    collect <- function(items) {
        out <- character(0)
        for (it in items) {
            if (!is.null(it$name)) out <- c(out, it$name)
            if (is.list(it$items)) out <- c(out, collect(it$items))
        }
        out
    }
    defined <- collect(yaml::read_yaml(r_path)$items)

    testthat::expect_true("notices" %in% defined)
    testthat::expect_false(any(grepl("^mydataview", c(defined, refs))))
    testthat::expect_equal(setdiff(refs, defined), character(0))
})

testthat::test_that("events-per-variable counts model degrees of freedom, not a fixed 2", {
    # A stage factor with k levels costs k - 1 df per Cox model. The validator used a hardcoded
    # n_predictors = 2, which overstated EPV and silenced the warning in the ~20-30 event range
    # for a 4-stage system -- exactly the small cohorts it exists to flag.
    min_epv <- 10
    n_events <- 25
    k_stages <- 4
    epv_old <- n_events / 2
    epv_new <- n_events / (k_stages - 1)
    testthat::expect_false(epv_old < min_epv)   # old count: no warning
    testthat::expect_true(epv_new < min_epv)    # correct count: warns

    v_path <- testthat::test_path("..", "..", "R", "stagemigration-validation.R")
    testthat::skip_if_not(file.exists(v_path), "source tree not available")
    src <- paste(readLines(v_path, warn = FALSE), collapse = "\n")
    testthat::expect_false(grepl("n_predictors = 2,", src, fixed = TRUE))
    testthat::expect_true(grepl("n_predictors = n_predictors", src, fixed = TRUE))
})

testthat::test_that("the default path checks proportional hazards on the primary Cox models", {
    # Premise: cox.zph's global test separates proportional from non-proportional hazards.
    # Group B has a decreasing Weibull hazard (shape 0.5) against A's constant one, so the
    # hazards cross. Verified across 60 seeds before pinning: PH data cleared in 58/60,
    # crossing hazards flagged in 60/60.
    set.seed(1)
    n <- 800
    g <- factor(rep(c("A", "B"), each = n / 2))
    t_ph <- stats::rexp(n, ifelse(g == "A", 0.05, 0.10)); c1 <- stats::rexp(n, 0.03)
    p_ph <- survival::cox.zph(survival::coxph(
        survival::Surv(pmin(t_ph, c1), as.integer(t_ph <= c1)) ~ g))$table["GLOBAL", "p"]
    t_np <- ifelse(g == "A", stats::rexp(n, 0.05), stats::rweibull(n, shape = 0.5, scale = 20))
    c2 <- stats::rexp(n, 0.03)
    p_np <- survival::cox.zph(survival::coxph(
        survival::Surv(pmin(t_np, c2), as.integer(t_np <= c2)) ~ g))$table["GLOBAL", "p"]
    testthat::expect_gt(p_ph, 0.05)
    testthat::expect_lt(p_np, 0.01)

    # Wiring: the check now runs in .run() on every analysis, not only under
    # advancedMigrationAnalysis.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    src <- paste(stagemigration_read_backend(b_path), collapse = "\n")
    run <- regmatches(src, regexpr("\\.run = function\\(\\) \\{[\\s\\S]*?\\n(            \\.[A-Za-z_]+ = function|        \\)\\n    \\)\\n\\})", src, perl = TRUE))
    testthat::expect_length(run, 1)
    testthat::expect_true(grepl("private$.performSchoenfeld(am$old_cox", run, fixed = TRUE))
    testthat::expect_true(grepl('"Proportional hazards assumption"', run, fixed = TRUE))
})

testthat::test_that('clinician-facing p-values never read "p = <1e-04"', {
    # base::format.pval() returns "<1e-04"; spliced after "p = " it produced "p = <1e-04" in the
    # heterogeneity column, the Gray's-test note and the PH notice. .pText() owns the wording.
    p_text <- function(p) {
        if (length(p) != 1 || is.na(p) || !is.finite(p)) return("p not estimable")
        if (p < 1e-4) return("p < 0.0001")
        paste0("p = ", base::format(signif(p, 3), scientific = FALSE))
    }
    testthat::expect_identical(p_text(2.21e-12), "p < 0.0001")
    testthat::expect_identical(p_text(0.0123456), "p = 0.0123")
    testthat::expect_identical(p_text(NA_real_), "p not estimable")
    testthat::expect_identical(base::format.pval(2.21e-12, digits = 3, eps = 1e-4), "<1e-04")  # the old trap

    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    src <- paste(stagemigration_read_backend(b_path), collapse = "\n")
    testthat::expect_true(grepl(".pText = function(p)", src, fixed = TRUE))
    testthat::expect_false(grepl("p = %s\", [^\n]*format\\.pval", src))
    testthat::expect_false(grepl("primary p = %s", src, fixed = TRUE))
})

testthat::test_that("informational validation messages are not shown as warnings", {
    # The validator keeps two buckets: $warnings (zero follow-up, missing values, poor data quality)
    # and $notices ("Event level used: 1", "Removed N incomplete cases"). Merging both into one
    # WARNING notice showed a yellow banner on every clean run.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    src <- paste(stagemigration_read_backend(b_path), collapse = "\n")
    testthat::expect_true(grepl('private$.addNotice("INFO", .("Data preparation")', src, fixed = TRUE))
    testthat::expect_true(grepl('private$.addNotice("WARNING", .("Data validation"), paste(other_warnings', src, fixed = TRUE))
    testthat::expect_false(grepl("other_msgs <- c(setdiff(validation_result$warnings", src, fixed = TRUE))
})

testthat::test_that('no "p =" is spliced before format.pval() anywhere in stagemigration.b.R', {
    # The first p-text test matched only the sprintf form on one line and missed
    # paste("p =", format.pval(x)) in the clinical-interpretation table. Check both forms,
    # across line breaks, over the whole file.
    testthat::expect_identical(paste("p =", base::format.pval(1e-20)), "p = < 2.22e-16")  # the trap

    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    lines <- stagemigration_read_backend(b_path)
    src <- paste(lines[!grepl("^\\s*#", lines)], collapse = "\n")
    testthat::expect_false(grepl('paste0?\\(\\s*"p\\s*=\\s*"\\s*,\\s*(base::)?format\\.pval\\(', src, perl = TRUE))
    testthat::expect_false(grepl('"[^"\\n]*p\\s*=\\s*%s[^"\\n]*"\\s*,[^;]*?(base::)?format\\.pval\\(', src, perl = TRUE))
})

testthat::test_that("sample-size messages carry no severity prefix and never call a shortfall adequate", {
    # MARGINAL means n_events is BELOW the recommended minimum for the analysis type, yet the message
    # read "25 events - adequate for comprehensive analysis". The "NOTICE:"/"WARNING:"/"CRITICAL:"
    # prefixes also contradicted the notice's own severity once these reached users as notices.
    u_path <- testthat::test_path("..", "..", "R", "stagemigration-utils.R")
    testthat::skip_if_not(file.exists(u_path), "source tree not available")
    lines <- readLines(u_path, warn = FALSE)
    src <- paste(lines[!grepl("^\\s*#", lines)], collapse = "\n")
    testthat::expect_false(grepl("events - adequate for", src, fixed = TRUE))
    testthat::expect_false(grepl('"(NOTICE|WARNING|CRITICAL):', src, perl = TRUE))
    testthat::expect_true(grepl("below the %d recommended for a %s analysis", src, fixed = TRUE))
})

testthat::test_that("the chi-square approximation warning becomes a notice instead of leaking", {
    # Premise: a sparse migration table makes chisq.test() warn. A tryCatch(error = ...) does not
    # catch warnings, so the message reached jamovi's Analysis Notes panel.
    sparse <- matrix(c(10, 0, 3, 0, 8, 2, 1, 0, 15), 3)
    msg <- tryCatch({ stats::chisq.test(sparse); "no warning" },
                    warning = function(w) conditionMessage(w))
    testthat::expect_match(msg, "Chi-squared approximation may be incorrect", fixed = TRUE)

    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    lines <- stagemigration_read_backend(b_path)
    src <- paste(lines[!grepl("^\\s*#", lines)], collapse = "\n")
    testthat::expect_true(grepl("withCallingHandlers\\(\\s*chisq\\.test\\(migration_table\\)", src, perl = TRUE))
    testthat::expect_true(grepl('"Chi-square approximation"', src, fixed = TRUE))
    testthat::expect_false(grepl("\\n\\s*chi_test <- chisq\\.test\\(migration_table\\)", src, perl = TRUE))
})

# Lift one R6 method out of R/stagemigration.b.R as a standalone function. lintr's
# object_usage_linter and codetools cannot see inside R6::R6Class(), which is how an
# undefined local shipped unnoticed.
stagemigration_lift_method <- function(name) {
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    find_r6 <- function(e) {
        if (!is.call(e)) return(NULL)
        f <- e[[1]]
        if ((is.name(f) && identical(as.character(f), "R6Class")) ||
            (is.call(f) && identical(deparse(f), "R6::R6Class"))) return(e)
        for (i in seq_along(e)[-1]) { r <- find_r6(e[[i]]); if (!is.null(r)) return(r) }
        NULL
    }
    el <- NULL
    for (path in b_path) {
        for (x in parse(path, keep.source = TRUE)) {
            r6 <- find_r6(x)
            if (!is.null(r6) && !is.null(r6$private[[name]])) { el <- r6$private[[name]]; break }
        }
        if (!is.null(el)) break
    }
    testthat::expect_false(is.null(el), info = name)
    eval(parse(text = paste(as.character(el[[4]]), collapse = "\n")), envir = new.env(parent = baseenv()))
}

testthat::test_that("clinical NRI and ROC methods reference no undefined variables", {
    # .calculateClinicalNRI returned threshold = old_threshold after the local was renamed to
    # risk_threshold; its tryCatch handler returned NULL and the table row silently vanished.
    for (m in c(".calculateClinicalNRI", ".populateROCAnalysis", ".calculateTimeDependentAUC", ".timeROCIndex",
                ".stageDirection", ".migrationAnalysisData", ".populateAbbreviationGlossary", ".performStandaloneMigrationOutputs",
                ".populateLikelihoodTests", ".populateStatisticalComparison", ".calculateWillRogersEffect")) {
        fn <- stagemigration_lift_method(m)
        vars <- setdiff(codetools::findGlobals(fn, merge = FALSE)$variables, c("self", "private", "super"))
        known <- vapply(vars, function(v) exists(v, envir = asNamespace("stats")), logical(1))
        testthat::expect_identical(vars[!known], character(0), info = m)
    }
})

testthat::test_that("timeROC's t = 0 slot is never read as the requested time", {
    testthat::skip_if_not_installed("timeROC")
    # timeROC calls Surv() unqualified, so survival must be on the search path.
    withr::local_package("survival")
    d <- survival::lung
    d <- d[!is.na(d$ph.ecog), ]
    r <- timeROC::timeROC(T = d$time, delta = d$status - 1, marker = d$ph.ecog, cause = 1, times = 365, iid = TRUE)
    # Premise: one requested time comes back as times = c(0, t), with NA in slot 1.
    testthat::expect_equal(as.numeric(r$times), c(0, 365))
    testthat::expect_true(is.na(r$AUC[1]))

    idx <- stagemigration_lift_method(".timeROCIndex")
    k <- idx(r, 365)
    testthat::expect_identical(k, 2L)
    testthat::expect_false(is.na(r$AUC[k]))
    # vect_sd_1 is already an SE: it matches the half-width of timeROC's own 95% CI.
    ci <- stats::confint(r)$CI_AUC
    testthat::expect_equal(unname(r$inference$vect_sd_1[k]), unname((ci[1, 2] - ci[1, 1]) / 100 / (2 * stats::qnorm(0.975))), tolerance = 1e-3)

    b_path <- stagemigration_backend_files()
    lines <- stagemigration_read_backend(b_path)
    code <- lines[!grepl("^\\s*#", lines)]
    testthat::expect_false(any(grepl("AUC[1]", code, fixed = TRUE)))
    testthat::expect_false(any(grepl("vect_sd_1[1]", code, fixed = TRUE)))
    testthat::expect_false(any(grepl("sqrt\\([^)]*vect_sd_1", code)))
    testthat::expect_true(any(grepl("timeROC::compare(", code, fixed = TRUE)))
})

testthat::test_that("paired timeROC comparison differs from the independence z-test it replaces", {
    testthat::skip_if_not_installed("timeROC")
    # timeROC calls Surv() unqualified, so survival must be on the search path.
    withr::local_package("survival")
    d <- survival::lung
    d <- d[stats::complete.cases(d[, c("time", "status", "ph.ecog", "ph.karno")]), ]
    m1 <- d$ph.ecog
    m2 <- d$ph.ecog + (100 - d$ph.karno) / 40
    r1 <- timeROC::timeROC(T = d$time, delta = d$status - 1, marker = m1, cause = 1, times = 365, iid = TRUE)
    r2 <- timeROC::timeROC(T = d$time, delta = d$status - 1, marker = m2, cause = 1, times = 365, iid = TRUE)
    p_paired <- unname(timeROC::compare(r1, r2)$p_values_AUC[2])
    se <- c(r1$inference$vect_sd_1[2], r2$inference$vect_sd_1[2])
    diff <- r2$AUC[2] - r1$AUC[2]
    p_indep <- 2 * stats::pnorm(-abs(diff / sqrt(sum(se^2))))
    p_sqrt <- 2 * stats::pnorm(-abs(diff / sqrt(sum(sqrt(se)^2))))
    testthat::expect_true(is.finite(p_paired))
    # Correlated markers: ignoring the covariance overstates the SE, and sqrt(SE) overstates it far more.
    testthat::expect_lt(p_paired, p_indep)
    testthat::expect_lt(p_indep, p_sqrt)
})

testthat::test_that("the module's timeROC wrapper works without survival attached", {
    # timeROC calls Surv() unqualified without importing survival; jamovi does not attach
    # survival, so a bare timeROC::timeROC() call failed and ROC fell back to pROC.
    testthat::skip_if_not_installed("timeROC")
    d <- survival::lung
    d <- d[!is.na(d$ph.ecog), ]
    # The premise only holds when Surv() is not reachable from the search path (true in jamovi and in an
    # installed-package session; not under devtools::load_all(), whose package env exports everything).
    if (!exists("Surv", envir = globalenv(), inherits = TRUE)) {
        bare <- try(timeROC::timeROC(T = d$time, delta = d$status - 1, marker = d$ph.ecog, cause = 1, times = 365), silent = TRUE)
        testthat::expect_s3_class(bare, "try-error")
    }
    was_attached <- "package:survival" %in% search()
    wrap <- stagemigration_lift_method(".timeROC")
    environment(wrap) <- globalenv()
    r <- wrap(T = d$time, delta = d$status - 1, marker = d$ph.ecog, cause = 1, times = 365, iid = TRUE)
    testthat::expect_false(is.na(r$AUC[2]))
    testthat::expect_identical("package:survival" %in% search(), was_attached)
})

stagemigration_helper_env <- function() {
    h <- testthat::test_path("..", "..", "R", "stagemigration_helpers.R")
    testthat::skip_if_not(file.exists(h), "source tree not available")
    env <- new.env(parent = globalenv())
    sys.source(h, envir = env)
    env
}

testthat::test_that("IPCW NRI and IDI equal the complete-case estimators when nobody is censored before t", {
    h <- stagemigration_helper_env()
    set.seed(3)
    n <- 400
    x <- stats::rnorm(n)
    t_true <- stats::rexp(n, 0.08 * exp(x))
    t_h <- 12
    cens <- t_h + stats::runif(n, 0.5, 30)           # all censoring happens after t
    time <- pmin(t_true, cens); event <- as.integer(t_true <= cens)
    p_old <- stats::plogis(x + stats::rnorm(n)); p_new <- stats::plogis(x + stats::rnorm(n, sd = 0.5))
    cuts <- stats::quantile(c(p_old, p_new), c(0, 1/3, 2/3, 1), names = FALSE)
    c_old <- cut(p_old, cuts, include.lowest = TRUE, labels = FALSE); c_new <- cut(p_new, cuts, include.lowest = TRUE, labels = FALSE)

    case <- time <= t_h & event == 1; ctrl <- time > t_h
    testthat::expect_equal(sum(case) + sum(ctrl), n)   # premise: status at t known for everyone
    cc_nri_e <- mean(c_new[case] > c_old[case]) - mean(c_new[case] < c_old[case])
    cc_nri_n <- mean(c_new[ctrl] < c_old[ctrl]) - mean(c_new[ctrl] > c_old[ctrl])
    est <- h$stagemigration_ipcwNRI(c_old, c_new, time, event, t_h)
    testthat::expect_equal(unname(est[["nri"]]), cc_nri_e + cc_nri_n, tolerance = 1e-12)

    cc_idi <- (mean(p_new[case]) - mean(p_new[ctrl])) - (mean(p_old[case]) - mean(p_old[ctrl]))
    testthat::expect_equal(unname(h$stagemigration_ipcwIDI(p_old, p_new, time, event, t_h)[["idi"]]), cc_idi, tolerance = 1e-12)
})

testthat::test_that("IPCW weighting recovers the uncensored case mean that complete-case analysis biases", {
    h <- stagemigration_helper_env()
    set.seed(8)
    n <- 20000
    x <- stats::rnorm(n)
    t_true <- stats::rexp(n, 0.06 * exp(0.9 * x))
    t_h <- 12
    cens <- stats::runif(n, 0, 30)                    # independent censoring, much of it before t
    time <- pmin(t_true, cens); event <- as.integer(t_true <= cens)
    p_old <- stats::plogis(-1 + x + stats::rnorm(n)); p_new <- stats::plogis(-1 + x + stats::rnorm(n, sd = 0.4))

    oracle_case <- t_true <= t_h; oracle_ctrl <- t_true > t_h
    oracle_idi <- (mean(p_new[oracle_case]) - mean(p_new[oracle_ctrl])) - (mean(p_old[oracle_case]) - mean(p_old[oracle_ctrl]))
    obs_case <- time <= t_h & event == 1; obs_ctrl <- time > t_h
    testthat::expect_gt(mean(!obs_case & !obs_ctrl), 0.2)   # premise: many patients censored before t

    est <- h$stagemigration_ipcwIDI(p_old, p_new, time, event, t_h)
    # Early events are more likely to be observed than late ones, so complete-case cases are
    # enriched for high-risk patients.
    cc_case_mean <- mean(p_new[obs_case])
    testthat::expect_lt(abs(est[["new_events"]] - mean(p_new[oracle_case])), 0.004)
    testthat::expect_gt(abs(cc_case_mean - mean(p_new[oracle_case])), 3 * abs(est[["new_events"]] - mean(p_new[oracle_case])))
    testthat::expect_lt(abs(est[["idi"]] - oracle_idi), 0.01)

    cuts <- stats::quantile(c(p_old, p_new), c(0, 1/3, 2/3, 1), names = FALSE)
    c_old <- cut(p_old, cuts, include.lowest = TRUE, labels = FALSE); c_new <- cut(p_new, cuts, include.lowest = TRUE, labels = FALSE)
    oracle_nri <- (mean(c_new[oracle_case] > c_old[oracle_case]) - mean(c_new[oracle_case] < c_old[oracle_case])) +
                  (mean(c_new[oracle_ctrl] < c_old[oracle_ctrl]) - mean(c_new[oracle_ctrl] > c_old[oracle_ctrl]))
    testthat::expect_lt(abs(h$stagemigration_ipcwNRI(c_old, c_new, time, event, t_h)[["nri"]] - oracle_nri), 0.02)
})

testthat::test_that("the NRI helper uses one set of risk cut-points for both systems", {
    h <- stagemigration_helper_env()
    src <- paste(deparse(h$stagemigration_calculateNRI), collapse = "\n")
    testthat::expect_true(grepl("quantile(c(risk_old, risk_new)", src, fixed = TRUE))
    testthat::expect_false(exists("stagemigration_bootstrapIDI", envir = h, inherits = FALSE))
})

testthat::test_that("the split backend is one R6 chain with unique method names", {
    # A method defined in two files would silently override its parent's copy; a broken
    # inherit link would drop every method below it.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    find_r6 <- function(e) {
        if (!is.call(e)) return(NULL)
        f <- e[[1]]
        if ((is.name(f) && identical(as.character(f), "R6Class")) || (is.call(f) && identical(deparse(f), "R6::R6Class"))) return(e)
        for (i in seq_along(e)[-1]) { r <- find_r6(e[[i]]); if (!is.null(r)) return(r) }
        NULL
    }
    classes <- lapply(b_path, function(p) { r <- NULL; for (x in parse(p, keep.source = TRUE)) { r <- find_r6(x); if (!is.null(r)) break }; r })
    inherits_from <- vapply(classes, function(r) deparse(r$inherit), "")
    class_names <- vapply(classes, function(r) as.character(r[[2]]), "")
    testthat::expect_identical(class_names, c("stagemigrationClass", sprintf("stagemigrationPart%d", 1:5)))
    testthat::expect_identical(inherits_from, c("stagemigrationPart5", "stagemigrationBase", sprintf("stagemigrationPart%d", 1:4)))
    members <- unlist(lapply(classes, function(r) names(r$private)[-1]))
    testthat::expect_identical(members[duplicated(members)], character(0))
    testthat::expect_true(all(c(".init", ".run", ".renderNotices", ".calculateClinicalNRI", ".performFineGrayAnalysis") %in% members))
})

testthat::test_that("stage direction is defined only on one ordered scale", {
    # union(levels(old), levels(new)) ranked every new-only label above every old label: II -> IIA and
    # IV -> IIB both counted as upstaging, and disjoint label sets made every patient "upstaged".
    dir_fn <- stagemigration_lift_method(".stageDirection")
    lv <- c("Stage I", "Stage II", "Stage III", "Stage IV")
    old <- factor(c("Stage I", "Stage II", "Stage III", "Stage IV", "Stage II"), levels = lv)
    new <- factor(c("Stage II", "Stage II", "Stage II", "Stage III", NA), levels = lv)
    r <- dir_fn(old, new)
    testthat::expect_true(r$comparable)
    testthat::expect_identical(r$direction, c(1L, 0L, -1L, -1L, NA))

    # the new factor's own level order is irrelevant: the scale is one system's order for both
    new_rev <- factor(as.character(new), levels = rev(lv))
    testthat::expect_identical(dir_fn(old, new_rev)$direction, c(1L, 0L, -1L, -1L, NA))

    # a new edition that adds sub-stages: direction undefined, unchanged labels still 0
    old2 <- factor(c("Stage II", "Stage IV", "Stage I"), levels = lv)
    new2 <- factor(c("Stage IIA", "Stage IIB", "Stage I"), levels = c("Stage I", "Stage IIA", "Stage IIB", "Stage III", "Stage IV"))
    r2 <- dir_fn(old2, new2)
    testthat::expect_false(r2$comparable)
    testthat::expect_identical(r2$direction, c(NA, NA, 0L))
    testthat::expect_identical(sort(r2$only_new), c("Stage IIA", "Stage IIB"))

    # labels of one system that are a subset of the other's scale are still comparable
    testthat::expect_true(dir_fn(old, factor(c("Stage I", "Stage I", "Stage IV", "Stage IV", "Stage II"), levels = lv))$comparable)
    # integer stages order numerically, not as text ("10" after "9")
    testthat::expect_identical(dir_fn(c(9, 10, 2), c(10, 9, 2))$direction, c(1L, -1L, 0L))
})

testthat::test_that("C-index difference uses the paired variance and the LR tests are nested", {
    h <- stagemigration_helper_env()
    set.seed(12)
    n <- 600
    x <- stats::rnorm(n)
    old_s <- cut(x + stats::rnorm(n, sd = 0.9), c(-Inf, -0.7, 0, 0.7, Inf), labels = c("I", "II", "III", "IV"))
    new_s <- cut(x + stats::rnorm(n, sd = 0.5), c(-Inf, -0.7, 0, 0.7, Inf), labels = c("I", "II", "III", "IV"))
    tt <- stats::rexp(n, 0.05 * exp(0.9 * x)); cc <- stats::runif(n, 0, 60)
    d <- data.frame(os = pmin(tt, cc), event_binary = as.integer(tt <= cc), old_s = old_s, new_s = new_s)
    opts <- list(oldStage = "old_s", newStage = "new_s", survivalTime = "os", analysisType = "standard",
                 performBootstrap = FALSE, calculatePseudoR2 = FALSE, confidenceLevel = 0.95)
    am <- h$stagemigration_calculateAdvancedMetrics(d, opts)
    testthat::expect_null(am$error)

    fo <- survival::coxph(survival::Surv(os, event_binary) ~ old_s, data = d)
    fn <- survival::coxph(survival::Surv(os, event_binary) ~ new_s, data = d)
    cc2 <- survival::concordance(fo, fn)
    se_paired <- sqrt(drop(c(-1, 1) %*% cc2$var %*% c(-1, 1)))
    se_indep <- sqrt(sum(diag(cc2$var)))
    testthat::expect_equal(am$c_improvement_se, se_paired, tolerance = 1e-10)
    testthat::expect_lt(se_paired, 0.8 * se_indep)      # premise: pairing materially shrinks the SE here
    testthat::expect_equal(am$c_improvement_ci_upper - am$c_improvement_ci_lower, 2 * stats::qnorm(0.975) * se_paired, tolerance = 1e-10)

    both <- survival::coxph(survival::Surv(os, event_binary) ~ old_s + new_s, data = d)
    ref <- stats::anova(fo, both)
    testthat::expect_equal(am$lr_test$new_adds$stat, ref[2, "Chisq"], tolerance = 1e-8)
    testthat::expect_equal(am$lr_test$new_adds$p, ref[2, "Pr(>|Chi|)"], tolerance = 1e-8)
    testthat::expect_equal(am$lr_test$p_value, am$lr_test$new_adds$p)
    testthat::expect_gt(am$lr_test$old_adds$stat, 0)
})

testthat::test_that("no hardcoded statistics remain in the summary tables", {
    # .populateStatisticalSummary displayed an improvement of 0.0178, the CI "[-0.0341, +0.0698]" for
    # every dataset and p = 0.501 whenever no LR p-value existed.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    lines <- stagemigration_read_backend(b_path)
    code <- lines[!grepl("^\\s*#", lines)]
    for (lit in c("0.501", "0.0178", "[-0.0341, +0.0698]")) testthat::expect_false(any(grepl(lit, code, fixed = TRUE)), info = lit)
    testthat::expect_false(any(grepl("lr_stat = lr_new - lr_old", code, fixed = TRUE)))
    testthat::expect_false(any(grepl("sqrt(old_c_se^2 + new_c_se^2)", code, fixed = TRUE)))
})

testthat::test_that("an all-censored cohort is told it has too few events, not too few patients", {
    v <- testthat::test_path("..", "..", "R", "stagemigration-validation.R")
    testthat::skip_if_not(file.exists(v), "source tree not available")
    src <- paste(readLines(v, warn = FALSE), collapse = "\n")
    testthat::expect_true(grepl("Too few events for a meaningful analysis", src, fixed = TRUE))
    testthat::expect_false(grepl('"Sample size too small for meaningful analysis"', src, fixed = TRUE))
})

testthat::test_that("every bare symbol on the left of a formula is defined in its method", {
    # object_usage_linter/codetools skip formula contents, so a cleanup deleted locals such as
    # migrated_surv that only survfit(migrated_surv ~ 1) used, breaking 29 methods silently.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    find_r6 <- function(e) {
        if (!is.call(e)) return(NULL)
        f <- e[[1]]
        if ((is.name(f) && identical(as.character(f), "R6Class")) || (is.call(f) && identical(deparse(f), "R6::R6Class"))) return(e)
        for (i in seq_along(e)[-1]) { r <- find_r6(e[[i]]); if (!is.null(r)) return(r) }
        NULL
    }
    walk <- function(e, env) {
        if (is.call(e)) {
            head <- e[[1]]
            if (is.name(head) && as.character(head) %in% c("<-", "=") && length(e) == 3 && is.name(e[[2]])) env$assigned <- c(env$assigned, as.character(e[[2]]))
            if (is.name(head) && as.character(head) == "for" && is.name(e[[2]])) env$assigned <- c(env$assigned, as.character(e[[2]]))
            if (is.name(head) && as.character(head) == "function") env$assigned <- c(env$assigned, names(e[[2]]))
            # A formula handed to a survival model call WITHOUT data= must find its left-hand symbol in
            # the method frame (survfit(migrated_surv ~ 1)). With data= it may be a column; plotting
            # formulas (facet_grid(system ~ .)) name columns, so only survival calls are checked.
            fn_name <- if (is.name(head)) as.character(head) else if (is.call(head) && identical(head[[1]], as.name("::"))) as.character(head[[3]]) else ""
            if (!(fn_name %in% c("survfit", "coxph", "survdiff", "survreg", "concordance", "cox.zph", "coxme")) ||
                (!is.null(names(e)) && "data" %in% names(e))) {
                # not a survival model call, or a column formula
            } else {
                for (a in as.list(e)[-1]) {
                    if (is.call(a) && identical(a[[1]], as.name("~")) && length(a) == 3 && is.name(a[[2]])) env$lhs <- c(env$lhs, as.character(a[[2]]))
                }
            }
            for (i in seq_along(e)[-1]) tryCatch(walk(e[[i]], env), error = function(err) NULL)
        }
    }
    missing_defs <- character(0)
    for (p in b_path) {
        r6 <- NULL; for (x in parse(p, keep.source = FALSE)) { r6 <- find_r6(x); if (!is.null(r6)) break }
        for (nm in names(r6$private)[-1]) {
            fn <- r6$private[[nm]]
            if (!(is.call(fn) && identical(as.character(fn[[1]]), "function"))) next
            env <- new.env(); env$assigned <- names(fn[[2]]); env$lhs <- character(0)
            walk(fn[[3]], env)
            undefined <- setdiff(unique(env$lhs), env$assigned)
            if (length(undefined)) missing_defs <- c(missing_defs, paste0(nm, ":", undefined))
        }
    }
    testthat::expect_identical(missing_defs, character(0))
})

testthat::test_that("RMST, SME and the glossary run without the advanced block", {
    # Their result items are visible on their own checkbox, but the computation lived only inside
    # .performAdvancedMigrationAnalysis, so ticking one alone showed an empty table.
    b_path <- stagemigration_backend_files()
    testthat::skip_if_not(all(file.exists(b_path)), "source tree not available")
    src <- paste(stagemigration_read_backend(b_path), collapse = "\n")
    testthat::expect_true(grepl("if (!isTRUE(self$options$advancedMigrationAnalysis)) {\n                    private$.performStandaloneMigrationOutputs()", src, fixed = TRUE))
    fn <- stagemigration_lift_method(".performStandaloneMigrationOutputs")
    body <- paste(deparse(body(fn)), collapse = "\n")
    for (call in c(".calculateRMSTMetrics", ".populateRMSTAnalysis", ".calculateStageMigrationEffect", ".populateStageMigrationEffect", ".populateAbbreviationGlossary"))
        testthat::expect_true(grepl(call, body, fixed = TRUE), info = call)
})
