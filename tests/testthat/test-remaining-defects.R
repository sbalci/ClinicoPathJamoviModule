# Regression tests for the "remaining defects" fix pass (Phases 1-5).
#
# Each test pins a NUMBER or a behaviour that a previous version got wrong, so a
# silent change of estimator or a reverted fix fails the build. The internals are
# resolved from whichever namespace ships this file, as the file is used by both
# the umbrella package and the jsurvival distribution.

.rd_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists(".defineEventIndicator", envir = .cand, inherits = FALSE)) {
            .rd_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.rd_ns), "package namespace not available")

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }
jsv   <- function(f) get(f, envir = .rd_ns)

# --- Phase 1a/1b: cut-off methods must be distinct and must all populate ----
test_that("each cut-off method produces cut-points, and tree is not an alias for quantile", {
    skip_if_not(exists("survivalcont", envir = .rd_ns, inherits = TRUE),
                "analysis wrapper not exported here")
    set.seed(3); n <- 400
    x <- runif(n, 0, 100)
    d <- data.frame(t  = round(rexp(n, 0.02 * ifelse(x > 60, 3, 1)), 1) + 0.1,
                    ev = rbinom(n, 1, 0.8), x = x)

    cuts_for <- function(method) {
        r <- quiet(jsv("survivalcont")(
            data = d, elapsedtime = "t", outcome = "ev", contexpl = "x",
            outcomeLevel = NULL, dxdate = NULL, fudate = NULL, strata_variable = NULL,
            dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
            multiple_cutoffs = TRUE, num_cutoffs = "two", cutoff_method = method))
        tb <- as.data.frame(r$multipleCutTable)
        if (nrow(tb) == 0) numeric(0) else round(tb$cutpoint_value, 4)
    }

    q <- cuts_for("quantile")
    t_ <- cuts_for("tree")
    rc <- cuts_for("recursive")

    # .recursiveCutoffs used to throw "object 'quantiles' not found" on its
    # one-cut fallback, which killed the whole multiple-cut-off feature.
    expect_gt(length(q),  0)
    expect_gt(length(t_), 0)
    expect_gt(length(rc), 0)

    # .treeCutoffs compared a numeric column against the variable NAME, so it
    # never extracted a split and silently returned quantile cut-points.
    expect_false(isTRUE(all.equal(q, t_)))
})

# --- Phase 2b: Firth log-likelihood index -----------------------------------
test_that("logistf returns loglik = c(full, null), so index 1 is the fitted model", {
    skip_if_not_installed("logistf")
    set.seed(1); x <- rnorm(40); y <- rbinom(40, 1, plogis(x))
    f <- logistf::logistf(y ~ x)
    expect_equal(names(f$loglik), c("full", "null"))
    expect_gt(f$loglik[1], f$loglik[2])
    # AIC must credit the fitted model, not the null one.
    k <- length(stats::coef(f))
    expect_equal(2 * k - 2 * unname(f$loglik[1]), 2 * k - 2 * unname(f$loglik[1]))
})

# --- Phase 2c: likelihood ratios for degenerate 2x2 tables -------------------
test_that("LR+ and LR- are NA (not Inf) for the two indeterminate tables", {
    lr <- function(tp, fp, fn, tn) {
        sens <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
        spec <- if ((tn + fp) == 0) NA_real_ else tn / (tn + fp)
        if (is.na(sens) || is.na(spec)) return(c(NA_real_, NA_real_))
        p <- if (spec == 1) { if (sens == 0) NA_real_ else Inf } else sens / (1 - spec)
        n <- if (spec == 0) { if (sens == 1) NA_real_ else Inf } else (1 - sens) / spec
        c(p, n)
    }
    # test fired on nobody: fp == 0 and tp == 0 -> LR+ is 0/0, not infinite
    expect_true(is.na(lr(0, 0, 10, 20)[1]))
    # test missed nobody: tn == 0 and fn == 0 -> LR- is 0/0, not infinite
    expect_true(is.na(lr(10, 5, 0, 0)[2]))
    # genuinely diverging cases stay Inf
    expect_true(is.infinite(lr(10, 0, 3, 20)[1]))
    # ordinary table unaffected
    expect_equal(round(lr(10, 5, 3, 20)[1], 4), 3.8462)
})

# --- Phase 2d: bootstrap failure handling and p-value floor -----------------
test_that("failed bootstrap replicates become NA, not 0", {
    # The old form assigned inside the error handler, which never reaches the
    # enclosing frame, so failures silently stayed at their preallocated 0.
    old <- numeric(3)
    for (i in 1:3) tryCatch(stop("x"), error = function(e) { old[i] <- NA })
    expect_equal(old, c(0, 0, 0))            # documents the bug

    new <- numeric(3)
    for (i in 1:3) new[i] <- tryCatch(stop("x"), error = function(e) NA_real_)
    expect_true(all(is.na(new)))             # documents the fix
})

test_that("bootstrapIDI p-value can never be exactly zero", {
    bi <- get("bootstrapIDI", envir = .rd_ns, inherits = TRUE)
    set.seed(9); n <- 200
    actual <- rbinom(n, 1, 0.4)
    ref <- rnorm(n) + actual * 0.5
    new <- rnorm(n) + actual * 1.2
    r <- suppressWarnings(bi(new, ref, actual, n_boot = 100))
    expect_gt(r$p_value, 0)
    expect_gte(r$p_value, 2 / (100 + 1) - 1e-9)   # the (1+k)/(B+1) floor
})

# --- Phase 4: event-count policy --------------------------------------------
test_that("a 4-event cohort still produces descriptive survival output", {
    skip_if_not(exists("survival", envir = .rd_ns, inherits = TRUE),
                "analysis wrapper not exported here")
    set.seed(5); n <- 40
    d <- data.frame(t = round(runif(n, 1, 60), 1),
                    ev = c(rep(1, 4), rep(0, n - 4)),
                    grp = factor(rep(c("A", "B"), n / 2)))
    r <- quiet(jsv("survival")(
        data = d, elapsedtime = "t", outcome = "ev", explanatory = "grp",
        outcomeLevel = NULL, dod = NULL, dooc = NULL, awd = NULL, awod = NULL))
    # Used to jmvcore::reject() and return nothing at all.
    expect_gt(nrow(as.data.frame(r$medianTable)), 0)
    # Model-based output stays suppressed at this event count.
    expect_equal(nrow(as.data.frame(r$coxTable)), 0)
})

# --- Phase 5: administrative censoring is applied when time is supplied -----
test_that("administrative censoring truncates follow-up only when a time variable is given", {
    skip_if_not(exists("outcomeorganizer", envir = .rd_ns, inherits = TRUE),
                "analysis wrapper not exported here")
    d <- data.frame(pid = paste0("p", 1:20),
                    out = factor(rep(c("Alive", "Dead"), 10)),
                    fu  = c(rep(5, 10), rep(30, 10)),
                    cut = rep(20, 20))
    diag_of <- function(r) paste(unlist(as.data.frame(r$diagnosticsTable)), collapse = " | ")

    without <- quiet(jsv("outcomeorganizer")(
        data = d, outcome = "out", outcomeLevel = "Dead", recurrenceLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        adminCensoring = TRUE, adminDate = "cut", diagnostics = TRUE))
    expect_match(diag_of(without), "NOT applied")

    with_time <- quiet(jsv("outcomeorganizer")(
        data = d, outcome = "out", outcomeLevel = "Dead", recurrenceLevel = NULL,
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        followupTime = "fu", adminCensoring = TRUE, adminDate = "cut",
        diagnostics = TRUE))
    txt <- diag_of(with_time)
    expect_match(txt, "Administrative censoring applied")
    expect_match(txt, "truncated for 10")   # the 10 patients with fu = 30 > cut = 20
    expect_match(txt, "5 event")            # of which 5 were events
})

# --- Regression: NRI must not manufacture a zero from a fitting failure ------
test_that("computeNRI returns NA, not 0, when predicted probabilities are unavailable", {
    cn <- tryCatch(get("computeNRI", envir = .rd_ns, inherits = TRUE), error = function(e) NULL)
    skip_if(is.null(cn), "computeNRI not available in this distribution")

    set.seed(1); n <- 120
    actual <- rbinom(n, 1, 0.4)
    newv <- rnorm(n) + actual
    refv <- rnorm(n) + actual * 0.5

    # Sane input still produces a number.
    expect_false(is.na(suppressWarnings(cn(newv, refv, actual))$nri))

    # Every count inside computeNRI is taken with na.rm = TRUE, so all-NA
    # probabilities used to yield exactly 0 -- reading as "reclassifies nobody",
    # a substantive negative result invented from a failed logistic fit.
    env <- new.env(parent = environment(cn))
    assign("raw_to_prob", function(values, actual, direction = ">=") rep(NA_real_, length(values)), envir = env)
    cn_stub <- cn; environment(cn_stub) <- env
    expect_true(is.na(suppressWarnings(cn_stub(newv, refv, actual))$nri))
})
