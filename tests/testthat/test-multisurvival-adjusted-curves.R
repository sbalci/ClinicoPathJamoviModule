# Regression tests for CR-3: one estimator behind the adjusted plot and the
# adjusted tables.
#
# Symptom that prompted these: `ac_method` was read in exactly one place -- the
# method= argument of survminer::ggadjustedcurves() inside .plot_adj(). The
# adjusted survival table, the adjusted median table and their narratives built
# their own prediction (survfit() on a single mean/mode covariate profile), so
# switching from "average" to "conditional" redrew the plot and left the tables
# byte-identical. The at-risk / event counts in those tables came from the
# model's common risk set and were therefore the same number for every level
# while carrying a per-level label.

.msac_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("multisurvival", envir = .cand, inherits = FALSE)) {
            .msac_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.msac_ns), "multisurvival not available in this distribution")

.msac_quiet <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

# Age is strongly confounded with the adjustment variable, so standardising over
# everyone (average) and predicting one mean-covariate patient (conditional) are
# far apart. On a balanced covariate distribution they nearly coincide and the
# test would pass even with the defect back in place.
.msac_data <- function() {
    set.seed(7); n <- 400
    grp <- factor(sample(c("A", "B"), n, TRUE))
    age <- ifelse(grp == "B", stats::rnorm(n, 75, 6), stats::rnorm(n, 50, 6))
    data.frame(t   = round(stats::rexp(n, 0.02 * exp(0.9 * (grp == "B") + 0.06 * (age - 60))), 1) + 0.1,
               ev  = stats::rbinom(n, 1, 0.85),
               grp = grp, age = age)
}

.msac_run <- function(d, ...) {
    .msac_quiet(do.call(get("multisurvival", envir = .msac_ns),
        c(list(data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
               explanatory = "grp", contexpl = "age",
               dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
               ac = TRUE, adjexplanatory = "grp", ac_summary = TRUE,
               cutp = "12, 36, 60"), list(...))))
}

# Percentages are stored as formatted text ("73.6%").
.msac_pct <- function(x) as.numeric(sub("%", "", x))

# Render the adjusted plot and hand back the data frame it actually drew.
# last_plot() keeps the PREVIOUS plot when a render returns early without
# printing, so refuse to report anything unless this render said it drew.
.msac_plot_data <- function(res) {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    drew <- .msac_quiet(res$plot_adj$.render())
    if (!isTRUE(drew)) return(NULL)
    p <- try(ggplot2::last_plot(), silent = TRUE)
    if (inherits(p, "try-error")) NULL else p$data
}

test_that("average and conditional are different estimands, in the tables as well as the plot", {
    d <- .msac_data()
    avg  <- .msac_run(d, ac_method = "average")
    cond <- .msac_run(d, ac_method = "conditional")

    ta <- as.data.frame(avg$adjustedSurvTable)
    tc <- as.data.frame(cond$adjustedSurvTable)
    expect_gt(nrow(ta), 0)
    expect_equal(nrow(ta), nrow(tc))

    sa <- .msac_pct(ta$surv); sc <- .msac_pct(tc$surv)
    # The whole defect was that these were identical to the last decimal.
    expect_false(isTRUE(all.equal(sa, sc, tolerance = 1e-8)))
    expect_gt(max(abs(sa - sc)), 2)   # percentage points, not rounding noise

    ma <- as.data.frame(avg$adjustedMedianTable)$median
    mc <- as.data.frame(cond$adjustedMedianTable)$median
    expect_false(isTRUE(all.equal(ma, mc, tolerance = 1e-8)))
})

test_that("the average curve is the g-computation standardisation, not a reference patient", {
    d <- .msac_data()
    avg <- .msac_run(d, ac_method = "average")
    ta <- as.data.frame(avg$adjustedSurvTable)

    # Independent reference implementation: set every patient to the level, predict,
    # average the predicted survival across patients.
    fit <- survival::coxph(survival::Surv(t, ev) ~ grp + age, data = d)
    ref <- function(level, tp) {
        nd <- d; nd$grp <- factor(level, levels = levels(d$grp))
        sf <- survival::survfit(fit, newdata = nd)
        i <- which(sf$time <= tp)
        if (length(i) == 0) return(1)
        mean(sf$surv[max(i), ])
    }
    for (i in seq_len(nrow(ta))) {
        expect_equal(.msac_pct(ta$surv[i]) / 100,
                     ref(ta$strata[i], ta$time[i]), tolerance = 1e-3)
    }
})

test_that("the plotted curve and the tabulated numbers are the same estimator", {
    d <- .msac_data()
    for (m in c("average", "conditional")) {
        res <- .msac_run(d, ac_method = m)
        pd  <- .msac_plot_data(res)
        expect_true(is.data.frame(pd), info = m)
        expect_true(all(c("time", "surv", "group") %in% names(pd)), info = m)

        tb <- as.data.frame(res$adjustedSurvTable)
        checked <- 0
        for (i in seq_len(nrow(tb))) {
            g <- pd[pd$group == tb$strata[i] & pd$time <= tb$time[i], , drop = FALSE]
            if (nrow(g) == 0) next   # timepoint beyond the plotted x range
            # A step curve: its height at t is the last estimate at or before t.
            expect_equal(g$surv[nrow(g)], .msac_pct(tb$surv[i]) / 100,
                         tolerance = 1e-3, info = paste(m, tb$strata[i], tb$time[i]))
            checked <- checked + 1
        }
        expect_gt(checked, 0)
    }
})

test_that("adjusted-table counts are group-specific observed counts, not the common risk set", {
    d <- .msac_data()
    res <- .msac_run(d, ac_method = "average")
    tb  <- as.data.frame(res$adjustedSurvTable)

    for (tp in unique(tb$time)) {
        rows <- tb[tb$time == tp, , drop = FALSE]
        if (nrow(rows) < 2) next
        # These used to be one number repeated under every level heading.
        expect_gt(length(unique(rows$atrisk)), 1)
    }

    # ... and each one must be the observed count for that group.
    for (i in seq_len(nrow(tb))) {
        in_grp <- as.character(d$grp) == tb$strata[i]
        expect_equal(tb$atrisk[i], sum(in_grp & d$t >= tb$time[i]))
        expect_equal(tb$events[i], sum(in_grp & d$t <= tb$time[i] & d$ev == 1))
    }
})

test_that("an unsupported adjustment method is refused, never silently substituted", {
    d <- .msac_data()

    # `marginal` is no longer offered in the UI at all. survminer's estimator
    # fails whenever the adjustment variable is also a Cox covariate, and on
    # real data it did not fail fast -- it ran without returning, leaving the
    # plot spinning indefinitely. Removing it means jmvcore's List validation
    # now rejects the value before any model is fitted, which is a stronger
    # refusal than the backend guard. The guard is retained anyway for older or
    # hand-edited files, so accept either outcome; what must NEVER happen is a
    # populated table computed by some other estimator wearing this name.
    res <- tryCatch(.msac_run(d, ac_method = "marginal"), error = function(e) e)

    if (inherits(res, "error")) {
        expect_match(conditionMessage(res), "marginal|not valid|must be", ignore.case = TRUE)
    } else {
        expect_equal(nrow(as.data.frame(res$adjustedSurvTable)), 0)
        expect_equal(nrow(as.data.frame(res$adjustedMedianTable)), 0)
    }

    # Refusal must not be blanket: a supported estimand still produces output.
    avg <- .msac_run(d, ac_method = "average")
    expect_gt(nrow(as.data.frame(avg$adjustedSurvTable)), 0)
})

test_that("a stratified Cox fit is standardised over the union time grid, not averaged blindly", {
    set.seed(13); n <- 400
    site <- factor(sample(c("S1", "S2", "S3"), n, TRUE))
    grp  <- factor(sample(c("A", "B"), n, TRUE))
    age  <- ifelse(grp == "B", stats::rnorm(n, 72, 6), stats::rnorm(n, 55, 6))
    base <- c(S1 = 0.02, S2 = 0.05, S3 = 0.09)[as.character(site)]
    d <- data.frame(t = round(stats::rexp(n, base * exp(0.8 * (grp == "B") + 0.03 * (age - 60))), 1) + 0.1,
                    ev = stats::rbinom(n, 1, 0.85), grp = grp, age = age, site = site)

    res <- .msac_quiet(do.call(get("multisurvival", envir = .msac_ns), list(
        data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
        explanatory = c("grp", "site"), contexpl = "age",
        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
        use_stratify = TRUE, stratvar = "site",
        ac = TRUE, adjexplanatory = "grp", ac_summary = TRUE,
        ac_method = "average", cutp = "12, 36")))
    tb <- as.data.frame(res$adjustedSurvTable)
    expect_gt(nrow(tb), 0)

    # survfit() on a stratified fit concatenates each subject's curve on its own
    # stratum's time grid; rowMeans() straight off $surv would average unrelated
    # time points. Re-evaluate on the union grid the same way and compare.
    fit <- survival::coxph(
        survival::Surv(t, ev) ~ grp + age + survival::strata(site), data = d)
    ref <- function(level, tp) {
        nd <- d; nd$grp <- factor(level, levels = levels(d$grp))
        sf <- survival::survfit(fit, newdata = nd)
        g  <- sort(unique(sf$time))
        m  <- matrix(summary(sf, times = g, extend = TRUE)$surv, nrow = length(g))
        i  <- which(g <= tp)
        if (length(i) == 0) return(1)
        mean(m[max(i), ])
    }
    for (i in seq_len(nrow(tb))) {
        expect_equal(.msac_pct(tb$surv[i]) / 100,
                     ref(tb$strata[i], tb$time[i]), tolerance = 2e-3)
    }
})

test_that("the estimand is named alongside every adjusted table", {
    d <- .msac_data()
    expect_match(as.character(.msac_run(d, ac_method = "average")$adjustedSurvTableSummary$content),
                 "standardised over the observed patients")
    expect_match(as.character(.msac_run(d, ac_method = "conditional")$adjustedMedianSummary$content),
                 "one reference patient")
})

test_that("CR-5 does not regress: the Fine-Gray branch still draws cumulative incidence", {
    set.seed(11); n <- 300
    grp <- factor(sample(c("A", "B"), n, TRUE))
    age <- ifelse(grp == "B", stats::rnorm(n, 72, 6), stats::rnorm(n, 55, 6))
    status <- factor(sample(c("DOD", "DOOC", "AWOD"), n, TRUE, prob = c(.45, .25, .30)),
                     levels = c("DOD", "DOOC", "AWOD"))
    d <- data.frame(t = round(stats::rexp(n, 0.03), 1) + 0.1,
                    ev = status, grp = grp, age = age)

    res <- .msac_quiet(do.call(get("multisurvival", envir = .msac_ns), list(
        data = d, elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
        explanatory = "grp", contexpl = "age",
        multievent = TRUE, analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = NULL, awod = "AWOD",
        ac = TRUE, adjexplanatory = "grp", ac_summary = TRUE,
        ac_method = "average", cutp = "12, 36")))

    pd <- .msac_plot_data(res)
    expect_true(is.data.frame(pd))
    # The competing-risks branch plots CIF, not survival, and must be untouched
    # by the shared survival estimator introduced for CR-3.
    expect_true("cif" %in% names(pd))
    expect_true(all(pd$cif >= 0 & pd$cif <= 1))
    # Cumulative incidence only goes up.
    for (g in unique(pd$group)) {
        v <- pd$cif[pd$group == g]
        expect_true(all(diff(v) >= -1e-8))
    }
})
