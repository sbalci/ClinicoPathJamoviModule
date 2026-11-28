delayed_entry_sample <- function() {
    i <- seq_len(48L)
    data.frame(
        enter = c(0, 0, 7, 5, 1 + i %% 8),
        stop = c(5, 5, 13, 14, 12 + i / 10),
        event = c(1L, 0L, 0L, 0L, as.integer((i %% 5) %in% c(0, 2))),
        group = factor(rep(c("B", "A", "A", "B"), 13), levels = c("B", "A")),
        score = c(2, 1, 3, 2, (i %% 11) / 3)
    )
}

test_that("delayed entry uses (start, stop] risk sets and explicit references", {
    d <- delayed_entry_sample()
    d[["stratum group"]] <- d$group
    ans <- .fitDelayedEntryCox(d, "enter", "stop", "event", c("stratum group", "score"))
    ref <- survival::coxph(survival::Surv(enter, stop, event) ~ group + score,
                           data = d, ties = "efron")
    expect_equal(unname(stats::coef(ans$fit)), unname(stats::coef(ref)))
    expect_equal(unname(stats::vcov(ans$fit)), unname(stats::vcov(ref)))
    expect_identical(attr(ans$fit$y, "type"), "counting")
    expect_identical(ans$estimates$reference, c("B", ""))
    expect_identical(ans$estimates$level, c("A", ""))
    detail <- survival::coxph.detail(ans$fit, riskmat = TRUE)
    at_5 <- match(5, detail$time)
    expect_false(is.na(at_5))
    expect_equal(as.integer(detail$riskmat[1:4, at_5]), c(1L, 1L, 0L, 0L))
    expect_equal(as.integer(detail$nrisk[at_5]), sum(d$enter < 5 & d$stop >= 5))
})

test_that("public analysis returns the same hazard ratios and intervals as coxph", {
    d <- delayed_entry_sample()
    ref <- survival::coxph(survival::Surv(enter, stop, event) ~ group + score,
                           data = d, ties = "efron")
    # 0/1-factors cover binary labels from jamovi nominal columns.
    d$event <- factor(d$event)
    res <- delayedentrycox(d, "enter", "stop", "event", c("group", "score"))
    tab <- res$estimates$asDF
    counts <- res$sample$asDF
    expect_equal(tab$hr, unname(exp(stats::coef(ref))))
    expect_equal(tab$lower, unname(exp(stats::confint(ref)[, 1L])))
    expect_equal(tab$upper, unname(exp(stats::confint(ref)[, 2L])))
    expect_equal(tab$p, unname(summary(ref)$coefficients[, "Pr(>|z|)"]))
    expect_equal(counts$participants, nrow(d))
    expect_equal(counts$events, ref$nevent)
    expect_equal(counts$censored, nrow(d) - ref$nevent)
})

test_that("invalid delayed entry data are refused rather than silently repaired", {
    d <- delayed_entry_sample()
    fit <- function(data) .fitDelayedEntryCox(data, "enter", "stop", "event", c("group", "score"))
    expect_error(fit(transform(d, stop = enter)), "strictly later")
    expect_error(fit(transform(d, enter = -1)), "nonnegative")
    expect_error(fit(transform(d, event = event + 1L)), "Code event as 0")
    expect_error(fit(transform(d, event = 0L)), "observed event")
    expect_error(fit(transform(d, score = NA_real_)), "complete")
    expect_error(fit(transform(d, group = factor("B"))), "at least two observed")
    expect_error(.fitDelayedEntryCox(d, "enter", "stop", "event", "enter"), "distinct")
})
