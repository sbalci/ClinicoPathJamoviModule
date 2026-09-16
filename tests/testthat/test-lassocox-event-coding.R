# Event coding: lassocox never infers the event value from level order or numeric size.
lassocox_event_data <- function(status) {
    set.seed(20260915)
    n <- length(status)
    data.frame(time = stats::rexp(n, 0.1) + 0.5, status = status,
               x1 = stats::rnorm(n), x2 = stats::rnorm(n), x3 = stats::rnorm(n))
}

lassocox_event_run <- function(df, outcomeLevel, censorLevel) {
    skip_if_not_installed("glmnet")
    ClinicoPath::lassocox(data = df, elapsedtime = "time", outcome = "status",
        outcomeLevel = outcomeLevel, censorLevel = censorLevel,
        explanatory = c("x1", "x2", "x3"), nfolds = 5,
        cv_plot = FALSE, coef_plot = FALSE, survival_plot = FALSE)
}

lassocox_levels_used <- function(res) {
    ms <- as.data.frame(res$modelSummary)
    c(event = ms$value[ms$statistic == "Event Level Used"],
      censored = ms$value[ms$statistic == "Censored Level Used"])
}

test_that("a factor outcome without an Event Level stops instead of using level order", {
    # sort(c("Died", "Survived"))[2] used to make "Survived" the event
    df <- lassocox_event_data(factor(rep(c("Died", "Survived"), 60)))
    expect_error(lassocox_event_run(df, NULL, NULL), "Choose the Event Level")
})

test_that("a numeric 1/2 outcome without an Event Level stops instead of using the larger value", {
    df <- lassocox_event_data(rep(c(1, 2), 60))
    expect_error(lassocox_event_run(df, NULL, NULL), "does not guess which value is the event")
})

test_that("an empty Censored Level is the other observed value, also when the event is the smaller code", {
    # used to take min() = 1 and stop with "Event level and censored level must be different"
    df <- lassocox_event_data(rep(c(1, 2), 60))
    expect_identical(lassocox_levels_used(lassocox_event_run(df, "1", NULL)),
                     c(event = "1", censored = "2"))
})

test_that("numeric 0/1 still reads 1 as the event without an Event Level", {
    df <- lassocox_event_data(rep(c(0, 1), 60))
    expect_identical(lassocox_levels_used(lassocox_event_run(df, NULL, NULL)),
                     c(event = "1", censored = "0"))
})
