# Regression test for the smoothed hazard plot (R/singlearm.b.R .smoothedHazardPlot).
#
# The plot used to finite-difference basehaz() and feed the resulting points to
# an UNWEIGHTED loess. Each point is a rate estimated from ~one event, and
# 1/(n_i * dt_i) is reciprocal-exponential -- no finite mean -- so the local mean
# loess computes had no target. On constant-hazard data the plotted level came
# out an order of magnitude too high. The fix weights each point by its
# person-time and keeps the event-free intervals, which makes the local fit a
# local events/person-time rate.
#
# This calls the SHIPPED private method, not a re-implementation: the method only
# touches image$state, a handful of options and three private helpers, so it can
# be rebound onto a stub environment. `print` is stubbed to capture the ggplot
# instead of opening a device.

test_that("smoothed hazard curve recovers a constant hazard", {
    skip_if_not_installed("survival")
    skip_if_not_installed("ggplot2")

    # Whichever of the two shipping packages is actually loaded for this run.
    pkg <- intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces())[1]
    skip_if(is.na(pkg), "neither ClinicoPath nor jsurvival is loaded")
    cls <- tryCatch(get("singlearmClass", envir = asNamespace(pkg)),
                    error = function(e) NULL)
    skip_if(is.null(cls), "singlearmClass not available")

    set.seed(1)
    n <- 300; lambda <- 0.1
    tt <- rexp(n, lambda); cc <- rexp(n, 0.03)
    d <- data.frame(mytime = pmin(tt, cc), myoutcome = as.integer(tt <= cc))

    f <- cls$private_methods$.smoothedHazardPlot
    env <- new.env(parent = environment(f))
    captured <- NULL
    env$self <- list(options = list(
        hazard_smoothing = TRUE,
        timetypeoutput   = "months",
        # jmvcore::.() resolves translations through self$options$translate
        translate        = function(text, n = 1) text))
    env$private <- list(
        .isCompetingRisk       = function(...) FALSE,
        .safeExecute           = function(expr, context = NULL) force(expr),
        .calculateAdaptiveSpan = function(n_points) 0.3,
        .hazardIntervals       = cls$private_methods$.hazardIntervals)
    env$. <- function(text, ...) text
    env$print <- function(x, ...) { captured <<- x; invisible(x) }
    environment(f) <- env

    f(image = list(state = list(name1time = "mytime", name2outcome = "myoutcome",
                                cleanData = d)))

    expect_false(is.null(captured))
    haz <- captured$data$hazard
    expect_true(all(is.finite(haz)))
    # True hazard is 0.1 everywhere. The shipped-before-fix curve ranged
    # 0.00-8.49 on this seed; the weighted fit gives 0.077-0.144.
    expect_gt(median(haz), 0.05)
    expect_lt(max(haz), 0.20)
})

test_that("event-free intervals are kept in the hazard smoother", {
    skip_if_not_installed("survival")

    # Dropping zero-event intervals (the old `inst_hazard > 0` filter) throws
    # away the exposure that makes a rate a rate and biases the curve upward.
    set.seed(2)
    n <- 200
    tt <- rexp(n, 0.1); cc <- rexp(n, 0.05)
    d <- data.frame(time = pmin(tt, cc), status = as.integer(tt <= cc))
    sf <- survival::survfit(survival::Surv(time, status) ~ 1, data = d, type = "fh")
    dt <- diff(c(0, sf$time))
    keep <- sf$n.risk * dt > 0

    # censoring times contribute person-time with zero events
    expect_gt(sum(keep & sf$n.event == 0), 0)

    pooled <- sum(sf$n.event[keep]) / sum((sf$n.risk * dt)[keep])
    expect_gt(pooled, 0.05)
    expect_lt(pooled, 0.20)
})
