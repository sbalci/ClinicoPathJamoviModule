# One record per independent participant; this is left truncation, not
# a general repeated-interval or time-dependent covariate interface.
.fitDelayedEntryCox <- function(data, entrytime, exittime, event, predictors) {
    selected <- c(entrytime, exittime, event, predictors)
    if (!is.data.frame(data) || !nrow(data))
        stop("No observations are available.", call. = FALSE)
    if (length(entrytime) != 1L || length(exittime) != 1L || length(event) != 1L ||
        !length(predictors) || anyNA(selected) || anyDuplicated(selected) ||
        !all(selected %in% names(data)) || anyDuplicated(names(data)))
        stop("Choose distinct columns for entry, exit, event, and baseline covariates.", call. = FALSE)

    entry <- data[[entrytime]]
    exit <- data[[exittime]]
    status <- data[[event]]
    valid_time <- function(x) is.numeric(x) && !is.object(x) && is.null(dim(x)) &&
        all(is.finite(x) & x >= 0)
    if (!valid_time(entry) || !valid_time(exit))
        stop("Entry and exit must be finite, nonnegative numeric times in the same units.",
             call. = FALSE)
    bad <- sum(exit <= entry)
    if (bad)
        stop(sprintf("Exit must be strictly later than entry on every row (%d invalid).", bad),
             call. = FALSE)
    # Nominal numeric columns can arrive from jamovi as factors.
    if (is.factor(status)) {
        status <- as.character(status)
        if (!anyNA(status) && all(status %in% c("0", "1")))
            status <- as.integer(status)
    }
    if (!(is.numeric(status) || is.logical(status)) || !is.null(dim(status)) ||
        anyNA(status) || !all(status %in% c(0, 1)))
        stop("Code event as 0 for censoring and 1 for event. Do not leave missing values.",
             call. = FALSE)
    if (!any(status == 1))
        stop("At least one observed event is required.", call. = FALSE)

    x <- as.data.frame(data[predictors])
    for (i in seq_along(x)) {
        v <- x[[i]]
        if (!is.null(dim(v)) || anyNA(v) ||
            !((is.numeric(v) && all(is.finite(v))) || is.factor(v) || is.logical(v)))
            stop("Covariates must be complete finite numeric, categorical, or logical columns.",
                 call. = FALSE)
        if (is.logical(v))
            v <- factor(v, levels = c(FALSE, TRUE))
        if (is.factor(v)) {
            v <- factor(v, levels = levels(v), ordered = FALSE)
            v <- droplevels(v)
            if (nlevels(v) < 2L)
                stop("Each categorical covariate needs at least two observed levels.", call. = FALSE)
            # Preserve the dataset's level order, but never switch to polynomial
            # contrasts for an ordinal variable or use a session-wide contrast setting.
            attr(v, "contrasts") <- stats::contr.treatment(levels(v), base = 1L)
        }
        x[[i]] <- v
    }

    # Model formula contains only internal identifiers. Source column names
    # remain labels, so spaces, punctuation, and formula operators are safe.
    aliases <- paste0("cov", seq_along(predictors), "_")
    names(x) <- aliases
    model_data <- cbind(data.frame(.entry = entry, .exit = exit, .status = as.integer(status)), x)
    f <- stats::reformulate(aliases, response = "survival::Surv(.entry, .exit, .status)")
    fit <- withCallingHandlers(
        survival::coxph(f, data = model_data, ties = "efron",
                        na.action = stats::na.fail, singular.ok = FALSE, x = TRUE, y = TRUE),
        warning = function(w) {
            stop(paste("Cox estimates withheld:", conditionMessage(w)), call. = FALSE)
        })
    coefficient <- stats::coef(fit)
    se <- sqrt(diag(stats::vcov(fit)))
    if (any(!is.finite(coefficient)) || any(!is.finite(se) | se <= 0))
        stop("Cox estimates withheld: the coefficient or standard error is not estimable.",
             call. = FALSE)

    z <- stats::qnorm(0.975)
    ratios <- exp(cbind(coefficient, coefficient - z * se, coefficient + z * se))
    if (any(!is.finite(ratios) | ratios <= 0))
        stop("Cox estimates withheld: an estimated hazard ratio or interval exceeds numeric limits.",
             call. = FALSE)
    rows <- lapply(seq_along(x), function(i) {
        levels_i <- levels(x[[i]])
        factor_i <- is.factor(x[[i]])
        j <- which(startsWith(names(coefficient), aliases[i]))
        data.frame(
            term = predictors[i],
            level = if (factor_i) levels_i[-1L] else "",
            reference = if (factor_i) levels_i[1L] else "",
            hr = ratios[j, 1L], lower = ratios[j, 2L], upper = ratios[j, 3L],
            p = 2 * stats::pnorm(-abs(coefficient[j] / se[j])),
            row.names = NULL
        )
    })
    list(fit = fit, estimates = do.call(rbind, rows),
         sample = list(participants = fit$n, events = fit$nevent, censored = fit$n - fit$nevent))
}

delayedentrycoxClass <- if (requireNamespace("jmvcore", quietly=TRUE)) R6::R6Class(
    "delayedentrycoxClass",
    inherit = delayedentrycoxBase,
    private = list(
        .init = function() {
            self$results$guidance$setContent(paste0("<p>", .(
                "Use one row per participant and a common time origin and unit. Enter the time at enrollment and the time of event or censoring, not the duration from enrollment. A participant is at risk after entry, through exit."
            ), "</p><p>", .(
                "For attained age, enter age at enrollment and age at event or censoring, both in years. This analysis does not fit competing risks, repeated events, or time-varying predictors."
            ), "</p>"))
        },
        .run = function() {
            opts <- self$options
            if (any(lengths(list(opts$entrytime, opts$exittime, opts$event, opts$predictors)) == 0L))
                return(invisible(NULL))
            result <- tryCatch(
                .fitDelayedEntryCox(self$data, opts$entrytime, opts$exittime, opts$event,
                                    opts$predictors),
                error = function(e) jmvcore::reject(conditionMessage(e)))
            self$results$sample$addRow(rowKey = 1L, values = result$sample)
            for (i in seq_len(nrow(result$estimates)))
                self$results$estimates$addRow(rowKey = i, values = as.list(result$estimates[i, ]))
        }))
