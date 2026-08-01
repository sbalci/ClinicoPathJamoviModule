# Extracted from test-singlearm-hazard-weighting.R:42

# test -------------------------------------------------------------------------
skip_if_not_installed("survival")
skip_if_not_installed("ggplot2")
pkg <- intersect(c("ClinicoPath", "jsurvival"), loadedNamespaces())[1]
skip_if(is.na(pkg), "neither ClinicoPath nor jsurvival is loaded")
cls <- tryCatch(get("singlearmClass", envir = asNamespace(pkg)),
                    error = function(e) NULL)
skip_if(is.null(cls), "singlearmClass not available")
set.seed(1)
n <- 300
lambda <- 0.1
tt <- rexp(n, lambda)
cc <- rexp(n, 0.03)
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
        .safeExecute           = function(expr, context = NULL) eval(substitute(expr)),
        .calculateAdaptiveSpan = function(n_points) 0.3,
        .hazardIntervals       = cls$private_methods$.hazardIntervals)
