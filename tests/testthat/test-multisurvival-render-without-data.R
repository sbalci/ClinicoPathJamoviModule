# Regression tests for the 2026-09-15 jamovi library audit (jsurvival) HIGH finding.
#
# jmvcore sets private$.data <- NULL as soon as .run() returns, and re-reads the
# dataset for a redraw or export only for an Image that declares
# `requiresData: true`. plot_adj and survMetricsPlot do not declare it, yet their
# renderers used to refit the Cox model from self$data -- so on resize, on
# reopening a saved .omv and on Export they hit "Data contains no (complete) rows"
# and put the whole analysis into an error state.
#
# Ordinary tests could not see it: the R wrapper passes `data =` (jmvcore never
# clears data it was handed) and the run that fitted the model still had warm
# caches. These tests recreate the redraw condition -- no dataset, empty caches --
# and check that the renderers draw from image$state alone.

.msrd_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("multisurvival", envir = .cand, inherits = FALSE)) {
            .msrd_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.msrd_ns), "multisurvival not available in this distribution")

.msrd_quiet <- function(expr) {
    f <- tempfile(); sink(f); on.exit(sink(), add = TRUE); suppressWarnings(force(expr))
}

.msrd_data <- function() {
    set.seed(11); n <- 240
    grp <- factor(sample(c("A", "B"), n, TRUE))
    age <- stats::rnorm(n, 60, 8)
    data.frame(t   = round(stats::rexp(n, 0.03 * exp(0.7 * (grp == "B") + 0.03 * (age - 60))), 1) + 0.1,
               ev  = stats::rbinom(n, 1, 0.8),
               grp = grp, age = age)
}

.msrd_run <- function(...) {
    .msrd_quiet(do.call(get("multisurvival", envir = .msrd_ns),
        c(list(data = .msrd_data(), elapsedtime = "t", outcome = "ev", outcomeLevel = NULL,
               explanatory = "grp", contexpl = "age",
               dod = NULL, dooc = NULL, awd = NULL, awod = NULL), list(...))))
}

# What jamovi hands a renderer on resize / .omv reopen / export: an analysis with
# no dataset and none of the per-run caches.
.msrd_as_redraw <- function(res) {
    analysis <- res$.__enclos_env__$private$.parent
    expect_true(inherits(analysis, "multisurvivalClass"))
    p <- analysis$.__enclos_env__$private
    p$.data <- NULL
    p$.dataCache <- NULL
    p$.dataComputed <- FALSE
    p$.coxCache <- NULL
    p$.coxComputed <- FALSE
    invisible(analysis)
}

.msrd_render <- function(image) {
    grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
    isTRUE(.msrd_quiet(image$.render()))
}

test_that("adjusted curves redraw from state alone, with no dataset and no cached model", {
    res <- .msrd_run(ac = TRUE, adjexplanatory = "grp", ac_method = "average")
    st <- res$plot_adj$state
    expect_setequal(names(st), c("curves", "finegray"))   # no dataset, no coxph fit
    expect_true(is.data.frame(st$curves) && nrow(st$curves) > 0)

    .msrd_as_redraw(res)
    expect_true(.msrd_render(res$plot_adj))
})

test_that("the Brier-score plot redraws from state alone", {
    skip_if_not_installed("riskRegression")
    res <- .msrd_run(show_survmetrics = TRUE, survmetrics_show_plots = TRUE)
    st <- res$survMetricsPlot$state
    expect_setequal(names(st), c("br", "ref"))
    expect_true(is.data.frame(st$br) && nrow(st$br) > 0)

    .msrd_as_redraw(res)
    expect_true(.msrd_render(res$survMetricsPlot))
})

test_that("HR, forest and KM plot state carries only the competing-risk flag", {
    # These images declare requiresData and rebuild data and model from self$data;
    # the cleaned dataset and the coxph fit are no longer serialised into the .omv.
    res <- .msrd_run(hr = TRUE, km = TRUE)
    expect_identical(names(res$plot$state), "has_competing")
    expect_identical(names(res$plot3$state), "has_competing")
    expect_identical(names(res$plotKM$state), "has_competing")
    expect_false(isTRUE(res$plot$state$has_competing))
})
