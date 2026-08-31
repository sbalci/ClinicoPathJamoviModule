# Regression guard for the survminer `fit$call$formula` defect class.
#
# survminer::ggsurvplot() -> surv_pvalue() -> .pvalue() does, in survminer's OWN frame:
#
#     sdiff <- survival::survdiff(eval(fit$call$formula), data = data)
#
# and the surv_summary()/.extract.survfit() path behind it runs even with pval = FALSE.
# survfit() records that formula with match.call(), so two shapes break:
#
#   1. the formula was passed through a VARIABLE  -> the call holds a bare symbol
#      -> "object of type 'symbol' is not subsettable"
#   2. the formula names something that lived only in the calling R6 method frame
#      (surv_obj <- Surv(...))                    -> "object 'surv_obj' not found",
#      because survminer evaluates it against namespace:survminer -> base -> globalenv,
#      a chain the method frame is never on.
#
# Both are usually SWALLOWED by a tryCatch that paints a placeholder, a degraded
# base-R plot, or an instructions panel - so the analysis "succeeds" and only the
# picture is wrong. Nothing short of actually driving survminer catches it, which is
# what the probe below does.
#
# Preferred fix at every call site: inline the formula over columns of the data frame
# that is handed to ggsurvplot(data = ...)
#     fit <- survival::survfit(Surv(time, event) ~ group, data = plot_data)
# (see R/progressionsurvival.b.R:295-305). Repairing fit$call$formula afterwards also
# works but drags the method frame's environment - self, private, data - along with the
# formula, more than doubling the serialized payload, so it is only safe where the fit
# never reaches image$setState() (see R/patientsimilarity.b.R:~852-910).

library(ClinicoPath)


# ---- the invariant -----------------------------------------------------------------

# A fit is survminer-safe iff the formula survminer recovers from fit$call$formula can be
# rebuilt and resolved WITHOUT the frame that built it. Reproduced here the way survminer
# does it: eval() in a frame that holds none of the builder's locals, then resolve the
# terms against `data` only. Verified to agree with real ggsurvplot() behaviour on all six
# construction shapes - see the self-check test below.
survminer_formula_ok <- function(fit, data) {
    recorded <- fit$call$formula
    if (is.null(recorded) || is.name(recorded))
        return(FALSE)                      # bare symbol: "not subsettable"
    isTRUE(tryCatch({
        f <- eval(recorded)                # survminer's frame, not the builder's
        stats::model.frame(f, data = as.data.frame(data))
        TRUE
    }, error = function(e) FALSE))
}

expect_survminer_safe <- function(fit, data, label) {
    if (!inherits(fit, "survfit"))
        return(invisible(NULL))            # ggsurvplot also accepts lists of fits
    testthat::expect_false(is.null(data), label = paste0(label, ": data= reached survminer"))
    testthat::expect_true(survminer_formula_ok(fit, data),
                          label = paste0(label, ": fit$call$formula is [",
                                         paste(deparse(fit$call$formula), collapse = " "),
                                         "] and resolves outside the builder frame"))
}


# ---- driving the analyses ----------------------------------------------------------

# jmvcore's Analysis$.render() returns TRUE whenever the render function returned
# ANYTHING non-NULL, so a renderer that bailed out with FALSE still reports TRUE. The
# renderer's own return value survives on image$plot, so check that too.
render_ok <- function(image) {
    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    isTRUE(image$.render()) && !identical(image$plot, FALSE)
}

# Runs `expr` with survminer::ggsurvplot intercepted, records every (fit, data) pair the
# module hands over, and still calls the real function so rendering failures surface too.
# Both the analysis run and the $.render() belong inside the block: some modules call
# survminer from .run(), others from the render function.
with_survminer_probe <- function(expr) {
    seen <- list()
    real_ggsurvplot <- survminer::ggsurvplot
    testthat::local_mocked_bindings(
        ggsurvplot = function(fit, data = NULL, ...) {
            seen[[length(seen) + 1L]] <<- list(fit = fit, data = data)
            real_ggsurvplot(fit, data = data, ...)
        },
        .package = "survminer")
    force(expr)
    seen
}

# `min` is 0 for the two backends that call a bare, @importFrom-ed ggsurvplot(): R copies
# an imported binding into the package's imports env at load time, so the mock cannot
# reach them once ClinicoPath is installed. Their survminer failures are not swallowed,
# so render_ok() alone is a real assertion there.
expect_survminer_fits <- function(seen, label, min = 1L) {
    testthat::expect_gte(length(seen), min)
    for (i in seq_along(seen))
        expect_survminer_safe(seen[[i]]$fit, seen[[i]]$data, paste0(label, " fit ", i))
}

# histopathology carries a handful of NAs; several backends reject those up front for
# reasons that have nothing to do with this defect.
km_data <- function(cols) {
    d <- as.data.frame(histopathology)
    d[stats::complete.cases(d[cols]), cols, drop = FALSE]
}

SURV <- c("OverallTime", "Outcome")


# ---- the invariant is not vacuous --------------------------------------------------

test_that("the invariant rejects exactly the constructions survminer chokes on", {
    # Built inside an R6 method, the way every backend builds them. A top-level version of
    # this test passes for the wrong reason: at top level the "method local" is in
    # globalenv, which IS on survminer's lookup chain.
    builder <- R6::R6Class("builder", public = list(
        make = function(kind, data) {
            surv_obj <- survival::Surv(data$time, data$event)
            f <- survival::Surv(time, event) ~ g
            switch(kind,
                inline   = survival::survfit(survival::Surv(time, event) ~ g, data = data),
                varform  = survival::survfit(f, data = data),
                localobj = survival::survfit(surv_obj ~ g, data = data),
                colobj   = { data$surv_obj <- surv_obj
                             survival::survfit(surv_obj ~ g, data = data) },
                repaired = { fit <- survival::survfit(f, data = data)
                             fit$call$formula <- f
                             fit })
        }))$new()

    set.seed(11)
    d <- data.frame(time = stats::rexp(60, 0.05),
                    event = stats::rbinom(60, 1, 0.6),
                    g = factor(sample(c("a", "b"), 60, TRUE)))
    expected <- c(inline = TRUE, varform = FALSE, localobj = FALSE,
                  colobj = TRUE, repaired = TRUE)

    for (kind in names(expected)) {
        fit <- builder$make(kind, d)
        probe <- d
        if (kind == "colobj")
            probe$surv_obj <- survival::Surv(d$time, d$event)
        gc()                                   # the builder frame is genuinely gone
        expect_equal(survminer_formula_ok(fit, probe), unname(expected[kind]), info = kind)

        # ... and the invariant agrees with what ggsurvplot actually does.
        really_ok <- isTRUE(tryCatch({
            grDevices::pdf(NULL); on.exit(grDevices::dev.off(), add = TRUE)
            print(survminer::ggsurvplot(fit, data = probe, pval = TRUE))
            TRUE
        }, error = function(e) FALSE))
        expect_equal(really_ok, unname(expected[kind]), info = paste("ggsurvplot", kind))
    }
})


# ---- per-analysis guards -----------------------------------------------------------

test_that("jvisr hands survminer a self-contained formula", {
    skip_if_not_installed("visR")
    # jvisr only reaches survminer when visR is missing or throws - and that is exactly the
    # branch that builds its own survfit() from a formula VARIABLE. With visR healthy the
    # survminer fallback is dead code and this test would be vacuous, so visR is failed on
    # purpose to put .jvisr_plot_fallback under test.
    local_mocked_bindings(estimate_KM = function(...) stop("forced"), .package = "visR")
    local_mocked_bindings(visr = function(...) stop("forced"), .package = "visR")

    d <- km_data(c(SURV, "Group"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::jvisr(
            data = d, analysis_type = "kaplan_meier", time_var = "OverallTime",
            event_var = "Outcome", strata_var = "Group", aval_var = NULL, cnsr_var = NULL,
            theme_style = "classic", confidence_interval = FALSE,
            # fun_type's default "surv" is passed straight through as ggsurvplot(fun=),
            # which rejects it ("Unrecognized survival function argument") - a separate
            # defect that kills this fallback before survminer sees the formula.
            fun_type = "event")
        expect_true(render_ok(res$plot))
    })
    expect_survminer_fits(seen, "jvisr")
})

test_that("survivalfeaturerank hands survminer a self-contained formula", {
    d <- km_data(c(SURV, "Sex", "Group", "Grade_Level", "LVI"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::survivalfeaturerank(
            data = d, survtime = "OverallTime", event = "Outcome", eventLevel = "1",
            features = c("Sex", "Group", "Grade_Level", "LVI"),
            showTopKM = TRUE, topN = 2)
        expect_true(render_ok(res$kmPlot1))
        expect_true(render_ok(res$kmPlot2))
    })
    expect_survminer_fits(seen, "survivalfeaturerank")
})

test_that("survivalcont hands survminer a self-contained formula", {
    d <- km_data(c(SURV, "Age"))
    d$Outcome <- factor(d$Outcome)
    seen <- with_survminer_probe({
        res <- ClinicoPath::survivalcont(
            data = d, elapsedtime = "OverallTime", outcome = "Outcome", outcomeLevel = "1",
            contexpl = "Age", dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
            sc = TRUE, findcut = TRUE)
        expect_true(render_ok(res$plot5))
    })
    expect_survminer_fits(seen, "survivalcont")
})

test_that("epidemiosurvival hands survminer a self-contained formula", {
    # Whole-number follow-up times: the two-group branch runs epitools::rateratio on the
    # accumulated person-time, and fractional person-time makes it die ("missing value
    # where TRUE/FALSE needed") inside the tryCatch that also guards the survfit, leaving
    # .survival_results NULL and the plot never reaching survminer.
    d <- km_data(c(SURV, "Group"))
    d$OverallTime <- round(d$OverallTime)
    seen <- with_survminer_probe({
        res <- ClinicoPath::epidemiosurvival(
            data = d, time_var = "OverallTime", event_var = "Outcome",
            exposure_var = "Group", age_var = NULL, calendar_time = NULL,
            population_weights = NULL, subcohort_indicator = NULL,
            stratification_vars = NULL, competing_events = NULL, survival_curves = TRUE)
        expect_true(render_ok(res$survival_curves_plot))
    })
    expect_survminer_fits(seen, "epidemiosurvival")
})

test_that("concordanceindex hands survminer a self-contained formula", {
    d <- km_data(c(SURV, "Age"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::concordanceindex(
            data = d, time = "OverallTime", event = "Outcome", event_code = "1",
            predictor = "Age", additional_predictors = NULL, stratify_by = NULL,
            plot_risk_group_kaplan_meier = TRUE)
        expect_true(render_ok(res$riskGroupKMPlot))
    })
    expect_survminer_fits(seen, "concordanceindex")
})

test_that("survivalendpoints hands survminer a self-contained formula", {
    # inputType = "dates" cannot read histopathology's "2019.07.08 00:00:00" strings
    # (unrelated defect), so the numeric path is used with a zero baseline.
    d <- km_data(c("ID", SURV))
    d$Start <- 0
    seen <- with_survminer_probe({
        res <- ClinicoPath::survivalendpoints(
            data = d, patientId = "ID", startDate = "Start", lastFollowup = "OverallTime",
            deathEvent = "Outcome", inputType = "numeric",
            calculateOS = TRUE, showKMPlot = TRUE)
        expect_true(render_ok(res$kmPlot))
    })
    expect_survminer_fits(seen, "survivalendpoints")
})

test_that("clinicalheatmap hands survminer a self-contained formula", {
    # rowVar must be character and splitRows >= 2, or the survival block dies inside its
    # empty `error = function(e) {}` handler - on a numeric-vs-character dplyr join, or on
    # a single cluster - and kmPlot's state is silently never set.
    d <- km_data(c("ID", SURV, "Group", "MeasurementA"))
    d$Outcome <- factor(d$Outcome)
    d$ID <- as.character(d$ID)
    seen <- with_survminer_probe({
        res <- ClinicoPath::clinicalheatmap(
            data = d, rowVar = "ID", colVar = "Group", valueVar = "MeasurementA",
            clusterRows = TRUE, splitRows = 2, survivalAnalysis = TRUE,
            survivalTime = "OverallTime",
            survivalEvent = "Outcome", survivalEventLevel = "1")
        expect_true(render_ok(res$clusterSurvival$kmPlot))
    })
    expect_survminer_fits(seen, "clinicalheatmap")
})

test_that("patientsimilarity hands survminer a self-contained formula", {
    d <- km_data(c(SURV, "Age", "MeasurementA", "MeasurementB", "Measurement1"))
    d$Outcome <- factor(d$Outcome)
    seen <- with_survminer_probe({
        res <- ClinicoPath::patientsimilarity(
            data = d, vars = !!c("Age", "MeasurementA", "MeasurementB", "Measurement1"),
            method = "pca", performClustering = TRUE, nClusters = 2,
            survivalAnalysis = TRUE, survivalTime = "OverallTime",
            survivalEvent = "Outcome", survivalEventLevel = "1")
        expect_true(render_ok(res$survivalPlot))
    })
    expect_survminer_fits(seen, "patientsimilarity")
})

test_that("jiwillsurvive hands survminer a self-contained formula", {
    # jiwillsurvive's Images carry no renderFun and its plot function print()s straight to
    # the device, so $.render() cannot be the assertion here - the probe is. Its ggsurvplot
    # failures are swallowed into results$instructions, which is checked too.
    # show_statistics = FALSE dodges an unrelated crash (survdiff has no $df, so
    # .outputLogRankTest's pchisq(chisq, NULL) dies before survminer is ever reached).
    d <- km_data(c(SURV, "Group"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::jiwillsurvive(
            data = d, analysis_type = "survival_model", time_var = "OverallTime",
            event_var = "Outcome", group_var = "Group", show_statistics = FALSE)
        expect_false(grepl("Error creating survival plot", res$instructions$content,
                           fixed = TRUE))
    })
    expect_survminer_fits(seen, "jiwillsurvive")
})

test_that("progressionsurvival hands survminer a self-contained formula", {
    d <- km_data(c("ID", SURV, "Group"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::progressionsurvival(
            data = d, time_var = "OverallTime", progression_var = "Outcome",
            death_var = NULL, treatment_var = "Group", stratification_vars = NULL,
            patient_id = "ID", baseline_vars = NULL, kaplan_meier_curves = TRUE)
        expect_true(render_ok(res$km_plot))
    })
    expect_survminer_fits(seen, "progressionsurvival", min = 0L)   # bare imported call
})

test_that("mediansurvival hands survminer a self-contained formula", {
    # Single-group only: the grouped path dies before survminer on an unrelated defect -
    # quantile(km_fit, 0.5)$quantile is a MATRIX for a stratified fit, so
    # .extractGroupedMedianSurvival builds columns named X50/X50.1 and
    # .createSurvivalPlot's median_data$median[i] is length zero
    # ("argument is of length zero"). Restore the grouped case once that is fixed.
    d <- km_data(SURV)
    seen <- with_survminer_probe({
        res <- ClinicoPath::mediansurvival(
            data = d, elapsedtime = "OverallTime", outcome = "Outcome",
            explanatory = NULL, outcomeLevel = "1")
        expect_true(render_ok(res$survivalPlot))
    })
    expect_survminer_fits(seen, "mediansurvival", min = 0L)        # bare imported call
})

test_that("ihccluster hands survminer a self-contained formula", {
    # .plotSurvival reads time, event AND clusters out of self$results$summary$state,
    # so all three are nrow(state$df) long by construction - the survival columns ride
    # along as auxVars through .prepareData's row filtering.
    d <- km_data(c(SURV, "LVI", "PNI", "Anti-X-intensity", "Anti-Y-intensity"))
    seen <- with_survminer_probe({
        res <- ClinicoPath::ihccluster(
            data = d, catVars = c("LVI", "PNI"),
            contVars = c("Anti-X-intensity", "Anti-Y-intensity"),
            survivalTime = "OverallTime", survivalEvent = "Outcome",
            nClusters = 2)
        expect_true(render_ok(res$survivalPlot))
    })
    expect_survminer_fits(seen, "ihccluster")
})

test_that("alluvialSurvival hands survminer a self-contained formula", {
    skip(paste("alluvialSurvival needs one row per patient per timepoint;",
               "histopathology is one row per patient. It also dies before survminer on",
               "max(Outcome) over a factor, because survivalVar is permitted: [factor]."))
})
