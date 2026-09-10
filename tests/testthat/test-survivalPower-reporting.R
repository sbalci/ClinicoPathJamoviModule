# Regression tests for the second survivalPower audit.
#
# The Schoenfeld core was verified correct against Monte Carlo (see
# test-survivalPower-release-review.R). Every defect these tests pin was in the
# layer around it: text that contradicted the computation, a sample size that
# sized the wrong thing, and curves drawn for a different design than the one
# reported. All of them produced plausible output, so none would fail loudly.

sp <- function(...) {
    analysis <- survivalPowerClass$new(
        options = do.call(survivalPowerOptions$new, list(...)),
        data = data.frame(x = 1)
    )
    suppressWarnings(try(analysis$run(), silent = TRUE))
    analysis
}

sp_value <- function(analysis) {
    as.character(analysis$results$power_summary$asDF$calculated_value[1])
}

sp_text <- function(content) {
    gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(content)))
}

sp_n <- function(analysis) {
    as.numeric(sub(".*Total Sample Size: ([0-9]+).*", "\\1", sp_value(analysis)))
}


test_that("narrative output reports the hazard ratio the analysis actually used", {
    # Regression: six sites read self$options$effect_size raw. Under
    # effect_size_type "median_ratio" a value of 1.5 means HR 0.667 -- a benefit --
    # but the plain-language summary announced "hazard ratio of 1.5" and
    # "Large increase in risk expected", inverting the conclusion.
    a <- sp(effect_size_type = "median_ratio", effect_size = 1.5,
            show_summary = TRUE, show_glossary = TRUE)

    expect_equal(a$.__enclos_env__$private$.get_effect_hr(), 1 / 1.5)

    summary_text <- sp_text(a$results$natural_language_summary$content)
    expect_match(summary_text, "hazard ratio of 0.667", fixed = TRUE)
    expect_match(summary_text, "lower hazard", fixed = TRUE)
    expect_false(grepl("increase in hazard", summary_text, fixed = TRUE))
    # The derived HR is not the number in the box, so say where it came from.
    expect_match(summary_text, "Effect Size Conversion", fixed = TRUE)

    # The sensitivity table must perturb around the same hazard ratio.
    s <- sp(effect_size_type = "median_ratio", effect_size = 1.5,
            sensitivity_analysis = TRUE)$results$sensitivity_analysis_table$asDF
    base <- as.numeric(s$base_case[s$parameter == "Hazard Ratio"])
    expect_equal(base, round(1 / 1.5, 2))  # the column is formatted to 2 dp
    expect_false(isTRUE(all.equal(base, 1.5)))  # never the raw median ratio
})


test_that("a multi-arm trial is sized as a trial, not as one pairwise comparison", {
    # Regression: the headline was the two-arm N at the adjusted alpha. The
    # multi-arm table then split that across the arms and reported 60.9% power
    # per comparison for a design the user asked to have 80%.
    pairwise <- sp(alpha_level = 0.025)
    trial <- sp(study_design = "multi_arm", number_of_arms = 3,
                multiple_comparisons = "dunnett")

    # k arms sharing one control: N = N_pair * (r + k - 1) / (1 + r); at r = 1, 1.5x.
    expect_equal(sp_n(trial), ceiling(sp_n(pairwise) * 1.5))

    tbl <- trial$results$multi_arm_table$asDF
    expect_equal(nrow(tbl), 2)
    # Each comparison must now hit the requested power.
    expect_true(all(abs(tbl$power - 80) < 1.5))
    expect_equal(tbl$sample_size_per_arm[1], as.integer(round(sp_n(pairwise) / 2)))

    # Two arms is not a multi-arm trial and must be untouched.
    expect_equal(
        sp_n(sp(study_design = "multi_arm", number_of_arms = 2, multiple_comparisons = "none")),
        sp_n(sp())
    )

    # Unequal allocation uses r, not 1/k.
    unequal <- sp(study_design = "multi_arm", number_of_arms = 3,
                  multiple_comparisons = "bonferroni", allocation_ratio = 2)
    pairwise_2to1 <- sp(alpha_level = 0.025, allocation_ratio = 2)
    expect_equal(sp_n(unequal), ceiling(sp_n(pairwise_2to1) * (2 + 3 - 1) / (1 + 2)))
})


test_that("total study power is a probability, not an invented multiplier", {
    # Regression: with adjustment it was individual_power * a made-up factor
    # (1/(k-1), 1/sqrt(k-1), 0.9, 0.85); without adjustment it assumed the
    # comparisons were independent although they share a control arm.
    skip_if_not_installed("mvtnorm")

    h <- sp()$.__enclos_env__$private
    set.seed(7)
    for (p in c(0.5, 0.8)) {
        for (k in c(2, 4)) {
            for (rho in c(0, 0.5)) {
                analytic <- h$.disjunctive_power(p, 0.025, k, rho)
                z <- qnorm(1 - 0.025 / 2)
                ncp <- z + qnorm(p)
                sigma <- matrix(rho, k, k)
                diag(sigma) <- 1
                chol_s <- chol(sigma)
                simulated <- mean(replicate(
                    40000, any(ncp + as.vector(rnorm(k) %*% chol_s) > z)
                ))
                expect_equal(analytic, simulated, tolerance = 0.01,
                    info = sprintf("p=%.1f k=%d rho=%.1f", p, k, rho))
            }
        }
    }

    # A correlated set must not beat the independent one.
    expect_lt(h$.disjunctive_power(0.8, 0.025, 3, 0.5),
              h$.disjunctive_power(0.8, 0.025, 3, 0.0))
})


test_that("the copy-ready sentence states the test that was actually run", {
    # Regression: "to detect a hazard ratio of 1 ... at a 2% significance level
    # (two-sided)" for a one-sided non-inferiority design at alpha 0.025 -- wrong
    # objective, wrong rounding, wrong sidedness, and the margin never mentioned.
    ni <- sp(test_type = "non_inferiority", ni_type = "relative_margin",
             effect_size = 1.0, ni_margin = 1.25, alpha_level = 0.025,
             show_interpretation = TRUE)
    sentence <- sp_text(ni$results$clinical_interpretation$content)

    expect_match(sentence, "rule out a hazard ratio of 1.25", fixed = TRUE)
    expect_match(sentence, "2.5% significance level (one-sided)", fixed = TRUE)
    # round(0.025 * 100) printed "2%" here and in five other narrative sites.
    expect_false(grepl("2% significance", sentence, fixed = TRUE))
    expect_false(grepl(
        "2% significance",
        sp_text(sp(test_type = "non_inferiority", ni_type = "relative_margin",
                   effect_size = 1.0, ni_margin = 1.25, alpha_level = 0.025,
                   show_summary = TRUE)$results$natural_language_summary$content),
        fixed = TRUE
    ))

    # Superiority keeps the usual wording.
    sup <- sp_text(sp(show_interpretation = TRUE)$results$clinical_interpretation$content)
    expect_match(sup, "detect a hazard ratio of 0.75", fixed = TRUE)
    expect_match(sup, "5% significance level (two-sided)", fixed = TRUE)
})


test_that("regulatory guidance matches the convention for the test being run", {
    # Regression: one-sided 0.025 IS the non-inferiority convention, but the row
    # flagged it "Conservative" and said to justify it to the regulator.
    ni <- sp(test_type = "non_inferiority", ni_type = "relative_margin",
             effect_size = 1.0, ni_margin = 1.25, alpha_level = 0.025,
             show_interpretation = TRUE)
    row <- subset(ni$results$regulatory_table$asDF, regulatory_aspect == "Significance Level")

    expect_match(row$requirement, "One-sided", fixed = TRUE)
    expect_equal(row$compliance_status, "Standard")
    expect_false(grepl("Justify", row$recommendation, fixed = TRUE))

    # And the non-inferiority inflation is derived, not the old fixed "20-50%".
    ni_tbl <- ni$results$non_inferiority_table$asDF
    claim <- ni_tbl$clinical_interpretation[ni_tbl$parameter == "Sample Size Requirement"]
    expect_false(grepl("20-50%", claim, fixed = TRUE))
    expect_match(claim, "^Requires [0-9]+% of the events")
})


test_that("plots are drawn for the design that was reported", {
    # Regression 1: the hazard-ratio grid included exactly 1.0, where the
    # log-rank solver stops, so sapply threw and the whole sensitivity plot
    # rendered blank -- for every assumed HR at or above 0.70, the default included.
    for (hr in c(0.60, 0.70, 0.75, 0.85, 0.95)) {
        state <- sp(effect_size = hr, sensitivity_analysis = TRUE)$results$sensitivity_plot$state
        expect_false(is.null(state), info = paste("HR", hr, "must produce plot data"))
        expect_true(all(abs(state$data$hazard_ratio - 1) > 1e-8))
    }

    # Regression 2: the curves used the raw alpha and no design scaling, so a
    # multi-arm plot read 583 where the headline said 706.
    trial <- sp(study_design = "multi_arm", number_of_arms = 3,
                multiple_comparisons = "bonferroni")
    curve <- trial$results$sample_size_plot$state$data
    at_design <- curve$sample_size[which.min(abs(curve$hazard_ratio - 0.75))]
    expect_equal(at_design, sp_n(trial))
})


test_that("analysis-specific interpretation and detail tables are populated", {
    # Regression 1: the module emitted "Statistical Power: 64.2 %" with a space,
    # which its own regex \\d+\\.?\\d*% could not match, so the Power
    # Interpretation block was silently dropped from every power analysis.
    a <- sp(analysis_type = "power", sample_size_input = 400, show_interpretation = TRUE)
    expect_match(sp_value(a), "^Statistical Power: [0-9.]+% ")
    expect_match(sp_text(a$results$clinical_interpretation$content),
                 "Power Interpretation", fixed = TRUE)

    # Regression 2: a third copy of the effect-size solver with a hardcoded
    # c(0.1, 1.5) bracket printed "Not determined" in the detail table while the
    # summary above it reported a value.
    b <- sp(analysis_type = "effect_size", sample_size_input = 400)
    detail <- b$results$effect_size_results$asDF
    reported <- detail$value[detail$parameter == "Minimum Detectable HR"]
    expect_false(identical(reported, "Not determined"))
    expect_match(sp_value(b), reported, fixed = TRUE)
})


test_that("an unreachable event target explains itself", {
    # Regression: with dropout the per-subject event probability has a ceiling, so
    # a fixed n can never reach some event targets. The answer "Unable to
    # determine study duration with current settings" was correct but told the
    # user nothing actionable.
    a <- sp(analysis_type = "duration", sample_size_input = 400)
    expect_match(sp_value(a), "No duration achieves")
    expect_match(sp_value(a), "yield at most")

    # A feasible design still returns a duration.
    b <- sp(analysis_type = "duration", sample_size_input = 800)
    expect_match(sp_value(b), "Required Study Duration")
})


test_that("the glossary describes the allocation ratio the code implements", {
    # Regression: the glossary said "2:1 means twice as many in treatment group";
    # .allocation_props puts twice as many in CONTROL. A user following the
    # glossary got the reverse of the allocation they intended.
    h <- sp()$.__enclos_env__$private
    props <- h$.allocation_props(2)
    expect_gt(props$control, props$treatment)

    text <- sp_text(sp(show_glossary = TRUE)$results$statistical_glossary$content)
    expect_match(text, "twice as many in the control group", fixed = TRUE)
    expect_false(grepl("twice as many in treatment group", text, fixed = TRUE))
})


test_that("Cox honours the same design adjustments as log-rank", {
    # Regression: Cox read alpha_level raw and applied no design factors, so a
    # multi-arm or cluster-randomised Cox design silently ignored both.
    cox_plain <- sp(test_type = "cox_regression")
    cox_multi <- sp(test_type = "cox_regression", study_design = "multi_arm",
                    number_of_arms = 3, multiple_comparisons = "bonferroni")
    expect_gt(sp_n(cox_multi), sp_n(cox_plain))

    cox_cluster <- sp(test_type = "cox_regression", study_design = "cluster_randomized",
                      cluster_size = 50, icc = 0.05)
    expect_equal(sp_n(cox_cluster), ceiling(sp_n(cox_plain) * (1 + 49 * 0.05)),
                 tolerance = 2)
})


test_that("non-uniform accrual is refused rather than silently treated as uniform", {
    # Regression: .adjust_sample_for_accrual() raised an R warning -- invisible in
    # jamovi -- and returned the uniform-accrual sample size, while the
    # assumptions table printed the chosen pattern as though it had been applied.
    for (pattern in c("linear_increasing", "exponential", "custom")) {
        a <- sp(accrual_pattern = pattern)
        expect_true(is.na(sp_value(a)), info = paste(pattern, "must not report a number"))
        expect_match(sp_text(a$results$notices$content), "Accrual Pattern Not Supported",
                     fixed = TRUE, info = pattern)
    }
    expect_match(sp_value(sp(accrual_pattern = "uniform")), "Total Sample Size")
})


test_that("simulation validation runs only for the test it actually simulates", {
    # Regression: the validator always simulates a two-sided superiority log-rank
    # test and compares it with the superiority formula. Under non-inferiority the
    # table agreed with itself while disagreeing with the NI power reported above.
    skip_if_not_installed("survival")

    ni <- sp(test_type = "non_inferiority", ni_type = "relative_margin",
             effect_size = 1.0, ni_margin = 1.25, alpha_level = 0.025,
             run_simulation_validation = TRUE, simulation_runs = 1000)
    expect_equal(ni$results$simulation_validation_table$rowCount, 0)
    expect_match(sp_text(ni$results$notices$content),
                 "Simulation Validation Not Applicable", fixed = TRUE)

    lr <- sp(run_simulation_validation = TRUE, simulation_runs = 1000)
    expect_gt(lr$results$simulation_validation_table$rowCount, 0)
})


test_that("the Monte Carlo loop is interruptible and a restart is not swallowed", {
    # Regression: up to 100,000 survdiff fits ran with no checkpoint, so changing
    # an option could not interrupt them. private$.checkpoint() raises a restart
    # as simpleError(code = "restart"); both the survdiff tryCatch inside the loop
    # and the one around it catch every error, so a misplaced checkpoint would
    # either keep looping or report the user's option change as a failed simulation.
    skip_if_not_installed("survival")

    make <- function() {
        survivalPowerClass$new(
            options = survivalPowerOptions$new(
                run_simulation_validation = TRUE, simulation_runs = 1000
            ),
            data = data.frame(x = 1)
        )$.__enclos_env__$private
    }

    # Outside jamovi there is no callback: checkpoint is a no-op.
    expect_false(is.null(make()$.run_simulation_analysis()))

    # Engine requests a restart.
    p <- make()
    calls <- 0L
    p$.checkpointCB <- function(results) {
        calls <<- calls + 1L
        "restart"
    }
    err <- tryCatch({
        p$.run_simulation_analysis()
        NULL
    }, error = function(e) e)

    expect_false(is.null(err))
    expect_identical(err$code, "restart")
    expect_equal(calls, 1L)  # stopped at the first checkpoint, not looped past it
    titles <- vapply(p$.noticeList, function(n) n$title, character(1))
    expect_false("Simulation Validation Unavailable" %in% titles)

    # Engine says continue: every checkpoint is reached and the run completes.
    q <- make()
    seen <- 0L
    q$.checkpointCB <- function(results) {
        seen <<- seen + 1L
        NULL
    }
    expect_false(is.null(q$.run_simulation_analysis()))
    expect_equal(seen, 1000L %/% 50L)
})


test_that("count options reject fractional values at the option boundary", {
    # Regression: number_of_arms, interim_analyses and simulation_runs were typed
    # Number. number_of_arms = 2.5 reported 819 subjects with an adjusted alpha of
    # 0.0333 and an empty multi-arm table; interim_analyses = 1.6 silently became 1.
    # OptionInteger validates in options$check(), which run() calls -- not when the
    # options object is built. That is where the R wrapper and jamovi enforce it.
    run_opts <- function(...) {
        survivalPowerClass$new(options = survivalPowerOptions$new(...), data = data.frame(x = 1))$run()
    }
    expect_error(run_opts(study_design = "multi_arm", number_of_arms = 2.5), "integer")
    expect_error(run_opts(interim_analyses = 1.6), "integer")
    expect_error(run_opts(simulation_runs = 1500.5), "integer")
    expect_no_error(survivalPowerOptions$new(number_of_arms = 3, interim_analyses = 2)$check())
})


test_that("the narrative interpretation panel is off unless requested", {
    empty <- function(x) length(x) == 0 || all(!nzchar(x))
    expect_true(empty(sp()$results$clinical_interpretation$content))
    on <- sp_text(sp(show_interpretation = TRUE)$results$clinical_interpretation$content)
    expect_match(on, "Clinical Interpretation", fixed = TRUE)
})


test_that("the educational panel states clinically correct definitions", {
    # Regression: Cox called the hazard ratio "relative risk"; non-inferiority was
    # described without its margin; log-rank listed "same censoring patterns" as an
    # assumption; every entry explained a p-value rather than a power calculation.
    edu <- function(...) sp_text(sp(show_explanations = TRUE, ...)$results$educational_explanations$content)

    cox <- edu(test_type = "cox_regression")
    expect_match(cox, "not a relative risk", fixed = TRUE)

    ni <- edu(test_type = "non_inferiority", ni_type = "relative_margin",
              effect_size = 1.0, alpha_level = 0.025)
    expect_match(ni, "by more than the pre-specified margin", fixed = TRUE)

    lr <- edu()
    expect_false(grepl("same censoring patterns", lr, fixed = TRUE))
    expect_match(lr, "does not predict", fixed = TRUE)
})


test_that("the guided workflow does not claim progress it cannot observe", {
    # Regression: step 5 "Your analysis is complete!" fired whenever effect size,
    # power or alpha differed from its default; steps 1-3 were unreachable.
    wf <- sp_text(sp(guided_mode = TRUE, effect_size = 0.6)$results$guided_workflow$content)
    expect_false(grepl("analysis is complete", wf, fixed = TRUE))
    expect_match(wf, "Step 1", fixed = TRUE)
})


test_that("the sensitivity plot draws its base-case highlight", {
    # Regression: the highlight was added inside `+ if (...) { plot <- plot + ... }`,
    # where `plot` was not yet the ggplot, so the red base-case point was silently
    # dropped: 2 layers (line, points) instead of 3.
    a <- sp(sensitivity_analysis = TRUE)
    st <- a$results$sensitivity_plot$state
    expect_true(any(st$data$is_base_case))
    built <- a$.__enclos_env__$private$.build_sensitivity_plot(st$data)
    expect_equal(length(built$layers), 3)
})


test_that("sensitivity scenarios are compared like with like", {
    # Regression: scenarios used a 0.67 average-follow-up approximation without
    # dropout while the base came from the exact headline, so an unchanged
    # parameter still reported a change.
    h <- sp(sensitivity_analysis = TRUE)$.__enclos_env__$private
    base <- h$.sensitivity_value()
    expect_equal(h$.format_sensitivity_impact(base, base), "Minimal impact (<1% change)")

    # A scenario equals a full analysis run at that parameter value.
    expect_equal(h$.sensitivity_value(hr = 0.6), sp_n(sp(effect_size = 0.6)))
    expect_equal(h$.sensitivity_value(median = 18), sp_n(sp(control_median_survival = 18)))
})
