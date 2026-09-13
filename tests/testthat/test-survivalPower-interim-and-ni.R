# Regression tests for the check-function pass on survivalPower.
#
# Each test pins a defect that returned a plausible-looking number rather than
# failing, so a regression here is silent by construction. Ground truth is
# gsDesign or a sibling code path, never the module's own arithmetic.

sp_run <- function(...) {
    opts <- survivalPowerOptions$new(...)
    analysis <- survivalPowerClass$new(options = opts, data = data.frame(x = 1))
    suppressWarnings(try(analysis$run(), silent = TRUE))
    analysis
}

sp_value <- function(analysis) {
    as.character(analysis$results$power_summary$asDF$calculated_value[1])
}

sp_notices <- function(analysis) {
    as.character(analysis$results$notices$content)
}

sp_number <- function(analysis, pattern) {
    as.numeric(sub(pattern, "\\1", sp_value(analysis)))
}


test_that("a non-inferiority effect at or beyond the margin yields no sample size", {
    # Regression: hr >= margin only raised an R warning (invisible in jamovi),
    # then log_effect was flipped with -abs(), so a trial that can never
    # demonstrate non-inferiority was handed a confident sample size.
    for (hr in c(1.25, 1.5, 2.0)) {
        a <- sp_run(
            test_type = "non_inferiority", ni_type = "relative_margin",
            effect_size = hr, ni_margin = 1.25
        )
        expect_true(is.na(sp_value(a)),
            info = paste("HR", hr, "must not report a sample size"))
        expect_match(sp_notices(a), "Effect Not Below Non-inferiority Margin")
    }

    # A feasible margin still sizes, and discloses the one-sided alpha that
    # alpha_level is silently reinterpreted as for non-inferiority.
    ok <- sp_run(
        test_type = "non_inferiority", ni_type = "relative_margin",
        effect_size = 1.0, ni_margin = 1.25
    )
    expect_match(sp_value(ok), "Total Sample Size")
    expect_match(sp_notices(ok), "One-sided Alpha")
})


test_that("interim analyses reach gsDesign and inflate the sample size", {
    # Regression: nSurv() was called with k/timing/sfu, which it has no formals
    # for, so every call threw "unused arguments" and fell through to the fixed
    # design. interim_analyses and alpha_spending had no effect on any number.
    skip_if_not_installed("gsDesign")

    fixed <- sp_run(test_type = "cox_regression", interim_analyses = 0)
    seqd <- sp_run(
        test_type = "cox_regression", interim_analyses = 2,
        alpha_spending = "obrien_fleming"
    )

    expect_match(sp_value(fixed), "Calculated via gsDesign")
    expect_match(sp_value(seqd), "group-sequential, 3 looks")

    pat <- ".*Total Sample Size: ([0-9]+).*"
    expect_gt(sp_number(seqd, pat), sp_number(fixed, pat))

    # The integer enrollment must meet the selected achieved-power calculation;
    # gsSurv's sizing call alone can be a participant short of that target.
    n <- sp_number(seqd, pat)
    power_at <- function(n) {
        a <- sp_run(test_type = "cox_regression", analysis_type = "power",
            interim_analyses = 2, alpha_spending = "obrien_fleming", sample_size_input = n)
        a$.__enclos_env__$private$primary_numbers$power
    }
    expect_gte(power_at(n), 0.8)
    expect_lt(power_at(n - 1), 0.8)
})


test_that("interim boundaries and spend come from the design, not hand arithmetic", {
    # Regression: the O'Brien-Fleming spending function was multiplied by alpha
    # a second time (0.00028 spent at t = 0.5 where 0.00558 is correct), and the
    # boundary was divided by sqrt(t) on top of that, giving 5.14 for a look
    # whose true boundary is 2.77.
    skip_if_not_installed("gsDesign")

    a <- sp_run(
        test_type = "cox_regression", interim_analyses = 2,
        alpha_spending = "obrien_fleming"
    )
    tbl <- a$results$interim_analysis_table$asDF
    expect_equal(nrow(tbl), 2)

    truth <- gsDesign::gsSurv(
        k = 3, timing = c(1, 2, 3) / 3, sfu = gsDesign::sfLDOF,
        test.type = 1, alpha = 0.025, sided = 1, beta = 0.20, lambdaC = log(2) / 12,
        hr = 0.75, eta = -log(1 - 0.05) / 12, ratio = 1,
        R = 24, T = 36, minfup = 12
    )
    expect_equal(tbl$boundary_value, round(truth$upper$bound[1:2], 3))
    expect_true(all(tbl$boundary_value < 4.5))  # the old code reported 5.14

    # Conditional power was three hardcoded literals (75.0 at the first look).
    # Ground truth is direct simulation of the B-value process: with the trend
    # continuing at the design alternative, CP(t) = P(B(1) > b_final | B(t) = theta*t).
    # theta must be THIS design's drift; the fixed-design z_alpha + z_beta
    # understates it (83.9% vs 86.4% at the first of three looks).
    drift <- truth$delta * sqrt(max(truth$n.I))
    b_final <- truth$upper$bound[3]
    set.seed(42)
    for (i in 1:2) {
        t_i <- truth$timing[i]
        simulated <- mean(
            drift * t_i + rnorm(2e5, drift * (1 - t_i), sqrt(1 - t_i)) > b_final
        ) * 100
        expect_equal(tbl$conditional_power[i], simulated, tolerance = 0.01)
    }
    expect_true(all(tbl$conditional_power > 0 & tbl$conditional_power < 100))

    # A spending function is required to derive boundaries at all.
    none <- sp_run(
        test_type = "cox_regression", interim_analyses = 2,
        alpha_spending = "none"
    )
    expect_equal(none$results$interim_analysis_table$rowCount, 0)
    expect_match(sp_notices(none), "No Alpha Spending Function")
})


test_that("Cox duration and detectable effect invert the chosen method", {
    # Regression: the Cox branches duplicated solvers that already existed.
    # Duration used lambda * (1 - dropout/12) -- an annual proportion folded
    # into a monthly hazard -- ignored the HR and ignored staggered entry.
    # Effect size hardcoded a c(0.1, 1.5) bracket with no sign check, so
    # ordinary inputs returned "unable to determine".
    shared <- list(
        sample_size_input = 600, effect_size = 0.75,
        control_median_survival = 12, accrual_period = 24,
        follow_up_period = 12, dropout_rate = 0.05
    )

    for (type in c("duration", "effect_size")) {
        args <- c(shared, list(analysis_type = type))
        lr <- do.call(sp_run, c(args, list(test_type = "log_rank")))
        cox <- do.call(sp_run, c(args, list(test_type = "cox_regression")))

        expect_false(grepl("Unable to determine", sp_value(cox)),
            info = paste(type, "must produce an answer"))
        solved <- cox$.__enclos_env__$private$primary_numbers
        verify_args <- modifyList(args, list(analysis_type = "power", test_type = "cox_regression"))
        if (type == "effect_size") verify_args$effect_size <- solved$hr_detectable else
            verify_args$follow_up_period <- solved$duration - args$accrual_period
        verified <- do.call(sp_run, verify_args)
        expect_equal(verified$.__enclos_env__$private$primary_numbers$power, 0.8,
                     tolerance = 1e-6, info = paste(type, "must invert its own Lachin-Foulkes method"))
    }
})


test_that("Monte Carlo validation is seeded and run once per analysis", {
    # Regression: the shipped path never seeded (simulation_seed was read only
    # by a method nothing called), and two callers each ran a fresh 10,000-run
    # simulation, so one output reported two different empirical powers.
    skip_if_not_installed("survival")

    powers <- vapply(1:2, function(i) {
        sp_run(
            run_simulation_validation = TRUE, simulation_runs = 1000,
            simulation_seed = 7
        )$results$simulation_validation_table$asDF$simulated[1]
    }, numeric(1))
    expect_equal(powers[1], powers[2])

    other <- sp_run(
        run_simulation_validation = TRUE, simulation_runs = 1000,
        simulation_seed = 99
    )$results$simulation_validation_table$asDF$simulated[1]
    expect_false(isTRUE(all.equal(powers[1], other)))

    # Both consumers of the simulation must see the same run.
    both <- sp_run(
        run_simulation_validation = TRUE, sensitivity_analysis = TRUE,
        simulation_runs = 1000, simulation_seed = 7
    )
    sim_row <- both$results$simulation_validation_table$asDF$simulated[1]
    assumption <- subset(
        both$results$assumptions_table$asDF,
        assumption == "Simulation Validation"
    )$impact
    expect_match(assumption, sprintf("%.1f%%", sim_row * 100), fixed = TRUE)
})
