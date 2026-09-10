# Regression tests from the survivalPower release review (2026-09-10).
#
# Sample size multiplied by the design multipliers (multi-arm shared control,
# cluster design effect) and, for Cox only, by the group-sequential inflation.
# Power, detectable effect and duration applied none of them, and log-rank and
# non-inferiority ignored interim looks entirely. Every test below states the
# invariant that exposes that: an analysis run at the sample size another
# analysis produced must return the target it was sized for.

run_sp <- function(...) {
    a <- survivalPowerClass$new(
        options = do.call(survivalPowerOptions$new, list(...)),
        data = data.frame(x = 1)
    )
    suppressWarnings(a$run())
    a
}
nums <- function(a) a$.__enclos_env__$private$primary_numbers
notice_titles <- function(a) vapply(a$.__enclos_env__$private$.noticeList, function(n) n$title, "")

ob_inflation <- function(one_sided_alpha, power = 0.8, looks = 3) {
    max(gsDesign::gsDesign(
        k = looks, test.type = 1, alpha = one_sided_alpha, beta = 1 - power, n.fix = 1,
        timing = seq_len(looks) / looks, sfu = gsDesign::sfLDOF
    )$n.I)
}

test_that("interim looks inflate log-rank and non-inferiority, not only Cox", {
    skip_if_not_installed("gsDesign")
    seq <- list(interim_analyses = 2, alpha_spending = "obrien_fleming")

    lr_fixed <- nums(run_sp())$events
    lr_seq <- nums(do.call(run_sp, seq))$events
    expect_equal(lr_seq, ceiling(lr_fixed * ob_inflation(0.025)))

    ni <- list(test_type = "non_inferiority", ni_type = "relative_margin", effect_size = 1.0, alpha_level = 0.025)
    ni_fixed <- nums(do.call(run_sp, ni))$events
    ni_seq <- nums(do.call(run_sp, c(ni, seq)))$events
    expect_equal(ni_seq, ceiling(ni_fixed * ob_inflation(0.025)))
})

test_that("the Cox sequential design is efficacy-only", {
    skip_if_not_installed("gsDesign")
    # Regression: gsSurv's default test.type = 4 added a hidden non-binding futility
    # bound and sized the 3-look design at 618 instead of 588.
    cox <- run_sp(test_type = "cox_regression", interim_analyses = 2, alpha_spending = "obrien_fleming")
    truth <- gsDesign::gsSurv(
        k = 3, test.type = 1, alpha = 0.025, sided = 1, beta = 0.2, timing = c(1, 2, 3) / 3,
        sfu = gsDesign::sfLDOF, lambdaC = log(2) / 12, hr = 0.75, eta = -log(1 - 0.05) / 12,
        ratio = 1, R = 24, T = 36, minfup = 12
    )
    expect_equal(nums(cox)$n, ceiling(sum(truth$eNC[3, ]) + sum(truth$eNE[3, ])))
})

test_that("sequential power agrees with gsDesign's boundary-crossing probability", {
    skip_if_not_installed("gsDesign")
    # Independent of the inflation inversion: gsProbability integrates the
    # probability of crossing any efficacy boundary for a given drift.
    h <- run_sp(interim_analyses = 2, alpha_spending = "obrien_fleming")$.__enclos_env__$private
    design <- h$.gs_info_design()
    for (info in c(60, 120, 200)) {
        theta <- abs(log(0.75)) * 0.5  # sqrt(p(1-p)) at 1:1 allocation
        fixed_power <- function(f) pnorm(sqrt(info / f) * theta - qnorm(0.975))
        solved <- h$.sequential_power(fixed_power)
        crossing <- gsDesign::gsProbability(
            k = 3, theta = theta, n.I = info * design$timing,
            a = rep(-20, 3), b = design$upper$bound
        )
        expect_equal(solved, sum(crossing$upper$prob[, 1]), tolerance = 0.005,
                     info = paste("information", info))
    }
})

test_that("power, detectable HR and duration honour the design the sample size used", {
    designs <- list(
        multi_arm = list(study_design = "multi_arm", number_of_arms = 3, multiple_comparisons = "bonferroni"),
        cluster = list(study_design = "cluster_randomized", cluster_size = 50, icc = 0.05),
        sequential = list(interim_analyses = 2, alpha_spending = "obrien_fleming"),
        ni_multi_arm = list(test_type = "non_inferiority", ni_type = "relative_margin", effect_size = 1.0,
                            alpha_level = 0.025, study_design = "multi_arm", number_of_arms = 3,
                            multiple_comparisons = "bonferroni")
    )
    for (nm in names(designs)) {
        d <- designs[[nm]]
        n <- nums(do.call(run_sp, d))$n
        power <- nums(do.call(run_sp, c(d, list(analysis_type = "power", sample_size_input = n))))$power
        expect_equal(power, 0.80, tolerance = 0.015, info = paste(nm, "power at its own sample size"))
    }
    # Regression values: 93.8 percent (3 arms) and 99.9 percent (cluster) at n sized for 80.
    hd <- nums(do.call(run_sp, c(designs$multi_arm, list(analysis_type = "effect_size", sample_size_input = 1059))))$hr_detectable
    expect_equal(hd, 0.75, tolerance = 0.01)
})

test_that("detail tables report the headline's numbers", {
    ma <- run_sp(analysis_type = "power", sample_size_input = 1059, study_design = "multi_arm",
                 number_of_arms = 3, multiple_comparisons = "bonferroni")
    tbl <- ma$results$power_results$asDF
    expect_equal(tbl$value[tbl$parameter == "Calculated Power"], paste0(round(nums(ma)$power * 100, 1), "%"))
    expect_match(tbl$value[tbl$parameter == "Significance Level"], "2.5% (two-sided)", fixed = TRUE)

    du <- run_sp(analysis_type = "duration", sample_size_input = 1500, study_design = "multi_arm",
                 number_of_arms = 3, multiple_comparisons = "bonferroni")
    dt <- du$results$study_duration_results$asDF
    expect_equal(dt$value[dt$parameter == "Required Events"], paste(round(nums(du)$events), "events"))
    expect_gt(nums(du)$events, 380)  # alpha 0.025 needs more than the 380 of alpha 0.05
})

test_that("the non-inferiority interim table is populated at one-sided alpha", {
    skip_if_not_installed("gsDesign")
    # Regression: gsSurv at the assumed HR of 1 failed, leaving the table blank.
    a <- run_sp(test_type = "non_inferiority", ni_type = "relative_margin", effect_size = 1.0,
                alpha_level = 0.025, interim_analyses = 2, alpha_spending = "obrien_fleming")
    tbl <- a$results$interim_analysis_table$asDF
    expect_false(anyNA(tbl$boundary_value))
    expect_false("Interim Boundaries Unavailable" %in% notice_titles(a))
    expect_equal(tbl$boundary_value, round(gsDesign::gsDesign(
        k = 3, test.type = 1, alpha = 0.025, beta = 0.2, n.fix = 1, timing = c(1, 2, 3) / 3,
        sfu = gsDesign::sfLDOF
    )$upper$bound[1:2], 3))
})

test_that("unsupported design choices are refused or disclosed, not silently ignored", {
    crossover <- run_sp(study_design = "crossover")
    expect_true(is.null(nums(crossover)$n))
    expect_true("Crossover Design Not Supported" %in% notice_titles(crossover))

    stratified <- run_sp(study_design = "stratified")
    expect_equal(nums(stratified)$n, nums(run_sp())$n)
    expect_true("Stratified Design Sized as Unstratified" %in% notice_titles(stratified))

    landmark <- run_sp(effect_size_type = "survival_difference", effect_size = 0.15)
    expect_true("Survival Difference Landmark" %in% notice_titles(landmark))

    unequal <- run_sp(allocation_ratio = 2)
    expect_true("Unequal Allocation Approximation" %in% notice_titles(unequal))
    expect_false("Unequal Allocation Approximation" %in% notice_titles(run_sp()))
})
