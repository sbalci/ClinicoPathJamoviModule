# Test suite for brierscore function
#
# ⚠️ IMPORTANT: These tests only verify that the function runs and produces
# output structures. They do NOT validate statistical correctness.
#
# TODO FOR CLINICAL VALIDATION:
# 1. Add tests comparing against riskRegression::Score() or pec::pec()
# 2. Test against known datasets with published Brier scores
# 3. Validate IPCW calculations against established implementations
# 4. Test confidence interval coverage with simulation studies
#
# Until these validations are added, this function should NOT be used for
# clinical decision-making or regulatory submissions.

test_that("brierscore handles basic inputs", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    skip_if_not_installed("survival")

    # Simulate survival data
    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    # Run analysis
    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred_surv",
        prediction_time = 50
    )

    expect_s3_class(result, "brierscoreResults")
    expect_true(result$brierSummary$rowCount > 0)
})

test_that("brierscore handles factor events with event_code", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = factor(sample(c("Alive", "Death"), n, replace = TRUE)),
        pred_surv = runif(n, 0.3, 0.9)
    )

    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        event_code = "Death",
        predicted_survival = "pred_surv",
        prediction_time = 50
    )

    expect_s3_class(result, "brierscoreResults")
})

test_that("brierscore escapes variable names with spaces", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        "Follow Up Time" = rexp(n, 0.01),
        "Event Status" = rbinom(n, 1, 0.7),
        "Predicted Survival" = runif(n, 0.3, 0.9),
        check.names = FALSE
    )

    result <- jmv::brierscore(
        data = data,
        time = "Follow Up Time",
        event = "Event Status",
        predicted_survival = "Predicted Survival",
        prediction_time = 50
    )

    expect_s3_class(result, "brierscoreResults")
})

test_that("brierscore model comparison works", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred1 = runif(n, 0.3, 0.9),
        pred2 = runif(n, 0.4, 0.8),
        pred3 = runif(n, 0.5, 0.95)
    )

    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred1",
        compare_multiple_models = TRUE,
        additional_predictions = c("pred2", "pred3"),
        model_names = "Model A, Model B, Model C",
        prediction_time = 50
    )

    expect_true(result$modelComparison$rowCount >= 3)
    expect_true(result$pairwiseComparisons$rowCount >= 3)
})

test_that("brierscore rejects IBS (not implemented)", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    expect_error({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 50,
            calculate_ibs = TRUE
        )
    }, "NOT SUPPORTED")
})

test_that("brierscore validates prediction range", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, -0.1, 1.2)  # Invalid range
    )

    expect_warning({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 50
        )
    }, "between 0 and 1")
})

test_that("brierscore handles stratified analysis (VALID feature)", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9),
        treatment = factor(sample(c("A", "B"), n, replace = TRUE))
    )

    # Stratified Brier is OK - evaluates same time point per stratum
    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred_surv",
        prediction_time = 50,
        stratified_brier = TRUE,
        stratify_by = "treatment"
    )

    expect_true(result$stratifiedBrier$rowCount >= 2)
})

test_that("brierscore calibration table populated when requested", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred_surv",
        prediction_time = 50,
        plot_calibration_curve = TRUE,
        calibration_groups = 5
    )

    expect_true(result$calibrationTable$rowCount >= 5)
})

test_that("brierscore rejects multiple time points (INVALID)", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    # Multiple time points would incorrectly reuse same predictions
    expect_error({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 50,
            multiple_time_points = TRUE,
            time_points = "25, 50, 75, 100"
        )
    }, "NOT SUPPORTED")
})

test_that("brierscore handles reference model comparison", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9),
        ref_pred = runif(n, 0.4, 0.85)
    )

    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred_surv",
        prediction_time = 50,
        reference_model = TRUE,
        reference_predictions = "ref_pred"
    )

    expect_true(result$modelComparison$rowCount >= 2)
})

test_that("brierscore handles small sample size warning", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 15  # Very small sample
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    expect_warning({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 50
        )
    }, "small sample")
})

test_that("brierscore errors on prediction time beyond follow-up", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.1),  # Short follow-up times
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    expect_error({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 1000  # Way beyond max time
        )
    }, "exceeds maximum follow-up")
})

test_that("brierscore scaled Brier score calculated correctly", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = rbinom(n, 1, 0.7),
        pred_surv = runif(n, 0.3, 0.9)
    )

    result <- jmv::brierscore(
        data = data,
        time = "time",
        event = "event",
        predicted_survival = "pred_surv",
        prediction_time = 50,
        scaled_brier = TRUE
    )

    summary_df <- result$brierSummary$asDF()
    expect_true(!is.na(summary_df$scaled_brier[1]))
    expect_true(!is.na(summary_df$null_model_brier[1]))
})

test_that("brierscore handles character event variable", {
    skip_if_not_installed("survival")

    set.seed(123)
    n <- 100
    data <- data.frame(
        time = rexp(n, 0.01),
        event = sample(c("censored", "event"), n, replace = TRUE),
        pred_surv = runif(n, 0.3, 0.9)
    )

    expect_warning({
        result <- jmv::brierscore(
            data = data,
            time = "time",
            event = "event",
            predicted_survival = "pred_surv",
            prediction_time = 50
        )
    }, "event_code not specified")

    expect_s3_class(result, "brierscoreResults")
})
