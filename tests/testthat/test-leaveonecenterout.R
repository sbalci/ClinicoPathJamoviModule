# ──────────────────────────────────────────────────────────────────────────────
# Tests for leaveonecenterout (Leave-One-Center-Out Cross-Validation)
#
# Note: The wrapper function requires outcomeLevel and elapsedtime as explicit
# arguments (Level/Variable types have no defaults in jamovi R wrappers).
# Always pass them, even when not logically needed by the model type.
# ──────────────────────────────────────────────────────────────────────────────

# ── Logistic model tests ─────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: logistic model on multicenter data", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score", "tumor_size_mm", "grade"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "logistic"
    )

    expect_true(r$designSummary$rowCount > 0)
    expect_equal(r$perCenterResults$rowCount, 5)
    expect_true(r$pooledPerformance$rowCount > 0)
    expect_true(nchar(r$interpretation$content) > 0)
})


testthat::test_that("leaveonecenterout: logistic with LASSO", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score", "tumor_size_mm", "ctdna_level", "grade", "stage"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "logistic",
        useLasso = TRUE,
        lambdaMethod = "lambda.1se"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
})


testthat::test_that("leaveonecenterout: LASSO lambda.min selection", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score", "tumor_size_mm"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "logistic",
        useLasso = TRUE,
        lambdaMethod = "lambda.min"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
})


# ── Cox model tests ──────────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: Cox model with time variable", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "os_status",
        outcomeLevel = "Dead",
        predictors = c("age", "ki67_score", "grade"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "cox"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
    expect_true(nchar(r$interpretation$content) > 0)
})


testthat::test_that("leaveonecenterout: Cox with LASSO", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "os_status",
        outcomeLevel = "Dead",
        predictors = c("age", "ki67_score", "tumor_size_mm", "grade"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "cox",
        useLasso = TRUE
    )

    expect_equal(r$perCenterResults$rowCount, 5)
})


# ── Linear model tests ───────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: linear model on continuous outcome", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "tumor_shrinkage",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score", "tumor_size_mm"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        modelType = "linear"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
    expect_true(nchar(r$interpretation$content) > 0)
})


# ── Option toggles ───────────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: pooledPerformance toggle", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r1 <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        pooledPerformance = TRUE
    )
    expect_true(r1$pooledPerformance$rowCount > 0)

    r2 <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        pooledPerformance = FALSE
    )
    expect_equal(r2$pooledPerformance$rowCount, 0)
})


testthat::test_that("leaveonecenterout: reproducibility with same seed", {
    load(test_path("../../data/loocv_multicenter.rda"))

    args <- list(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("age", "ki67_score", "tumor_size_mm"),
        centerVariable = "institution",
        elapsedtime = "os_time",
        useLasso = TRUE,
        random_seed = 123
    )

    r1 <- do.call(leaveonecenterout, args)
    r2 <- do.call(leaveonecenterout, args)

    df1 <- r1$perCenterResults$asDF
    df2 <- r2$perCenterResults$asDF
    expect_equal(df1$auc, df2$auc)
})


# ── Small dataset tests ──────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: small dataset with 3 centers", {
    load(test_path("../../data/loocv_small.rda"))

    r <- leaveonecenterout(
        data = loocv_small,
        outcome = "diagnosis",
        outcomeLevel = "Positive",
        predictors = c("age", "marker1", "marker2"),
        centerVariable = "center",
        elapsedtime = "surv_time",
        modelType = "logistic"
    )

    expect_equal(r$perCenterResults$rowCount, 3)
})


testthat::test_that("leaveonecenterout: small Cox dataset", {
    load(test_path("../../data/loocv_small.rda"))

    r <- leaveonecenterout(
        data = loocv_small,
        outcome = "surv_event",
        outcomeLevel = "Event",
        predictors = c("age", "marker1"),
        centerVariable = "center",
        elapsedtime = "surv_time",
        modelType = "cox"
    )

    expect_equal(r$perCenterResults$rowCount, 3)
})


testthat::test_that("leaveonecenterout: small linear dataset", {
    load(test_path("../../data/loocv_small.rda"))

    r <- leaveonecenterout(
        data = loocv_small,
        outcome = "continuous_outcome",
        outcomeLevel = "Positive",
        predictors = c("age", "marker1", "marker2"),
        centerVariable = "center",
        elapsedtime = "surv_time",
        modelType = "linear"
    )

    expect_equal(r$perCenterResults$rowCount, 3)
})


# ── Edge cases ────────────────────────────────────────────────────────────────

testthat::test_that("leaveonecenterout: error message with fewer than 3 centers", {
    load(test_path("../../data/loocv_small.rda"))

    df2 <- loocv_small[loocv_small$center %in% c("Clinic_X", "Clinic_Y"), ]
    df2$center <- droplevels(df2$center)

    r <- leaveonecenterout(
        data = df2,
        outcome = "diagnosis",
        outcomeLevel = "Positive",
        predictors = c("age", "marker1"),
        centerVariable = "center",
        elapsedtime = "surv_time"
    )

    # Error is shown in todo HTML, not thrown
    expect_true(grepl("Data Error|3 centers", r$todo$content))
})


testthat::test_that("leaveonecenterout: error message with too few observations", {
    load(test_path("../../data/loocv_small.rda"))

    df_tiny <- loocv_small[1:10, ]

    r <- leaveonecenterout(
        data = df_tiny,
        outcome = "diagnosis",
        outcomeLevel = "Positive",
        predictors = c("age", "marker1"),
        centerVariable = "center",
        elapsedtime = "surv_time"
    )

    # Error is shown in todo HTML
    expect_true(grepl("Data Error|Too few", r$todo$content))
})


testthat::test_that("leaveonecenterout: single predictor works", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = "ki67_score",
        centerVariable = "institution",
        elapsedtime = "os_time"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
})


testthat::test_that("leaveonecenterout: factor-only predictors work", {
    load(test_path("../../data/loocv_multicenter.rda"))

    r <- leaveonecenterout(
        data = loocv_multicenter,
        outcome = "treatment_response",
        outcomeLevel = "Responder",
        predictors = c("grade", "stage", "gender"),
        centerVariable = "institution",
        elapsedtime = "os_time"
    )

    expect_equal(r$perCenterResults$rowCount, 5)
})
