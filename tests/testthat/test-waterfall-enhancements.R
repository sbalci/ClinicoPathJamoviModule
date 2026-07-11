test_that("conventional sort puts worst (highest) response on the left", {
    # emulate the sort logic used by .prepareWaterfallPlotData
    df <- data.frame(response = c(-50, 10, -20, 40, 0))
    decreasing <- TRUE  # conventional
    ordered <- df[order(df$response, decreasing = decreasing, na.last = TRUE), , drop = FALSE]
    # leftmost bar (row 1) must be the largest (worst) value
    expect_equal(ordered$response[1], 40)
    # rightmost bar must be the smallest (best) value
    expect_equal(ordered$response[nrow(ordered)], -50)
})

test_that("responseCategoryVar overrides computed RECIST category (new-lesion PD)", {
    d <- read.csv(testthat::test_path("..", "..", "data", "waterfall_annotation_test_data.csv"))
    # without override: PT003 (-44.2%) computes as PR
    r0 <- ClinicoPath::waterfall(data = d, patientID = "PatientID",
            responseVar = "Response", inputType = "percentage")
    wd0 <- r0$waterfallplot$state$data$waterfall
    expect_equal(as.character(wd0$recist_category[wd0$PatientID == "PT003"]), "PR")
    # with override: PT003 becomes PD (new lesion despite shrinkage)
    r1 <- ClinicoPath::waterfall(data = d, patientID = "PatientID",
            responseVar = "Response", inputType = "percentage",
            responseCategoryVar = "Category")
    wd1 <- r1$waterfallplot$state$data$waterfall
    expect_equal(as.character(wd1$recist_category[wd1$PatientID == "PT003"]), "PD")
})
