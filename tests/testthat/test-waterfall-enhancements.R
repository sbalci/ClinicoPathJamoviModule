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
