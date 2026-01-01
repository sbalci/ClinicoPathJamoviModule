
test_that("jjpubr works", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
    # Source the files
    source("../../R/jjpubr.h.R")
    source("../../R/jjpubr.b.R")

    # Create test data
    set.seed(123)
    data <- data.frame(
        group = factor(rep(c("A", "B", "C"), each = 20)),
        value = c(rnorm(20, 10, 2), rnorm(20, 12, 2), rnorm(20, 15, 2)),
        cont_x = rnorm(60),
        cont_y = rnorm(60)
    )

    # Test Box Plot
    options <- jjpubrOptions$new(
        plotType = "boxplot",
        xvar = "group",
        yvar = "value",
        addStats = TRUE,
        statMethod = "auto",
        pairwiseComparisons = TRUE,
        palette = "jco"
    )

    analysis <- jjpubrClass$new(
        options = options,
        data = data
    )

    expect_error(analysis$run(), NA)
    expect_true(!is.null(analysis$results$plot))

    # Test Scatter Plot
    options_scatter <- jjpubrOptions$new(
        plotType = "scatter",
        xvar = "cont_x",
        yvar = "cont_y",
        addCorr = TRUE,
        addMarginal = TRUE
    )

    analysis_scatter <- jjpubrClass$new(
        options = options_scatter,
        data = data
    )

    expect_error(analysis_scatter$run(), NA)
    expect_true(!is.null(analysis_scatter$results$plot))
})
