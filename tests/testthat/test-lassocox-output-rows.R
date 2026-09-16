# jamovi writes an output column by row position unless the analysis sends row
# numbers, so a filtered dataset used to attach every risk score to the wrong
# patient. These also cover the renderer fallbacks, which tests never reach
# through the results object.

lassocox_rows_file <- function(relative) {
    installed <- tryCatch(
        system.file(sub("^inst/", "", relative), package = "ClinicoPath"),
        error = function(e) "")
    candidates <- c(relative, file.path("..", "..", relative), installed)
    hit <- candidates[nzchar(candidates) & file.exists(candidates)]
    if (length(hit) == 0L)
        skip(paste("Source artifact not available:", relative))
    hit[[1L]]
}

lassocox_rows_data <- function(drop = 0L) {
    env <- new.env(parent = emptyenv())
    load(lassocox_rows_file(file.path("data", "lassocox_breast_cancer.rda")), envir = env)
    d <- env$lassocox_breast_cancer
    if (drop > 0L) d[-seq_len(drop), , drop = FALSE] else d
}

lassocox_rows_predictors <- c("age", "tumor_size_cm", "grade", "ki67_percent")

lassocox_rows_run <- function(d, lambda = "lambda.min") {
    skip_if_not_installed("glmnet")
    # The wrapper quotes `explanatory`, so a bare variable name would be taken for
    # a column name: the predictors have to appear as a literal call here.
    suppressWarnings(ClinicoPath::lassocox(
        data = d, elapsedtime = "survival_months", outcome = "death",
        outcomeLevel = "Dead", censorLevel = "Alive",
        explanatory = c("age", "tumor_size_cm", "grade", "ki67_percent"),
        lambda = lambda, cv_plot = FALSE, survival_plot = FALSE))
}


test_that("the saved risk score carries the dataset row numbers", {
    # row names 21..250, exactly what an analysis receives while a jamovi filter is on
    d <- lassocox_rows_data(drop = 20L)
    res <- lassocox_rows_run(d)
    priv <- res$riskScore$.__enclos_env__$private

    expect_identical(priv$.rowNums, as.integer(rownames(d)))
    expect_length(unlist(priv$.values), nrow(d))
})


test_that("an empty model gives the coefficient plot a state to explain", {
    d <- lassocox_rows_data()
    res <- lassocox_rows_run(d, lambda = "lambda.1se")
    tab <- as.data.frame(res$coefficients)

    expect_identical(tab$variable, "No variables selected")
    # NULL state cannot be told apart from "the analysis has not run"
    expect_false(is.null(res$coef_plot$state))
    expect_length(res$coef_plot$state$var_names, 0L)
})


test_that("plot renderers draw from state alone and refuse a missing state", {
    skip_if_not_installed("glmnet")
    d <- lassocox_rows_data()
    opts <- do.call(getFromNamespace("lassocoxOptions", "ClinicoPath")$new, list(
        elapsedtime = "survival_months", outcome = "death", outcomeLevel = "Dead",
        censorLevel = "Alive", explanatory = lassocox_rows_predictors, lambda = "lambda.min"))
    analysis <- getFromNamespace("lassocoxClass", "ClinicoPath")$new(options = opts, data = d)
    suppressWarnings(analysis$run())
    render <- analysis$.__enclos_env__$private
    results <- analysis$results

    grDevices::pdf(NULL)
    on.exit(grDevices::dev.off(), add = TRUE)
    ggtheme <- ggplot2::theme_bw()

    expect_true(render$.cvPlot(list(state = results$cv_plot$state), ggtheme, NULL))
    expect_true(render$.coefPlot(list(state = results$coef_plot$state), ggtheme, NULL))
    expect_true(render$.survivalPlot(list(state = results$survival_plot$state), ggtheme, NULL))

    expect_false(render$.cvPlot(list(state = NULL), ggtheme, NULL))
    expect_false(render$.coefPlot(list(state = NULL), ggtheme, NULL))
    expect_false(render$.survivalPlot(list(state = NULL), ggtheme, NULL))

    # an empty model explains itself rather than drawing a blank panel
    expect_true(render$.coefPlot(list(state = list(var_names = character(0))), ggtheme, NULL))
})


test_that("no translatable string in lassocox carries a line break", {
    src <- readLines(lassocox_rows_file(file.path("R", "lassocox.b.R")))
    expect_identical(grep('\\.\\("[^"]*\\\\n', src, value = TRUE), character(0))
})
