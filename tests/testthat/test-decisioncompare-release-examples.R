test_that('all bundled decisioncompare examples run with required levels and valid denominators', {
    env <- new.env(parent = globalenv())
    captured <- new.env(parent = emptyenv())
    captured$results <- list()
    captured$data <- list()
    env$decisioncompare <- function(...) {
        captured$data[[length(captured$data) + 1L]] <- list(...)$data
        result <- ClinicoPath::decisioncompare(...)
        captured$results[[length(captured$results) + 1L]] <- result
        invisible(result)
    }
    path <- testthat::test_path('..', '..', 'inst', 'examples', 'decisioncompare_example.R')
    expect_no_error(sys.source(path, envir = env))
    expect_length(captured$results, 10)
    for (result in captured$results) {
        expect_gt(result$cTable1$getCell(rowKey='Total', col='Total')$value, 0)
        expect_gt(result$cTable2$getCell(rowKey='Total', col='Total')$value, 0)
    }
    # Example 7 must really exclude the third level, not silently recode it as negative.
    d <- captured$data[[7]]
    expect_equal(captured$results[[7]]$cTable1$getCell(rowKey='Total', col='Total')$value,
                 sum(d$Test1 %in% c('Positive', 'Negative') & !is.na(d$GoldStandard)))
    expect_lt(captured$results[[7]]$cTable1$getCell(rowKey='Total', col='Total')$value, nrow(d))
})
