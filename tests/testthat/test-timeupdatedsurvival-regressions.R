# Regression tests for two defects that together left every fitted model showing only an
# error footnote (found 2026-10-06 during the deleteRows() sweep):
#   1. table$deleteRows(rowNo = 1): deleteRows() takes no arguments, so clearing the
#      `rows: 1` placeholder threw "unused argument" before any table was filled.
#   2. a private field named `.results`: that is jmvcore Analysis's own field for the
#      results tree, so `private$.results <- results` replaced the tree with a plain list.
# The public wrapper builds a fresh instance per call; the second run below needs the class.

test_that("timeupdatedsurvival fills its tables and keeps its results tree across re-runs", {
    Opt <- getFromNamespace("timeupdatedsurvivalOptions", "ClinicoPath")
    Cls <- getFromNamespace("timeupdatedsurvivalClass", "ClinicoPath")
    set.seed(7)
    n <- 150
    d <- data.frame(time = rexp(n, 0.05) + 0.5, status = rbinom(n, 1, 0.7),
                    age = rnorm(n, 60, 8), size = rnorm(n, 3, 1))
    a <- Cls$new(options = Opt$new(timeVar = "time", statusVar = "status",
                                   covariates = c("age", "size"), timeVaryingCovs = "size",
                                   modelType = "timevarycox", showExplanations = TRUE),
                 data = d)
    a$init()

    for (pass in 1:2) {
        a$run()
        expect_true(inherits(a$results, "Group"))
        expect_equal(nrow(a$results$modelSummary$asDF), 3)   # age, size, size_time
        expect_length(a$results$modelSummary$getCell(rowNo = 1, col = "parameter")$footnotes, 0)
        expect_true(nzchar(a$results$methodExplanation$content))
    }
})
