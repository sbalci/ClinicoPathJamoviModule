# Regression tests for decisiongraph (2026-10-06/07):
#   1. Its per-strategy cache was a private field named `.results`, which is jmvcore Analysis's
#      own field for the results tree. .retrieveState() runs first in every .run() and set it to
#      NULL, so the whole results tree was gone before anything could be filled.
#   2. .getMemoryUsage() tested exists("memory.size"), which is TRUE on every platform since
#      R 4.2 kept the function as a stub that warns "no longer supported" on each call.
# Neither test asserts the summary values: the tree builder is a known open defect (the
# strategies are taken from `decision` nodes, of which only the root exists).

dg_data <- function() data.frame(
    id = 1:6,
    treatment = c(rep("Surgery", 3), rep("Medical", 3)),
    prob_success = c(0.8, 0.7, 0.9, 0.75, 0.85, 0.6),
    cost_success = c(10000, 12000, 9000, 11000, 10500, 15000),
    utility_success = c(0.9, 0.85, 0.95, 0.88, 0.92, 0.75)
)

dg_object <- function() {
    Opt <- getFromNamespace("decisiongraphOptions", "ClinicoPath")
    Cls <- getFromNamespace("decisiongraphClass", "ClinicoPath")
    a <- Cls$new(options = Opt$new(decisions = "treatment", probabilities = "prob_success",
                                   costs = "cost_success", utilities = "utility_success",
                                   treeType = "costeffectiveness",
                                   calculateExpectedValues = TRUE, summaryTable = TRUE),
                 data = dg_data())
    a$init()
    a
}

test_that("decisiongraph keeps its results tree across runs", {
    a <- dg_object()
    for (pass in 1:2) {   # pass 2 = same instance, where .retrieveState() reads the stored state
        suppressMessages(a$run())
        expect_true(inherits(a$results, "Group"))
        expect_false(is.null(a$results$summaryTable))
        expect_true(nzchar(a$results$text1$content))
    }
})

test_that("decisiongraph does not call the defunct memory.size()/memory.limit()", {
    a <- dg_object()
    warned <- character()
    withCallingHandlers(suppressMessages(a$run()),
                        warning = function(w) {
                            warned <<- c(warned, conditionMessage(w))
                            invokeRestart("muffleWarning")
                        })
    expect_false(any(grepl("no longer supported", warned)))
})
