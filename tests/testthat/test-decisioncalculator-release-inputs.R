test_that("calculator clears restored narratives before fatal input and recovers", {
  ns <- environment(decisioncalculator)
  make <- function(counts, panel_options = list()) {
    opts <- do.call(ns$decisioncalculatorOptions$new,
      c(counts, utils::modifyList(list(showSummary = TRUE, fagan = TRUE,
                                     showAbout = TRUE), panel_options)))
    a <- ns$decisioncalculatorClass$new(data = data.frame(), options = opts)
    a$init()
    a
  }
  expect_error(ns$decisioncalculator(TP = -1, FN = 10, FP = 6, TN = 24),
               "Negative counts detected", fixed = TRUE)
  counts <- list(TP = 40, FN = 10, FP = 6, TN = 24)
  good <- make(counts)
  suppressWarnings(good$run(noThrow = TRUE))
  expect_true(good$complete)
  panels <- c("summary", "faganSummary", "assumptions")
  old <- lapply(panels, function(nm) good$results[[nm]]$content)
  expect_true(all(nzchar(unlist(old))))
  invalids <- list(list(TP = -1), list(TP = 0, FN = 0, FP = 0, TN = 0),
                   list(TP = 0, FN = 0), list(TN = 0, FP = 0))
  for (invalid in invalids) {
    bad <- make(utils::modifyList(counts, invalid))
    # Mimic restored Html content independently of schema invalidation.
    for (i in seq_along(panels)) bad$results[[panels[i]]]$setContent(old[[i]])
    suppressWarnings(bad$run(noThrow = TRUE))
    expect_identical(bad$status, "error")
    expect_true(bad$errored)
    expect_false(bad$complete)
    for (nm in panels) expect_identical(bad$results[[nm]]$content, "", info = nm)
    fresh <- make(utils::modifyList(counts, invalid))
    suppressWarnings(fresh$run(noThrow = TRUE))
    for (nm in panels) expect_identical(bad$results[[nm]]$content,
                                       fresh$results[[nm]]$content)
  }
  hidden <- make(counts, list(showSummary = FALSE, fagan = FALSE, showAbout = FALSE))
  for (i in seq_along(panels)) hidden$results[[panels[i]]]$setContent(old[[i]])
  suppressWarnings(hidden$run(noThrow = TRUE))
  expect_true(hidden$complete)
  for (nm in panels) expect_identical(hidden$results[[nm]]$content, "")
  recovered <- make(counts)
  suppressWarnings(recovered$run(noThrow = TRUE))
  expect_true(recovered$complete)
  expect_false(recovered$errored)
  for (i in seq_along(panels)) expect_identical(recovered$results[[panels[i]]]$content,
                                               old[[i]])
})
