test_that("zero-cell explanation names the selective ratio corrections", {
  r <- decisioncalculator(TP = 20, FN = 0, FP = 5, TN = 25)
  texts <- function(x) {
    if (inherits(x, c("Html", "Notice", "Preformatted"))) return(x$content)
    if (inherits(x, c("Group", "Array")))
      return(paste(vapply(x$items, texts, ""), collapse = " "))
    ""
  }
  content <- texts(r)
  expect_match(content, "LR+ only when TP or FP is zero", fixed = TRUE)
  expect_match(content, "LR- only when FN or TN is zero", fixed = TRUE)
  expect_match(content, "need not equal", fixed = TRUE)
  expect_equal(r$ratioTable$asDF$LRP[1], 6)
})
