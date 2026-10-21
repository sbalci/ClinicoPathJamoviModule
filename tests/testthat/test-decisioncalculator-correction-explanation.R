test_that("zero-cell explanation names the selective ratio corrections", {
  r <- decisioncalculator(TP = 20, FN = 0, FP = 5, TN = 25)
  texts <- function(x) {
    if (inherits(x, c("Html", "Notice", "Preformatted"))) return(x$content)
    if (inherits(x, c("Group", "Array")))
      return(paste(vapply(x$items, texts, ""), collapse = " "))
    ""
  }
  content <- texts(r)
  # Rule changed 2026-10-21 (XA-01): correct only an empty DENOMINATOR; an empty numerator is an exact 0.
  expect_match(content, "corrected only when its denominator cell is empty", fixed = TRUE)
  expect_match(content, "the ratio is exactly 0", fixed = TRUE)
  expect_match(content, "need not equal", fixed = TRUE)
  expect_equal(r$ratioTable$asDF$LRP[1], 6)
  expect_identical(r$ratioTable$asDF$LRN[1], 0)   # FN = 0: exact 0, no longer 0.5-corrected
})
