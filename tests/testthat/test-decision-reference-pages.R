test_that("reference pages are strings in the compiled full-response catalog", {
  refs <- get(".jmvrefs", environment(decision))
  for (key in names(refs)) {
    pages <- refs[[key]]$pages
    if (!is.null(pages)) {
      expect_type(pages, "character")
      expect_length(pages, 1L)
    }
  }
})
