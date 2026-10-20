testthat::test_that("dependency pruning reads roxygen only from R source files", {
  e <- new.env(parent = globalenv())
  sys.source(testthat::test_path("..", "..", "_updateModules_utils.R"), envir = e)
  script <- tempfile(fileext = ".R")
  binary <- tempfile(fileext = ".rda")
  writeLines("#' @importFrom vcd Kappa", script)
  writeBin(as.raw(c(0xff, 0xfe, 0x0a, 0xff)), binary)
  on.exit(unlink(c(script, binary)), add = TRUE)
  testthat::expect_warning(
    out <- e$prune_conflicts(c("vcd", "irr"), c(script, binary)), NA)
  testthat::expect_identical(out, "vcd")
})
