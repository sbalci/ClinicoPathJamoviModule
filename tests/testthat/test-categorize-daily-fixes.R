# Backend lifecycle checks; native .omv restoration/export remains a separate gate.
testthat::test_that('VAL-categorize-01 clears rendered results and recovers', {
  ns <- asNamespace('ClinicoPath')
  o <- get('categorizeOptions', ns)$new(var='x', method='equal', nbins=2)
  a <- get('categorizeClass', ns)$new(options=o, data=data.frame(x=0:4))
  a$run(); p <- a$.__enclos_env__$private
  expected <- a$results$freqTable$asDF
  testthat::expect_equal(expected$n, c(3L,2L))
  selected <- o$option('var')
  for (invalid in c('empty','constant','missing')) {
    selected$value <- if (invalid == 'empty') NULL else 'x'
    p$.data <- data.frame(x=if (invalid == 'constant') rep(1,5) else
      if (invalid == 'missing') rep(NA_real_,5) else 0:4)
    p$.init(); p$.run()
    testthat::expect_equal(a$results$freqTable$rowCount, 0L, info=invalid)
    testthat::expect_equal(a$results$breakpointsTable$rowCount, 0L, info=invalid)
    testthat::expect_null(a$results$plot$state, info=invalid)
    testthat::expect_identical(gsub('<[^>]*>', '', a$results$summaryText$content), '', info=invalid)
    testthat::expect_identical(gsub('<[^>]*>', '', a$results$rcode$content), '', info=invalid)
    selected$value <- 'x'; p$.data <- data.frame(x=0:4)
    p$.init(); p$.run()
    testthat::expect_equal(a$results$freqTable$asDF, expected, info=invalid)
    testthat::expect_equal(a$results$plot$state$breaks, c(0,2,4), info=invalid)
  }
})
testthat::test_that('VAL-categorize-02 printed ranges distinguish tiny endpoints', {
  for (scale in c(1e-15, 1e-100)) for (rc in c(TRUE,FALSE)) {
    x <- seq(1,5,length.out=9)*scale
    r <- categorize(data=data.frame(x=x), var='x', method='equal', rightclosed=rc)
    b <- r$breakpointsTable$asDF$value
    f <- r$freqTable$asDF
    bounds <- t(vapply(f$range, function(s) as.numeric(strsplit(
      substring(s,2,nchar(s)-1), ',',fixed=TRUE)[[1]]), numeric(2)))
    # Check each bound relative to its independently derived nonzero value.
    expected <- cbind(seq(1,4)*scale,seq(2,5)*scale)
    testthat::expect_equal(unname(bounds/expected), matrix(1,4,2), tolerance=1e-12)
    testthat::expect_equal(length(unique(as.numeric(bounds))),5L)
    testthat::expect_identical(f$category,f$range)
    testthat::expect_equal(f$n,if (rc) c(3L,2L,2L,2L) else c(2L,2L,2L,3L))
    testthat::expect_equal(b/(seq(1,5)*scale),rep(1,5),tolerance=1e-12)
  }
})
