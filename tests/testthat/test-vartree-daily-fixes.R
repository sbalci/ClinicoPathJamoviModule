library(testthat)
test_that('vartree clears computed outputs and recovers across invalid input', {
  d <- data.frame(Stage=factor(c('I','I','II','II','II','I')))
  for (mode in c('empty','unset','excluded')) {
    opts <- ClinicoPath:::vartreeOptions$new(vars='Stage', ptable=TRUE, excl=TRUE)
    a <- ClinicoPath:::vartreeClass$new(options=opts, data=d)
    p <- a$.__enclos_env__$private
    p$.run()
    expect_match(as.character(a$results$reportSentence$content), 'N=6', fixed=TRUE)
    expect_match(as.character(a$results$text1$content), '<svg', fixed=TRUE)
    if (mode=='empty') p$.data <- d[FALSE,,drop=FALSE]
    if (mode=='unset') { o <- opts$option('vars'); o$value <- NULL }
    if (mode=='excluded') p$.data <- data.frame(Stage=factor(rep(NA_character_,6)))
    p$.run()
    for (id in c('text1','text2','reportSentence','interpretation')) {
      expect_true(all(!nzchar(trimws(as.character(a$results[[id]]$content)))), info=paste(mode,id))
    }
    expect_false(grepl('Analysis Complete', as.character(a$results$notices$content), fixed=TRUE))
    p$.data <- d; o <- opts$option('vars'); o$value <- 'Stage'; p$.run()
    expect_match(as.character(a$results$reportSentence$content), 'N=6', fixed=TRUE)
    expect_match(as.character(a$results$text1$content), '<svg', fixed=TRUE)
  }
})
