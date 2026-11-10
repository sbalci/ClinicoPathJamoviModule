library(testthat)
test_that('OD01 invalid reruns clear previous computed outputs and recover', {
  ns <- asNamespace('ClinicoPath')
  opts <- get('outlierdetectionOptions', ns)$new(vars='x', method_category='univariate', univariate_methods='zscore', show_method_comparison=TRUE, show_visualization=TRUE, show_exclusion_summary=TRUE, show_interpretation=TRUE)
  dat <- data.frame(x=c(seq(-1,1,length.out=39),20))
  obj <- get('outlierdetectionClass', ns)$new(options=opts, data=dat)
  p <- obj$.__enclos_env__$private
  panels <- c('outlier_table','method_comparison','exclusion_summary','interpretation')
  for (invalid in c('unset','empty','missing')) {
    p$.data <- dat; opt <- opts$option('vars'); opt$value <- 'x'; p$.run()
    expect_true(nchar(as.character(obj$results$outlier_table$content))>100)
    expect_false(is.null(obj$results$plot$state))
    if (invalid=='unset') opt$value <- character(0)
    if (invalid=='empty') p$.data <- dat[FALSE,,drop=FALSE]
    if (invalid=='missing') p$.data <- data.frame(x=rep(NA_real_,40))
    p$.run()
    for (panel in panels) expect_true(is.null(obj$results[[panel]]$content) || nchar(as.character(obj$results[[panel]]$content))==0, info=paste(invalid,panel))
    expect_null(obj$results$plot$state, info=invalid)
    p$.data <- dat; opt$value <- 'x'; p$.run()
    expect_true(nchar(as.character(obj$results$outlier_table$content))>100)
    expect_false(is.null(obj$results$plot$state))
  }
})
