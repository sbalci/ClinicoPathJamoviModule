library(testthat)
daily_al <- function(d, ...) do.call(ClinicoPath::alluvial, c(list(data=d, vars=c('A','B'), showFlowTable=TRUE), list(...)))
test_that('explicit NA factor levels follow the missing-value option in both panels', {
  d <- data.frame(A=ordered(c('a',NA,'NA','a'), levels=c('a','NA',NA), exclude=NULL), B=factor(c('x','x','y','x')), w=c(1,2,3,4))
  for(engine in c('easyalluvial','ggalluvial')) for(excl in c(FALSE,TRUE)) {
    r <- daily_al(d, engine=engine, excl=excl, weight=if(engine=='ggalluvial') 'w' else NULL, condensationvar='A')
    n <- if(excl) 3L else 4L
    expect_equal(sum(r$flowTable$asDF$n), n)
    expect_equal(sum(r$flowTable$asDF$pct), 1)
    expect_false(anyNA(as.character(r$plot$state$data$A)))
    expect_false(anyNA(as.character(r$plot2$state$data$A)))
    expect_equal(nrow(r$plot2$state$data), n)
    expect_true('NA' %in% as.character(r$plot$state$data$A))
    expect_true(is.ordered(r$plot2$state$data$A))
    expect_match(r$notices$content, '1 of 4 rows had a missing value')
    if(engine=='ggalluvial') expect_equal(sum(r$flowTable$asDF$w), if(excl) 8 else 10)
  }
})
test_that('invalid or cleared selections remove old flow rows and notes, and recover', {
  d <- data.frame(A=factor(c('a','a','b')),B=factor(c('x','y','y')))
  for(v in list(NULL,'A')) {
    options <- ClinicoPath:::alluvialOptions$new(vars=c('A','B'), showFlowTable=TRUE)
    a <- ClinicoPath:::alluvialClass$new(options=options, data=d)
    p <- a$.__enclos_env__$private
    p$.run(); baseline <- a$results$flowTable$asDF
    expect_equal(sum(baseline$n), 3)
    selected <- options$option('vars')
    selected$value <- v
    p$.run()
    expect_equal(nrow(a$results$flowTable$asDF), 0L)
    expect_null(a$results$plot$state)
    expect_null(a$results$flowTable$notes[['base']])
    selected$value <- c('A','B')
    p$.run()
    expect_equal(a$results$flowTable$asDF, baseline)
  }
})
test_that('a path selected by weight is described as largest weighted path', {
  d <- data.frame(A=factor(c('a','a','a','b')),B=factor(c('x','x','x','y')),w=c(1,1,1,20))
  r <- daily_al(d,engine='ggalluvial',weight='w')
  expect_equal(r$flowTable$asDF$n[1],1)
  expect_equal(r$flowTable$asDF$w[1],20)
  expect_match(r$notices$content,'Largest weighted path: b')
  expect_false(grepl('Commonest path: b',r$notices$content,fixed=TRUE))
})
