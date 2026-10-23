testthat::test_that('explicit missing ordinal levels are missing in reader outputs', {
  lv <- c('Low','High',NA_character_)
  d <- data.frame(a=ordered(c('Low','High',NA),levels=lv,exclude=NULL),
    b=ordered(c('Low','High','Low'),levels=lv,exclude=NULL))
  o <- list(readerWorkflow='ordinal_cutoff',vars=c('a','b'),ordinalCutoff='High',readerIntervals=FALSE)
  p <- .paReaderPrepare(d,o)
  testthat::expect_equal(unname(p$calls[,1]), c(FALSE,TRUE,NA))
  testthat::expect_equal(p$status[,1],c('determinate','determinate','missing'))
  testthat::expect_equal(p$scale,c('Low','High'))
  r <- do.call(pathagreement,c(list(data=d),o))
  x <- r$readerDataSummary$asDF
  testthat::expect_equal(x$missing[x$reader=='a'],1)
  testthat::expect_equal(x$usable[x$reader=='a'],2)
  testthat::expect_equal(r$cutoffAgreementTable$asDF$n,rep(2,5))
  d$a <- ordered(as.character(d$a),levels=c('Low','High'))
  d$b <- ordered(as.character(d$b),levels=c('Low','High'))
  fresh <- do.call(pathagreement,c(list(data=d),o))
  testthat::expect_equal(r$cutoffAgreementTable$asDF,fresh$cutoffAgreementTable$asDF)
})
testthat::test_that('reader factor missingness preserves literal NA and rejects missing clusters', {
  d <- data.frame(a=factor(c('Y','N','NA',NA),exclude=NULL),b=factor(c('Y','N','NA','Y')),
    patient=factor(c('p1','p2','p3',NA),exclude=NULL))
  o <- list(readerWorkflow='binary',vars=c('a','b'),readerPositive='Y',readerNegative='N')
  p <- .paReaderPrepare(d,o)
  testthat::expect_equal(p$status[,1],c('determinate','determinate','unmapped','missing'))
  testthat::expect_equal(as.character(p$original_raw$a),c('Y','N','NA',NA))
  testthat::expect_error(.paReaderPrepare(d,c(o,list(readerCluster='patient'))),'cluster ID')
})
testthat::test_that('normalization preserves ordered metadata and caller input', {
  lv <- c('Low',NA_character_,'High')
  d <- data.frame(a=ordered(c('Low',NA,'High'),lv,exclude=NULL),
                  b=ordered(c('Low','High','High'),lv,exclude=NULL))
  attr(d$a,'label') <- 'Reader A'; attr(d$a,'values') <- c(10,99,20)
  original <- d
  p <- .paReaderPrepare(d,list(readerWorkflow='ordinal_cutoff',vars=c('a','b'),ordinalCutoff='High'))
  testthat::expect_identical(d,original)
  testthat::expect_true(is.ordered(p$raw$a))
  testthat::expect_identical(attr(p$raw$a,'label'),'Reader A')
  testthat::expect_identical(attr(p$raw$a,'values'),c(10,20))
  testthat::expect_equal(unname(p$calls[,1]),c(FALSE,NA,TRUE))
})
