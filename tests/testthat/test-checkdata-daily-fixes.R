# Frozen 2026-10-29: missingness from R factor labels; lifecycle compared to fresh results.
library(testthat)
cd_daily_run <- function(x) do.call(ClinicoPath::checkdata,
  list(data=data.frame(x=x),var='x',showDistribution=TRUE,showDuplicates=TRUE,
       showPatterns=TRUE,showSummary=TRUE))
cd_daily_value <- function(r, metric) r$missingVals$asDF$value[r$missingVals$asDF$metric == metric]

test_that('VAL-checkdata-01 actual NA factor levels are missing; literal NA is a category', {
  for (ordered in c(FALSE,TRUE)) {
    x <- factor(c('B','A',NA,'NA','A',NA),levels=c('B','A','NA',NA),exclude=NULL,ordered=ordered)
    attr(x,'label') <- 'Sample category'; attr(x,'values') <- c(20L,10L,30L,NA_integer_)
    before <- x
    r <- cd_daily_run(x)
    expect_identical(x,before)
    expect_identical(cd_daily_value(r,'Missing Values'),'2 (33.3%)')
    expect_identical(cd_daily_value(r,'Complete Cases'),'4 (66.7%)')
    expect_identical(cd_daily_value(r,'Unique Values'),'3 (75.0%)')
    t <- r$distribution$asDF
    expect_equal(t$value[t$metric=='Number of Categories'],3)
    expect_match(t$interpretation[t$metric=='Modal Category (frequency)'],'A (2 of 4, 50.0%)',fixed=TRUE)
    expect_equal(t$value[t$metric=='Category Balance Index (Entropy)'],round(1.5/log2(3),3))
    dup <- r$duplicates$asDF
    expect_equal(sum(dup$count),4)
    expect_false(anyNA(dup$value))
    expect_equal(dup$count[dup$value=='NA'],1L)
    expect_match(r$naturalSummary$content,'33.3% missing data',fixed=TRUE)
    expect_false(grepl('only missing values',r$qualityText$content,fixed=TRUE))
  }
})

test_that('VAL-checkdata-01 an all-missing NA factor is not a complete category', {
  x <- factor(rep(NA_character_,6),exclude=NULL)
  r <- cd_daily_run(x)
  expect_identical(cd_daily_value(r,'Missing Values'),'6 (100.0%)')
  expect_identical(cd_daily_value(r,'Complete Cases'),'0 (0.0%)')
  expect_identical(cd_daily_value(r,'Unique Values'),'0 (0.0%)')
  expect_true(all(is.na(r$distribution$asDF$value)))
  expect_equal(nrow(r$duplicates$asDF),0)
  expect_match(r$qualityText$content,'contains only missing values')
})

test_that('VAL-checkdata-02 removing selection clears previous computed outputs and recovers', {
  ns <- asNamespace('ClinicoPath')
  opts <- get('checkdataOptions',ns)$new(var='x',showDistribution=TRUE,
    showDuplicates=TRUE,showPatterns=TRUE,showSummary=TRUE,showAbout=TRUE,showCaveats=TRUE)
  d <- data.frame(x=c(rep(1,6),2,10))
  a <- get('checkdataClass',ns)$new(options=opts,data=d); a$run()
  first <- a$results$outliers$asDF
  expect_gt(nrow(first),0)
  var_opt <- opts$option('var');var_opt$value <- NULL
  p <- a$.__enclos_env__$private;p$.init();p$.run()
  for (nm in c('missingVals','outliers','outlierMethodSummary','distribution','duplicates','patterns'))
    expect_equal(nrow(a$results[[nm]]$asDF),0,info=nm)
  expect_false(a$results$outlierMethodSummary$visible)
  for (nm in c('notices','naturalSummary','aboutAnalysis','caveatsAssumptions','noOutliers'))
    expect_identical(a$results[[nm]]$content,'<div></div>',info=nm)
  expect_identical(a$results$qualityText$content,'')
  var_opt$value <- 'x';p$.init();p$.run()
  fresh <- do.call(ClinicoPath::checkdata,c(list(data=d,var='x'),
    list(showDistribution=TRUE,showDuplicates=TRUE,showPatterns=TRUE,showSummary=TRUE,showAbout=TRUE,showCaveats=TRUE)))
  for (nm in c('missingVals','outliers','outlierMethodSummary','distribution','duplicates','patterns'))
    expect_equal(a$results[[nm]]$asDF,fresh[[nm]]$asDF,info=nm)
  expect_identical(a$results$naturalSummary$content,fresh$naturalSummary$content)
})

test_that('VAL-checkdata-02 reused type/options do not retain old rows titles or notes', {
  ns <- asNamespace('ClinicoPath')
  d <- data.frame(a=c(rep(1,6),2,10),b=1:8)
  opts <- get('checkdataOptions',ns)$new(var='a',showDistribution=TRUE,showDuplicates=TRUE)
  a <- get('checkdataClass',ns)$new(options=opts,data=d);a$run();p <- a$.__enclos_env__$private
  var_opt <- opts$option('var');var_opt$value <- 'b';p$.data <- d;p$.init();p$.run()
  fresh <- do.call(ClinicoPath::checkdata,list(data=d,var='b',showDistribution=TRUE,showDuplicates=TRUE))
  for (nm in c('missingVals','outliers','distribution','duplicates','outlierMethodSummary'))
    expect_equal(a$results[[nm]]$asDF,fresh[[nm]]$asDF,info=nm)
  show_opt <- opts$option('showOutliers');show_opt$value <- FALSE;p$.init();p$.run()
  expect_equal(nrow(a$results$outlierMethodSummary$asDF),0)
  expect_false(a$results$outlierMethodSummary$visible)
  expect_equal(nrow(a$results$outliers$asDF),0)
})
