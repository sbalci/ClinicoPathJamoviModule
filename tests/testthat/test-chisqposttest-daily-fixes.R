library(testthat)
cq_daily_data <- function() {
  data.frame(r=factor(rep(c('A','B','C'),each=40)),
    c=factor(c(rep('X',35),rep('Y',5),rep('X',20),rep('Y',20),rep('X',5),rep('Y',35))))
}
cq_daily_run <- function(d,...) do.call(chisqposttest,c(list(data=d,rows='r',cols='c'),list(...)))

test_that('VAL-chisqposttest-01 actual NA levels are excluded before every calculation', {
  d<-cq_daily_data(); ref<-cq_daily_run(d,showClinicalSummary=TRUE,exportResults=TRUE)
  for (ordered in c(FALSE,TRUE)) {
    x<-data.frame(r=factor(c(as.character(d$r),NA,NA),levels=c('C','A','B',NA),
                          exclude=NULL,ordered=ordered),
                  c=factor(c(as.character(d$c),'X','Y')))
    attr(x$r,'label')<-'Grade';before<-x
    got<-cq_daily_run(x,showClinicalSummary=TRUE,exportResults=TRUE)
    expect_identical(x,before)
    expect_equal(got$chisqTable$asDF$value,45)
    expect_equal(got$chisqTable$asDF$df,2L)
    expect_equal(got$chisqTable$asDF$p,exp(-45/2),tolerance=1e-12)
    expect_match(got$notices$content,'2 of 122 rows',fixed=TRUE)
    expect_match(got$notices$content,'120 observations in a 3 x 2 table',fixed=TRUE)
    expect_match(got$clinicalSummary$content,'n = 120',fixed=TRUE)
    expect_equal(sum(as.numeric(got$exportTable$asDF$value[
      got$exportTable$asDF$category=='Contingency Table'])),120)
    expect_equal(nrow(got$posthocTable$asDF),3L)
    # Same policy for frequency-table row and count variables.
    w<-as.data.frame(table(d$r,d$c));names(w)<-c('r','c','n')
    w<-rbind(w,data.frame(r=NA_character_,c='X',n=9))
    w$r<-factor(w$r,exclude=NULL);weighted<-cq_daily_run(w,counts='n')
    expect_equal(weighted$chisqTable$asDF,ref$chisqTable$asDF)
    expect_match(weighted$notices$content,'1 of 7 rows',fixed=TRUE)
  }
  # The literal text NA is observed data, independent of a genuine NA level.
  x<-data.frame(r=factor(c(as.character(d$r),'NA','NA',NA),exclude=NULL),
                c=factor(c(as.character(d$c),'X','Y','X')))
  got<-cq_daily_run(x)
  expect_equal(got$chisqTable$asDF$df,3L)
  expect_match(got$notices$content,'1 of 123 rows',fixed=TRUE)
  expect_match(got$notices$content,'122 observations in a 4 x 2 table',fixed=TRUE)
})

test_that('VAL-chisqposttest-02 empty selection clears prior computed results and recovers', {
  d<-cq_daily_data()
  settings<-list(rows='r',cols='c',showClinicalSummary=TRUE,copyReadySentences=TRUE,
    showResiduals=TRUE,exportResults=TRUE,showDetailedTables=TRUE,showEducational=TRUE,
    showAssumptionsCheck=TRUE,showGlossary=TRUE)
  opts<-do.call(chisqposttestOptions$new,settings)
  a<-chisqposttestClass$new(options=opts,data=d);a$run();p<-a$.__enclos_env__$private
  expect_equal(nrow(a$results$posthocTable$asDF),3L)
  opt<-opts$option('rows');opt$value<-NULL;p$.init();p$.run()
  expect_true(all(is.na(a$results$chisqTable$asDF$value)))
  expect_true(all(is.na(a$results$chisqTable$asDF$p)))
  expect_equal(nrow(a$results$posthocTable$asDF),0L)
  expect_equal(nrow(a$results$exportTable$asDF),0L)
  for(nm in c('contingencyTable','clinicalSummary','reportSentences','assumptionsCheck',
              'residualsGuidance','residualsAnalysis','detailedComparisons','multipleTestingInfo',
              'educationalOverview','glossaryPanel','weightedDataInfo'))
    expect_identical(a$results[[nm]]$content,'<div></div>',info=nm)
  opt$value<-'r';p$.init();p$.run()
  fresh<-do.call(chisqposttest,c(list(data=d),settings))
  for(nm in c('chisqTable','posthocTable','exportTable'))
    expect_equal(a$results[[nm]]$asDF,fresh[[nm]]$asDF,info=nm)
  expect_identical(a$results$clinicalSummary$content,fresh$clinicalSummary$content)
  # A subsequent valid null table must not retain old detailed significant comparisons.
  p$.data<-data.frame(r=factor(rep(c('A','B','C'),each=40)),
                     c=factor(rep(c('X','Y'),60)))
  p$.init();p$.run()
  expect_equal(nrow(a$results$posthocTable$asDF),0L)
  expect_identical(a$results$detailedComparisons$content,'<div></div>')
  expect_match(a$results$clinicalSummary$content,'was not statistically significant at',fixed=TRUE)
})

test_that('VAL-chisqposttest-03 residual example respects selected cutoff', {
  d<-cq_daily_data()
  for(cutoff in c(2,3.2,4)) {
    got<-cq_daily_run(d,showResiduals=TRUE,residualsCriterion='fixed',residualsCutoff=cutoff)
    text<-got$residualsGuidance$content
    expect_match(text,sprintf('cutoff \u00b1%.2f',cutoff),fixed=TRUE)
    expect_identical(grepl('does not exceed',text,fixed=TRUE),3.2<=cutoff)
    expect_identical(grepl('exceeds the selected cutoff',text,fixed=TRUE),3.2>cutoff)
  }
})
library(testthat)
test_that('VAL-chisqposttest-04 omnibus gate is described as policy not universal validity', {
  tab<-matrix(c(59,41,42,58,50,50),3,byrow=TRUE)
  # Independent arithmetic: a prespecified Bonferroni family can reject a pair
  # even when the omnibus is not significant. This does not change the UI policy.
  e<-outer(rowSums(tab),colSums(tab))/sum(tab)
  global_p<-exp(-sum((tab-e)^2/e)/2) # chi-square df2 survival
  sub<-tab[1:2,];se<-outer(rowSums(sub),colSums(sub))/sum(sub)
  adjusted_pair_p<-min(1,3*2*pnorm(-sqrt(sum((sub-se)^2/se))))
  expect_gt(global_p,.05);expect_lt(adjusted_pair_p,.05)
  d<-expand.grid(r=c('A','B','C'),c=c('X','Y'));d$n<-as.vector(tab)
  for(method in c('chisquare','fisher')) {
    got<-do.call(chisqposttest,list(data=d,rows='r',cols='c',counts='n',testSelection=method))
    expect_equal(nrow(got$posthocTable$asDF),0L)
    text<-got$multipleTestingInfo$content
    expect_false(grepl('only valid when',text,fixed=TRUE))
    expect_false(grepl('constitutes data dredging',text,fixed=TRUE))
    expect_match(text,'policy',fixed=TRUE)
  }
  # 1-(1-alpha)^m is the independence calculation, not an arbitrary dependence result.
  d$n<-c(35,20,5,5,20,35)
  got<-do.call(chisqposttest,list(data=d,rows='r',cols='c',counts='n',showEducational=TRUE))
  expect_match(got$multipleTestingInfo$content,'if the tests were independent',fixed=TRUE)
})
