library(testthat)
test_that('invalid reruns clear prior Venn results and recover', {
 ns <- asNamespace('ClinicoPath')
 d <- data.frame(A=factor(c('Yes','Yes','No','No')), B=factor(c('Yes','No','Yes','No')))
 for (bad in c('empty','missing','deselected')) {
  a <- get('vennClass',ns)$new(options=get('vennOptions',ns)$new(var1='A',var2='B',var1true='Yes',var2true='Yes',clinicalSummary=TRUE,reportSentences=TRUE,showSetCalculations=TRUE,showMembershipTable=TRUE),data=d)
  a$init(); a$run()
  expect_equal(a$results$summary$asDF$trueCount,c(2L,2L))
  p <- a$.__enclos_env__$private
  opt <- a$options$option('var2')
  if (bad=='empty') p$.data <- d[FALSE,,drop=FALSE]
  if (bad=='missing') { z <- d; z$A[] <- NA; p$.data <- z }
  if (bad=='deselected') opt$value <- NULL
  a$run()
  for (nm in c('clinicalSummary','reportSentences','setCalculations'))
   expect_false(grepl('[0-9]',as.character(a$results[[nm]]$content)),info=paste(bad,nm))
  for (nm in c('plotGgvenn','plotGgVennDiagram','plotUpsetR','plotComplexUpset'))
   expect_null(a$results[[nm]]$state,info=paste(bad,nm))
  expect_equal(a$results$membershipTable$rowCount,0L,info=bad)
  sm <- a$results$summary$asDF
  expect_true(all(is.na(sm$trueCount)),info=bad)
  p$.data <- d; opt$value <- 'B'; a$run()
  expect_equal(a$results$summary$asDF$trueCount,c(2L,2L))
 }
})
