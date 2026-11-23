testthat::test_that('missing required selections clear computed outputs and recover', {
 d <- data.frame(study=paste0('S',1:12),tp=c(40,30,90,50,60,25,38,42,75,20,65,36),fp=c(10,25,5,30,8,32,9,26,4,30,12,18),fn=c(30,10,5,10,12,28,9,27,8,30,7,14),tn=c(40,35,95,30,65,18,48,32,65,20,70,42))
 a<-ClinicoPath:::diagnosticmetaClass$new(options=ClinicoPath:::diagnosticmetaOptions$new(study='study',true_positives='tp',false_positives='fp',false_negatives='fn',true_negatives='tn',show_analysis_summary=TRUE,forest_plot=TRUE,sroc_plot=TRUE,funnel_plot=TRUE,publication_bias=TRUE),data=d)
 a$init();a$run()
 expected <- a$results$bivariateresults$asDF$estimate
 for (key in c('study','true_positives','false_positives','false_negatives','true_negatives')) {
  opt<-a$options$option(key);old<-opt$value;opt$value<-NULL;a$run()
  for (nm in c('bivariateresults','hsrocresults','heterogeneity','publicationbias','predictiveValues')) {
   tab<-a$results[[nm]]
   for (col in tab$columns) if(col$type %in% c('number','integer'))
    testthat::expect_true(all(is.na(tab$asDF[[col$name]])),info=paste(key,nm,col$name))
   testthat::expect_length(tab$notes,0)
  }
  testthat::expect_equal(a$results$individualstudies$rowCount,0)
  testthat::expect_equal(a$results$metaregression$rowCount,0)
  testthat::expect_true(!nzchar(a$results$summary$content))
  for(nm in c('forestplot','srocplot','funnelplot')) testthat::expect_null(a$results[[nm]]$state)
  opt$value<-old;a$run()
  testthat::expect_equal(a$results$bivariateresults$asDF$estimate,expected,tolerance=1e-8)
 }
 for (empty in list(d[0, ], NULL)) {
  a$.__enclos_env__$private$.data <- empty
  a$.__enclos_env__$private$.run()
  testthat::expect_true(all(is.na(a$results$bivariateresults$asDF$estimate)))
  testthat::expect_true(!nzchar(a$results$summary$content))
  a$.__enclos_env__$private$.data <- d
  a$run()
  testthat::expect_equal(a$results$bivariateresults$asDF$estimate,expected,tolerance=1e-8)
 }
})
