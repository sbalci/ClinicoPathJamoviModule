library(testthat)
dq_call_daily <- function(d, ...) do.call(ClinicoPath::dataquality, c(list(data=d, vars=names(d)),list(...)))
dq_row_daily <- function(r, name) {
 doc <- xml2::read_html(as.character(r$text$content))
 rows <- xml2::xml_find_all(doc, '//tr[td]')
 cells <- lapply(rows, function(x) trimws(xml2::xml_text(xml2::xml_find_all(x,'./td'))))
 hits <- Filter(function(x) identical(x[1],name),cells)
 stopifnot(length(hits)==1); hits[[1]]
}
test_that('DQ02 all panels compare unrounded missing counts with threshold', {
 for (nmiss in c(249L,250L,251L,1251L)) {
  d <- data.frame(flag=c(rep(NA,nmiss),seq_len(2500-nmiss)),other=seq_len(2500))
  r <- dq_call_daily(d,check_missing=TRUE,plot_missing_patterns=TRUE)
  expect_match(r$summary$content, sprintf('%d variable%s exceed%s 10%% missing threshold',
    as.integer(nmiss>250),if(nmiss>250) '' else 's',if(nmiss>250) 's' else ''),fixed=TRUE)
  expect_identical(grepl('Variables exceeding 10% missing:',r$text$content,fixed=TRUE), nmiss>250)
  expect_identical(grepl('Moderate Missingness',r$recommendations$content,fixed=TRUE),nmiss>250 && nmiss<=1250)
  expect_identical(grepl('High Missingness',r$recommendations$content,fixed=TRUE),nmiss>1250)
 }
})
test_that('DQ03 zero IQR still applies the advertised Tukey fences', {
 for(x in list(c(rep(0,11),10), c(-10,rep(0,11)),rep(0,12))) {
  r <- dq_call_daily(data.frame(x=x)); cells <- dq_row_daily(r,'x')
  expect_equal(as.numeric(cells[10:11]),c(sum(x!=0),round(100*sum(x!=0)/length(x),1)))
 }
})
test_that('DQ04 reused analyses clear HTML and plot state on invalid input and recover', {
 ns <- asNamespace('ClinicoPath'); d <- data.frame(x=1:35,y=rep(c('a','b'),length.out=35))
 opts <- get('dataqualityOptions',ns)$new(vars=names(d),check_missing=TRUE,plot_data_overview=TRUE,
   plot_missing_patterns=TRUE,plot_data_types=TRUE,showExplanations=TRUE)
 a <- get('dataqualityClass',ns)$new(options=opts,data=d); a$run()
 p <- a$.__enclos_env__$private
 baseline <- as.character(a$results$text$content)
 for(mode in c('unset','empty')) {
  if(mode=='unset') { opt <- opts$option('vars'); opt$value <- NULL } else {p$.data <- d[0,]}
  p$.run()
  for(nm in c('text','summary','recommendations','explanations')) {
   expect_false(grepl('Variable Quality Summary|Plain-Language Summary|Recommended Actions|Understanding Quality Metrics',
      as.character(a$results[[nm]]$content)))
  }
  for(nm in c('plotDataOverview','plotMissingPatterns','plotDataTypes')) expect_null(a$results[[nm]]$state)
  opt <- opts$option('vars'); opt$value <- names(d); p$.data <- d; p$.run()
  expect_identical(as.character(a$results$text$content),baseline)
  expect_true(all(vapply(c('plotDataOverview','plotMissingPatterns','plotDataTypes'),function(nm)!is.null(a$results[[nm]]$state),logical(1))))
 }
})
