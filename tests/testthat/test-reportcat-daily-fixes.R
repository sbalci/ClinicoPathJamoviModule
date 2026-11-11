library(testthat)
rc_daily_call <- function(args) {
  withCallingHandlers(do.call(ClinicoPath::reportcat, args), warning = function(w) {
    if (grepl("cannot open file.*[/]sass[/]", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })
}
test_that('actual NA factor levels are missing while literal NA remains observed', {
  for (ordered in c(FALSE, TRUE)) {
    f <- factor(c('A','B','B',NA,NA,NA,'NA'), levels=c('A','B','NA',NA), exclude=NULL, ordered=ordered)
    before <- f
    r <- rc_daily_call(list(data=data.frame(g=f),vars='g'))
    txt <- as.character(r$text$content)
    expect_match(txt,'Missing values: 3',fixed=TRUE)
    expect_match(txt,'of 4 valid cases',fixed=TRUE)
    expect_match(txt,'B: n = 2, 50.0% of valid cases',fixed=TRUE)
    expect_match(txt,'NA: n = 1, 25.0% of valid cases',fixed=TRUE)
    expect_match(as.character(r$reportSentences$content),'n = 2 of 4 valid cases, 50.0%',fixed=TRUE)
    expect_match(as.character(r$clinicalSummary$content),'42.9%',fixed=TRUE)
    expect_identical(f,before)
  }
})
test_that('a variable containing only a missing factor level is excluded', {
  f <- factor(rep(NA_character_,6),levels=NA_character_,exclude=NULL)
  expect_error(rc_daily_call(list(data=data.frame(g=f),vars='g')),'nothing to summarize')
})
