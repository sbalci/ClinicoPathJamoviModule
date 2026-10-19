library(testthat)
options(testthat.progress.max_fails = Inf)
or_daily_data <- function() {
  data.frame(test = factor(rep(c('Present','Present','Absent','Absent'), c(36,12,24,48)),
                          levels = c('Absent','Present')),
             out = factor(rep(c('Yes','No','Yes','No'), c(36,12,24,48)),
                          levels = c('No','Yes')))
}
or_daily_text <- function(x) gsub('[[:space:]]+', ' ', gsub('<[^>]*>', ' ', paste(x, collapse=' ')))
or_daily_run <- function(d, firth=FALSE, explanatory='test', nom=FALSE, diag=NULL) {
  do.call(ClinicoPath::oddsratio, list(data=d, explanatory=explanatory, outcome='out',
    outcomeLevel='Yes', predictorLevel='Present', diagnosticPredictor=diag,
    showNomogram=nom, usePenalized=firth, showExplanations=TRUE))
}

test_that('VAL-oddsratio-01 explicit missing predictor levels match true missingness', {
  # Base R as.character recovers actual missingness; literal NA strings are categories.
  for (ordered in c(FALSE, TRUE)) for (firth in c(FALSE, TRUE)) {
    d <- or_daily_data()
    if (ordered) d$test <- ordered(d$test, levels=levels(d$test))
    d$test <- addNA(d$test); d$test[1:5] <- NA
    attr(d$test, 'label') <- 'Exposure label'
    attr(d$test, 'values') <- c(10L,20L,NA_integer_)
    before <- d
    clean <- d; clean$test <- factor(as.character(d$test), levels=c('Absent','Present'), ordered=ordered)
    a <- or_daily_run(d, firth=firth); b <- or_daily_run(clean, firth=firth)
    expect_identical(or_daily_text(a$text$content), or_daily_text(b$text$content))
    expect_identical(or_daily_text(a$text2$content), or_daily_text(b$text2$content))
    expect_match(or_daily_text(a$text2$content), if(firth) 'Observations: 115' else 'Number in model = 115', fixed=TRUE)
    expect_match(or_daily_text(a$infoMessages$content), '5 out of 120', fixed=TRUE)
    expect_identical(d, before)
  }
})

test_that('VAL-oddsratio-01 explicit missing outcomes match true missingness', {
  d <- or_daily_data(); d$out <- addNA(d$out); d$out[1:5] <- NA
  clean <- d; clean$out <- factor(as.character(d$out), levels=c('No','Yes'))
  for (firth in c(FALSE, TRUE)) {
    a <- or_daily_run(d, firth=firth); b <- or_daily_run(clean, firth=firth)
    expect_identical(or_daily_text(a$text$content), or_daily_text(b$text$content))
    expect_identical(or_daily_text(a$text2$content), or_daily_text(b$text2$content))
  }
})

test_that('VAL-oddsratio-01 diagnostic missingness stays separate from model missingness', {
  d <- or_daily_data(); d$diag <- addNA(d$test); d$diag[1:5] <- NA
  clean <- d; clean$diag <- factor(as.character(d$diag), levels=c('Absent','Present'))
  a <- or_daily_run(d, nom=TRUE, diag='diag'); b <- or_daily_run(clean, nom=TRUE, diag='diag')
  expect_identical(or_daily_text(a$diagnosticMetrics$content), or_daily_text(b$diagnosticMetrics$content))
  # Independently counted: five true positives removed; model uses the complete test column.
  expect_match(or_daily_text(a$diagnosticMetrics$content), 'TP: 31, FP: 12, FN: 24, TN: 48', fixed=TRUE)
  expect_match(or_daily_text(a$diagnosticMetrics$content), 'Based on 115 observations', fixed=TRUE)
  expect_match(or_daily_text(a$text2$content), 'Number in model = 120', fixed=TRUE)
})

test_that('VAL-oddsratio-01 literal NA remains an observed category', {
  d <- or_daily_data(); x <- as.character(d$test); x[1:5] <- 'NA'
  d$test <- factor(x, levels=c('Absent','Present','NA'))
  a <- or_daily_run(d, firth=TRUE)
  expect_match(or_daily_text(a$text2$content), 'Observations: 120', fixed=TRUE)
  expect_match(or_daily_text(a$text$content), ' NA ', fixed=TRUE)
})

test_that('VAL-oddsratio-02 Firth criterion discloses parameterisation dependence', {
  # logistf anova uses a common/full-model penalty for nested model comparison.
  d <- or_daily_data(); d$x <- seq_len(nrow(d))/30 + sin(seq_len(nrow(d)))
  criterion <- numeric()
  for (scale in c(1,100)) {
    z <- d; z$x <- z$x * scale
    a <- or_daily_run(z, firth=TRUE, explanatory='x')
    txt <- or_daily_text(a$text2$content)
    criterion <- c(criterion, as.numeric(sub(' .*', '', sub('.*Penalized AIC: ', '', txt))))
    expect_false(grepl('Compare it only with other Firth models', txt, fixed=TRUE))
    expect_match(txt, 'not suitable for comparing separately fitted Firth models', fixed=TRUE)
    expect_match(txt, 'parameterisation', fixed=TRUE)
    expect_match(txt, 'common-penalty', fixed=TRUE)
  }
  # Jeffreys log penalty gains log(100); displayed criterion is rounded to 0.1.
  expect_lt(abs(diff(criterion) + 2 * log(100)), 0.11)
})
