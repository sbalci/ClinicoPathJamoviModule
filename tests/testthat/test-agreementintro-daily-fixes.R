# Missingness oracle: explicit NA factor levels denote missing, literal "NA" is a category.
# R base factor documentation, Details and Examples (factor(exclude = NULL)).
daily_guide <- function(data, ...) {
  args <- utils::modifyList(list(data = data, purpose = 'categorical', scale = 'nominal',
    nRaters = 2L, panel = 'same', missingRatings = 'yes', repeatedReadings = 'no',
    analysisUnit = 'independent', inspectData = TRUE, ratings = c('A','B'), caseID = 'case'),
    list(...), keep.null = TRUE)
  do.call(ClinicoPath::agreementintro, args)
}
daily_fixture <- function() data.frame(case = 1:4,
  A = factor(c('low','high',NA,'low'), levels = c('low','mid','high')),
  B = factor(c('low','high','low','high'), levels = c('low','mid','high')))

testthat::test_that('AGI-D01 explicit NA factor levels match real missing ratings', {
  for (ordered in c(FALSE, TRUE)) {
    d <- daily_fixture()
    if (ordered) d[2:3] <- lapply(d[2:3], function(x) ordered(x, levels = levels(x)))
    expected <- daily_guide(d, scale = if (ordered) 'ordinal' else 'nominal',
      categoryOrder = 'confirmed', weights = 'linear')
    d$A <- addNA(d$A)
    original <- serialize(d, NULL)
    actual <- NULL
    testthat::expect_no_error(actual <- daily_guide(d,
      scale = if (ordered) 'ordinal' else 'nominal', categoryOrder = 'confirmed', weights = 'linear'))
    if (!is.null(actual)) {
      for (item in c('columnSummary','dataSummary','recommendations','settings'))
        testthat::expect_equal(actual[[item]]$asDF, expected[[item]]$asDF, info = item)
      testthat::expect_identical(actual$status$content, expected$status$content)
      testthat::expect_identical(actual$inspectionNotes$content, expected$inspectionNotes$content)
    }
    testthat::expect_identical(serialize(d,NULL), original)
  }
})
testthat::test_that('AGI-D01 missing reference classes and IDs cannot bypass inspection', {
  d <- daily_fixture(); d$A <- factor(c('low','high','low','high'))
  d$truth <- addNA(factor(c('low','high',NA,'low')))
  actual <- NULL
  testthat::expect_no_error(actual <- daily_guide(d, purpose='readerAccuracy', scale='binary',
    reference='truth', referenceType='disease', positiveLabel='high', negativeLabel='low',
    referencePositive='high', referenceNegative='low'))
  if (!is.null(actual)) testthat::expect_match(actual$status$content, 'READER_BINARY', fixed=TRUE)
  d$case <- addNA(factor(c('c1',NA,'c3','c4')))
  actual <- NULL
  testthat::expect_no_error(actual <- daily_guide(d))
  if (!is.null(actual)) {
    testthat::expect_match(actual$status$content, 'identifier contains missing', fixed=TRUE)
    testthat::expect_equal(nrow(actual$recommendations$asDF),0)
  }
})
testthat::test_that('AGI-D01 literal NA category remains observed', {
  d <- daily_fixture(); d$A <- factor(c('low','high','NA','low'))
  r <- daily_guide(d)
  testthat::expect_equal(r$columnSummary$asDF$missing, c(0,0))
  testthat::expect_equal(r$columnSummary$asDF$observed, c(4,4))
  testthat::expect_equal(r$columnSummary$asDF$categories, c(3,2))
})
testthat::test_that('AGI-D02 displayed catalogue versions match source schemas', {
  r <- do.call(ClinicoPath::agreementintro, list(data=data.frame(),showCatalogue=TRUE))
  root <- getNamespaceInfo(asNamespace('ClinicoPath'), 'path')
  testthat::skip_if_not(file.exists(file.path(root, 'jamovi/agreement.a.yaml')),
    'target schemas are not included in an installed package')
  for (n in c('agreement','pathagreement','kappaSizeCI','kappaSizePower','kappaSizeFixedN')) {
    expected <- yaml::read_yaml(file.path(root,'jamovi',paste0(n,'.a.yaml')))$version
    testthat::expect_identical(r$catalogue$asDF$version[r$catalogue$asDF$analysis==n], expected, info=n)
  }
})
testthat::test_that('AGI-D03 wide condition and occasion IDs require complete values', {
  for (role in c('condition','occasion')) for (kind in c('NA','blank','factorNA')) {
    d <- daily_fixture(); d$A[3] <- 'low'
    d$key <- switch(kind, 'NA'=c('first',NA,'first','first'),
      blank=c('first',' ','first','first'), factorNA=addNA(factor(c('first',NA,'first','first'))))
    args <- setNames(list('key'),role)
    r <- do.call(daily_guide,c(list(data=d), args))
    testthat::expect_match(r$status$content,'identifier contains missing',fixed=TRUE,
      info=paste(role,kind))
    testthat::expect_equal(nrow(r$recommendations$asDF),0,info=paste(role,kind))
  }
})
