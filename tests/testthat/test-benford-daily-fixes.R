# Syntax is parsed and inspected, never executed. Restored options may bypass types.
make_bf_syntax <- function() {
  o <- ClinicoPath:::benfordOptions$new()
  list(options=o, analysis=ClinicoPath:::benfordClass$new(options=o,
       data=data.frame(v=1:100)))
}
testthat::test_that('saved character option values remain literal in regenerated syntax', {
  for (key in c('var', 'digits')) for (value in c(
    '2", var = {stop("CANARY")}, digits = "2', 'trailing\\', 'line\nbreak',
    'Serum <lab> & "quote"')) {
    obj <- make_bf_syntax(); opt <- obj$options$option(key); opt$value <- value
    expr <- tryCatch(parse(text=obj$analysis$asSource())[[1]], error=function(e) NULL)
    testthat::expect_false(is.null(expr), info=paste(key, value))
    if (is.null(expr)) next
    testthat::expect_identical(expr[[key]], value)
    testthat::expect_setequal(names(as.list(expr)[-1]), c('data', key))
    testthat::expect_false(is.call(expr[[key]]))
  }
})
testthat::test_that('undeclared and list options cannot add syntax arguments', {
  obj <- make_bf_syntax()
  obj$options$.addOption(jmvcore:::Option$new(
    'digits = 1, var = {stop("CANARY")}, unused', 3))
  expr <- parse(text=obj$analysis$asSource())[[1]]
  testthat::expect_identical(names(as.list(expr)[-1]), 'data')
  for (key in c('var', 'digits')) {
    obj <- make_bf_syntax(); opt <- obj$options$option(key)
    opt$value <- list('bad"name'= '2", var = {stop("CANARY")}, digits = "2')
    expr <- tryCatch(parse(text=obj$analysis$asSource())[[1]], error=function(e) NULL)
    testthat::expect_false(is.null(expr))
    if (!is.null(expr)) testthat::expect_identical(names(as.list(expr)[-1]), 'data')
  }
})
testthat::test_that('ordinary syntax preserves defaults and numeric options', {
  obj <- make_bf_syntax(); expr <- parse(text=obj$analysis$asSource())[[1]]
  testthat::expect_identical(names(as.list(expr)[-1]), 'data')
  opt <- obj$options$option('digits'); opt$value <- 3L
  expr <- parse(text=obj$analysis$asSource())[[1]]
  testthat::expect_equal(expr$digits, 3)
})
testthat::test_that('variable syntax cannot inject a callable numeric argument', {
  obj <- make_bf_syntax(); opt <- obj$options$option('var')
  payload <- 'NULL, digits = {stop("CANARY")}'
  opt$value <- payload
  expr <- parse(text=obj$analysis$asSource())[[1]]
  # match.call checks wrapper binding without forcing any supplied expression.
  bound <- match.call(ClinicoPath::benford, expr)
  testthat::expect_identical(bound$var, payload)
  testthat::expect_false('digits' %in% names(as.list(bound)))
})
