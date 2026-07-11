
test_that('circos builds an adjacency matrix from an edge list', {
  skip_if_not_installed('jmvcore')
  skip_if_not_installed('circlize')

  data <- data.frame(
    from_site = c('Primary', 'Primary', 'Regional'),
    to_site   = c('Regional', 'Tertiary', 'Tertiary'),
    n_patients = c(120, 45, 80))

  expect_no_error({
    model <- circos(
      data = data, inputMode = 'edges',
      fromVar = 'from_site', toVar = 'to_site', valueVar = 'n_patients',
      directional = TRUE, symmetric = FALSE,
      showMatrix = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))
  # matrix table has one row per source category
  mt <- model$results$matrixTable$asDF
  expect_true(nrow(mt) >= 3)
})

test_that('circos cross-tabulates two categorical variables', {
  skip_if_not_installed('jmvcore')
  skip_if_not_installed('circlize')

  set.seed(2)
  data <- data.frame(
    a = sample(c('A', 'B', 'C'), 100, TRUE),
    b = sample(c('X', 'Y'), 100, TRUE))
  expect_no_error({
    model <- circos(data = data, inputMode = 'crosstab',
                    fromVar = 'a', toVar = 'b', showMatrix = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))
})
