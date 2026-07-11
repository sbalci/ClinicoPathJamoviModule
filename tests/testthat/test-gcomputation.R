
test_that('gcomputation recovers a marginal effect adjusting for confounders', {
  skip_if_not_installed('jmvcore')

  set.seed(2026); n <- 800
  age <- round(rnorm(n, 60, 10))
  stage <- factor(sample(c('I', 'II', 'III'), n, TRUE, c(.4, .35, .25)))
  sev <- as.integer(stage)
  treated <- rbinom(n, 1, plogis(-1 + 0.5 * sev + 0.02 * (age - 60)))
  death <- rbinom(n, 1, plogis(-1.5 + 0.7 * treated + 0.6 * sev + 0.03 * (age - 60)))
  data <- data.frame(
    death = factor(ifelse(death == 1, 'Dead', 'Alive'), levels = c('Alive', 'Dead')),
    treated = factor(ifelse(treated == 1, 'Treated', 'Control'), levels = c('Control', 'Treated')),
    age = age, stage = stage)

  expect_no_error({
    model <- gcomputation(
      data = data, outcome = 'death', outcomeType = 'binary',
      outcomeEvent = 'Dead', treatment = 'treated', treatmentLevel = 'Treated',
      covariates = c('age', 'stage'), bootstrap_n = 200, conf_level = 0.95,
      showCounterfactual = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$mainTable$asDF
  rd <- res$estimate[res$measure == 'Risk difference (ATE)']
  # adjusted RD should be positive but below the confounded crude (~0.31)
  expect_true(rd > 0 && rd < 0.31)
  # risk ratio row present
  expect_true('Risk ratio' %in% res$measure)
})
