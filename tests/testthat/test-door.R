
test_that('door probability equals Mann-Whitney AUC', {
  skip_if_not_installed('jmvcore')

  set.seed(3)
  n <- 80
  data <- rbind(
    data.frame(arm = 'Treatment',
               door = sample(1:4, n, TRUE, c(.4, .3, .2, .1))),
    data.frame(arm = 'Control',
               door = sample(1:4, n, TRUE, c(.15, .25, .3, .3))))
  data$arm <- factor(data$arm, levels = c('Control', 'Treatment'))

  expect_no_error({
    model <- door(
      data = data, group = 'arm', refLevel = 'Control',
      doorRank = 'door', rankDirection = 'lower', conf_level = 0.95,
      showDistribution = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  res <- model$results$mainTable$asDF
  door_prob <- res$estimate[1]
  # compare with Mann-Whitney AUC (lower door = more desirable -> use -rank)
  tx <- data$door[data$arm == 'Treatment']; ct <- data$door[data$arm == 'Control']
  auc <- wilcox.test(-tx, -ct)$statistic / (length(tx) * length(ct))
  expect_equal(round(door_prob, 3), round(unname(auc), 3))
})
