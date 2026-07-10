
test_that('winratio primary-endpoint analysis works', {
  skip_if_not_installed('jmvcore')

  set.seed(42)
  n <- 60
  t_tx <- rexp(n, 1/24); t_ct <- rexp(n, 1/12); cens <- 36
  data <- data.frame(
    arm    = factor(rep(c('Treatment', 'Control'), each = n),
                    levels = c('Control', 'Treatment')),
    dtime  = pmin(c(t_tx, t_ct), cens),
    devent = factor(as.integer(c(t_tx, t_ct) <= cens))
  )

  expect_no_error({
    model <- winratio(
      data = data,
      group = 'arm', refLevel = 'Control',
      time1 = 'dtime', status1 = 'devent', eventLevel1 = '1',
      conf_level = 0.95, ciMethod = 'analytic',
      showWinOdds = TRUE, showNetBenefit = TRUE,
      showComponents = TRUE, showPlot = TRUE,
      showSummary = TRUE, showExplanation = TRUE
    )
  })

  expect_true(inherits(model, 'jmvcoreClass'))

  # Win ratio should favour Treatment (WR > 1) with better simulated survival
  res <- model$results$mainTable$asDF
  wr <- res$estimate[res$statistic == 'Win ratio']
  expect_true(is.finite(wr) && wr > 1)

  # Wins + losses + ties must equal n1 * n0
  counts <- model$results$countsTable$asDF
  total <- counts$count[counts$label == 'Total pairs']
  expect_equal(total, n * n)
})

test_that('winratio hierarchical composite (three endpoints) works', {
  skip_if_not_installed('jmvcore')

  set.seed(2026); n <- 80; cens <- 36
  mk <- function(arm, dr, hr, wm) {
    dt <- rexp(n, dr); ht <- rexp(n, hr)
    data.frame(arm = arm,
               dtime = pmin(dt, cens), dev = factor(as.integer(dt <= cens)),
               htime = pmin(ht, cens), hev = factor(as.integer(ht <= cens)),
               walk = rnorm(n, wm, 40))
  }
  data <- rbind(mk('Treatment', 1/48, 1/30, 25), mk('Control', 1/24, 1/15, -10))
  data$arm <- factor(data$arm, levels = c('Control', 'Treatment'))

  expect_no_error({
    model <- winratio(
      data = data,
      group = 'arm', refLevel = 'Control',
      time1 = 'dtime', status1 = 'dev', eventLevel1 = '1',
      time2 = 'htime', status2 = 'hev', eventLevel2 = '1',
      contEndpoint = 'walk', contDirection = 'higher',
      ciMethod = 'analytic', showComponents = TRUE
    )
  })

  comp <- model$results$componentsTable$asDF
  # every priority endpoint should appear in the contribution breakdown
  expect_equal(nrow(comp), 3)
  # the primary (death) endpoint should decide the most pairs
  expect_true(comp$decided[1] >= max(comp$decided[-1]))
})
