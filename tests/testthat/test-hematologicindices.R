
test_that('hematologicindices computes NLR/PLR/SII/PNI correctly', {
  skip_if_not_installed('jmvcore')

  # single deterministic row to check the arithmetic
  data <- data.frame(
    neut = 6, lymph = 1.5, plt = 300, mono = 0.5,
    alb = 4.0, crp = 15)

  expect_no_error({
    model <- hematologicindices(
      data = data, neutrophils = 'neut', lymphocytes = 'lymph',
      platelets = 'plt', monocytes = 'mono', albumin = 'alb',
      albuminUnit = 'gdl', crp = 'crp',
      indices = c('nlr', 'plr', 'lmr', 'sii', 'pni', 'car', 'gps'),
      gpsType = 'modified',
      showIndicesTable = TRUE, showPlot = FALSE, showExplanation = TRUE)
  })
  expect_true(inherits(model, 'jmvcoreClass'))

  it <- model$results$indicesTable$asDF
  # NLR = 6/1.5 = 4 ; PLR = 300/1.5 = 200 ; LMR = 1.5/0.5 = 3
  expect_equal(it$mean[it$index == 'NLR'], 4)
  expect_equal(it$mean[it$index == 'PLR'], 200)
  expect_equal(it$mean[it$index == 'LMR'], 3)
  # SII = 300*6/1.5 = 1200
  expect_equal(it$mean[it$index == 'SII'], 1200)
  # PNI = 10*4.0 + 0.005*(1.5*1000) = 40 + 7.5 = 47.5
  expect_equal(it$mean[it$index == 'PNI'], 47.5)
})

test_that('mGPS scoring follows CRP/albumin thresholds', {
  skip_if_not_installed('jmvcore')
  # CRP 15 (>10) + albumin 30 g/L (<35) -> mGPS 2
  data <- data.frame(neut = 5, lymph = 2, plt = 250, alb = 30, crp = 15)
  model <- hematologicindices(
    data = data, neutrophils = 'neut', lymphocytes = 'lymph', platelets = 'plt',
    albumin = 'alb', albuminUnit = 'gl', crp = 'crp',
    indices = c('gps'), gpsType = 'modified')
  gt <- model$results$gpsTable$asDF
  expect_equal(gt$n[gt$score == 'mGPS = 2'], 1)
})

test_that('hematologicindices links an index to survival', {
  skip_if_not_installed('jmvcore')
  set.seed(1); n <- 200
  lymph <- pmax(0.2, rnorm(n, 1.8, 0.5)); neut <- pmax(0.5, rnorm(n, 5, 2))
  plt <- pmax(50, rnorm(n, 270, 80))
  time <- round(rexp(n, 0.05 * (0.5 + (neut/lymph)/5)), 1)
  dead <- factor(sample(c('Alive', 'Dead'), n, TRUE), levels = c('Alive', 'Dead'))
  data <- data.frame(neut, lymph, plt, time, dead)
  expect_no_error({
    model <- hematologicindices(
      data = data, neutrophils = 'neut', lymphocytes = 'lymph', platelets = 'plt',
      indices = c('nlr'), survivalIndex = 'nlr', splitMethod = 'median',
      survivalTime = 'time', survivalStatus = 'dead', eventLevel = 'Dead',
      showSurvival = TRUE)
  })
  st <- model$results$survivalTable$asDF
  expect_equal(nrow(st), 2)
  expect_equal(st$hr[1], 1)     # Low group reference
})
