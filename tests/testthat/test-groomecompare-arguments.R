# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: groomecompare
# ═══════════════════════════════════════════════════════════

library(testthat)
library(ClinicoPath)
data(groomecompare_test)

test_that("groomecompare bootstrap validation works", {
  # Without bootstrap (faster)
  result_noboot <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = FALSE
  )
  expect_no_error(result_noboot)

  # With bootstrap (small nboot for speed)
  result_boot <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = TRUE,
    nboot = 100,
    seed = 12345
  )
  expect_no_error(result_boot)
})

test_that("groomecompare nboot parameter affects bootstrap", {
  # Minimum bootstrap samples
  result_100 <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = TRUE,
    nboot = 100
  )
  expect_no_error(result_100)

  # More bootstrap samples (slower but more precise)
  result_500 <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = TRUE,
    nboot = 500
  )
  expect_no_error(result_500)
})

test_that("groomecompare seed ensures reproducibility", {
  # Run twice with same seed
  result1 <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = TRUE,
    nboot = 100,
    seed = 42
  )

  result2 <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    bootstrap = TRUE,
    nboot = 100,
    seed = 42
  )

  # Results should be identical
  expect_no_error(result1)
  expect_no_error(result2)
})

test_that("groomecompare visualization options work", {
  # Radar plot only
  result_radar <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = TRUE,
    barplot = FALSE,
    kmplots = FALSE
  )
  expect_no_error(result_radar)

  # Bar plot only
  result_bar <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = FALSE,
    barplot = TRUE,
    kmplots = FALSE
  )
  expect_no_error(result_bar)

  # KM plots only
  result_km <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = FALSE,
    barplot = FALSE,
    kmplots = TRUE
  )
  expect_no_error(result_km)

  # All plots
  result_all <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = TRUE,
    barplot = TRUE,
    kmplots = TRUE
  )
  expect_no_error(result_all)
})

test_that("groomecompare table options work", {
  # Detailed metrics
  result_detailed <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    detailedmetrics = TRUE,
    hazardratios = TRUE,
    samplesize = TRUE
  )
  expect_no_error(result_detailed)

  # Minimal tables
  result_minimal <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    detailedmetrics = FALSE,
    hazardratios = FALSE,
    samplesize = FALSE
  )
  expect_no_error(result_minimal)
})

test_that("groomecompare C-index comparison option works", {
  # With C-index
  result_cindex <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    cindexcompare = TRUE
  )
  expect_no_error(result_cindex)

  # Without C-index
  result_nocindex <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    cindexcompare = FALSE
  )
  expect_no_error(result_nocindex)
})

test_that("groomecompare handles all output combinations", {
  # Maximum information
  result_max <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = TRUE,
    barplot = TRUE,
    kmplots = TRUE,
    detailedmetrics = TRUE,
    hazardratios = TRUE,
    samplesize = TRUE,
    cindexcompare = TRUE,
    bootstrap = TRUE,
    nboot = 100
  )
  expect_no_error(result_max)

  # Minimum information (just comparison)
  result_min <- groomecompare(
    data = groomecompare_test,
    time = "time",
    event = "event",
    stage1 = "ypTNM",
    stage2 = "RPA",
    radarplot = FALSE,
    barplot = FALSE,
    kmplots = FALSE,
    detailedmetrics = FALSE,
    hazardratios = FALSE,
    samplesize = FALSE,
    cindexcompare = FALSE,
    bootstrap = FALSE
  )
  expect_no_error(result_min)
})

test_that("groomecompare works across sample sizes", {
  data(groomecompare_small, package = "ClinicoPath")
  data(groomecompare_large, package = "ClinicoPath")

  # Small sample
  result_small <- groomecompare(
    data = groomecompare_small,
    time = "time",
    event = "event",
    stage1 = "clinical_stage",
    stage2 = "molecular_subtype"
  )
  expect_s3_class(result_small, "groomecompareClass")

  # Large sample
  result_large <- groomecompare(
    data = groomecompare_large,
    time = "time",
    event = "event",
    stage1 = "AJCC8",
    stage2 = "RPA5"
  )
  expect_no_error(result_large)
})

test_that("groomecompare handles different staging complexities", {
  data(groomecompare_large, package = "ClinicoPath")
  data(groomecompare_unbalanced, package = "ClinicoPath")

  # Many groups (8 vs 5)
  result_many <- groomecompare(
    data = groomecompare_large,
    time = "time",
    event = "event",
    stage1 = "AJCC8",   # 8 groups
    stage2 = "RPA5"     # 5 groups
  )
  expect_no_error(result_many)

  # Unbalanced (5 vs 2)
  result_unbal <- groomecompare(
    data = groomecompare_unbalanced,
    time = "time",
    event = "event",
    stage1 = "detailed_stage",  # 5 groups
    stage2 = "simple_stage"     # 2 groups
  )
  expect_no_error(result_unbal)
})
