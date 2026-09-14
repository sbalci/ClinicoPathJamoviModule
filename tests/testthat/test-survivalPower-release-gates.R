test_that("NI inversion does not describe an unused survival-difference landmark", {
  make <- function(...) {
    analysis <- survivalPowerClass$new(
      options = survivalPowerOptions$new(...), data = data.frame())
    analysis$run()
    analysis
  }
  inverse <- make(test_type = "non_inferiority", analysis_type = "effect_size",
    sample_size_input = 1000, effect_size_type = "survival_difference", effect_size = 0.05)
  expect_false(grepl("Survival Difference Landmark", inverse$results$notices$content,
    fixed = TRUE))
  reference <- make(test_type = "non_inferiority", analysis_type = "effect_size",
    sample_size_input = 1000)
  expect_equal(inverse$results$effect_size_results$asDF,
    reference$results$effect_size_results$asDF)
  forward <- make(analysis_type = "power",
    effect_size_type = "survival_difference", effect_size = 0.05)
  expect_match(forward$results$notices$content, "Survival Difference Landmark", fixed = TRUE)
})

test_that("all survivalPower citations have complete library metadata", {
  path <- testthat::test_path("..", "..", "jamovi")
  keys <- yaml::read_yaml(file.path(path, "survivalPower.r.yaml"))$refs
  refs <- yaml::read_yaml(file.path(path, "00refs.yaml"))$refs
  for (key in keys) {
    expect_true(key %in% names(refs), info = key)
    for (field in c("title", "author", "url")) {
      value <- refs[[key]][[field]]
      expect_true(is.character(value) && length(value) == 1L && nzchar(value),
        info = paste(key, field))
    }
  }
})

test_that("NI effect inversion reaches its target using independently integrated events", {
  for (alpha in c(0.025, 0.05)) {
    for (ratio in c(0.2, 1, 5)) {
      analysis <- survivalPowerClass$new(options = survivalPowerOptions$new(
        test_type = "non_inferiority", analysis_type = "effect_size",
        sample_size_input = 1000, alpha_level = alpha, allocation_ratio = ratio),
        data = data.frame())
      analysis$run()
      hr <- analysis$.__enclos_env__$private$primary_numbers$hr_detectable
      probability <- function(hr) {
        event <- log(2) / 12 * hr
        dropout <- -log1p(-0.05) / 12
        integrate(function(entry) {
          event / (event + dropout) * (-expm1(-(event + dropout) * (36 - entry)))
        }, 0, 24)$value / 24
      }
      events <- 1000 / (1 + ratio) * (ratio * probability(1) + probability(hr))
      mean <- log(1.25 / hr) * sqrt(events * ratio / (1 + ratio)^2)
      expect_equal(pnorm(mean - qnorm(1 - alpha)), 0.8, tolerance = 1e-7)
    }
  }
})
