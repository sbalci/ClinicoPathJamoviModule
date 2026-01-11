# Tests for modalitycomparison function

library(testthat)
library(jmvcore)

# Load the package
if (requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all()
}

test_that("modalitycomparison works with basic settings", {
  set.seed(123)
  n <- 50
  
  # Semi-correlated data
  m1 <- sample(c('A', 'B'), n, replace = TRUE)
  m2 <- m1
  # Introduce some errors
  idx <- sample(n, 10)
  m2[idx] <- ifelse(m1[idx] == 'A', 'B', 'A')
  
  data <- data.frame(
    mod1 = factor(m1, levels = c('A', 'B')),
    mod2 = factor(m2, levels = c('A', 'B')),
    case_ids = paste0("Case", 1:n)
  )
  
  expect_no_error({
    result <- modalitycomparison(
      data = data,
      modality1_var = 'mod1',
      modality2_var = 'mod2',
      case_id = 'case_ids',
      modality1_name = "Manual",
      modality2_name = "Digital",
      show_discordance_analysis = TRUE,
      score_categories = 'auto',
      show_contingency_table = TRUE,
      calculate_weighted_kappa = FALSE,
      confidence_intervals = TRUE,
      directional_analysis = TRUE,
      low_end_focus = FALSE,
      show_plots = FALSE
    )
  })
})

test_that("modalitycomparison returns results object", {
  set.seed(456)
  n <- 30
  
  data <- data.frame(
    g1 = factor(sample(c('Low', 'High'), n, replace = TRUE)),
    g2 = factor(sample(c('Low', 'High'), n, replace = TRUE))
  )
  
  result <- modalitycomparison(
    data = data,
    modality1_var = 'g1',
    modality2_var = 'g2',
    score_categories = 'auto'
  )
  
  expect_true(inherits(result, "modalitycomparisonResults"))
})
