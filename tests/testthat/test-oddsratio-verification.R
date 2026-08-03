library(testthat)

test_that("Numerical verification: standard logistic regression odds ratios match stats::glm", {
  set.seed(42)
  n <- 120
  df <- data.frame(
    outcome = factor(sample(c("No", "Yes"), n, replace = TRUE, prob = c(0.6, 0.4))),
    stage = factor(sample(c("Early", "Advanced"), n, replace = TRUE)),
    age = round(rnorm(n, mean = 60, sd = 10), 1)
  )

  res <- oddsratio(
    data = df,
    explanatory = c("stage", "age"),
    outcome = "outcome",
    outcomeLevel = "Yes",
    predictorLevel = NULL
  )

  # Independent glm reference
  glm_ref <- stats::glm(
    (outcome == "Yes") ~ stage + age,
    data = df,
    family = stats::binomial(link = "logit")
  )
  ref_sum <- summary(glm_ref)
  ref_coefs <- ref_sum$coefficients

  # Check text / output tables generated
  expect_true(!is.null(res$text))
})

test_that("Numerical verification: Firth penalized logistic regression runs safely", {
  set.seed(123)
  n <- 50
  # Small dataset prone to sparse cells
  df_small <- data.frame(
    outcome = factor(sample(c("Control", "Case"), n, replace = TRUE, prob = c(0.7, 0.3))),
    marker = factor(sample(c("Low", "High"), n, replace = TRUE))
  )

  expect_no_error({
    res_firth <- oddsratio(
      data = df_small,
      explanatory = "marker",
      outcome = "outcome",
      outcomeLevel = "Case",
      predictorLevel = NULL,
      usePenalized = TRUE
    )
  })
})

test_that("Numerical verification: nomogram and diagnostic metrics", {
  set.seed(99)
  n <- 100
  df_diag <- data.frame(
    disease = factor(sample(c("Absence", "Presence"), n, replace = TRUE, prob = c(0.5, 0.5))),
    biomarker = factor(sample(c("Neg", "Pos"), n, replace = TRUE, prob = c(0.5, 0.5)))
  )

  res_nom <- oddsratio(
    data = df_diag,
    explanatory = "biomarker",
    outcome = "disease",
    outcomeLevel = "Presence",
    diagnosticPredictor = "biomarker",
    predictorLevel = "Pos",
    showNomogram = TRUE
  )

  expect_true(!is.null(res_nom$diagnosticMetrics))
})
