context("riverplot")

test_that("riverplot works", {
  devtools::load_all()
  library(ggplot2)
  library(ggalluvial)
  library(dplyr)
  
  # Create test data - patient treatment journey
  set.seed(123)
  n_patients <- 200
  
  # Longitudinal format data with ID tracking
  longitudinal_data <- data.frame(
    patient_id = rep(1:n_patients, each = 3),
    visit = rep(c("Baseline", "Month3", "Month6"), n_patients),
    response = sample(c("Complete", "Partial", "Stable", "Progression"), 
                     n_patients * 3, replace = TRUE, 
                     prob = c(0.15, 0.35, 0.35, 0.15)),
    tumor_size = runif(n_patients * 3, 10, 100)
  )
  
  # Wide format data
  wide_data <- longitudinal_data %>%
    tidyr::pivot_wider(
      id_cols = patient_id,
      names_from = visit,
      values_from = response,
      names_prefix = "Response_"
    ) %>%
    mutate(
      weight = sample(1:5, n(), replace = TRUE)
    )
  
  # Test 1: Basic alluvial with curveGranularity
  expect_no_error({
    result1 <- riverplot(
      data = wide_data,
      strata = c("Response_Baseline", "Response_Month3", "Response_Month6"),
      plotType = "alluvial",
      curveType = "cubic",
      curveGranularity = 200,
      labelNodes = TRUE,
      backgroundLabels = TRUE,
      showCounts = TRUE
    )
  })

  # Test 2: Flow diagram with individual tracking
  expect_no_error({
    result2 <- riverplot(
      data = longitudinal_data,
      id = "patient_id",
      time = "visit",
      strata = "response",
      plotType = "flow",
      nodeStyle = "regular",
      edgeStyle = "gradient",
      enableDiagnostics = TRUE
    )
  })

  # Test 3: Sankey with node styling
  expect_no_error({
    result3 <- riverplot(
      data = wide_data,
      strata = c("Response_Baseline", "Response_Month3", "Response_Month6"),
      weight = "weight",
      plotType = "sankey",
      nodeStyle = "point",
      edgeStyle = "sin",
      gravity = "top",
      addMidPoints = TRUE,
      reorderEdges = TRUE
    )
  })
})