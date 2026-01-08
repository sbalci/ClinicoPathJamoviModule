# ═══════════════════════════════════════════════════════════
# Argument Combination Tests: jjarcdiagram
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjarcdiagram handles analysis presets", {
  devtools::load_all()

  data(jjarcdiagram_test)

  presets <- c("custom", "gene_interaction", "patient_network",
               "pathway_network", "comorbidity_network")

  for (preset in presets) {
    result <- jjarcdiagram(
      data = jjarcdiagram_test,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      analysisPreset = preset
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for preset:", preset))
  }
})

test_that("jjarcdiagram handles arc coloring modes", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  color_modes <- c("source", "target", "gradient")

  for (mode in color_modes) {
    result <- jjarcdiagram(
      data = jjarcdiagram_gene_network,
      source = "source",
      target = "target",
      group = "pathway",
      colorByGroup = TRUE,
      arcColorMode = mode
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for arc color mode:", mode))
  }
})

test_that("jjarcdiagram handles weight mode options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  weight_modes <- c("strength", "distance")

  for (mode in weight_modes) {
    result <- jjarcdiagram(
      data = jjarcdiagram_test,
      source = "source",
      target = "target",
      weight = "weight",
      weightMode = mode
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for weight mode:", mode))
  }
})

test_that("jjarcdiagram handles edge aggregation options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # With edge aggregation
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight",
    aggregateEdges = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Without edge aggregation
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight",
    aggregateEdges = FALSE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles sort direction", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Ascending sort
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    sortNodes = "degree",
    sortDecreasing = FALSE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Descending sort
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    sortNodes = "degree",
    sortDecreasing = TRUE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles arc transparency levels", {
  devtools::load_all()

  data(jjarcdiagram_test)

  transparency_levels <- c(0, 0.25, 0.5, 0.75, 1.0)

  for (alpha in transparency_levels) {
    result <- jjarcdiagram(
      data = jjarcdiagram_test,
      source = "source",
      target = "target",
      arcTransparency = alpha
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for transparency:", alpha))
  }
})

test_that("jjarcdiagram handles label size variations", {
  devtools::load_all()

  data(jjarcdiagram_test)

  label_sizes <- c(0.5, 0.8, 1.0, 1.5, 2.0)

  for (size in label_sizes) {
    result <- jjarcdiagram(
      data = jjarcdiagram_test,
      source = "source",
      target = "target",
      labelSize = size
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for label size:", size))
  }
})

test_that("jjarcdiagram handles show/hide options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # All show options TRUE
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    group = "group",
    showStats = TRUE,
    showLegend = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showGlossary = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # All show options FALSE
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showStats = FALSE,
    showLegend = FALSE,
    showSummary = FALSE,
    showAssumptions = FALSE,
    showGlossary = FALSE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles comprehensive parameter combinations", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Full feature set for gene network
  result <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway",
    analysisPreset = "gene_interaction",
    showNodes = TRUE,
    nodeSize = "degree",
    nodeSizeValue = 2.5,
    sortNodes = "group",
    sortDecreasing = FALSE,
    horizontal = FALSE,
    arcWidth = "weight",
    arcWidthValue = 1.5,
    arcTransparency = 0.5,
    directed = TRUE,
    aggregateEdges = TRUE,
    weightMode = "strength",
    arcColorMode = "source",
    colorByGroup = TRUE,
    showStats = TRUE,
    showLegend = TRUE,
    labelSize = 1.0,
    plotTitle = "Comprehensive Network Analysis",
    showSummary = TRUE,
    showAssumptions = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles NULL optional parameters", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Explicit NULL for optional parameters
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = NULL,
    group = NULL
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles different network scenarios", {
  devtools::load_all()

  # Gene regulatory network
  data(jjarcdiagram_gene_network)
  result1 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway",
    analysisPreset = "gene_interaction",
    directed = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Patient similarity network
  data(jjarcdiagram_patient_network)
  result2 <- jjarcdiagram(
    data = jjarcdiagram_patient_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "subtype",
    analysisPreset = "patient_network",
    directed = FALSE
  )
  expect_s3_class(result2, "jjarcdiagramResults")

  # Disease comorbidity network
  data(jjarcdiagram_disease_network)
  result3 <- jjarcdiagram(
    data = jjarcdiagram_disease_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "category",
    analysisPreset = "comorbidity_network",
    directed = FALSE
  )
  expect_s3_class(result3, "jjarcdiagramResults")

  # Protein interaction network
  data(jjarcdiagram_protein_network)
  result4 <- jjarcdiagram(
    data = jjarcdiagram_protein_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "function_group",
    analysisPreset = "pathway_network",
    directed = FALSE
  )
  expect_s3_class(result4, "jjarcdiagramResults")

  # Treatment pathway network
  data(jjarcdiagram_treatment_network)
  result5 <- jjarcdiagram(
    data = jjarcdiagram_treatment_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "treatment_type",
    analysisPreset = "custom",
    directed = TRUE
  )
  expect_s3_class(result5, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles visual customization combinations", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Minimal visual style
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    arcWidthValue = 0.5,
    arcTransparency = 0.8,
    labelSize = 0.6
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Maximum visual style
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight",
    group = "group",
    showNodes = TRUE,
    nodeSize = "degree",
    nodeSizeValue = 5,
    arcWidth = "weight",
    arcWidthValue = 3,
    arcTransparency = 0.2,
    labelSize = 1.5,
    colorByGroup = TRUE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})
