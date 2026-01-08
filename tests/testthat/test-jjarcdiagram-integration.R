# ═══════════════════════════════════════════════════════════
# Integration Tests: jjarcdiagram
# ═══════════════════════════════════════════════════════════
#
# Tests for integration with network datasets, data workflows,
# and realistic clinical/biological scenarios.

library(testthat)

test_that("jjarcdiagram integrates with test datasets", {
  devtools::load_all()

  # Test comprehensive dataset loads
  data(jjarcdiagram_test, package = "ClinicoPath")
  expect_true(exists("jjarcdiagram_test"))
  expect_s3_class(jjarcdiagram_test, "data.frame")

  # Test gene network loads
  data(jjarcdiagram_gene_network)
  expect_true(exists("jjarcdiagram_gene_network"))

  # Test patient network loads
  data(jjarcdiagram_patient_network)
  expect_true(exists("jjarcdiagram_patient_network"))

  # Test disease network loads
  data(jjarcdiagram_disease_network)
  expect_true(exists("jjarcdiagram_disease_network"))

  # Test protein network loads
  data(jjarcdiagram_protein_network)
  expect_true(exists("jjarcdiagram_protein_network"))

  # Test treatment network loads
  data(jjarcdiagram_treatment_network)
  expect_true(exists("jjarcdiagram_treatment_network"))
})

test_that("jjarcdiagram test datasets have proper structure", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Check required columns
  expect_true("source" %in% names(jjarcdiagram_test))
  expect_true("target" %in% names(jjarcdiagram_test))
  expect_true("weight" %in% names(jjarcdiagram_test))
  expect_true("group" %in% names(jjarcdiagram_test))

  # Check data types
  expect_true(is.character(jjarcdiagram_test$source) ||
              is.factor(jjarcdiagram_test$source))
  expect_true(is.character(jjarcdiagram_test$target) ||
              is.factor(jjarcdiagram_test$target))
  expect_true(is.numeric(jjarcdiagram_test$weight))
})

test_that("jjarcdiagram scenario: Gene regulatory network analysis", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Comprehensive gene network visualization
  result <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway",
    analysisPreset = "gene_interaction",
    directed = TRUE,
    arcWidth = "weight",
    colorByGroup = TRUE,
    showNodes = TRUE,
    nodeSize = "degree",
    sortNodes = "group",
    showStats = TRUE,
    plotTitle = "Cancer Gene Regulatory Network"
  )

  expect_s3_class(result, "jjarcdiagramResults")
  expect_true(!is.null(result$plot))
  expect_true(!is.null(result$stats))
})

test_that("jjarcdiagram scenario: Patient similarity clustering", {
  devtools::load_all()

  data(jjarcdiagram_patient_network)

  # Patient network with subtype coloring
  result <- jjarcdiagram(
    data = jjarcdiagram_patient_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "subtype",
    analysisPreset = "patient_network",
    directed = FALSE,
    arcWidth = "weight",
    colorByGroup = TRUE,
    arcColorMode = "gradient",
    showNodes = TRUE,
    nodeSize = "degree",
    sortNodes = "group",
    showStats = TRUE,
    plotTitle = "Patient Similarity Network by Cancer Subtype"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram scenario: Disease comorbidity analysis", {
  devtools::load_all()

  data(jjarcdiagram_disease_network)

  # Comorbidity network analysis
  result <- jjarcdiagram(
    data = jjarcdiagram_disease_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "category",
    analysisPreset = "comorbidity_network",
    directed = FALSE,
    arcWidth = "weight",
    colorByGroup = TRUE,
    showNodes = TRUE,
    nodeSize = "degree",
    sortNodes = "degree",
    sortDecreasing = TRUE,
    showStats = TRUE,
    showLegend = TRUE,
    plotTitle = "Chronic Disease Comorbidity Network"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram scenario: Protein interaction network", {
  devtools::load_all()

  data(jjarcdiagram_protein_network)

  # Protein-protein interactions
  result <- jjarcdiagram(
    data = jjarcdiagram_protein_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "function_group",
    analysisPreset = "pathway_network",
    directed = FALSE,
    arcWidth = "weight",
    colorByGroup = TRUE,
    showNodes = TRUE,
    nodeSize = "degree",
    sortNodes = "group",
    showStats = TRUE,
    plotTitle = "Cell Cycle Protein Interaction Network"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram scenario: Treatment pathway sequencing", {
  devtools::load_all()

  data(jjarcdiagram_treatment_network)

  # Treatment transition pathways
  result <- jjarcdiagram(
    data = jjarcdiagram_treatment_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "treatment_type",
    directed = TRUE,
    arcWidth = "weight",
    colorByGroup = TRUE,
    showNodes = TRUE,
    nodeSize = "fixed",
    nodeSizeValue = 3,
    sortNodes = "group",
    showStats = TRUE,
    plotTitle = "Cancer Treatment Pathway Network"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram works with filtered networks", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Filter to high-confidence interactions only
  high_conf <- subset(jjarcdiagram_gene_network, weight > 0.85)

  result <- jjarcdiagram(
    data = high_conf,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway",
    plotTitle = "High-Confidence Gene Interactions (>0.85)"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram works with subsetted networks", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Subset to specific network type
  gene_only <- subset(jjarcdiagram_test, network_type == "Gene Regulatory")

  result <- jjarcdiagram(
    data = gene_only,
    source = "source",
    target = "target",
    weight = "weight",
    group = "group"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles combined network types", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Full comprehensive dataset with multiple network types
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight",
    group = "network_type",
    colorByGroup = TRUE,
    showNodes = TRUE,
    sortNodes = "group"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram works with CSV imported data", {
  skip_if_not(file.exists("data/jjarcdiagram_test.csv"),
              "CSV test file not available")

  devtools::load_all()

  # Read from CSV
  csv_data <- read.csv("data/jjarcdiagram_test.csv", stringsAsFactors = TRUE)

  result <- jjarcdiagram(
    data = csv_data,
    source = "source",
    target = "target",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram works with OMV imported data", {
  skip_if_not(file.exists("data/jjarcdiagram_test.omv"),
              "OMV test file not available")
  skip_if_not(requireNamespace("jmvReadWrite", quietly = TRUE),
              "jmvReadWrite package not available")

  devtools::load_all()

  # Read from OMV
  omv_data <- jmvReadWrite::read_omv("data/jjarcdiagram_test.omv")

  result <- jjarcdiagram(
    data = omv_data,
    source = "source",
    target = "target",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram preserves data integrity", {
  devtools::load_all()

  data(jjarcdiagram_test)
  original_data <- jjarcdiagram_test

  # Run analysis
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target"
  )

  # Original data should be unchanged
  expect_equal(jjarcdiagram_test, original_data)
})

test_that("jjarcdiagram handles multiple visualization styles", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Horizontal layout
  result1 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    horizontal = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Vertical layout with nodes
  result2 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    horizontal = FALSE,
    showNodes = TRUE
  )
  expect_s3_class(result2, "jjarcdiagramResults")

  # Gradient arc coloring
  result3 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    group = "pathway",
    colorByGroup = TRUE,
    arcColorMode = "gradient"
  )
  expect_s3_class(result3, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles derived network metrics", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Network with degree-based node sizing
  result1 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    showNodes = TRUE,
    nodeSize = "degree",
    sortNodes = "degree",
    sortDecreasing = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Network with weight-based arc widths
  result2 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    arcWidth = "weight"
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram generates complete outputs", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Request all outputs
  result <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway",
    showStats = TRUE,
    showSummary = TRUE,
    showAssumptions = TRUE,
    showGlossary = TRUE,
    showLegend = TRUE
  )

  # Check all output elements exist
  expect_true(!is.null(result$plot))
  expect_true(!is.null(result$stats))
  expect_true(!is.null(result$summary))
})

test_that("jjarcdiagram handles real-world network sizes", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  # Small network (< 20 edges)
  expect_true(nrow(jjarcdiagram_gene_network) < 50)

  # Medium network from comprehensive dataset
  data(jjarcdiagram_test)
  expect_true(nrow(jjarcdiagram_test) > 20)

  # Both should process successfully
  result1 <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target"
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target"
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})
