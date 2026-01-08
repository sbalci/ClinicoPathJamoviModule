# ═══════════════════════════════════════════════════════════
# Basic Functionality Tests: jjarcdiagram
# ═══════════════════════════════════════════════════════════

library(testthat)

test_that("jjarcdiagram function exists and loads", {
  devtools::load_all()

  expect_true(exists("jjarcdiagram"))
})

test_that("jjarcdiagram runs with minimal required arguments", {
  devtools::load_all()

  data(jjarcdiagram_test, package = "ClinicoPath")

  # Minimal required arguments (source and target only)
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram produces expected output structure", {
  devtools::load_all()

  data(jjarcdiagram_test)

  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showStats = TRUE
  )

  # Check for main plot output
  expect_true(!is.null(result$plot))

  # Check for statistics when requested
  expect_true(!is.null(result$stats))
})

test_that("jjarcdiagram handles gene regulatory network", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  result <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "pathway"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles patient similarity network", {
  devtools::load_all()

  data(jjarcdiagram_patient_network)

  result <- jjarcdiagram(
    data = jjarcdiagram_patient_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "subtype"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles disease comorbidity network", {
  devtools::load_all()

  data(jjarcdiagram_disease_network)

  result <- jjarcdiagram(
    data = jjarcdiagram_disease_network,
    source = "source",
    target = "target",
    weight = "weight",
    group = "category"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles unweighted networks", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Without weight variable
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles weighted networks", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # With weight variable
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles grouped networks", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # With grouping variable
  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    group = "group",
    colorByGroup = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles directed networks", {
  devtools::load_all()

  data(jjarcdiagram_gene_network)

  result <- jjarcdiagram(
    data = jjarcdiagram_gene_network,
    source = "source",
    target = "target",
    directed = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles undirected networks", {
  devtools::load_all()

  data(jjarcdiagram_patient_network)

  result <- jjarcdiagram(
    data = jjarcdiagram_patient_network,
    source = "source",
    target = "target",
    directed = FALSE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles node display options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # With nodes shown
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showNodes = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Without nodes
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showNodes = FALSE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles node sizing options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Fixed size
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showNodes = TRUE,
    nodeSize = "fixed",
    nodeSizeValue = 2
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Size by degree
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showNodes = TRUE,
    nodeSize = "degree"
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles node sorting options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  sort_methods <- c("none", "name", "group", "degree")

  for (method in sort_methods) {
    result <- jjarcdiagram(
      data = jjarcdiagram_test,
      source = "source",
      target = "target",
      group = "group",
      sortNodes = method
    )

    expect_s3_class(result, "jjarcdiagramResults",
                   info = paste("Failed for sort method:", method))
  }
})

test_that("jjarcdiagram handles layout orientation", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Horizontal layout
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    horizontal = TRUE
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Vertical layout
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    horizontal = FALSE
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles arc width options", {
  devtools::load_all()

  data(jjarcdiagram_test)

  # Fixed width
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    arcWidth = "fixed",
    arcWidthValue = 1.5
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Width by weight
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    weight = "weight",
    arcWidth = "weight"
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles custom labels and titles", {
  devtools::load_all()

  data(jjarcdiagram_test)

  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    plotTitle = "Custom Network Title",
    labelSize = 1.2
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles statistics display", {
  devtools::load_all()

  data(jjarcdiagram_test)

  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    showStats = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
  expect_true(!is.null(result$stats))
})
