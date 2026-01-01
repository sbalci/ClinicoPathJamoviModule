context("test-jjarcdiagram")

# Load required library
library(ClinicoPath)

test_that("jjarcdiagram works with basic network data", {
  skip_if_not_installed('jmvReadWrite')
  devtools::load_all()
  
  # Load test data
  data("arcDiagram", package = "ClinicoPath")
  
  # Test basic functionality
  expect_error(
    jjarcdiagram(
      data = arcDiagram,
      source = "source",
      target = "target",
      showNodes = TRUE,
      horizontal = TRUE
    ),
    NA
  )
  
  # Test with weights
  expect_error(
    jjarcdiagram(
      data = arcDiagram,
      source = "source",
      target = "target",
      weight = "weight",
      showNodes = TRUE,
      horizontal = TRUE
    ),
    NA
  )
  
  # Test with groups
  expect_error(
    jjarcdiagram(
      data = arcDiagram,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      colorByGroup = TRUE,
      showLegend = TRUE
    ),
    NA
  )
})

test_that("jjarcdiagram works with different network datasets", {
  
  # Test with social network data
  data("jjarcdiagram_social_network", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = social_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      colorByGroup = TRUE,
      showStats = TRUE
    ),
    NA
  )
  
  # Test with academic network data
  data("jjarcdiagram_academic_network", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      weight = "publications",
      group = "department",
      sortNodes = "degree",
      nodeSize = "degree"
    ),
    NA
  )
  
  # Test with organizational hierarchy data
  data("jjarcdiagram_org_hierarchy", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = org_hierarchy_data,
      source = "employee",
      target = "reports_to",
      weight = "relationship_strength",
      group = "department",
      directed = TRUE,
      sortNodes = "group"
    ),
    NA
  )
})

test_that("jjarcdiagram works with different node sorting options", {
  
  data("jjarcdiagram_social_network", package = "ClinicoPath")
  
  # Test different sorting methods
  sort_methods <- c("none", "name", "degree", "group")
  
  for (method in sort_methods) {
    test_that(paste("jjarcdiagram works with", method, "sorting"), {
      expect_error(
        jjarcdiagram(
          data = social_network_data,
          source = "source",
          target = "target",
          weight = "weight",
          group = "group",
          sortNodes = method,
          sortDecreasing = FALSE
        ),
        NA
      )
    })
  }
  
  # Test descending sort
  expect_error(
    jjarcdiagram(
      data = social_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      sortNodes = "degree",
      sortDecreasing = TRUE
    ),
    NA
  )
})

test_that("jjarcdiagram works with different node sizing options", {
  
  data("jjarcdiagram_academic_network", package = "ClinicoPath")
  
  # Test fixed node size
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      nodeSize = "fixed",
      nodeSizeValue = 3
    ),
    NA
  )
  
  # Test degree-based node size
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      nodeSize = "degree",
      nodeSizeValue = 2
    ),
    NA
  )
  
  # Test different node size values
  size_values <- c(0.5, 1, 2, 5)
  
  for (size in size_values) {
    test_that(paste("jjarcdiagram works with node size", size), {
      expect_error(
        jjarcdiagram(
          data = academic_network_data,
          source = "author1",
          target = "author2",
          nodeSize = "fixed",
          nodeSizeValue = size
        ),
        NA
      )
    })
  }
})

test_that("jjarcdiagram works with different arc width options", {
  
  data("jjarcdiagram_supply_chain", package = "ClinicoPath")
  
  # Test fixed arc width
  expect_error(
    jjarcdiagram(
      data = supply_chain_data,
      source = "supplier",
      target = "customer",
      arcWidth = "fixed",
      arcWidthValue = 2
    ),
    NA
  )
  
  # Test weight-based arc width
  expect_error(
    jjarcdiagram(
      data = supply_chain_data,
      source = "supplier",
      target = "customer",
      weight = "volume",
      arcWidth = "weight",
      arcWidthValue = 3
    ),
    NA
  )
  
  # Test different arc transparency values
  transparency_values <- c(0.1, 0.3, 0.5, 0.8, 1.0)
  
  for (alpha in transparency_values) {
    test_that(paste("jjarcdiagram works with transparency", alpha), {
      expect_error(
        jjarcdiagram(
          data = supply_chain_data,
          source = "supplier",
          target = "customer",
          weight = "volume",
          arcTransparency = alpha
        ),
        NA
      )
    })
  }
})

test_that("jjarcdiagram works with layout options", {
  
  data("jjarcdiagram_gene_network", package = "ClinicoPath")
  
  # Test horizontal layout
  expect_error(
    jjarcdiagram(
      data = gene_network_data,
      source = "regulator",
      target = "target",
      weight = "regulation_score",
      group = "pathway",
      horizontal = TRUE
    ),
    NA
  )
  
  # Test vertical layout
  expect_error(
    jjarcdiagram(
      data = gene_network_data,
      source = "regulator",
      target = "target",
      weight = "regulation_score",
      group = "pathway",
      horizontal = FALSE
    ),
    NA
  )
})

test_that("jjarcdiagram works with directed and undirected networks", {
  
  data("jjarcdiagram_org_hierarchy", package = "ClinicoPath")
  
  # Test directed network
  expect_error(
    jjarcdiagram(
      data = org_hierarchy_data,
      source = "employee",
      target = "reports_to",
      weight = "relationship_strength",
      directed = TRUE
    ),
    NA
  )
  
  # Test undirected network
  expect_error(
    jjarcdiagram(
      data = org_hierarchy_data,
      source = "employee",
      target = "reports_to",
      weight = "relationship_strength",
      directed = FALSE
    ),
    NA
  )
})

test_that("jjarcdiagram works with group coloring and legends", {
  
  data("jjarcdiagram_social_network", package = "ClinicoPath")
  
  # Test with group coloring enabled
  expect_error(
    jjarcdiagram(
      data = social_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      colorByGroup = TRUE,
      showLegend = TRUE
    ),
    NA
  )
  
  # Test with group coloring disabled
  expect_error(
    jjarcdiagram(
      data = social_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      colorByGroup = FALSE,
      showLegend = FALSE
    ),
    NA
  )
})

test_that("jjarcdiagram works with network statistics", {
  
  data("jjarcdiagram_academic_network", package = "ClinicoPath")
  
  # Test with statistics enabled
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      weight = "publications",
      group = "department",
      showStats = TRUE
    ),
    NA
  )
  
  # Test with statistics disabled
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      weight = "publications",
      group = "department",
      showStats = FALSE
    ),
    NA
  )
})

test_that("jjarcdiagram works with custom labels and titles", {
  
  data("jjarcdiagram_simple_weighted", package = "ClinicoPath")
  
  # Test with custom title and label size
  expect_error(
    jjarcdiagram(
      data = simple_weighted_network,
      source = "from_node",
      target = "to_node",
      weight = "connection_weight",
      plotTitle = "Custom Network Title",
      labelSize = 1.2,
      showNodes = TRUE
    ),
    NA
  )
  
  # Test with different label sizes
  label_sizes <- c(0.3, 0.6, 0.9, 1.2, 1.5)
  
  for (size in label_sizes) {
    test_that(paste("jjarcdiagram works with label size", size), {
      expect_error(
        jjarcdiagram(
          data = simple_weighted_network,
          source = "from_node",
          target = "to_node",
          labelSize = size
        ),
        NA
      )
    })
  }
})

test_that("jjarcdiagram works with edge case datasets", {
  
  # Test with minimal network data
  data("jjarcdiagram_minimal_network", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = minimal_network_data,
      source = "from",
      target = "to",
      weight = "strength",
      group = "category"
    ),
    NA
  )
  
  # Test with self-loop data
  data("jjarcdiagram_selfloop_network", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = selfloop_network_data,
      source = "node1",
      target = "node2",
      weight = "weight",
      group = "type",
      showNodes = TRUE
    ),
    NA
  )
})

test_that("jjarcdiagram works with large networks", {
  
  # Test with large network data
  data("jjarcdiagram_large_network", package = "ClinicoPath")
  
  expect_error(
    jjarcdiagram(
      data = large_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      group = "cluster",
      nodeSize = "degree",
      sortNodes = "degree",
      showStats = TRUE
    ),
    NA
  )
})

test_that("jjarcdiagram works without optional variables", {
  
  data("jjarcdiagram_simple_weighted", package = "ClinicoPath")
  
  # Test without weight variable
  expect_error(
    jjarcdiagram(
      data = simple_weighted_network,
      source = "from_node",
      target = "to_node",
      showNodes = TRUE,
      horizontal = TRUE
    ),
    NA
  )
  
  # Test without group variable
  expect_error(
    jjarcdiagram(
      data = simple_weighted_network,
      source = "from_node",
      target = "to_node",
      weight = "connection_weight",
      showNodes = TRUE
    ),
    NA
  )
})

test_that("jjarcdiagram works with different variable combinations", {
  
  data("jjarcdiagram_gene_network", package = "ClinicoPath")
  
  # Test different grouping variables
    test_that("jjarcdiagram works with pathway as grouping variable", {
      expect_error(
        jjarcdiagram(
          data = gene_network_data,
          source = "regulator",
          target = "target",
          weight = "regulation_score",
          group = "pathway",
          colorByGroup = TRUE
        ),
        NA
      )
    })

    test_that("jjarcdiagram works with effect_type as grouping variable", {
      expect_error(
        jjarcdiagram(
          data = gene_network_data,
          source = "regulator",
          target = "target",
          weight = "regulation_score",
          group = "effect_type",
          colorByGroup = TRUE
        ),
        NA
      )
    })
})

test_that("jjarcdiagram handles show/hide node options", {
  
  data("jjarcdiagram_academic_network", package = "ClinicoPath")
  
  # Test with nodes shown
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      weight = "publications",
      showNodes = TRUE
    ),
    NA
  )
  
  # Test with nodes hidden
  expect_error(
    jjarcdiagram(
      data = academic_network_data,
      source = "author1",
      target = "author2",
      weight = "publications",
      showNodes = FALSE
    ),
    NA
  )
})

test_that("jjarcdiagram works with supply chain hierarchical data", {
  
  data("jjarcdiagram_supply_chain", package = "ClinicoPath")
  
  # Test supply chain network with industry grouping
  expect_error(
    jjarcdiagram(
      data = supply_chain_data,
      source = "supplier",
      target = "customer",
      weight = "volume",
      group = "industry",
      sortNodes = "group",
      colorByGroup = TRUE,
      showStats = TRUE,
      plotTitle = "Supply Chain Network"
    ),
    NA
  )
})

test_that("jjarcdiagram works with comprehensive feature combinations", {
  
  data("jjarcdiagram_social_network", package = "ClinicoPath")
  
  # Test with all features enabled
  expect_error(
    jjarcdiagram(
      data = social_network_data,
      source = "source",
      target = "target",
      weight = "weight",
      group = "group",
      showNodes = TRUE,
      nodeSize = "degree",
      nodeSizeValue = 2,
      sortNodes = "degree",
      sortDecreasing = TRUE,
      horizontal = TRUE,
      arcWidth = "weight",
      arcWidthValue = 2,
      arcTransparency = 0.6,
      directed = TRUE,
      colorByGroup = TRUE,
      showStats = TRUE,
      showLegend = TRUE,
      labelSize = 1.0,
      plotTitle = "Comprehensive Social Network Analysis"
    ),
    NA
  )
})
