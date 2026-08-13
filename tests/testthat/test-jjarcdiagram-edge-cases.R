# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jjarcdiagram
# ═══════════════════════════════════════════════════════════

library(testthat)

# A jamovi analysis does not throw when a variable is missing - it writes the
# welcome/instructions panel and returns. expect_error() therefore asserted the
# OPPOSITE of the desired behaviour: it passed only if the analysis crashed.
arc_todo <- function(res) gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", as.character(res$todo$content)))
arc_notices <- function(res)
  gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(res$notices$content), collapse = " ")))

test_that("jjarcdiagram prompts for the variables it still needs", {

  data(jjarcdiagram_test)

  # Missing source
  expect_match(arc_todo(jjarcdiagram(data = jjarcdiagram_test, target = "target")),
               "Arc Diagram")

  # Missing target
  expect_match(arc_todo(jjarcdiagram(data = jjarcdiagram_test, source = "source")),
               "Arc Diagram")

  # Missing data
  expect_error(
    jjarcdiagram(
      source = "source",
      target = "target"
    )
  )
})

test_that("jjarcdiagram handles minimal network (2 nodes)", {

  # Create minimal network
  minimal_net <- data.frame(
    from = "A",
    to = "B",
    strength = 1.0
  )

  result <- jjarcdiagram(
    data = minimal_net,
    source = "from",
    target = "to",
    weight = "strength"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles single edge network", {

  single_edge <- data.frame(
    node1 = "Gene_A",
    node2 = "Gene_B",
    score = 0.95
  )

  result <- jjarcdiagram(
    data = single_edge,
    source = "node1",
    target = "node2",
    weight = "score"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles self-loops", {

  # Network with self-loops (node connects to itself)
  selfloop_net <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "A", "C"),  # A→A is a self-loop
    weight = c(1.0, 0.5, 0.8)
  )

  # Self-loops are removed and the removal is disclosed in the notices panel;
  # nothing is signalled as an R condition.
  expect_match(arc_notices(jjarcdiagram(data = selfloop_net, source = "from",
                                        target = "to", weight = "weight")),
               "self-loop")
})

test_that("jjarcdiagram handles duplicate edges", {

  # Network with duplicate edges
  duplicate_edges <- data.frame(
    from = c("A", "A", "B"),
    to = c("B", "B", "C"),  # A→B appears twice
    weight = c(1.0, 0.5, 0.8)
  )

  # Should aggregate with aggregateEdges=TRUE
  result <- jjarcdiagram(
    data = duplicate_edges,
    source = "from",
    target = "to",
    weight = "weight",
    aggregateEdges = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles missing weight values", {

  data(jjarcdiagram_test)
  test_data_na <- jjarcdiagram_test
  test_data_na$weight[1:5] <- NA

  # Rows with a missing weight are dropped, and that exclusion must be stated -
  # every statistic below it is computed on the surviving edges only.
  n <- arc_notices(jjarcdiagram(data = test_data_na, source = "source",
                                target = "target", weight = "weight"))
  expect_match(n, "Rows Excluded")
  expect_match(n, "5 of")
})

test_that("jjarcdiagram handles all NA weights", {

  data(jjarcdiagram_test)
  test_data_all_na <- jjarcdiagram_test
  test_data_all_na$weight <- NA_real_

  # Every row is incomplete, so there is no network left to draw.
  expect_match(arc_notices(jjarcdiagram(data = test_data_all_na, source = "source",
                                        target = "target", weight = "weight")),
               "No complete rows")
})

test_that("jjarcdiagram handles missing node labels", {

  data(jjarcdiagram_test)
  test_data_na_nodes <- jjarcdiagram_test
  test_data_na_nodes$source[1] <- NA_character_
  test_data_na_nodes$target[2] <- NA_character_

  # Edges with a missing endpoint are dropped, and the exclusion is disclosed.
  expect_match(arc_notices(jjarcdiagram(data = test_data_na_nodes, source = "source",
                                        target = "target")),
               "Rows Excluded")
})

test_that("jjarcdiagram handles factor vs character variables", {

  data(jjarcdiagram_test)

  # Convert to character
  char_data <- jjarcdiagram_test
  char_data$source <- as.character(char_data$source)
  char_data$target <- as.character(char_data$target)

  result_char <- jjarcdiagram(
    data = char_data,
    source = "source",
    target = "target"
  )

  # Keep as factor
  result_factor <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target"
  )

  # Both should work
  expect_s3_class(result_char, "jjarcdiagramResults")
  expect_s3_class(result_factor, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles special characters in node names", {

  special_net <- data.frame(
    from = c("Node (1)", "Node-2", "Node_3"),
    to = c("Node-2", "Node_3", "Node/4"),
    strength = c(0.8, 0.9, 0.7)
  )

  result <- jjarcdiagram(
    data = special_net,
    source = "from",
    target = "to",
    weight = "strength"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles very long node names", {

  long_name_net <- data.frame(
    from = c("VeryLongGeneName_ABCDEFGHIJKLMNOPQRSTUVWXYZ_123456789",
             "AnotherLongName"),
    to = c("AnotherLongName",
           "YetAnotherVeryLongGeneName_WithManyCharacters"),
    weight = c(0.8, 0.9)
  )

  result <- jjarcdiagram(
    data = long_name_net,
    source = "from",
    target = "to",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles zero weights", {

  zero_weight_net <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D"),
    weight = c(0, 0.5, 0)
  )

  result <- jjarcdiagram(
    data = zero_weight_net,
    source = "from",
    target = "to",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles negative weights", {

  # Negative weights (e.g., negative correlation)
  negative_weight_net <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D"),
    weight = c(-0.5, 0.8, -0.3)
  )

  # This block used to pass by catching the igraph C-level abort ("Weight vector
  # must be positive") - i.e. it was pinning a crash. Negative weights are now
  # rejected with an explanation instead.
  expect_no_error(
    jjarcdiagram(
      data = negative_weight_net,
      source = "from",
      target = "to",
      weight = "weight"
    )
  )
  expect_match(arc_notices(jjarcdiagram(data = negative_weight_net, source = "from",
                                        target = "to", weight = "weight")),
               "Negative Edge Weights")
})

test_that("jjarcdiagram handles extremely large weights", {

  large_weight_net <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D"),
    weight = c(1000, 5000, 10000)
  )

  result <- jjarcdiagram(
    data = large_weight_net,
    source = "from",
    target = "to",
    weight = "weight",
    arcWidth = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles constant weights", {

  constant_weight_net <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D"),
    weight = c(1.0, 1.0, 1.0)
  )

  result <- jjarcdiagram(
    data = constant_weight_net,
    source = "from",
    target = "to",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles disconnected components", {

  # Network with disconnected components
  disconnected_net <- data.frame(
    from = c("A", "B", "D", "E"),
    to = c("B", "C", "E", "F"),
    weight = c(0.8, 0.9, 0.7, 0.85)
  )

  result <- jjarcdiagram(
    data = disconnected_net,
    source = "from",
    target = "to",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles single node (isolate)", {

  # Network where one node has no connections
  isolate_net <- data.frame(
    from = c("A", "B"),
    to = c("B", "C"),
    weight = c(0.8, 0.9)
  )

  result <- jjarcdiagram(
    data = isolate_net,
    source = "from",
    target = "to",
    weight = "weight"
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles missing group values", {

  data(jjarcdiagram_test)
  test_data_na_group <- jjarcdiagram_test
  test_data_na_group$group[1:3] <- NA

  # Should handle NA groups
  result <- jjarcdiagram(
    data = test_data_na_group,
    source = "source",
    target = "target",
    group = "group",
    colorByGroup = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles single group", {

  single_group_net <- data.frame(
    from = c("A", "B", "C"),
    to = c("B", "C", "D"),
    weight = c(0.8, 0.9, 0.7),
    category = rep("Group1", 3)
  )

  result <- jjarcdiagram(
    data = single_group_net,
    source = "from",
    target = "to",
    group = "category",
    colorByGroup = TRUE
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles empty plot title", {

  data(jjarcdiagram_test)

  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    plotTitle = ""
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles very long plot title", {

  data(jjarcdiagram_test)

  long_title <- paste(rep("Very Long Title", 20), collapse = " ")

  result <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    plotTitle = long_title
  )

  expect_s3_class(result, "jjarcdiagramResults")
})

test_that("jjarcdiagram handles parameter boundary values", {

  data(jjarcdiagram_test)

  # Minimum values
  result1 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    nodeSizeValue = 0.1,
    arcWidthValue = 0.1,
    arcTransparency = 0,
    labelSize = 0.1
  )
  expect_s3_class(result1, "jjarcdiagramResults")

  # Maximum values
  result2 <- jjarcdiagram(
    data = jjarcdiagram_test,
    source = "source",
    target = "target",
    nodeSizeValue = 10,
    arcWidthValue = 5,
    arcTransparency = 1,
    labelSize = 2
  )
  expect_s3_class(result2, "jjarcdiagramResults")
})
