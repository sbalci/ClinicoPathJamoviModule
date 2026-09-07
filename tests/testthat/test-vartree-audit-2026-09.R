# Regression cover for the vartree fixes from the 2026-09 module audit.
# Each block fails against the pre-audit backend / yaml.

library(testthat)

vt <- function(d, ...) {
  args <- list(data = d, percvar = NULL, percvarLevel = NULL, summaryvar = NULL,
               prunebelow = NULL, pruneLevel1 = NULL, pruneLevel2 = NULL,
               follow = NULL, followLevel1 = NULL, followLevel2 = NULL)
  do.call(vartree, utils::modifyList(args, list(...)))
}

# the numbers actually drawn in the tree
svg_text <- function(x) {
  s <- as.character(x)
  paste(gsub("<[^>]*>", "", regmatches(s, gregexpr("<text[^>]*>[^<]*</text>", s))[[1]]),
        collapse = " | ")
}

# A(big = 80, mid = 17, rare = 3, NA = 2) x B alternating x / y. Observed
# patterns: big x 40, big y 40, mid x 9, mid y 8, rare x 1, rare y 2, NA x 1, NA y 1.
audit_data <- function() data.frame(
  A = factor(c(rep("big", 80), rep("mid", 17), rep("rare", 3), NA, NA)),
  B = factor(rep(c("x", "y"), length.out = 102)))

test_that("vars carries default: NULL, and the no-variable call is jmvcore-bound", {
  # `vars` must carry `default: NULL`, otherwise the generated wrapper makes it a
  # REQUIRED argument and vartree(data = d) throws
  # 'argument "vars" is missing, with no default' before .run() can answer.
  schema <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "vartree.a.yaml"))
  vars_opt <- Filter(function(o) identical(o$name, "vars"), schema$options)[[1]]
  expect_true("default" %in% names(vars_opt))
  expect_null(vars_opt$default)

  # The welcome panel is not reachable through the R wrapper with no variables at
  # all: jmvcore's select() fails with "invalid 'row.names' length" before .run()
  # is entered, which is jmvcore-wide rather than a vartree defect (summarydata,
  # reportcat, dataquality and alluvial all behave the same way). The jamovi GUI is
  # unaffected. Assert the contract that actually holds.
  expect_error(vt(audit_data()), "row.names")

  # With one variable the backend is reached and answers.
  res <- vt(audit_data(), vars = names(audit_data())[1])
  expect_gt(length(as.character(res$todo$content)), 0)
})

test_that("pattern and sequence mode pruning report whole patterns, NA patterns included", {
  # vtree prunes complete combinations there (no NA exemption, full N kept as the
  # percentage denominator); the hierarchical walk used to describe nodes that
  # were never drawn.
  skip_if_not_installed("vtree")
  d <- audit_data()
  for (mode in c("pattern", "sequence")) {
    args <- list(d, vars = c("A", "B"), useprunesmaller = TRUE, prunesmaller = 10, pct = TRUE)
    args[[mode]] <- TRUE
    res <- do.call(vt, args)
    notices <- as.character(res$notices$content)

    expect_match(notices, "6 pattern\\(s\\) holding 22 case\\(s\\)")
    expect_match(notices, "NA > x (n=1)", fixed = TRUE)
    expect_match(notices, "mid > x (n=9)", fixed = TRUE)
    expect_false(grepl("exempt from the threshold", notices, fixed = TRUE))
    expect_false(grepl("will therefore not add up", notices, fixed = TRUE))

    drawn <- svg_text(res$text1$content)
    expect_match(drawn, "40 (39%)", fixed = TRUE)   # 40 / 102: pruned cases stay in the denominator
    expect_false(grepl("mid", drawn, fixed = TRUE))
    expect_false(grepl("rare", drawn, fixed = TRUE))

    expect_match(as.character(res$reportSentence$content),
                 "6 subgroups holding fewer than 10 cases were not displayed (22 cases in total).",
                 fixed = TRUE)
  }

  # same data, hierarchical tree: rare (3) goes at depth 1, mid>x (9), mid>y (8),
  # NA>x (1), NA>y (1) at depth 2, the NA node (2) itself is exempt under vp.
  hier <- as.character(vt(d, vars = c("A", "B"), useprunesmaller = TRUE,
                          prunesmaller = 10)$notices$content)
  expect_match(hier, "5 node\\(s\\) holding 22 case\\(s\\)")
  expect_match(hier, "exempt from the threshold", fixed = TRUE)
})

test_that("the pruning helper models pattern mode directly", {
  ns <- asNamespace("ClinicoPath")
  obj <- get("vartreeClass", ns)$new(
    options = get("vartreeOptions", ns)$new(vars = "a"),
    data = data.frame(a = factor(c("x", "y"))))
  prune <- obj$.__enclos_env__$private$.prunedByThreshold
  d <- audit_data()

  pat <- prune(d, c("A", "B"), 10, vp = TRUE, pattern = TRUE)
  expect_equal(pat$nodes, 6L)
  expect_equal(pat$cases, 22L)
  expect_equal(pat$min_shown, 40L)
  expect_equal(pat$min_shown_pct, 40L)
  expect_true(any(grepl("NA > y (n=1)", pat$labels, fixed = TRUE)))
  # 'Valid percentages' changes nothing for patterns
  expect_equal(prune(d, c("A", "B"), 10, vp = FALSE, pattern = TRUE)$nodes, 6L)
  # nothing below the threshold -> empty report
  expect_equal(prune(d, c("A", "B"), 1, pattern = TRUE)$nodes, 0L)

  # the hierarchical walk keeps its NA exemption
  hier <- prune(d, c("A", "B"), 10, vp = TRUE)
  expect_equal(hier$nodes, 5L)
  expect_equal(hier$cases, 22L)
  expect_equal(hier$min_shown, 2L)
})

test_that("a prune or follow variable with no level selected is reported with its levels", {
  # .buildConditionalOption() returned NULL and the tree was drawn unchanged
  # without a word.
  skip_if_not_installed("vtree")
  d <- audit_data()

  n1 <- as.character(vt(d, vars = c("A", "B"), prunebelow = "A")$notices$content)
  expect_match(n1, "Prune-Below Level Not Selected")
  expect_match(n1, "big, mid, rare", fixed = TRUE)

  n2 <- as.character(vt(d, vars = c("A", "B"), follow = "B")$notices$content)
  expect_match(n2, "Follow-Below Level Not Selected")
  expect_match(n2, "x, y", fixed = TRUE)

  n3 <- as.character(vt(d, vars = c("A", "B"), prunebelow = "A", pruneLevel1 = "big")$notices$content)
  expect_false(grepl("Level Not Selected", n3, fixed = TRUE))
})

test_that("the report sentence keeps its plural forms and exclusion figure", {
  skip_if_not_installed("vtree")
  d <- data.frame(G = factor(c(rep("a", 30), rep("b", 20), NA)), age = stats::rnorm(51))
  s <- as.character(vt(d, vars = "G", excl = TRUE, summaryvar = "age",
                       summarylocation = "allnodes")$reportSentence$content)
  expect_match(s, "examined 1 categorical variable ('G') across N=50 observations, in which 2 distinct subgroup combinations occurred.", fixed = TRUE)
  expect_match(s, "Missing value exclusion removed 1 case (2.0%).", fixed = TRUE)
  expect_match(s, "for 'age' were displayed at all nodes.", fixed = TRUE)
})

test_that("the UI enables vp and legend exactly when the backend reads them", {
  # `vp` also decides the NA exemption from prunesmaller (read whatever `pct` is);
  # vtree's showlegend is independent of showvarnames.
  find_control <- function(node, name) {
    if (!is.list(node)) return(NULL)
    if (identical(node$name, name) && !is.null(node$type)) return(node)
    for (ch in node) {
      hit <- find_control(ch, name)
      if (!is.null(hit)) return(hit)
    }
    NULL
  }
  uyaml <- yaml::read_yaml("../../jamovi/vartree.u.yaml")
  expect_identical(find_control(uyaml$children, "vp")$enable, "(!excl)")
  expect_null(find_control(uyaml$children, "legend")$enable)

  ayaml <- yaml::read_yaml("../../jamovi/vartree.a.yaml")
  vars <- Filter(function(o) identical(o$name, "vars"), ayaml$options)[[1]]
  expect_true("default" %in% names(vars))
})
