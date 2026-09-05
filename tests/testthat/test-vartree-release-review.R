# Regression cover for the defects found during the vartree release review.
# Each block fails against the pre-review backend.

library(testthat)

vt <- function(d, ...) {
  args <- list(data = d, percvar = NULL, percvarLevel = NULL, summaryvar = NULL,
               prunebelow = NULL, pruneLevel1 = NULL, pruneLevel2 = NULL,
               follow = NULL, followLevel1 = NULL, followLevel2 = NULL)
  do.call(vartree, utils::modifyList(args, list(...)))
}

# read the numbers actually drawn in the tree
svg_text <- function(x) {
  s <- as.character(x)
  paste(gsub("<[^>]*>", "", regmatches(s, gregexpr("<text[^>]*>[^<]*</text>", s))[[1]]),
        collapse = " | ")
}

test_that("pruning by minimum size is disclosed with exact counts", {
  # vtree silently drops nodes below the threshold, so branch counts stop adding
  # up to their parent: root 60 with branches 40 + 17 and no sign of the missing
  # 3. vtree does print "No nodes were smaller than N" - to the R console, which
  # a jamovi user never sees.
  skip_if_not_installed("vtree")

  d <- data.frame(Stage = factor(c(rep("I", 40), rep("II", 17), rep("III", 3))),
                  Grade = factor(rep(c("Low", "High"), 30)))

  res <- vt(d, vars = "Stage", useprunesmaller = TRUE, prunesmaller = 5)
  notices <- as.character(res$notices$content)

  expect_match(notices, "Branches hidden by the minimum-size setting")
  expect_match(notices, "1 node\\(s\\) holding 3 case\\(s\\)")
  expect_match(notices, "III (n=3)", fixed = TRUE)
  expect_match(notices, "will therefore not add up")

  # the tree really has lost that branch
  drawn <- svg_text(res$text1$content)
  expect_match(drawn, "I, 40", fixed = TRUE)
  expect_match(drawn, "II, 17", fixed = TRUE)
  expect_false(grepl("III", drawn, fixed = TRUE))
})

test_that("nested pruning counts each hidden node once", {
  # A node inside an already-pruned ancestor must not be counted again.
  skip_if_not_installed("vtree")

  d <- data.frame(
    Stage = factor(c(rep("I", 42), rep("II", 15), rep("III", 3))),
    Grade = factor(c(rep("Low", 40), rep("High", 2),
                     rep("Low", 8), rep("High", 7), rep("Low", 3))))

  notices <- as.character(
    vt(d, vars = c("Stage", "Grade"), useprunesmaller = TRUE,
       prunesmaller = 5)$notices$content)

  # III (3) at depth 1 and I > High (2) at depth 2 = 2 nodes, 5 cases.
  # III > Low (3) is inside the pruned III and must NOT be added again.
  expect_match(notices, "2 node\\(s\\) holding 5 case\\(s\\)")
  expect_match(notices, "III (n=3)", fixed = TRUE)
  expect_match(notices, "I > High (n=2)", fixed = TRUE)
  expect_false(grepl("III > Low", notices, fixed = TRUE))
})

test_that("no pruning notice appears when nothing is pruned", {
  skip_if_not_installed("vtree")
  d <- data.frame(Stage = factor(c(rep("I", 30), rep("II", 30))))

  off <- as.character(vt(d, vars = "Stage")$notices$content)
  expect_false(grepl("Branches hidden", off))

  # threshold set, but every node clears it
  on_ok <- as.character(
    vt(d, vars = "Stage", useprunesmaller = TRUE, prunesmaller = 5)$notices$content)
  expect_false(grepl("Branches hidden", on_ok))
})

test_that("the pruning helper matches a hand-computed rule", {
  ns <- asNamespace("ClinicoPath")
  obj <- get("vartreeClass", ns)$new(
    options = get("vartreeOptions", ns)$new(vars = "a"),
    data = data.frame(a = factor(c("x", "y"))))
  prune <- obj$.__enclos_env__$private$.prunedByThreshold

  d <- data.frame(A = factor(c(rep("p", 10), rep("q", 2))),
                  B = factor(c(rep("m", 9), "n", rep("m", 2))))

  # depth 1: q has 2 (<5) -> pruned. depth 2: p>n has 1 (<5) -> pruned.
  #          q>m is inside pruned q -> not counted again.
  got <- prune(d, c("A", "B"), 5)
  expect_equal(got$nodes, 2L)
  expect_equal(got$cases, 3L)      # 2 from q, 1 from p>n
  expect_true(any(grepl("q (n=2)", got$labels, fixed = TRUE)))
  expect_true(any(grepl("p > n (n=1)", got$labels, fixed = TRUE)))

  # threshold of 1 prunes nothing
  expect_equal(prune(d, c("A", "B"), 1)$nodes, 0L)
  # a NULL / non-finite threshold is a no-op
  expect_equal(prune(d, c("A", "B"), NULL)$nodes, 0L)
  expect_equal(prune(d, c("A", "B"), 0)$nodes, 0L)
})

test_that("the notice clears when the pruning options change", {
  # `notices` did not list the pruning options in clearWith, so a stale warning
  # could outlive the setting that produced it.
  ryaml <- yaml::read_yaml("../../jamovi/vartree.r.yaml")
  notices <- Filter(function(i) identical(i$name, "notices"), ryaml$items)[[1]]
  expect_true("useprunesmaller" %in% notices$clearWith)
  expect_true("prunesmaller" %in% notices$clearWith)
})

test_that("node counts reproduce a hand-computed tabulation", {
  skip_if_not_installed("vtree")
  set.seed(7); n <- 150
  d <- data.frame(Stage = factor(sample(c("I", "II", "III"), n, TRUE)),
                  Grade = factor(sample(c("Low", "High"), n, TRUE)))

  drawn <- svg_text(vt(d, vars = c("Stage", "Grade"))$text1$content)

  tab <- table(d$Stage)
  for (lv in names(tab))
    expect_match(drawn, sprintf("%s, %d", lv, tab[[lv]]), fixed = TRUE)
  expect_match(drawn, as.character(n))     # root total
})

test_that("an invalid percentage level is reported, not thrown", {
  skip_if_not_installed("vtree")
  d <- data.frame(group = factor(c("A", "B")), outcome = factor(c("Yes", "No")))

  res <- vt(d, vars = "group", percvar = "outcome", percvarLevel = "Maybe", pct = TRUE)
  expect_match(as.character(res$notices$content), "Invalid Percentage Level")
})

test_that("a variable with more than 12 levels is flagged, a narrower one is not", {
  set.seed(7)
  d <- data.frame(stage = factor(sample(paste0("S", 1:30), 200, TRUE)),
                  sex   = factor(sample(c("F", "M"), 200, TRUE)))
  wide <- as.character(vt(d, vars = c("stage", "sex"))$notices$content)
  expect_match(wide, "Variable With Many Levels")
  expect_match(wide, "'stage': 30")
  narrow <- as.character(vt(d, vars = "sex")$notices$content)
  expect_false(grepl("Variable With Many Levels", narrow))
})

test_that("vtree's console message() is not emitted on pruning", {
  set.seed(3)
  d <- data.frame(a = factor(sample(c("x", "y"), 120, TRUE)), b = factor(sample(c("p", "q"), 120, TRUE)))
  expect_silent(vt(d, vars = c("a", "b"), useprunesmaller = TRUE, prunesmaller = 2))
})

test_that("a tree whose DOT source exceeds 1024 characters raises no path-length warning", {
  set.seed(11); n <- 100
  d <- data.frame(grp = factor(sample(c("A", "B"), n, TRUE)), resp = factor(sample(c("Yes", "No"), n, TRUE)),
                  age = round(rnorm(n, 60, 10), 1))
  d$resp[c(3, 9, 27)] <- NA; d$age[c(5, 40)] <- NA
  expect_no_warning(vt(d, vars = "grp", percvar = "resp", percvarLevel = "Yes", pct = TRUE,
                       summaryvar = "age", summarylocation = "allnodes"))
})

test_that("the colour-blind palette changes node fills and the interpretation says so", {
  set.seed(5)
  d <- data.frame(a = factor(sample(c("x", "y"), 80, TRUE)), b = factor(sample(c("p", "q"), 80, TRUE)))
  def <- vt(d, vars = c("a", "b"))
  cb  <- vt(d, vars = c("a", "b"), palette = "colorblind")
  expect_false(identical(as.character(def$text1$content), as.character(cb$text1$content)))
  expect_match(as.character(cb$interpretation$content), "Colour-blind-safe palette applied")
  expect_false(grepl("Colour-blind-safe", as.character(def$interpretation$content)))
  # clean style overrides fills, so the palette must make no difference there
  expect_identical(as.character(vt(d, vars = "a", style = "clean")$text1$content),
                   as.character(vt(d, vars = "a", style = "clean", palette = "colorblind")$text1$content))
})

test_that("the small-percentage warning never quotes an NA node that carries no percentage", {
  # 60 cases: two big groups plus 3 NA on the split variable. With valid
  # percentages ON the NA node (n=3) shows no percentage, so the warning must
  # stay silent; with them OFF the NA node carries 5% and the warning fires on 3.
  d <- data.frame(g = factor(c(rep("A", 30), rep("B", 27), NA, NA, NA)))
  on  <- as.character(vt(d, vars = "g", pct = TRUE, vp = TRUE)$notices$content)
  off <- as.character(vt(d, vars = "g", pct = TRUE, vp = FALSE)$notices$content)
  expect_false(grepl("Percentages Shown for Very Small Subgroups", on))
  expect_match(off, "Percentages Shown for Very Small Subgroups")
  expect_match(off, "holds 3 case")
})

test_that("reference-level percentage and mean/SD reproduce hand computation with missing values", {
  set.seed(11); n <- 100
  d <- data.frame(grp = factor(sample(c("A", "B"), n, TRUE)), resp = factor(sample(c("Yes", "No"), n, TRUE)),
                  age = round(rnorm(n, 60, 10), 1))
  d$resp[c(3, 9, 27)] <- NA; d$age[c(5, 40)] <- NA
  drawn <- svg_text(vt(d, vars = "grp", percvar = "resp", percvarLevel = "Yes", pct = TRUE, vp = TRUE,
                       summaryvar = "age", summarylocation = "allnodes")$text1$content)
  a <- d[d$grp == "A", ]
  # node A: count and % of root; reference level among NON-missing resp; mean/SD over non-missing age
  expect_match(drawn, sprintf("A, %d (%d%%)", nrow(a), round(100 * nrow(a) / n)), fixed = TRUE)
  expect_match(drawn, sprintf("resp=Yes: %d (%d%%)", sum(a$resp == "Yes", na.rm = TRUE),
                              round(100 * mean(a$resp == "Yes", na.rm = TRUE))), fixed = TRUE)
  expect_match(drawn, sprintf("mean=%.1f", mean(a$age, na.rm = TRUE)), fixed = TRUE)
  expect_match(drawn, sprintf("SD=%.1f", sd(a$age, na.rm = TRUE)), fixed = TRUE)
  # root: reference-level denominator excludes the 3 missing resp, reported as mv=3
  expect_match(drawn, sprintf("resp=Yes: %d (%d%%) | mv=3", sum(d$resp == "Yes", na.rm = TRUE),
                              round(100 * mean(d$resp == "Yes", na.rm = TRUE))), fixed = TRUE)
})
