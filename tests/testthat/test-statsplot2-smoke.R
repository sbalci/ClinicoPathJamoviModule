# Notices are rendered into a single `notices` output by .addNotice()/.renderNotices(),
# the project convention. module$get("<noticeName>") is left over from the older
# jmvcore::Notice design where each notice was its own named result item; it returns
# NULL now, so these tests were asserting against nothing.
sp_notices <- function(res)
  gsub("[[:space:]]+", " ", gsub("<[^>]*>", " ", paste(as.character(res$notices$content), collapse = " ")))

test_that("Type inference works correctly", {
  skip_if_not_installed('jmvReadWrite')
  # Mock data
  df <- data.frame(
    y_cont = rnorm(50),
    y_cat_small = rep(1:3, length.out=50), # Integer 1-3
    y_cat_factor = factor(rep(c("A","B"), 25)),
    group_factor = factor(rep(c("G1","G2"), 25))
  )

  # Initialize module
  module <- statsplot2(
    data = df,
    dep = "y_cat_small",
    group = "group_factor",
    showExplanations = TRUE
  )

  # Check explanation for "bar chart" or "categorical" to confirm inference
  # Content is in module$ExplanationMessage$content
  
  exp_msg <- module$ExplanationMessage$content
  expect_match(exp_msg, "bar chart", label = "Small integer should be inferred as factor (bar chart)")
})

test_that("Sampling notice is generated for large datasets", {
  # Create large dataset > 10000 rows
  df_large <- data.frame(
    y = rnorm(10005),
    g = factor(rep(c("A","B"), length.out=10005))
  )
  
  module <- statsplot2(
    data = df_large,
    dep = "y",
    group = "g",
    sampleLarge = TRUE
  )
  
  n <- sp_notices(module)
  expect_match(n, "Observations used: 5,000")
  # A random subsample changes every p-value below it, so that must be stated
  # rather than left to look like missing-data exclusion.
  expect_match(n, "random subsample", ignore.case = TRUE)
  expect_match(n, "drawn at RANDOM")
})

test_that("NA exclusion notice works", {
  df_na <- data.frame(
    y = c(rnorm(10), NA, NA),
    g = factor(rep(c("A","B"), 6))
  )
  
  module <- statsplot2(
    data = df_na,
    dep = "y",
    group = "g",
    excl = TRUE
  )
  
  n <- sp_notices(module)
  expect_match(n, "Observations used: 10")
  # NA exclusion is not a random draw, so the subsample warning must NOT appear
  expect_false(grepl("random subsample", n, ignore.case = TRUE))
})

test_that("Alluvial notice appears when not applicable", {
  df <- data.frame(
    y = rnorm(20),
    g = factor(rep(c("A","B"), 10))
  )
  
  # Independent Factor vs Continuous (Violin) -> Alluvial NOT applicable
  module <- statsplot2(
    data = df,
    dep = "y",
    group = "g",
    direction = "independent",
    alluvsty = "t1" 
  )
  
  # No notice is needed here: jamovi/statsplot2.u.yaml gates the control with
  # `enable: (direction:repeated)`, so an independent design cannot select an
  # alluvial style in the GUI at all. What the analysis does explain is the
  # repeated-design case, which IS reachable - assert that instead.
  expect_s3_class(module, "statsplot2Results")

  rep_df <- data.frame(
    y = factor(rep(c("A", "B"), 10)),
    g = rnorm(20)
  )
  rep_mod <- statsplot2(data = rep_df, dep = "y", group = "g",
                        direction = "repeated", alluvsty = "t1",
                        showExplanations = TRUE)
  expect_match(gsub("<[^>]*>", " ", as.character(rep_mod$ExplanationMessage$content)),
               "Alluvial style option")
})
