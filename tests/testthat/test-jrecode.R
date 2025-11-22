
test_that("jrecode basic functionality works", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  library(dplyr)
  data(iris)

  # Test basic recoding
  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S\nversicolor : VC\nvirginica : VG",
    show_code = TRUE,
    show_table = TRUE,
    show_levels = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  # Check that code was generated
  expect_true(!is.null(analysis$results$code_output$content))
  expect_true(grepl("dplyr::recode", analysis$results$code_output$content))
  expect_true(grepl("S", analysis$results$code_output$content))

  # Check that comparison table was populated
  table_df <- analysis$results$comparison$asDF
  expect_true(nrow(table_df) > 0)
  expect_true("S" %in% table_df$recoded)
  expect_true("VC" %in% table_df$recoded)
  expect_true("VG" %in% table_df$recoded)

  # Check that levels table was populated
  levels_df <- analysis$results$levels_table$asDF
  expect_equal(nrow(levels_df), 3)
  expect_true("setosa" %in% levels_df$level)
  expect_true(all(levels_df$count == 50))
})


test_that("jrecode handles alternative separators", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test with semicolon separator
  options1 <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa ; S\nversicolor ; VC",
    show_table = TRUE
  )

  analysis1 <- jrecodeClass$new(options = options1, data = iris)
  analysis1$run()

  table_df1 <- analysis1$results$comparison$asDF
  expect_true("S" %in% table_df1$recoded)

  # Test with arrow separator (backward compatibility)
  options2 <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa -> S\nversicolor -> VC",
    show_table = TRUE
  )

  analysis2 <- jrecodeClass$new(options = options2, data = iris)
  analysis2$run()

  table_df2 <- analysis2$results$comparison$asDF
  expect_true("S" %in% table_df2$recoded)
})


test_that("jrecode handles else_level options correctly", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test copy (default)
  options_copy <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S",
    else_level = "copy",
    show_table = TRUE
  )

  analysis_copy <- jrecodeClass$new(options = options_copy, data = iris)
  analysis_copy$run()

  table_copy <- analysis_copy$results$comparison$asDF
  expect_true("versicolor" %in% table_copy$recoded)  # Unchanged
  expect_true("virginica" %in% table_copy$recoded)   # Unchanged

  # Test NA
  options_na <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S",
    else_level = "na",
    show_table = TRUE
  )

  analysis_na <- jrecodeClass$new(options = options_na, data = iris)
  analysis_na$run()

  table_na <- analysis_na$results$comparison$asDF
  expect_true("S" %in% table_na$recoded)
  # NA values will appear as "NA" in character representation

  # Test Other
  options_other <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S",
    else_level = "other",
    show_table = TRUE
  )

  analysis_other <- jrecodeClass$new(options = options_other, data = iris)
  analysis_other$run()

  table_other <- analysis_other$results$comparison$asDF
  expect_true("Other" %in% table_other$recoded)
})


test_that("jrecode validates rules and shows notices", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test invalid format
  options_invalid <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "this is not valid format",
    show_code = TRUE
  )

  analysis_invalid <- jrecodeClass$new(options = options_invalid, data = iris)
  analysis_invalid$run()

  # Should show error notice
  expect_true(analysis_invalid$results$notices$visible)

  # Test unknown level
  options_unknown <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "unknown_level : X",
    show_code = TRUE
  )

  analysis_unknown <- jrecodeClass$new(options = options_unknown, data = iris)
  analysis_unknown$run()

  # Should show warning about unknown level
  expect_true(analysis_unknown$results$notices$visible)
})


test_that("jrecode handles quotes in input", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test with quotes
  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "'setosa' : 'S'\n\"versicolor\" : \"VC\"",
    show_table = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  table_df <- analysis$results$comparison$asDF
  expect_true("S" %in% table_df$recoded)
  expect_true("VC" %in% table_df$recoded)
})


test_that("jrecode generates valid R code", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S\nversicolor : VC\nvirginica : VG",
    new_var_name = "Species_short",
    show_code = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  code <- analysis$results$code_output$content

  # Check code contains expected elements
  expect_true(grepl("dplyr::recode", code))
  expect_true(grepl("Species_short", code))
  expect_true(grepl("'setosa' = 'S'", code))
  expect_true(grepl("'versicolor' = 'VC'", code))
  expect_true(grepl("'virginica' = 'VG'", code))

  # Check that base R alternative is also provided
  expect_true(grepl("Alternatively, using base R", code))
})


test_that("jrecode handles empty rules gracefully", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "",
    show_code = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  # Should auto-populate identity rules instead of failing
  expect_true(nchar(analysis$options$recode_rules) > 0)
  expect_true(analysis$results$notices$visible)
  expect_true(grepl("recoding rule", analysis$results$notices$content))
})

test_that("jrecode auto-populates rules with available levels when empty", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "",
    show_code = FALSE,
    show_table = FALSE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  auto_rules <- analysis$options$recode_rules
  expect_true(grepl("setosa -> setosa", auto_rules, fixed = TRUE))
  expect_true(grepl("versicolor -> versicolor", auto_rules, fixed = TRUE))
  expect_true(grepl("virginica -> virginica", auto_rules, fixed = TRUE))
})

test_that("jrecode gracefully handles missing variable selections", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  options <- jrecodeOptions$new(
    dep = "Race",  # not in iris
    recode_rules = ""
  )

  analysis <- jrecodeClass$new(options = options, data = iris)

  expect_silent(analysis$run())
  expect_true(analysis$results$instructions$visible)
})


test_that("jrecode shows instructions when no variable selected", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  options <- jrecodeOptions$new(
    dep = NULL
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  # Should show instructions
  expect_true(analysis$results$instructions$visible)
  expect_true(grepl("Interactive Recoding Tool", analysis$results$instructions$content))
})


test_that("jrecode variable name validation works", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test invalid variable name
  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "setosa : S",
    new_var_name = "123invalid",  # Starts with number
    recoded_output = TRUE,
    show_code = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  # Should show error about invalid variable name
  expect_true(analysis$results$notices$visible)
  expect_true(grepl("Invalid variable name", analysis$results$notices$content))
})


test_that("jrecode handles grouping/collapsing levels", {
  skip_if_not_installed("jmvcore")

  source("../../R/jrecode.h.R")
  source("../../R/jrecode.b.R")
  data(iris)

  # Test collapsing multiple levels to one
  options <- jrecodeOptions$new(
    dep = "Species",
    recode_rules = "versicolor : virginica_group\nvirginica : virginica_group",
    show_table = TRUE
  )

  analysis <- jrecodeClass$new(options = options, data = iris)
  analysis$run()

  table_df <- analysis$results$comparison$asDF

  # Both versicolor and virginica should map to virginica_group
  versicolor_row <- table_df[table_df$original == "versicolor", ]
  virginica_row <- table_df[table_df$original == "virginica", ]

  expect_equal(versicolor_row$recoded, "virginica_group")
  expect_equal(virginica_row$recoded, "virginica_group")

  # Setosa should remain unchanged (copy default)
  setosa_row <- table_df[table_df$original == "setosa", ]
  expect_equal(setosa_row$recoded, "setosa")
})
