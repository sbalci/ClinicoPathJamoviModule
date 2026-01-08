# ═══════════════════════════════════════════════════════════
# Edge Cases and Error Handling Tests: jwaffle
# ═══════════════════════════════════════════════════════════
#
# Tests boundary conditions, error cases, and data issues
# Generated: 2026-01-06

library(testthat)
library(ClinicoPath)

# Load test datasets
data(jwaffle_test, package = "ClinicoPath", envir = environment())
data(jwaffle_small, package = "ClinicoPath", envir = environment())
data(jwaffle_demographics, package = "ClinicoPath", envir = environment())

# ═══════════════════════════════════════════════════════════
# 1. Small Sample Sizes
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles small sample (n=30)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_small,
    groups = "category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles very small sample per facet", {
  devtools::load_all()

  # n=30 with faceting creates very small groups
  result <- jwaffle(
    data = jwaffle_small,
    groups = "response",
    facet = "group"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 2. Missing Data
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles missing data in groups variable", {
  devtools::load_all()

  test_data_na <- jwaffle_test
  test_data_na$response_category[1:30] <- NA

  result <- jwaffle(
    data = test_data_na,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles missing data in facet variable", {
  devtools::load_all()

  test_data_na <- jwaffle_test
  test_data_na$treatment[1:25] <- NA

  result <- jwaffle(
    data = test_data_na,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles missing data in counts variable", {
  devtools::load_all()

  test_data_na <- jwaffle_test
  test_data_na$patient_count[1:20] <- NA

  result <- jwaffle(
    data = test_data_na,
    groups = "response_category",
    counts = "patient_count"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles missing data in multiple variables", {
  devtools::load_all()

  test_data_na <- jwaffle_test
  test_data_na$response_category[1:15] <- NA
  test_data_na$treatment[16:30] <- NA

  result <- jwaffle(
    data = test_data_na,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 3. Invalid Arguments
# ═══════════════════════════════════════════════════════════

test_that("jwaffle errors on non-existent groups variable", {
  devtools::load_all()

  expect_error(
    jwaffle(
      data = jwaffle_test,
      groups = "nonexistent_var"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jwaffle errors on non-existent facet variable", {
  devtools::load_all()

  expect_error(
    jwaffle(
      data = jwaffle_test,
      groups = "response_category",
      facet = "nonexistent_var"
    ),
    regexp = "not found|does not exist|invalid",
    ignore.case = TRUE
  )
})

test_that("jwaffle errors on numeric groups variable", {
  devtools::load_all()

  # groups should be categorical
  expect_error(
    jwaffle(
      data = jwaffle_test,
      groups = "patient_count"  # Numeric instead of categorical
    ),
    regexp = "categorical|factor|not.*numeric",
    ignore.case = TRUE
  )
})

test_that("jwaffle errors on empty dataset", {
  devtools::load_all()

  empty_data <- jwaffle_test[0, ]

  expect_error(
    jwaffle(
      data = empty_data,
      groups = "response_category"
    ),
    regexp = "empty|no.*rows|insufficient",
    ignore.case = TRUE
  )
})

# ═══════════════════════════════════════════════════════════
# 4. Boundary Values
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles minimum rows (1)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    rows = 1
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles large rows value (20)", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",
    rows = 20
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 5. Variable Names with Special Characters
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles variable names with spaces", {
  devtools::load_all()

  test_data_spaces <- jwaffle_test
  names(test_data_spaces)[names(test_data_spaces) == "response_category"] <- "Response Category"

  result <- jwaffle(
    data = test_data_spaces,
    groups = "Response Category"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 6. Data Types
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles tibble input", {
  devtools::load_all()

  tibble_data <- tibble::as_tibble(jwaffle_test)

  result <- jwaffle(
    data = tibble_data,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles data.frame input", {
  devtools::load_all()

  df_data <- as.data.frame(jwaffle_test)

  result <- jwaffle(
    data = df_data,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 7. Duplicate Rows
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles duplicate rows", {
  devtools::load_all()

  test_data_dup <- rbind(jwaffle_test, jwaffle_test[1:50, ])

  result <- jwaffle(
    data = test_data_dup,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 8. Unbalanced Categories
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles highly unbalanced groups", {
  devtools::load_all()

  # Create highly unbalanced data (95% in one category)
  test_data_unbal <- jwaffle_test
  indices_to_change <- sample(1:nrow(test_data_unbal), round(nrow(test_data_unbal) * 0.95))
  test_data_unbal$response_category[indices_to_change] <- "Complete Response"

  result <- jwaffle(
    data = test_data_unbal,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles rare categories (<1%)", {
  devtools::load_all()

  # Create data where one category is very rare
  test_data_rare <- jwaffle_test
  indices_to_change <- sample(1:nrow(test_data_rare), round(nrow(test_data_rare) * 0.005))
  test_data_rare$response_category[indices_to_change] <- "Progressive Disease"
  test_data_rare$response_category[-indices_to_change] <- sample(
    c("Complete Response", "Partial Response", "Stable Disease"),
    length(-indices_to_change), replace = TRUE
  )

  result <- jwaffle(
    data = test_data_rare,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 9. Single Category Level
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles single category level", {
  devtools::load_all()

  test_data_single <- jwaffle_test
  test_data_single$response_category <- "Complete Response"  # All same category

  result <- jwaffle(
    data = test_data_single,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 10. Many Category Levels
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles 5 category levels", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group"  # 5 levels
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles 5 category levels with faceting", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",  # 5 levels
    facet = "region"       # 5 levels
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 11. Zero Count Values
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles some zero counts", {
  devtools::load_all()

  test_data_zero <- jwaffle_test
  test_data_zero$patient_count[1:20] <- 0

  result <- jwaffle(
    data = test_data_zero,
    groups = "response_category",
    counts = "patient_count"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 12. Character vs Factor Variables
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles character groups variable", {
  devtools::load_all()

  test_data_char <- jwaffle_test
  test_data_char$response_category <- as.character(test_data_char$response_category)

  result <- jwaffle(
    data = test_data_char,
    groups = "response_category"
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles character facet variable", {
  devtools::load_all()

  test_data_char <- jwaffle_test
  test_data_char$treatment <- as.character(test_data_char$treatment)

  result <- jwaffle(
    data = test_data_char,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 13. Empty Titles
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles empty title string", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    mytitle = ""
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles empty legend title string", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_test,
    groups = "response_category",
    show_legend = TRUE,
    legendtitle = ""
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 14. Large Facet Levels
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles faceting with 5 levels", {
  devtools::load_all()

  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "gender",
    facet = "age_group"  # 5 levels creates 5 waffle charts
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 15. Extreme Row Values
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles very few rows with many categories", {
  devtools::load_all()

  # 2 rows with 5 categories = only 20 squares for 5 colors
  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "age_group",  # 5 categories
    rows = 2               # Only 20 squares total
  )

  expect_s3_class(result, "jwaffleResults")
})

test_that("jwaffle handles many rows with few categories", {
  devtools::load_all()

  # 15 rows with 2 categories = 150 squares for 2 colors
  result <- jwaffle(
    data = jwaffle_demographics,
    groups = "gender",  # 2 categories
    rows = 15           # 150 squares total
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 16. Decimal Count Values
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles decimal count values", {
  devtools::load_all()

  test_data_decimal <- jwaffle_test
  test_data_decimal$decimal_count <- runif(nrow(test_data_decimal), 0.5, 2.5)

  result <- jwaffle(
    data = test_data_decimal,
    groups = "response_category",
    counts = "decimal_count"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 17. Negative Count Values
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles negative count values", {
  devtools::load_all()

  test_data_negative <- jwaffle_test
  test_data_negative$negative_count <- c(-1, rep(1, nrow(test_data_negative) - 1))

  # Should error or warn
  expect_condition(
    jwaffle(
      data = test_data_negative,
      groups = "response_category",
      counts = "negative_count"
    )
  )
})

# ═══════════════════════════════════════════════════════════
# 18. Very Long Category Names
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles long category names", {
  devtools::load_all()

  test_data_long <- jwaffle_test
  levels(test_data_long$response_category) <- c(
    "Complete Response (No Evidence of Disease)",
    "Partial Response (>30% Reduction)",
    "Stable Disease (No Significant Change)",
    "Progressive Disease (>20% Increase)"
  )

  result <- jwaffle(
    data = test_data_long,
    groups = "response_category",
    show_legend = TRUE
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 19. Single Facet Level
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles single facet level", {
  devtools::load_all()

  test_data_single_facet <- jwaffle_test
  test_data_single_facet$treatment <- "Chemotherapy"  # All same

  result <- jwaffle(
    data = test_data_single_facet,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})

# ═══════════════════════════════════════════════════════════
# 20. All Missing in One Facet Level
# ═══════════════════════════════════════════════════════════

test_that("jwaffle handles all missing in one facet level", {
  devtools::load_all()

  test_data_facet_na <- jwaffle_test
  # Set all values to NA for one treatment group
  chemo_indices <- which(test_data_facet_na$treatment == "Chemotherapy")
  test_data_facet_na$response_category[chemo_indices] <- NA

  result <- jwaffle(
    data = test_data_facet_na,
    groups = "response_category",
    facet = "treatment"
  )

  expect_s3_class(result, "jwaffleResults")
})
