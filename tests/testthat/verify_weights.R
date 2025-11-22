
# Verification script for jjbarstats weighted calculations

library(testthat)
library(jmvcore)
library(R6)

# Source the necessary files
source("R/jjbarstats.h.R")
source("R/jjbarstats.b.R")

# Create a dummy dataset with weights
set.seed(123)
df <- data.frame(
  group = factor(rep(c("A", "B"), each = 5)),
  outcome = factor(rep(c("Yes", "No"), 5)),
  count = sample(10:50, 10, replace = TRUE)
)

# Helper function to mimic jjbarstats internals (since they are private)
# We'll copy the logic here for verification
get_weighted_table <- function(data, var1, var2, counts_var) {
  formula_str <- paste0(counts_var, " ~ ", var1, " + ", var2)
  xtabs(as.formula(formula_str), data = data)
}

# 1. Verify Weighted Table
test_that("Weighted table matches xtabs", {
  wt_table <- get_weighted_table(df, "outcome", "group", "count")
  
  # Manual calculation
  manual_table <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("No", "Yes"), c("A", "B")))
  for (i in 1:nrow(df)) {
    manual_table[as.character(df$outcome[i]), as.character(df$group[i])] <- 
      manual_table[as.character(df$outcome[i]), as.character(df$group[i])] + df$count[i]
  }
  
  expect_equal(as.matrix(wt_table), manual_table, ignore_attr = TRUE)
})

# 2. Verify Chi-square on weighted data
test_that("Chi-square on weighted data is correct", {
  wt_table <- get_weighted_table(df, "outcome", "group", "count")
  chisq_res <- chisq.test(wt_table)
  
  # Compare with expanding data
  expanded_df <- df[rep(row.names(df), df$count), c("group", "outcome")]
  chisq_expanded <- chisq.test(table(expanded_df$outcome, expanded_df$group))
  
  expect_equal(chisq_res$statistic, chisq_expanded$statistic)
  expect_equal(chisq_res$p.value, chisq_expanded$p.value)
})

print("Verification complete!")
