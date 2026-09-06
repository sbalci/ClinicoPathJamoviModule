# Regression cover for the 2026-09 audit findings on agepyramid.
# Each block fails against the pre-audit backend.

library(testthat)

test_that("the all-rows-dropped reject names the exclusion cause and the column", {
  # .rejectClean() wipes the notices panel, including the WARNING that named
  # the age column, so the banner used to read only "No valid rows remain
  # after filtering age and gender values" - no cause, no column.

  # Usual trigger: an age column chosen as a text-coded factor.
  d_text_age <- data.frame(yrs = factor(c("a", "b", "c")),
                           sex = factor(c("Female", "Male", "Female")))
  expect_error(
    agepyramid(data = d_text_age, age = "yrs", gender = "sex",
               female = "Female", male = "Male"),
    "an age in 'yrs' that is negative, infinite or not a number \\(3 rows\\)")

  # Every gender value matches neither selected level.
  d_other <- data.frame(age = c(30, 40, 50),
                        sex = factor(rep("Other", 3), levels = c("Female", "Male", "Other")))
  expect_error(
    agepyramid(data = d_other, age = "age", gender = "sex",
               female = "Female", male = "Male"),
    "a value of 'sex' matching neither the female nor the male level \\(3 rows\\)")

  # Every row lost to a source NA (removed by naOmit before either counter).
  d_na <- data.frame(age = c(NA_real_, NA_real_), sex = factor(c("Female", "Male")))
  expect_error(
    agepyramid(data = d_na, age = "age", gender = "sex",
               female = "Female", male = "Male"),
    "a missing age or gender in the source data \\(2 rows\\)")
})
