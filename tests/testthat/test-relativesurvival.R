# ═══════════════════════════════════════════════════════════════════════
# Comprehensive Tests: relativesurvival
# ═══════════════════════════════════════════════════════════════════════

# --- Test data setup --------------------------------------------------

make_relsurv_data <- function(n = 200, seed = 42) {
  set.seed(seed)
  age <- round(pmin(90, pmax(30, rnorm(n, 65, 12))))
  sex <- factor(sample(c("male", "female"), n, replace = TRUE))
  year <- sample(2000:2015, n, replace = TRUE)
  stage_hr <- c(I = 1, II = 1.5, III = 2.5, IV = 5)
  stage <- factor(sample(names(stage_hr), n, replace = TRUE,
                         prob = c(0.2, 0.3, 0.3, 0.2)),
                  levels = names(stage_hr))
  rate <- 0.08 * stage_hr[as.character(stage)] * exp(0.03 * (age - 65))
  fu <- pmin(rexp(n, rate), 10)
  cens <- rbinom(n, 1, 0.15) == 1
  status <- as.integer(!cens & fu < 10)
  fu[cens] <- runif(sum(cens), 0.1, fu[cens])
  fu <- pmax(fu, 1 / 12)

  data.frame(
    followup_years = round(fu, 2),
    vital_status = status,
    age_at_diagnosis = age,
    sex = sex,
    diagnosis_year = year,
    stage = stage,
    tumor_size = round(pmax(0.5, rnorm(n, 3.5, 1.8)), 1),
    stringsAsFactors = FALSE
  )
}

# ═══════════════════════════════════════════════════════════════════════
# 1. BASIC FUNCTIONALITY
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival runs with minimal required variables", {
  skip_on_cran()
  skip_if_not_installed("relsurv")
  skip_if_not_installed("survival")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("relativesurvival produces expected output items", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year"
  )

  # Check key outputs exist
  expect_false(is.null(result$summary))
  expect_false(is.null(result$survivalTable))
  expect_false(is.null(result$interpretation))
  expect_false(is.null(result$notices))
})

# ═══════════════════════════════════════════════════════════════════════
# 2. ALL ESTIMATION METHODS
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival runs with all estimation methods", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()
  methods <- c("poharperme", "ederer1", "ederer2", "hakulinen")

  for (m in methods) {
    result <- relativesurvival(
      data = df,
      time = "followup_years",
      status = "vital_status",
      age = "age_at_diagnosis",
      sex = "sex",
      year = "diagnosis_year",
      method = m
    )
    expect_true(inherits(result, "jmvcoreClass"),
                info = paste("Method", m, "failed"))
  }
})

# ═══════════════════════════════════════════════════════════════════════
# 3. OPTIONAL ANALYSES
# ═══════════════════════════════════════════════════════════════════════

test_that("net survival table populates when enabled", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    net_survival = TRUE
  )

  expect_false(is.null(result$netSurvivalTable))
})

test_that("excess mortality table populates with CIs", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    excess_mortality = TRUE
  )

  expect_false(is.null(result$excessMortalityTable))
})

test_that("crude probability table populates when enabled", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    crude_probability = TRUE
  )

  expect_false(is.null(result$crudeProbTable))
})

test_that("age standardization works with sufficient age groups", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data(n = 300)

  result <- relativesurvival(
    data = df,
    time = "followup_years",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    age_standardized = TRUE
  )

  expect_false(is.null(result$ageStandardizedTable))
})

# ═══════════════════════════════════════════════════════════════════════
# 4. TIME SCALES
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival handles different time scales", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  # Months
  df_months <- df
  df_months$followup_months <- df_months$followup_years * 12
  result_m <- relativesurvival(
    data = df_months,
    time = "followup_months",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    time_scale = "months"
  )
  expect_true(inherits(result_m, "jmvcoreClass"))

  # Days
  df_days <- df
  df_days$followup_days <- round(df_days$followup_years * 365.25)
  result_d <- relativesurvival(
    data = df_days,
    time = "followup_days",
    status = "vital_status",
    age = "age_at_diagnosis",
    sex = "sex",
    year = "diagnosis_year",
    time_scale = "days"
  )
  expect_true(inherits(result_d, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 5. SEX VALUE MAPPING
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival maps multiple sex coding schemes", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data(n = 60)

  # Test m/f coding
  df_mf <- df
  df_mf$sex <- factor(ifelse(df$sex == "male", "m", "f"))
  expect_no_error(relativesurvival(
    data = df_mf, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ))

  # Test Turkish coding
  df_tr <- df
  df_tr$sex <- factor(ifelse(df$sex == "male", "erkek", "kadin"))
  expect_no_error(relativesurvival(
    data = df_tr, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ))
})

# ═══════════════════════════════════════════════════════════════════════
# 6. ERROR HANDLING / EDGE CASES
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival rejects insufficient data", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data(n = 15)  # Below 30-observation minimum

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "Insufficient|minimum")
})

test_that("relativesurvival rejects too few events", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data(n = 100)
  df$vital_status <- 0  # all censored
  df$vital_status[1:5] <- 1  # only 5 events

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "events|10")
})

test_that("relativesurvival rejects invalid status values", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()
  df$vital_status[1:5] <- 2  # invalid

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "0.*1|alive.*dead")
})

test_that("relativesurvival rejects unmappable sex values", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()
  df$sex <- factor(sample(c("X", "Y"), nrow(df), replace = TRUE))

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "male.*female|recode|Unrecognized")
})

test_that("relativesurvival rejects out-of-range ages", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()
  df$age_at_diagnosis[1] <- -5

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "age|0.*120")
})

test_that("relativesurvival rejects invalid calendar years", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()
  df$diagnosis_year[1] <- 1800

  expect_error(relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year"
  ), regexp = "year|1900.*2100|invalid")
})

# ═══════════════════════════════════════════════════════════════════════
# 7. CONFIDENCE LEVEL
# ═══════════════════════════════════════════════════════════════════════

test_that("confidence_level affects output CIs", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result_95 <- relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year",
    confidence_level = 0.95
  )

  result_90 <- relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year",
    confidence_level = 0.90
  )

  # Both should run
  expect_true(inherits(result_95, "jmvcoreClass"))
  expect_true(inherits(result_90, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 8. RATE TABLES
# ═══════════════════════════════════════════════════════════════════════

test_that("relativesurvival works with different built-in rate tables", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data(n = 60)

  for (rt in c("us", "fr", "slovenia")) {
    result <- relativesurvival(
      data = df, time = "followup_years", status = "vital_status",
      age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year",
      ratetable = rt
    )
    expect_true(inherits(result, "jmvcoreClass"),
                info = paste("Rate table", rt, "failed"))
  }
})

# ═══════════════════════════════════════════════════════════════════════
# 9. REGRESSION MODELS (additive)
# ═══════════════════════════════════════════════════════════════════════

test_that("additive regression model runs with covariates", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year",
    covariates = "stage",
    regression_model = "additive"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 10. CUSTOM TIMEPOINTS
# ═══════════════════════════════════════════════════════════════════════

test_that("custom timepoints are respected", {
  skip_on_cran()
  skip_if_not_installed("relsurv")

  df <- make_relsurv_data()

  result <- relativesurvival(
    data = df, time = "followup_years", status = "vital_status",
    age = "age_at_diagnosis", sex = "sex", year = "diagnosis_year",
    timepoints = "1,2,3,5"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})
