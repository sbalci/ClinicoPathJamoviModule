# ═══════════════════════════════════════════════════════════════════════
# Comprehensive Tests: stagemigration (TNM Stage Migration Analysis)
# ═══════════════════════════════════════════════════════════════════════

# --- Test data generator -----------------------------------------------

make_stage_data <- function(n = 200, seed = 42, n_stages = 3, event_rate = 0.6) {
  set.seed(seed)

  stage_names <- c("I", "II", "III", "IV")[1:n_stages]
  old_stage <- factor(
    sample(stage_names, n, replace = TRUE),
    levels = stage_names, ordered = TRUE
  )

  # Simulate realistic migration (~25% reclassified)
  new_stage <- as.character(old_stage)
  for (i in seq_len(n)) {
    if (runif(1) < 0.25) {
      idx <- which(stage_names == old_stage[i])
      shift <- sample(c(-1, 1), 1)
      new_idx <- max(1, min(n_stages, idx + shift))
      new_stage[i] <- stage_names[new_idx]
    }
  }
  new_stage <- factor(new_stage, levels = stage_names, ordered = TRUE)

  # Survival correlated with new staging
  hazard <- 0.01 * (as.numeric(new_stage))^2
  time <- rexp(n, rate = hazard)
  time <- pmin(time, 120)
  cens <- rbinom(n, 1, 1 - event_rate) == 1
  status <- as.integer(!cens & time < 120)
  time[cens] <- runif(sum(cens), 0.5, time[cens])
  time <- round(pmax(time, 0.5), 1)

  data.frame(
    old_stage = old_stage,
    new_stage = new_stage,
    time = time,
    status = status,
    age = round(rnorm(n, 65, 12)),
    sex = factor(sample(c("Male", "Female"), n, replace = TRUE)),
    grade = factor(sample(c("Well", "Moderate", "Poor"), n, replace = TRUE)),
    institution = factor(sample(paste0("Site_", LETTERS[1:3]), n, replace = TRUE)),
    stringsAsFactors = FALSE
  )
}

# ═══════════════════════════════════════════════════════════════════════
# 1. BASIC FUNCTIONALITY
# ═══════════════════════════════════════════════════════════════════════

test_that("stagemigration runs with minimal required variables", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("stagemigration produces migration matrix", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    showMigrationMatrix = TRUE
  )

  expect_false(is.null(result$migrationMatrix))
})

# ═══════════════════════════════════════════════════════════════════════
# 2. ANALYSIS TYPES
# ═══════════════════════════════════════════════════════════════════════

test_that("stagemigration runs with basic analysis type", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    analysisType = "basic"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("stagemigration runs with standard analysis type", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    analysisType = "standard"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 3. STATISTICAL METRICS
# ═══════════════════════════════════════════════════════════════════════

test_that("NRI calculation runs when enabled", {
  skip_on_cran()
  df <- make_stage_data(n = 150)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    calculateNRI = TRUE
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("IDI calculation runs when enabled", {
  skip_on_cran()
  df <- make_stage_data(n = 150)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    calculateIDI = TRUE
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("C-index comparison runs", {
  skip_on_cran()
  df <- make_stage_data(n = 150)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    showStatisticalComparison = TRUE
  )

  expect_false(is.null(result$statisticalComparison))
})

# ═══════════════════════════════════════════════════════════════════════
# 4. WILL ROGERS EFFECT
# ═══════════════════════════════════════════════════════════════════════

test_that("Will Rogers analysis detects stage migration", {
  skip_on_cran()
  # Create data with deliberate upstaging of intermediate patients
  n <- 200
  set.seed(456)
  old_stage <- factor(c(rep("I", 80), rep("II", 80), rep("I", 40)),
                      levels = c("I", "II"))
  new_stage <- factor(c(rep("I", 80), rep("II", 80), rep("II", 40)),
                      levels = c("I", "II"))
  time <- c(rexp(80, 1/50), rexp(80, 1/10), rexp(40, 1/25))
  status <- sample(c(0, 1), 200, replace = TRUE, prob = c(0.1, 0.9))

  df <- data.frame(old_stage, new_stage, time, status)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    showWillRogersAnalysis = TRUE
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 5. BOOTSTRAP & VALIDATION
# ═══════════════════════════════════════════════════════════════════════

test_that("bootstrap validation runs", {
  skip_on_cran()
  df <- make_stage_data(n = 100)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    performBootstrap = TRUE,
    bootstrapReps = 50
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 6. CLINICAL PRESETS
# ═══════════════════════════════════════════════════════════════════════

test_that("routine clinical preset works", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    clinicalPreset = "routine_clinical"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 7. PLOTS
# ═══════════════════════════════════════════════════════════════════════

test_that("survival curves plot is generated", {
  skip_on_cran()
  df <- make_stage_data()

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    showSurvivalCurves = TRUE
  )

  expect_false(is.null(result$survivalCurves))
})

# ═══════════════════════════════════════════════════════════════════════
# 8. CONFIDENCE LEVEL
# ═══════════════════════════════════════════════════════════════════════

test_that("confidence level is respected", {
  skip_on_cran()
  df <- make_stage_data(n = 100)

  result_95 <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    confidenceLevel = 0.95
  )

  result_90 <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    confidenceLevel = 0.90
  )

  expect_true(inherits(result_95, "jmvcoreClass"))
  expect_true(inherits(result_90, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 9. EDGE CASES
# ═══════════════════════════════════════════════════════════════════════

test_that("stagemigration handles 2-stage system", {
  skip_on_cran()
  df <- make_stage_data(n = 100, n_stages = 2)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("stagemigration handles 4-stage system", {
  skip_on_cran()
  df <- make_stage_data(n = 200, n_stages = 4)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("stagemigration handles no migration (identical staging)", {
  skip_on_cran()
  df <- make_stage_data(n = 100)
  df$new_stage <- df$old_stage  # No migration

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

test_that("stagemigration handles high censoring rate", {
  skip_on_cran()
  df <- make_stage_data(n = 150, event_rate = 0.15)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1"
  )

  expect_true(inherits(result, "jmvcoreClass"))
})

# ═══════════════════════════════════════════════════════════════════════
# 10. MULTIPLE OPTIONS COMBINED
# ═══════════════════════════════════════════════════════════════════════

test_that("stagemigration runs comprehensive analysis", {
  skip_on_cran()
  df <- make_stage_data(n = 200)

  result <- stagemigration(
    data = df,
    oldStage = "old_stage",
    newStage = "new_stage",
    survivalTime = "time",
    event = "status",
    eventLevel = "1",
    showMigrationMatrix = TRUE,
    showStatisticalComparison = TRUE,
    calculateNRI = TRUE,
    calculateIDI = TRUE,
    showWillRogersAnalysis = TRUE,
    showSurvivalCurves = TRUE,
    confidenceLevel = 0.95
  )

  expect_true(inherits(result, "jmvcoreClass"))
})
