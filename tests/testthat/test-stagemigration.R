# ═══════════════════════════════════════════════════════════════════════
# NOTE: these tests were skip_on_cran()-gated and had never actually run.
# When first executed they failed 14/18 for two reasons that predate any
# behaviour change: (1) every assertion checked inherits(result, "jmvcoreClass"),
# a class that does not exist in jmvcore at all, so it could never be TRUE --
# stagemigration() returns analysis$results, a "stagemigrationResults" object;
# (2) one test passed bootstrapReps = 50, which the .a.yaml rejects (min 100).
# Run them with NOT_CRAN=true.
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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
    bootstrapReps = 100
  )

  expect_true(inherits(result, "stagemigrationResults"))
})

# ═══════════════════════════════════════════════════════════════════════
# 6. CLINICAL PRESETS
# ═══════════════════════════════════════════════════════════════════════

test_that("runs with default options (was: routine clinical preset)", {
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result_95, "stagemigrationResults"))
  expect_true(inherits(result_90, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
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

  expect_true(inherits(result, "stagemigrationResults"))
})

test_that("the removed clinical-preset option is gone from the schema", {
  # clinicalPreset was removed: all four presets were a no-op. .run() called
  # .applyClinicalPreset(), assigned the returned config to `preset_config`,
  # message()d two of its fields and never applied any of them to an option --
  # so "Routine Clinical" and "Publication Manuscript" produced byte-identical
  # analyses, while the migrationOverview note told the user a preset had been
  # applied. It also drove a sentence in the copy-ready clinical report that
  # claimed "Advanced methods included time-dependent ROC analysis, decision
  # curve analysis..." purely from the preset label, regardless of whether
  # those analyses had run. That sentence now keys off the real options.
  # Guard the removal so the dead option cannot quietly return.
  a_yaml <- readLines(test_path("..", "..", "jamovi", "stagemigration.a.yaml"), warn = FALSE)
  u_yaml <- readLines(test_path("..", "..", "jamovi", "stagemigration.u.yaml"), warn = FALSE)
  r_yaml <- readLines(test_path("..", "..", "jamovi", "stagemigration.r.yaml"), warn = FALSE)
  expect_false(any(grepl("clinicalPreset", a_yaml, fixed = TRUE)))
  expect_false(any(grepl("clinicalPreset", u_yaml, fixed = TRUE)))
  expect_false(any(grepl("clinicalPreset", r_yaml, fixed = TRUE)))

  # complexityMode had the same defect and was removed with it: a ComboBox in
  # the UI that the backend read zero times, while the docs claimed it changed
  # analysis scope and runtime ("5-10 min" / "1-2 hours").
  expect_false(any(grepl("complexityMode", a_yaml, fixed = TRUE)))
  expect_false(any(grepl("complexityMode", u_yaml, fixed = TRUE)))

  # The .b.R must not read it any more either (comments may still mention it).
  b_src <- readLines(test_path("..", "..", "R", "stagemigration.b.R"), warn = FALSE)
  code <- b_src[!grepl("^\\s*#", b_src)]
  expect_false(any(grepl("clinicalPreset", code, fixed = TRUE)))
  expect_false(any(grepl("applyClinicalPreset", code, fixed = TRUE)))

  # The R wrapper lives in the GENERATED R/stagemigration.h.R, which still
  # declares the option until someone runs jmvtools::prepare(). Skip rather
  # than fail while the generated header is stale.
  h_src <- readLines(test_path("..", "..", "R", "stagemigration.h.R"), warn = FALSE)
  skip_if(any(grepl("clinicalPreset", h_src, fixed = TRUE)),
          "R/stagemigration.h.R is stale - run jmvtools::prepare()")
  expect_error(
    stagemigration(data = stagemigration_lung_cancer, oldStage = "old_stage",
                   newStage = "new_stage", survivalTime = "survival_time",
                   event = "event", eventLevel = "1",
                   clinicalPreset = "routine_clinical"),
    "unused argument")
})

# ═══════════════════════════════════════════════════════════════════════
# RELEASE REVIEW (2026-09-10): reference values and clinical edge cases
# Each numeric check is against survival / hand computation, not the module.
# ═══════════════════════════════════════════════════════════════════════

sm_lung <- function() {
  e <- new.env()
  load(testthat::test_path("..", "..", "data", "stagemigration_lung_cancer.rda"), envir = e)
  d <- e$lung[, c("old_stage", "new_stage", "survival_time", "event")]
  d$old_stage <- factor(as.character(d$old_stage))
  d$new_stage <- factor(as.character(d$new_stage))
  d
}
sm_run <- function(data, ...) {
  ClinicoPath::stagemigration(data = data, oldStage = "old_stage", newStage = "new_stage",
                              survivalTime = "survival_time", event = "event", eventLevel = "1", ...)
}

test_that("migration overview counts match a direct cross-tabulation", {
  skip_on_cran()
  d <- sm_lung()
  ov <- sm_run(d)$migrationOverview$asDF
  oi <- match(as.character(d$old_stage), levels(d$old_stage))
  ni <- match(as.character(d$new_stage), levels(d$old_stage))
  expect_equal(as.numeric(as.character(ov$value)),
               c(nrow(d), sum(oi == ni), sum(oi != ni), sum(ni > oi), sum(ni < oi)))
})

test_that("C-index difference CI and p-value use the paired variance", {
  skip_on_cran()
  d <- sm_lung()
  res <- sm_run(d, showStatisticalComparison = TRUE, showConcordanceComparison = TRUE)
  fo <- survival::coxph(survival::Surv(survival_time, event) ~ old_stage, data = d)
  fn <- survival::coxph(survival::Surv(survival_time, event) ~ new_stage, data = d)
  cc <- survival::concordance(fo, fn)
  diff <- cc$concordance[2] - cc$concordance[1]
  se <- sqrt(drop(c(-1, 1) %*% cc$var %*% c(-1, 1)))
  sc <- res$statisticalComparison$asDF
  ci <- as.numeric(regmatches(sc$ci[sc$metric == "C-index Improvement"], gregexpr("[-+]?[0-9.]+", sc$ci[sc$metric == "C-index Improvement"]))[[1]])
  # the table prints 4 decimals, so compare on an absolute scale
  expect_true(all(abs(ci - unname(c(diff - qnorm(0.975) * se, diff + qnorm(0.975) * se))) < 1e-4))
  cmp <- res$concordanceComparison$asDF
  expect_identical(as.character(cmp$p_value[cmp$Model == "New Staging"]), sprintf("%.3f", 2 * pnorm(-abs(diff / se))))
})

test_that("likelihood-ratio rows are nested tests through the combined model", {
  skip_on_cran()
  d <- sm_lung()
  lt <- sm_run(d, performLikelihoodTests = TRUE)$likelihoodTests$asDF
  expect_equal(nrow(lt), 2)
  fo <- survival::coxph(survival::Surv(survival_time, event) ~ old_stage, data = d)
  fn <- survival::coxph(survival::Surv(survival_time, event) ~ new_stage, data = d)
  both <- survival::coxph(survival::Surv(survival_time, event) ~ old_stage + new_stage, data = d)
  expect_equal(as.numeric(lt$Chi_Square), c(anova(fo, both)[2, "Chisq"], anova(fn, both)[2, "Chisq"]), tolerance = 1e-6)
  expect_true(all(as.numeric(lt$Chi_Square) >= 0))
})

test_that("statistical summary reports the computed C-index difference, not constants", {
  skip_on_cran()
  d <- sm_lung()
  ss <- sm_run(d, showStatisticalSummary = TRUE)$statisticalSummary$asDF
  row <- ss[ss$Method == "C-index Improvement", ]
  cc <- survival::concordance(survival::coxph(survival::Surv(survival_time, event) ~ old_stage, data = d),
                              survival::coxph(survival::Surv(survival_time, event) ~ new_stage, data = d))
  expect_identical(as.character(row$Result), sprintf("%.4f", cc$concordance[2] - cc$concordance[1]))
  expect_false(identical(as.character(row$CI), "[-0.0341, +0.0698]"))
  expect_false(isTRUE(all.equal(as.numeric(row$p_value), 0.501)))
})

test_that("migration summary describes the Monte Carlo Fisher test honestly", {
  skip_on_cran()
  ms <- sm_run(sm_lung(), showMigrationSummary = TRUE)$migrationSummary$asDF
  expect_match(as.character(ms$value[ms$statistic == "Fisher's Exact Test"]), "Monte Carlo")
  expect_false(grepl("e-16", as.character(ms$value[ms$statistic == "Chi-square p-value"]), fixed = TRUE))
})

test_that("different stage label sets leave the direction of migration undefined", {
  skip_on_cran()
  d <- sm_lung()
  set.seed(5)
  ns <- as.character(d$new_stage)
  ii <- ns == "Stage II"
  ns[ii] <- sample(c("Stage IIA", "Stage IIB"), sum(ii), replace = TRUE)
  d$new_stage <- factor(ns)
  res <- sm_run(d)
  ov <- res$migrationOverview$asDF
  expect_identical(as.character(ov$value[ov$statistic == "Upstaged"]), "Not defined")
  expect_equal(as.numeric(as.character(ov$value[ov$statistic == "Unchanged Stage"])), sum(as.character(d$old_stage) == ns))
  expect_match(res$notices$content, "Direction of migration not determined", fixed = TRUE)
})

test_that("an all-censored cohort fails with an events message", {
  skip_on_cran()
  d <- sm_lung()
  d$event <- 0
  expect_error(sm_run(d), "Too few events")
})

test_that("special-character names and a labelled factor event give the same counts", {
  skip_on_cran()
  d <- sm_lung()
  d5 <- data.frame(`Old Stage (AJCC7)` = d$old_stage, `New Stage` = d$new_stage, `OS months` = d$survival_time,
                   `Vital status` = factor(ifelse(d$event == 1, "Dead", "Alive"), levels = c("Alive", "Dead")), check.names = FALSE)
  res <- ClinicoPath::stagemigration(data = d5, oldStage = "Old Stage (AJCC7)", newStage = "New Stage", survivalTime = "OS months",
                                     event = "Vital status", eventLevel = "Dead")
  expect_equal(as.numeric(as.character(res$migrationOverview$asDF$value[1])), nrow(d))
  expect_match(res$notices$content, sprintf("with %d events", sum(d$event == 1)), fixed = TRUE)
})

test_that("methods whose locals feed survival formulas run", {
  skip_on_cran()
  d <- sm_lung()
  d$event_binary <- as.integer(d$event == 1)
  opts <- ClinicoPath:::stagemigrationOptions$new(oldStage = "old_stage", newStage = "new_stage", survivalTime = "survival_time",
                                                 event = "event", eventLevel = "1")
  a <- ClinicoPath:::stagemigrationClass$new(options = opts, data = d)
  a$init()
  pv <- a$.__enclos_env__$private
  wr <- pv$.calculateWillRogersEffect(d, "Stage II", "Stage III", 27)
  expect_true(is.list(wr) && nzchar(wr$Will_Rogers_Evidence))
  sub <- d[d$new_stage == "Stage II", ]
  expect_equal(unname(pv$.calculateMedianSurvival(sub)),
               unname(summary(survival::survfit(survival::Surv(survival_time, event_binary) ~ 1, data = sub))$table["median"]))
})

test_that("space-bearing variable names work with DCA, bootstrap, homogeneity, and trend tests", {
  skip_on_cran()
  d <- sm_lung()
  d_spaces <- data.frame(
    `Old Stage` = d$old_stage,
    `New Stage` = d$new_stage,
    `OS Months` = d$survival_time,
    `Vital Status` = d$event,
    check.names = FALSE
  )
  res <- ClinicoPath::stagemigration(
    data = d_spaces,
    oldStage = "Old Stage",
    newStage = "New Stage",
    survivalTime = "OS Months",
    event = "Vital Status",
    eventLevel = "1",
    performDCA = TRUE,
    performBootstrap = TRUE,
    bootstrapReps = 100,
    performHomogeneityTests = TRUE,
    performTrendTests = TRUE
  )
  expect_true(inherits(res, "stagemigrationResults"))
  expect_false(is.null(res$dcaResults))
  expect_false(is.null(res$bootstrapResults))
  expect_false(is.null(res$homogeneityTests))
  expect_false(is.null(res$trendTests))
})
