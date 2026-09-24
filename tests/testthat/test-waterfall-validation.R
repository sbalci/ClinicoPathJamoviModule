# Regression tests for waterfall validation
# Generated per .claude/commands/validate-function.md
# Date: 2026-09-23

test_that("VAL-waterfall: Hand-calculated known answers on percentage data (Claims C01-C07, C14-C19)", {
  df_hand <- data.frame(
    id = paste0("PT", 1:6),
    resp = c(-100, -50, -30, 0, 20, 40),
    grp = factor(rep(c("DrugA", "DrugB"), each = 3))
  )

  res <- ClinicoPath::waterfall(
    data = df_hand,
    patientID = "id",
    responseVar = "resp",
    groupVar = "grp",
    showConfidenceIntervals = TRUE,
    generateCopyReadyReport = TRUE
  )

  st <- res$summaryTable$asDF
  st_keys <- gsub('^"|"$', '', rownames(st))

  # Claim C01: Category counts
  expect_equal(st[st_keys == "recist_CR", "n"], 1L)
  expect_equal(st[st_keys == "recist_PR", "n"], 2L)
  expect_equal(st[st_keys == "recist_SD", "n"], 1L)
  expect_equal(st[st_keys == "recist_PD", "n"], 2L)

  # Claim C02: Proportions
  expect_equal(st[st_keys == "recist_CR", "percent"], 1/6)
  expect_equal(st[st_keys == "recist_PR", "percent"], 2/6)
  expect_equal(st[st_keys == "recist_SD", "percent"], 1/6)
  expect_equal(st[st_keys == "recist_PD", "percent"], 2/6)

  # Claim C03: Evaluable N
  expect_equal(as.integer(res$clinicalMetrics$asDF$value[1]), 6L)

  # Claims C04-C07: ORR and DCR point estimates and exact Clopper-Pearson CIs
  ecm <- res$enhancedClinicalMetrics$asDF
  expect_equal(ecm$value[1], "50.0%")
  expect_equal(ecm$value[2], "66.7%")

  ci_orr_oracle <- round(stats::binom.test(3, 6)$conf.int * 100, 1)
  ci_dcr_oracle <- round(stats::binom.test(4, 6)$conf.int * 100, 1)
  expect_equal(ecm$ci_lower[1], ci_orr_oracle[1])
  expect_equal(ecm$ci_upper[1], ci_orr_oracle[2])
  expect_equal(ecm$ci_lower[2], ci_dcr_oracle[1])
  expect_equal(ecm$ci_upper[2], ci_dcr_oracle[2])

  # Claims C14-C19: Group comparisons
  gct <- res$groupComparisonTable$asDF
  expect_equal(gct$n_patients, c(3L, 3L))
  expect_equal(gct$orr, c(100, 0))
  expect_equal(gct$dcr, c(100.0, 33.3))

  gctest <- res$groupComparisonTest$asDF
  expect_equal(gctest$p_value[1], 0.1)
  expect_equal(gctest$p_value[2], 0.4)
})

test_that("VAL-waterfall: Hand-calculated known answers on longitudinal raw data (Claims C21-C27)", {
  df_hand_long <- data.frame(
    id = c("PT1", "PT1", "PT1", "PT1",
           "PT2", "PT2", "PT2", "PT2",
           "PT3", "PT3", "PT3"),
    time = c(0, 2, 4, 6,
             0, 2, 4, 6,
             0, 2, 4),
    meas = c(50, 25, 0, 10,
             40, 20, 20, 22,
             30, 30, 36)
  )

  res <- ClinicoPath::waterfall(
    data = df_hand_long,
    patientID = "id",
    responseVar = "meas",
    timeVar = "time",
    inputType = "raw",
    showResponseDuration = TRUE,
    showConfidenceIntervals = TRUE
  )

  rdt <- res$responseDurationTable$asDF
  rdt_keys <- gsub('^"|"$', '', rownames(rdt))

  # Claims C21-C23: TTR and DoR
  expect_equal(rdt[rdt_keys == "ttr", "value"], 2)
  expect_equal(rdt[rdt_keys == "dor_naive", "value"], 4)
  expect_equal(rdt[rdt_keys == "dor_km", "value"], 4)

  # Claims C24-C27: Person-time metrics
  ptt <- res$personTimeTable$asDF
  tot_pt <- ptt[ptt$category == "Total", ]
  expect_equal(tot_pt$patients, 3L)
  expect_equal(tot_pt$person_time, "16.0")

  cm <- res$clinicalMetrics$asDF
  expect_equal(as.numeric(cm$value[6]), 50.0)
})

test_that("VAL-waterfall: Reference parity on bundled datasets (Claims C01-C07, C17, C25)", {
  # waterfall_percentage_basic
  data("waterfall_percentage_basic", package = "ClinicoPath")
  res_p <- ClinicoPath::waterfall(
    data = waterfall_percentage_basic,
    patientID = "PatientID",
    responseVar = "Response",
    groupVar = "Treatment",
    showConfidenceIntervals = TRUE
  )

  st_p <- res_p$summaryTable$asDF
  st_p_keys <- gsub('^"|"$', '', rownames(st_p))
  expect_equal(st_p[st_p_keys == "recist_CR", "n"], 1L)
  expect_equal(st_p[st_p_keys == "recist_PR", "n"], 6L)
  expect_equal(st_p[st_p_keys == "recist_SD", "n"], 9L)
  expect_equal(st_p[st_p_keys == "recist_PD", "n"], 4L)
  expect_equal(res_p$enhancedClinicalMetrics$asDF$value[1], "35.0%")
  expect_equal(res_p$enhancedClinicalMetrics$asDF$value[2], "80.0%")
  expect_equal(round(res_p$groupComparisonTest$asDF$p_value[1], 6), 0.003096)

  # waterfall_raw_longitudinal
  data("waterfall_raw_longitudinal", package = "ClinicoPath")
  res_r <- ClinicoPath::waterfall(
    data = waterfall_raw_longitudinal,
    patientID = "PatientID",
    responseVar = "Measurement",
    timeVar = "Time",
    inputType = "raw",
    showResponseDuration = TRUE
  )

  st_r <- res_r$summaryTable$asDF
  st_r_keys <- gsub('^"|"$', '', rownames(st_r))
  expect_equal(st_r[st_r_keys == "recist_CR", "n"], 1L)
  expect_equal(st_r[st_r_keys == "recist_PR", "n"], 11L)
  expect_equal(st_r[st_r_keys == "recist_SD", "n"], 3L)
  expect_equal(st_r[st_r_keys == "recist_PD", "n"], 0L)
  expect_equal(as.numeric(res_r$personTimeTable$asDF[5, "person_time"]), 90)
})

test_that("VAL-waterfall: Metamorphic properties (permutation, time and measurement scaling)", {
  data("waterfall_percentage_basic", package = "ClinicoPath")

  # Row permutation invariance
  set.seed(42)
  wpb_perm <- waterfall_percentage_basic[sample(nrow(waterfall_percentage_basic)), ]
  res_orig <- ClinicoPath::waterfall(data = waterfall_percentage_basic, patientID = "PatientID", responseVar = "Response")
  res_perm <- ClinicoPath::waterfall(data = wpb_perm, patientID = "PatientID", responseVar = "Response")
  expect_equal(res_perm$summaryTable$asDF$n, res_orig$summaryTable$asDF$n)

  # Measurement rescaling invariance (raw input multiplied by a constant)
  df_hand_long <- data.frame(
    id = c("PT1", "PT1", "PT1", "PT1", "PT2", "PT2", "PT2", "PT2", "PT3", "PT3", "PT3"),
    time = c(0, 2, 4, 6, 0, 2, 4, 6, 0, 2, 4),
    meas = c(50, 25, 0, 10, 40, 20, 20, 22, 30, 30, 36)
  )
  df_scaled <- df_hand_long
  df_scaled$meas <- df_scaled$meas * 10
  res_hand_long <- ClinicoPath::waterfall(data = df_hand_long, patientID = "id", responseVar = "meas", timeVar = "time", inputType = "raw")
  res_scaled <- ClinicoPath::waterfall(data = df_scaled, patientID = "id", responseVar = "meas", timeVar = "time", inputType = "raw")
  expect_equal(res_scaled$summaryTable$asDF$n, res_hand_long$summaryTable$asDF$n)

  # Time rescaling (time * 30 scales TTR and DoR by 30)
  df_time_scaled <- df_hand_long
  df_time_scaled$time <- df_time_scaled$time * 30
  res_t_scaled <- ClinicoPath::waterfall(data = df_time_scaled, patientID = "id", responseVar = "meas", timeVar = "time", inputType = "raw", showResponseDuration = TRUE)
  rdt_t <- res_t_scaled$responseDurationTable$asDF
  rdt_t_keys <- gsub('^"|"$', '', rownames(rdt_t))
  expect_equal(rdt_t[rdt_t_keys == "ttr", "value"], 60)
  expect_equal(rdt_t[rdt_t_keys == "dor_naive", "value"], 120)
  expect_equal(rdt_t[rdt_t_keys == "dor_km", "value"], 120)
})

test_that("VAL-waterfall: Boundary conditions and degenerate inputs (Claims C01-C05, C28)", {
  # Single patient
  res_1 <- ClinicoPath::waterfall(data = data.frame(id = "PT1", resp = -50), patientID = "id", responseVar = "resp")
  expect_equal(as.integer(res_1$clinicalMetrics$asDF$value[1]), 1L)

  # All CR
  res_cr <- ClinicoPath::waterfall(data = data.frame(id = paste0("PT", 1:3), resp = rep(-100, 3)), patientID = "id", responseVar = "resp", showConfidenceIntervals = TRUE)
  expect_equal(res_cr$enhancedClinicalMetrics$asDF$value[1], "100.0%")
  expect_equal(res_cr$enhancedClinicalMetrics$asDF$ci_upper[1], 100.0)

  # All PD
  res_pd <- ClinicoPath::waterfall(data = data.frame(id = paste0("PT", 1:3), resp = rep(40, 3)), patientID = "id", responseVar = "resp", showConfidenceIntervals = TRUE)
  expect_equal(res_pd$enhancedClinicalMetrics$asDF$value[1], "0.0%")
  expect_equal(res_pd$enhancedClinicalMetrics$asDF$ci_lower[1], 0.0)

  # Impossible shrinkage capped (< -100%)
  res_cap <- ClinicoPath::waterfall(data = data.frame(id = c("P1", "P2"), resp = c(-150, -40)), patientID = "id", responseVar = "resp")
  expect_equal(res_cap$summaryTable$asDF[1, "n"], 1L)

  # Zero baseline excluded
  df_zb <- data.frame(id = c("P1", "P1", "P2", "P2"), time = c(0, 2, 0, 2), meas = c(0, 10, 30, 15))
  res_zb <- ClinicoPath::waterfall(data = df_zb, patientID = "id", responseVar = "meas", timeVar = "time", inputType = "raw")
  expect_equal(as.integer(res_zb$clinicalMetrics$asDF$value[1]), 1L)
})

test_that("VAL-waterfall: Category override updates clinical metrics (Claim C04)", {
  df_hand <- data.frame(
    id = paste0("PT", 1:6),
    resp = c(-100, -50, -30, 0, 20, 40),
    override = factor(c("CR", "PD", "PR", "SD", "PD", "PD")) # PT2 overridden to PD
  )
  res_ovr <- ClinicoPath::waterfall(
    data = df_hand,
    patientID = "id",
    responseVar = "resp",
    responseCategoryVar = "override",
    showConfidenceIntervals = TRUE
  )
  expect_equal(res_ovr$summaryTable$asDF[2, "n"], 1L) # PR dropped from 2 to 1
  expect_equal(res_ovr$summaryTable$asDF[4, "n"], 3L) # PD increased from 2 to 3
  expect_equal(res_ovr$enhancedClinicalMetrics$asDF$value[1], "33.3%")
})

test_that("VAL-waterfall: Plot rendering and R syntax export (Claims C29, C30, C32)", {
  data("waterfall_percentage_basic", package = "ClinicoPath")
  res <- ClinicoPath::waterfall(data = waterfall_percentage_basic, patientID = "PatientID", responseVar = "Response")

  tmp_wf <- tempfile(fileext = ".png")
  res$waterfallplot$saveAs(tmp_wf)
  expect_true(file.exists(tmp_wf) && file.info(tmp_wf)$size > 1000)
  unlink(tmp_wf)

  # asSource() syntax export
  opts <- ClinicoPath:::waterfallOptions$new(patientID = "PatientID", responseVar = "Response")
  wf_obj <- ClinicoPath:::waterfallClass$new(options = opts, data = waterfall_percentage_basic)
  src <- wf_obj$asSource()
  expect_true(grepl("ClinicoPath::waterfall", src))
})

test_that("VAL-waterfall-01: Module citation metadata in jamovi/00refs.yaml has no conflicting DOI in title", {
  refs_path <- if (file.exists("jamovi/00refs.yaml")) "jamovi/00refs.yaml" else if (file.exists("../../jamovi/00refs.yaml")) "../../jamovi/00refs.yaml"
  txt <- paste(readLines(refs_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  refs <- yaml::yaml.load(txt)
  expect_false(grepl("doi:10.5281/zenodo", refs$refs$ClinicoPathJamoviModule$title))
  expect_equal(refs$refs$ClinicoPathJamoviModule$title, "ClinicoPath jamovi Module")
  expect_equal(refs$refs$ClinicoPathJamoviModule$year, 2020)
  expect_equal(refs$refs$ClinicoPathJamoviModule$doi, "10.17605/OSF.IO/9SZUD")
})
