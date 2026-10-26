# =============================================================================
# Scientific & Numerical Validation Test Suite: outcomeorganizer
# Playbook: .claude/commands/validate-function.md (depth=exhaustive)
# Date: 2026-10-25
#
# Covers:
#   - Battery A (A1..A6): Known-answer hand-calculated truth tables
#   - Battery B (B1..B4): Canonical dataset & survival/cmprsk/mstate parity
#   - Battery C (C1..C6): Metamorphic relations (permutation, scaling, encoding, complement, scale, NA rows)
#   - Battery D (D1..D9): Boundary conditions & guard thresholds
#   - Battery E (E1..E3): Option matrix & high-risk option interactions
#   - Battery F (F1..F2): Monte Carlo simulation agreement
#   - Battery G (G1):     Cross-output conservation laws
#   - Battery H (H1):     HTML XSS escaping & token hygiene
#   - Battery I (I1):     Plot rendering & state-only resize redraw
#   - Battery J (J1):     Methodological citation metadata
#   - Open Defects (VAL-outcomeorganizer-01..09): Guarded by skip() with correct expectations
# =============================================================================
library(testthat)

# Locate repository root so this file runs both under devtools::test() and standalone testthat::test_file()
.val_root <- if (file.exists("DESCRIPTION")) {
  normalizePath(".", mustWork = TRUE)
} else if (file.exists("../../DESCRIPTION")) {
  normalizePath("../..", mustWork = TRUE)
} else {
  getwd()
}

# Fallback loader when run outside devtools::load_all()
if (!exists("outcomeorganizer", mode = "function") &&
    !(requireNamespace("ClinicoPath", quietly = TRUE) &&
      exists("outcomeorganizer", envir = asNamespace("ClinicoPath"), inherits = FALSE))) {
  suppressWarnings(suppressPackageStartupMessages({
    library(jmvcore)
    library(R6)
    library(dplyr)
    library(magrittr)
    library(janitor)
    library(labelled)
    library(glue)
    library(htmltools)
    library(ggplot2)
  }))
  assign(".", jmvcore::., envir = globalenv())
  source(file.path(.val_root, "R/utils.R"), local = globalenv())
  source(file.path(.val_root, "R/utils-eventindicator.R"), local = globalenv())
  source(file.path(.val_root, "R/outcomeorganizer.h.R"), local = globalenv())
  source(file.path(.val_root, "R/outcomeorganizer.b.R"), local = globalenv())
}

if (!exists("run_outcomeorganizer", mode = "function")) {
  source(file.path(.val_root, "tests/testthat/helper-outcomeorganizer.R"), local = globalenv())
}

for (.rda in list.files(file.path(.val_root, "data"), pattern = "^outcomeorganizer_.*\\.rda$", full.names = TRUE)) {
  load(.rda, envir = globalenv())
}

.val_html <- function(item) {
  if (is.null(item)) return("")
  cnt <- tryCatch(item$content, error = function(e) "")
  if (is.null(cnt) || length(cnt) == 0L) "" else paste(as.character(cnt), collapse = "\n")
}

.val_df <- function(item) {
  if (is.null(item)) return(data.frame())
  tryCatch(item$asDF, error = function(e) data.frame())
}

.val_run <- function(data, ...) {
  a <- run_outcomeorganizer_obj(data = data, ...)
  priv <- a$.__enclos_env__$private
  org <- tryCatch(priv$.organizeOutcomes(), error = function(e) NULL)
  list(
    analysis   = a,
    results    = a$results,
    private    = priv,
    org        = org,
    out_df     = .val_df(a$results$outputTable),
    diag_df    = .val_df(a$results$diagnosticsTable),
    myoutcome  = if (!is.null(org) && !is.null(org$df_outcome)) as.integer(org$df_outcome$myoutcome) else NULL,
    admin_time = if (!is.null(org) && !is.null(org$mydata) && "admin_time" %in% names(org$mydata)) as.numeric(org$mydata$admin_time) else NULL
  )
}

.val_oracle <- function(outcome,
                        analysistype = "os",
                        multievent = FALSE,
                        outcomeLevel = NULL,
                        recurrence = NULL,
                        recurrenceLevel = NULL,
                        dod = NULL, dooc = NULL, awd = NULL, awod = NULL,
                        useHierarchy = FALSE, eventPriority = 1L,
                        patientID = NULL, followupTime = NULL,
                        adminCensoring = FALSE, adminDate = NULL) {
  n <- length(outcome)
  status <- rep(NA_integer_, n)

  if (!multievent) {
    if ((is.numeric(outcome) || is.logical(outcome)) &&
        (is.null(outcomeLevel) || !nzchar(as.character(outcomeLevel)))) {
      num_v <- as.numeric(outcome)
      status[!is.na(num_v) & num_v == 0] <- 0L
      status[!is.na(num_v) & num_v == 1] <- 1L
    } else {
      chr_v <- as.character(outcome)
      lvl <- as.character(outcomeLevel)
      status[!is.na(chr_v) & chr_v == lvl] <- 1L
      status[!is.na(chr_v) & chr_v != lvl] <- 0L
    }
    if (analysistype %in% c("rfs", "pfs", "dfs")) {
      rec_chr <- as.character(recurrence)
      rec_lvl <- as.character(recurrenceLevel)
      rec_ev <- ifelse(is.na(rec_chr), NA_integer_, ifelse(rec_chr == rec_lvl, 1L, 0L))
      status <- ifelse(status == 1L | rec_ev == 1L, 1L,
                       ifelse(status == 0L & rec_ev == 0L, 0L, NA_integer_))
    } else if (analysistype == "ttp") {
      rec_chr <- as.character(recurrence)
      rec_lvl <- as.character(recurrenceLevel)
      status <- ifelse(is.na(rec_chr), NA_integer_, ifelse(rec_chr == rec_lvl, 1L, 0L))
    }
  } else {
    chr_v <- as.character(outcome)
    if (analysistype == "os") {
      status[!is.na(chr_v) & chr_v %in% c(awd, awod)] <- 0L
      status[!is.na(chr_v) & chr_v %in% c(dod, dooc)] <- 1L
    } else if (analysistype == "cause") {
      status[!is.na(chr_v) & chr_v %in% c(dooc, awd, awod)] <- 0L
      status[!is.na(chr_v) & chr_v == dod] <- 1L
    } else if (analysistype == "compete") {
      status[!is.na(chr_v) & chr_v %in% c(awd, awod)] <- 0L
      status[!is.na(chr_v) & chr_v == dod]  <- 1L
      status[!is.na(chr_v) & chr_v == dooc] <- 2L
    } else if (analysistype == "multistate") {
      status[!is.na(chr_v) & chr_v == awod] <- 0L
      status[!is.na(chr_v) & chr_v == awd]  <- 1L
      status[!is.na(chr_v) & chr_v == dod]  <- 2L
      status[!is.na(chr_v) & chr_v == dooc] <- 3L
    }
  }

  if (useHierarchy && !is.null(patientID)) {
    pri <- as.integer(eventPriority)
    pids <- unique(patientID[!is.na(patientID)])
    for (pid in pids) {
      idx <- which(!is.na(patientID) & patientID == pid)
      if (length(idx) <= 1L) next
      pri_rows <- idx[!is.na(status[idx]) & status[idx] == pri]
      if (length(pri_rows) == 0L) next
      if (!is.null(followupTime)) {
        t_sub <- as.numeric(followupTime[idx])
        first_t <- min(t_sub[status[idx] == pri], na.rm = TRUE)
        keep_local <- which(!is.na(status[idx]) & status[idx] == pri & t_sub == first_t)[1L]
        for (k in seq_along(idx)) {
          row_i <- idx[k]
          if (k == keep_local) {
            status[row_i] <- pri
          } else if (!is.na(t_sub[k]) && t_sub[k] >= first_t) {
            status[row_i] <- 0L
          }
        }
      } else {
        status[idx] <- pri
      }
    }
  }

  admin_time <- NULL
  if (adminCensoring && !is.null(followupTime) && !is.null(adminDate) && analysistype != "multistate") {
    fu_num  <- as.numeric(followupTime)
    cut_num <- as.numeric(adminDate)
    beyond  <- !is.na(fu_num) & !is.na(cut_num) & (fu_num > cut_num)
    status[beyond] <- 0L
    admin_time <- ifelse(beyond, cut_num, fu_num)
  }

  non_na <- status[!is.na(status)]
  codes  <- sort(unique(non_na))
  counts <- as.integer(vapply(codes, function(k) sum(non_na == k), integer(1)))
  pcts   <- round(counts / length(non_na) * 100, 1)

  list(
    status     = as.integer(status),
    admin_time = admin_time,
    summary    = data.frame(outcome = as.character(codes), count = counts, percentage = pcts, stringsAsFactors = FALSE)
  )
}

# =============================================================================
# BATTERY A: Known-Answer Hand-Calculated Truth Tables
# =============================================================================

test_that("A1: Binary factor OS recoding with missing values matches hand-calculated truth table", {
  d_a1 <- data.frame(
    vital = factor(
      c("Dead", "Alive", "Dead", NA, "Alive", "Dead", "Dead", "Alive", NA, "Dead"),
      levels = c("Alive", "Dead")
    )
  )
  res <- .val_run(data = d_a1, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "os", outputTable = TRUE)
  expect_identical(res$myoutcome, c(1L, 0L, 1L, NA_integer_, 0L, 1L, 1L, 0L, NA_integer_, 1L))
  expect_identical(res$out_df$outcome, c("0", "1"))
  expect_identical(res$out_df$count, c(3L, 5L))
  expect_equal(res$out_df$percentage, c(37.5, 62.5), tolerance = 0)
})

test_that("A2: Numeric 0/1 and logical TRUE/FALSE recoding match hand-calculated truth tables", {
  d_a2 <- data.frame(
    num01 = c(0, 1, 1, 0, 1, 0, 0, 1, 1, 1),
    lgl   = c(FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE)
  )
  r_def <- .val_run(data = d_a2, outcome = "num01", outcomeLevel = NULL, analysistype = "os", outputTable = TRUE)
  r_inv <- .val_run(data = d_a2, outcome = "num01", outcomeLevel = "0",  analysistype = "os", outputTable = TRUE)
  r_lgl <- .val_run(data = d_a2, outcome = "lgl",   outcomeLevel = "TRUE", analysistype = "os", outputTable = TRUE)

  expect_identical(r_def$myoutcome, c(0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L))
  expect_identical(r_def$out_df$count, c(4L, 6L))
  expect_identical(r_inv$myoutcome, c(1L, 0L, 0L, 1L, 0L, 1L, 1L, 0L, 0L, 0L))
  expect_identical(r_inv$out_df$count, c(6L, 4L))
  expect_identical(r_lgl$myoutcome, c(0L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L))
})

test_that("A3: 4-level multievent recoding across OS, Cause-Specific, Competing Risks, and Multistate", {
  d_a3 <- data.frame(
    status4 = factor(
      c(rep("DOD", 4), rep("DOOC", 2), rep("AWD", 3), rep("AWOD", 3)),
      levels = c("AWOD", "AWD", "DOD", "DOOC")
    )
  )
  r_os  <- .val_run(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "os",
                    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outputTable = TRUE)
  r_css <- .val_run(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "cause",
                    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outputTable = TRUE)
  r_cmp <- .val_run(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "compete",
                    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outputTable = TRUE)
  r_ms  <- .val_run(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "multistate",
                    dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outputTable = TRUE)

  expect_identical(r_os$myoutcome,  c(1L,1L,1L,1L, 1L,1L, 0L,0L,0L, 0L,0L,0L))
  expect_identical(r_os$out_df$count, c(6L, 6L))
  expect_identical(r_css$myoutcome, c(1L,1L,1L,1L, 0L,0L, 0L,0L,0L, 0L,0L,0L))
  expect_identical(r_css$out_df$count, c(8L, 4L))
  expect_identical(r_cmp$myoutcome, c(1L,1L,1L,1L, 2L,2L, 0L,0L,0L, 0L,0L,0L))
  expect_identical(r_cmp$out_df$count, c(6L, 4L, 2L))
  expect_identical(r_ms$myoutcome,  c(2L,2L,2L,2L, 3L,3L, 1L,1L,1L, 0L,0L,0L))
  expect_identical(r_ms$out_df$count, c(3L, 3L, 4L, 2L))
})

test_that("A4: RFS/PFS/DFS 3-valued logic and TTP progression-only recoding", {
  d_a4 <- data.frame(
    vital = factor(c("Alive","Alive","Alive","Alive","Dead","Dead","Dead", NA,   "Alive", NA),
                   levels = c("Alive", "Dead")),
    rec   = factor(c("No",   "No",   "Yes",  "Yes",  "No",  "Yes", NA,     "Yes","No",    "No"),
                   levels = c("No", "Yes"))
  )
  r_rfs <- .val_run(data = d_a4, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "rfs", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)
  r_pfs <- .val_run(data = d_a4, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "pfs", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)
  r_dfs <- .val_run(data = d_a4, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "dfs", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)
  r_ttp <- .val_run(data = d_a4, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "ttp", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)

  exp_rfs <- c(0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, NA_integer_)
  exp_ttp <- c(0L, 0L, 1L, 1L, 0L, 1L, NA_integer_, 1L, 0L, 0L)
  expect_identical(r_rfs$myoutcome, exp_rfs)
  expect_identical(r_pfs$myoutcome, exp_rfs)
  expect_identical(r_dfs$myoutcome, exp_rfs)
  expect_identical(r_rfs$out_df$count, c(3L, 6L))
  expect_identical(r_ttp$myoutcome, exp_ttp)
  expect_identical(r_ttp$out_df$count, c(5L, 4L))
})

test_that("A5: Administrative censoring resets post-cutoff events and truncates admin_time", {
  d_a5 <- data.frame(
    vital   = factor(c("Dead","Dead","Dead","Dead","Dead","Alive","Alive","Alive","Alive","Alive"),
                     levels = c("Alive","Dead")),
    fu_num  = c(12, 24, 36, 48, 60, 18, 30, 36, 50, 72),
    cut_num = rep(36, 10)
  )
  res <- .val_run(data = d_a5, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "os", adminCensoring = TRUE,
                  followupTime = "fu_num", adminDate = "cut_num", outputTable = TRUE)
  expect_identical(res$myoutcome, c(1L, 1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L))
  expect_identical(res$admin_time, c(12, 24, 36, 36, 36, 18, 30, 36, 36, 36))
  expect_identical(res$out_df$count, c(7L, 3L))
})

test_that("A6: Event hierarchy keeps earliest priority event and censors later rows", {
  d_a6 <- data.frame(
    pid    = c(1, 1, 1,  2, 2,  3, 3,  4, 4, 4),
    status = factor(c("AWD", "DOD", "AWOD",
                      "AWOD", "AWD",
                      "DOD", "AWD",
                      "AWOD", "DOOC", "DOD"),
                    levels = c("AWOD", "AWD", "DOD", "DOOC")),
    time   = c(5, 10, 15,  6, 12,  8, 14,  3, 9, 20)
  )
  r_time   <- .val_run(data = d_a6, outcome = "status", multievent = TRUE, analysistype = "os",
                       dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                       useHierarchy = TRUE, eventPriority = 1,
                       patientID = "pid", followupTime = "time", outputTable = TRUE)
  r_notime <- .val_run(data = d_a6, outcome = "status", multievent = TRUE, analysistype = "os",
                       dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                       useHierarchy = TRUE, eventPriority = 1,
                       patientID = "pid", followupTime = NULL, outputTable = TRUE)
  expect_identical(r_time$myoutcome,   c(0L, 1L, 0L,  0L, 0L,  1L, 0L,  0L, 1L, 0L))
  expect_identical(r_notime$myoutcome, c(1L, 1L, 1L,  0L, 0L,  1L, 1L,  1L, 1L, 1L))
})

# =============================================================================
# BATTERY B: Canonical Dataset & Downstream Handoff Parity
# =============================================================================

test_that("B1: Canonical survival::lung 1/2 coding matches survival::Surv(time, status == 2)", {
  skip_if_not_installed("survival")
  lung_df <- survival::lung
  lung_df$status_chr <- as.character(lung_df$status)
  res <- .val_run(data = lung_df, outcome = "status_chr", outcomeLevel = "2",
                  analysistype = "os", outputTable = TRUE)
  ora <- as.integer(lung_df$status == 2)
  expect_identical(res$myoutcome, ora)
  expect_identical(res$out_df$count, c(63L, 165L))
  expect_equal(res$out_df$percentage, c(27.6, 72.4), tolerance = 0)

  km_oo  <- survival::survfit(survival::Surv(lung_df$time, res$myoutcome) ~ 1)
  km_ref <- survival::survfit(survival::Surv(lung_df$time, lung_df$status == 2) ~ 1)
  expect_equal(km_oo$surv, km_ref$surv, tolerance = 1e-12)
})

test_that("B2: Canonical survival::colon two-row-per-patient RFS & TTP match hand-constructed endpoint", {
  skip_if_not_installed("survival")
  c_death <- subset(survival::colon, etype == 2)
  c_recur <- subset(survival::colon, etype == 1)
  c_merged <- merge(c_death[, c("id", "status", "time")],
                    c_recur[, c("id", "status", "time")],
                    by = "id", suffixes = c("_death", "_recur"))
  c_merged$vital_f <- factor(ifelse(c_merged$status_death == 1, "Dead", "Alive"), levels = c("Alive", "Dead"))
  c_merged$recur_f <- factor(ifelse(c_merged$status_recur == 1, "Relapsed", "Disease_Free"), levels = c("Disease_Free", "Relapsed"))

  r_rfs <- .val_run(data = c_merged, outcome = "vital_f", outcomeLevel = "Dead",
                    analysistype = "rfs", recurrence = "recur_f", recurrenceLevel = "Relapsed", outputTable = TRUE)
  r_ttp <- .val_run(data = c_merged, outcome = "vital_f", outcomeLevel = "Dead",
                    analysistype = "ttp", recurrence = "recur_f", recurrenceLevel = "Relapsed", outputTable = TRUE)

  expect_identical(r_rfs$myoutcome, as.integer(c_merged$status_death == 1 | c_merged$status_recur == 1))
  expect_identical(r_ttp$myoutcome, as.integer(c_merged$status_recur == 1))
  expect_identical(r_rfs$out_df$count, c(423L, 506L))
  expect_identical(r_ttp$out_df$count, c(461L, 468L))
})

test_that("B3: Competing risks 0/1/2 recoding matches cmprsk::cuminc CIF within 1e-12", {
  skip_if_not_installed("cmprsk")
  res <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                  multievent = TRUE, analysistype = "compete",
                  dod = "Dead_Disease", dooc = "Dead_Other",
                  awd = "Alive_Disease", awod = "Alive_NED", outputTable = TRUE)
  ora <- .val_oracle(outcomeorganizer_compete$outcome_status, multievent = TRUE, analysistype = "compete",
                     dod = "Dead_Disease", dooc = "Dead_Other", awd = "Alive_Disease", awod = "Alive_NED")
  expect_identical(res$myoutcome, ora$status)
  expect_identical(res$out_df$count, c(75L, 27L, 18L))

  ci_oo  <- cmprsk::cuminc(ftime = outcomeorganizer_compete$time, fstatus = res$myoutcome, cencode = 0)
  ci_ora <- cmprsk::cuminc(ftime = outcomeorganizer_compete$time, fstatus = ora$status, cencode = 0)
  expect_equal(ci_oo[["1 1"]]$est, ci_ora[["1 1"]]$est, tolerance = 1e-12)
  expect_equal(ci_oo[["1 2"]]$est, ci_ora[["1 2"]]$est, tolerance = 1e-12)
})

test_that("B4: Downstream .defineEventIndicator() handoff accepts recoded 0/1/2 and 0/1 outputs", {
  res_cmp <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                      multievent = TRUE, analysistype = "compete",
                      dod = "Dead_Disease", dooc = "Dead_Other",
                      awd = "Alive_Disease", awod = "Alive_NED")
  res_os  <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                      multievent = TRUE, analysistype = "os",
                      dod = "Dead_Disease", dooc = "Dead_Other",
                      awd = "Alive_Disease", awod = "Alive_NED")
  ds_cmp <- .defineEventIndicator(
    outcome = factor(res_cmp$myoutcome, levels = c(0, 1, 2), labels = c("Censored", "Disease_Death", "Other_Death")),
    multievent = TRUE, analysistype = "compete",
    dod = "Disease_Death", dooc = "Other_Death", awd = NULL, awod = "Censored"
  )
  ds_os <- .defineEventIndicator(
    outcome = res_os$myoutcome, multievent = FALSE, analysistype = "os", outcomeLevel = NULL
  )
  expect_identical(as.integer(ds_cmp$status), res_cmp$myoutcome)
  expect_identical(as.integer(ds_os$status), res_os$myoutcome)
})

# =============================================================================
# BATTERY C: Metamorphic Relations
# =============================================================================

test_that("C1: Row permutation equivariance preserves recoded vector and outputTable", {
  set.seed(20261025)
  perm <- sample(seq_len(nrow(outcomeorganizer_multievent)))
  r_orig <- .val_run(data = outcomeorganizer_multievent, outcome = "detailed_outcome",
                     multievent = TRUE, analysistype = "compete",
                     dod = "Dead_Disease", dooc = "Dead_Other_Causes",
                     awd = "Alive_With_Disease", awod = "Alive_Disease_Free", outputTable = TRUE)
  r_perm <- .val_run(data = outcomeorganizer_multievent[perm, , drop = FALSE], outcome = "detailed_outcome",
                     multievent = TRUE, analysistype = "compete",
                     dod = "Dead_Disease", dooc = "Dead_Other_Causes",
                     awd = "Alive_With_Disease", awod = "Alive_Disease_Free", outputTable = TRUE)
  expect_identical(r_orig$myoutcome[perm], r_perm$myoutcome)
  expect_identical(r_orig$out_df$count, r_perm$out_df$count)
  expect_equal(r_orig$out_df$percentage, r_perm$out_df$percentage, tolerance = 0)
})

test_that("C2: Row duplication (2x) doubles counts and keeps percentages invariant", {
  d_2x <- rbind(outcomeorganizer_compete, outcomeorganizer_compete)
  r_1x <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                   multievent = TRUE, analysistype = "compete",
                   dod = "Dead_Disease", dooc = "Dead_Other",
                   awd = "Alive_Disease", awod = "Alive_NED", outputTable = TRUE)
  r_2x <- .val_run(data = d_2x, outcome = "outcome_status",
                   multievent = TRUE, analysistype = "compete",
                   dod = "Dead_Disease", dooc = "Dead_Other",
                   awd = "Alive_Disease", awod = "Alive_NED", outputTable = TRUE)
  expect_identical(r_2x$out_df$count, 2L * r_1x$out_df$count)
  expect_equal(r_2x$out_df$percentage, r_1x$out_df$percentage, tolerance = 0)
})

test_that("C3: Bijective Unicode/Turkish/spaced variable and level renaming leaves recoding invariant", {
  d_c3 <- data.frame(
    check.names = FALSE,
    "Hasta Durumu (Sonuç)" = factor(
      ifelse(outcomeorganizer_basic$vital_status == "Dead",
             "Hastalıktan Ölü / Décédé (Gréade 3)",
             "Sağ / Überlebend (NED)"),
      levels = c("Hastalıktan Ölü / Décédé (Gréade 3)", "Sağ / Überlebend (NED)")
    )
  )
  r_base <- .val_run(data = outcomeorganizer_basic, outcome = "vital_status",
                     outcomeLevel = "Dead", analysistype = "os", outputTable = TRUE)
  r_uni  <- .val_run(data = d_c3, outcome = "Hasta Durumu (Sonuç)",
                     outcomeLevel = "Hastalıktan Ölü / Décédé (Gréade 3)",
                     analysistype = "os", outputTable = TRUE)
  expect_identical(r_uni$myoutcome, r_base$myoutcome)
  expect_identical(r_uni$out_df$count, r_base$out_df$count)
})

test_that("C4: Complement symmetry (OS) and competing-cause swap symmetry (1 <-> 2)", {
  r_dead  <- .val_run(data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Dead",
                      analysistype = "os", outputTable = TRUE)
  r_alive <- .val_run(data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Alive",
                      analysistype = "os", outputTable = TRUE)
  expect_identical(r_dead$myoutcome, 1L - r_alive$myoutcome)
  expect_identical(r_dead$out_df$count, rev(r_alive$out_df$count))

  r_c1 <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                   multievent = TRUE, analysistype = "compete",
                   dod = "Dead_Disease", dooc = "Dead_Other",
                   awd = "Alive_Disease", awod = "Alive_NED", outputTable = TRUE)
  r_c2 <- .val_run(data = outcomeorganizer_compete, outcome = "outcome_status",
                   multievent = TRUE, analysistype = "compete",
                   dod = "Dead_Other", dooc = "Dead_Disease",
                   awd = "Alive_Disease", awod = "Alive_NED", outputTable = TRUE)
  swapped <- ifelse(r_c1$myoutcome == 1L, 2L, ifelse(r_c1$myoutcome == 2L, 1L, 0L))
  expect_identical(r_c2$myoutcome, as.integer(swapped))
})

test_that("C5: Administrative censoring is equivariant under positive time rescaling (months -> days)", {
  d_m <- data.frame(
    vital = factor(c("Dead","Dead","Dead","Dead","Dead","Alive","Alive","Alive","Alive","Alive"),
                   levels = c("Alive","Dead")),
    fu_num  = c(12, 24, 36, 48, 60, 18, 30, 36, 50, 72),
    cut_num = rep(36, 10)
  )
  d_d <- d_m
  d_d$fu_num  <- d_m$fu_num * 30.4375
  d_d$cut_num <- d_m$cut_num * 30.4375
  r_m <- .val_run(data = d_m, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "os", adminCensoring = TRUE,
                  followupTime = "fu_num", adminDate = "cut_num", outputTable = TRUE)
  r_d <- .val_run(data = d_d, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "os", adminCensoring = TRUE,
                  followupTime = "fu_num", adminDate = "cut_num", outputTable = TRUE)
  expect_identical(r_m$myoutcome, r_d$myoutcome)
  expect_equal(r_d$admin_time, r_m$admin_time * 30.4375, tolerance = 1e-12)
})

test_that("C6: Adding irrelevant columns and all-NA rows leaves non-missing recoding invariant", {
  d_noise <- outcomeorganizer_basic
  d_noise$noise_num <- rnorm(nrow(d_noise))
  na_rows <- d_noise[1:3, ]
  na_rows$vital_status <- NA
  d_plus_na <- rbind(d_noise, na_rows)

  r_base <- .val_run(data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Dead",
                     analysistype = "os", outputTable = TRUE)
  r_na   <- .val_run(data = d_plus_na, outcome = "vital_status", outcomeLevel = "Dead",
                     analysistype = "os", outputTable = TRUE)
  expect_identical(r_na$out_df$count, r_base$out_df$count)
  expect_identical(r_na$myoutcome[seq_len(nrow(outcomeorganizer_basic))], r_base$myoutcome)
  expect_true(all(is.na(r_na$myoutcome[(nrow(outcomeorganizer_basic) + 1):nrow(d_plus_na)])))
})

# =============================================================================
# BATTERY D: Boundary Conditions & Guard Thresholds
# =============================================================================

test_that("D1: Degenerate inputs (outcome=NULL with patientID, 0-row data, all-NA outcome) halt cleanly", {
  r_todo <- run_outcomeorganizer(data = outcomeorganizer_basic, outcome = NULL, patientID = "patient_id")
  expect_match(.val_html(r_todo$todo), "Welcome to Enhanced Outcome Organizer", fixed = TRUE)
  expect_equal(r_todo$outputTable$rowCount, 0L)

  expect_error(
    run_outcomeorganizer(data = outcomeorganizer_basic[0, , drop = FALSE],
                         outcome = "vital_status", outcomeLevel = "Dead"),
    "Data contains no \\(complete\\) rows"
  )

  d_allna <- data.frame(vital = factor(rep(NA_character_, 15), levels = c("Alive", "Dead")))
  r_allna <- run_outcomeorganizer(data = d_allna, outcome = "vital", outcomeLevel = "Dead")
  expect_match(.val_html(r_allna$errors), "Outcome variable contains no non-missing values", fixed = TRUE)
})

test_that("D2: Numeric non-0/1 (1/2) without outcomeLevel rejects; with outcomeLevel='2' recodes cleanly", {
  d_12 <- data.frame(status12 = c(rep(1, 15), rep(2, 15)))
  expect_error(
    run_outcomeorganizer(data = d_12, outcome = "status12", outcomeLevel = NULL),
    "is numeric but not coded 0/1"
  )
  r_ok <- .val_run(data = d_12, outcome = "status12", outcomeLevel = "2", outputTable = TRUE)
  expect_identical(r_ok$out_df$count, c(15L, 15L))
})

test_that("D3: Sample size thresholds fire at exact n = 9, 10, 29, 30 boundaries", {
  mk_n <- function(n) {
    k <- max(5L, n %/% 2L)
    data.frame(vital = factor(c(rep("Dead", k), rep("Alive", n - k)), levels = c("Alive", "Dead")))
  }
  r9  <- run_outcomeorganizer(data = mk_n(9),  outcome = "vital", outcomeLevel = "Dead")
  r10 <- run_outcomeorganizer(data = mk_n(10), outcome = "vital", outcomeLevel = "Dead")
  r29 <- run_outcomeorganizer(data = mk_n(29), outcome = "vital", outcomeLevel = "Dead")
  r30 <- run_outcomeorganizer(data = mk_n(30), outcome = "vital", outcomeLevel = "Dead")

  expect_match(.val_html(r9$strongWarnings), "Very small sample size: 9 observations", fixed = TRUE)
  expect_false(grepl("Very small sample size", .val_html(r10$strongWarnings), fixed = TRUE))
  expect_match(.val_html(r10$warnings), "Small sample size: 10 observations", fixed = TRUE)
  expect_match(.val_html(r29$warnings), "Small sample size: 29 observations", fixed = TRUE)
  expect_false(grepl("Small sample size", .val_html(r30$warnings), fixed = TRUE))
})

test_that("D4: Missing outcome proportion thresholds fire at 10% (info), 13.3% (warning), and 53.3% (strongWarning)", {
  d10 <- data.frame(vital = factor(c(rep("Dead", 13), rep("Alive", 14), rep(NA, 3)), levels = c("Alive", "Dead")))
  d13 <- data.frame(vital = factor(c(rep("Dead", 13), rep("Alive", 13), rep(NA, 4)), levels = c("Alive", "Dead")))
  d53 <- data.frame(vital = factor(c(rep("DOD", 7), rep("AWOD", 7), rep(NA, 16)), levels = c("AWOD", "DOD")))

  r10 <- run_outcomeorganizer(data = d10, outcome = "vital", outcomeLevel = "Dead")
  r13 <- run_outcomeorganizer(data = d13, outcome = "vital", outcomeLevel = "Dead")
  r53 <- run_outcomeorganizer(data = d53, outcome = "vital", multievent = TRUE,
                              dod = "DOD", awod = "AWOD", analysistype = "os")

  expect_match(.val_html(r10$infoMessages), "Missing outcome data: 10% (3 out of 30 rows)", fixed = TRUE)
  expect_false(grepl("Large amount of missing outcome data", .val_html(r10$warnings), fixed = TRUE))
  expect_match(.val_html(r13$warnings), "Large amount of missing outcome data: 13.3% (4 out of 30 rows)", fixed = TRUE)
  expect_match(.val_html(r53$strongWarnings), "Most rows have no usable outcome", fixed = TRUE)
})

test_that("D5: Few-events warning fires at 4 events and clears at 5 events when outcomeLevel is specified", {
  d4 <- data.frame(vital = factor(c(rep("Dead", 4), rep("Alive", 26)), levels = c("Alive", "Dead")))
  d5 <- data.frame(vital = factor(c(rep("Dead", 5), rep("Alive", 25)), levels = c("Alive", "Dead")))
  r4 <- run_outcomeorganizer(data = d4, outcome = "vital", outcomeLevel = "Dead")
  r5 <- run_outcomeorganizer(data = d5, outcome = "vital", outcomeLevel = "Dead")
  expect_match(.val_html(r4$warnings), "Few events: only 4 of 30 non-missing outcomes", fixed = TRUE)
  expect_false(grepl("Few events", .val_html(r5$warnings), fixed = TRUE))
})

test_that("D6: Multi-event partition & RFS+multievent guards reject invalid configurations", {
  d_a3 <- data.frame(
    status4 = factor(c(rep("DOD", 4), rep("DOOC", 2), rep("AWD", 3), rep("AWOD", 3)),
                     levels = c("AWOD", "AWD", "DOD", "DOOC"))
  )
  expect_error(
    run_outcomeorganizer(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "os",
                         dod = "DOD", dooc = NULL, awd = "AWD", awod = "AWOD"),
    "Outcome level\\(s\\) not assigned to any category: DOOC"
  )
  expect_error(
    run_outcomeorganizer(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "compete",
                         dod = "DOD", dooc = "DOD", awd = "AWD", awod = "AWOD"),
    "Assigned to more than one: DOD"
  )
  expect_error(
    run_outcomeorganizer(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "multistate",
                         dod = "DOD", dooc = "DOD", awd = "AWD", awod = "AWOD"),
    "Assigned to more than one: DOD"
  )
  expect_error(
    run_outcomeorganizer(data = d_a3, outcome = "status4", multievent = TRUE, analysistype = "rfs",
                         dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD"),
    "not available with Multiple Event Levels enabled"
  )
})

test_that("D7: Interval censoring guards detect missing endpoint, numeric L>R, Date L>R, and scale mismatch", {
  d_int <- data.frame(
    vital   = factor(rep(c("Alive", "Dead"), each = 15), levels = c("Alive", "Dead")),
    L_ok    = seq(1, 30),
    R_ok    = seq(2, 31),
    R_bad   = c(0, seq(2, 30)),
    D_L     = as.Date("2020-01-01") + seq(1, 30),
    D_R_bad = as.Date("2020-01-01") + c(0, seq(2, 30))
  )
  r_miss  <- .val_run(data = d_int, outcome = "vital", outcomeLevel = "Dead",
                      intervalCensoring = TRUE, intervalStart = "L_ok", intervalEnd = NULL)
  r_num   <- .val_run(data = d_int, outcome = "vital", outcomeLevel = "Dead",
                      intervalCensoring = TRUE, intervalStart = "L_ok", intervalEnd = "R_bad")
  r_date  <- .val_run(data = d_int, outcome = "vital", outcomeLevel = "Dead",
                      intervalCensoring = TRUE, intervalStart = "D_L", intervalEnd = "D_R_bad")
  r_scale <- .val_run(data = d_int, outcome = "vital", outcomeLevel = "Dead",
                      intervalCensoring = TRUE, intervalStart = "L_ok", intervalEnd = "D_L")
  r_ok    <- .val_run(data = d_int, outcome = "vital", outcomeLevel = "Dead",
                      intervalCensoring = TRUE, intervalStart = "L_ok", intervalEnd = "R_ok", outputTable = TRUE)

  expect_match(.val_html(r_miss$results$warnings),
               "Interval censoring is switched on but needs BOTH an Interval Start and an Interval End variable",
               fixed = TRUE)
  expect_false(isTRUE(r_num$org$applied$interval))
  expect_match(.val_html(r_num$results$warnings), "1 row(s) have an interval start later than their interval end", fixed = TRUE)
  expect_false(isTRUE(r_date$org$applied$interval))
  expect_match(.val_html(r_date$results$warnings), "1 row(s) have an interval start later than their interval end", fixed = TRUE)
  expect_false(isTRUE(r_scale$org$applied$interval))
  expect_match(.val_html(r_scale$results$warnings), "Interval endpoints not on the same scale", fixed = TRUE)
  expect_true(isTRUE(r_ok$org$applied$interval))
  expect_identical(r_ok$out_df$count, c(15L, 15L))
})

test_that("D8: Administrative censoring guards catch all 8 misuse patterns", {
  d_adm <- data.frame(
    vital    = factor(rep(c("Dead", "Alive"), each = 15), levels = c("Alive", "Dead")),
    fu       = c(seq(10, 150, by = 10), seq(15, 155, by = 10)),
    cut_wipe = rep(5, 30),
    cut_w_ev = c(rep(5, 15), rep(200, 15)),
    cut_ser  = rep(45000, 30),
    cut_date = rep(as.Date("2025-01-01"), 30),
    fu_neg0  = c(-5, 0, seq(20, 140, by = 10), seq(15, 155, by = 10)),
    cut_part = c(rep(60, 25), rep(NA, 5))
  )
  d_ms <- data.frame(
    status4 = factor(c(rep("DOD", 4), rep("DOOC", 2), rep("AWD", 3), rep("AWOD", 3)),
                     levels = c("AWOD", "AWD", "DOD", "DOOC")),
    fu_num  = seq_len(12),
    cut_num = rep(6, 12)
  )
  r_miss  <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu", adminDate = NULL)
  r_scale <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu", adminDate = "cut_date")
  r_ms    <- .val_run(data = d_ms, outcome = "status4", multievent = TRUE, analysistype = "multistate",
                      dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                      adminCensoring = TRUE, followupTime = "fu_num", adminDate = "cut_num", outputTable = TRUE)
  r_all   <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu", adminDate = "cut_wipe")
  r_wev   <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu", adminDate = "cut_w_ev")
  r_ser   <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu", adminDate = "cut_ser")
  r_neg0  <- .val_run(data = d_adm, outcome = "vital", outcomeLevel = "Dead",
                      adminCensoring = TRUE, followupTime = "fu_neg0", adminDate = "cut_part")

  expect_false(isTRUE(r_miss$org$applied$admin))
  expect_match(.val_html(r_miss$results$warnings), "no cut-off date variable is selected", fixed = TRUE)
  expect_false(isTRUE(r_scale$org$applied$admin))
  expect_match(.val_html(r_scale$results$warnings), "the follow-up time is a number and the cut-off is a date", fixed = TRUE)
  expect_false(isTRUE(r_ms$org$applied$admin))
  expect_match(.val_html(r_ms$results$warnings), "multistate coding has no censored code", fixed = TRUE)
  expect_identical(r_ms$out_df$count, c(3L, 3L, 4L, 2L))
  expect_false(isTRUE(r_all$org$applied$admin))
  expect_match(.val_html(r_all$results$strongWarnings), "every one of the 15 events would be reset to censored", fixed = TRUE)
  expect_false(isTRUE(r_wev$org$applied$admin))
  expect_match(.val_html(r_wev$results$strongWarnings), "every one of the 15 events would be reset to censored", fixed = TRUE)
  expect_false(isTRUE(r_ser$org$applied$admin))
  expect_match(.val_html(r_ser$results$strongWarnings), "Administrative censoring had no effect", fixed = TRUE)
  expect_match(.val_html(r_neg0$results$warnings), "1 patient(s) have a NEGATIVE follow-up time", fixed = TRUE)
  expect_match(.val_html(r_neg0$results$warnings), "1 patient(s) have a follow-up time of zero", fixed = TRUE)
  expect_match(.val_html(r_neg0$results$warnings), "5 patient(s) were left untouched", fixed = TRUE)
})

test_that("D9: Non-contiguous filtered row names are preserved through .prepareData()", {
  d_base <- outcomeorganizer_admin_censor
  d_base$cut_months <- 18
  d_sub <- d_base[c(2, 5, 8, 11, 14, 17, 20, 23, 26, 29, 32, 35), ]
  res <- .val_run(data = d_sub, outcome = "vital_status", outcomeLevel = "Dead",
                  analysistype = "os", adminCensoring = TRUE,
                  followupTime = "survival_months", adminDate = "cut_months", outputTable = TRUE)
  expect_identical(rownames(res$org$df_outcome), rownames(d_sub))
  expect_identical(length(res$myoutcome), nrow(d_sub))
  expect_identical(length(res$admin_time), nrow(d_sub))
})

# =============================================================================
# BATTERY E: Option Coverage & High-Risk Option Interactions
# =============================================================================

test_that("E1: All 8 analysistype modes match independent oracle on bundled datasets", {
  r_os  <- .val_run(data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Dead", analysistype = "os")
  r_css <- .val_run(data = outcomeorganizer_multievent, outcome = "detailed_outcome", multievent = TRUE, analysistype = "cause",
                    dod = "Dead_Disease", dooc = "Dead_Other_Causes", awd = "Alive_With_Disease", awod = "Alive_Disease_Free")
  r_cmp <- .val_run(data = outcomeorganizer_multievent, outcome = "detailed_outcome", multievent = TRUE, analysistype = "compete",
                    dod = "Dead_Disease", dooc = "Dead_Other_Causes", awd = "Alive_With_Disease", awod = "Alive_Disease_Free")
  r_rfs <- .val_run(data = outcomeorganizer_rfs, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "rfs", recurrence = "recurrence", recurrenceLevel = "Yes")
  r_pfs <- .val_run(data = outcomeorganizer_pfs, outcome = "vital_status", outcomeLevel = "Dead",
                    analysistype = "pfs", recurrence = "progression", recurrenceLevel = "Yes")
  r_dfs <- .val_run(data = outcomeorganizer_rfs, outcome = "vital", outcomeLevel = "Dead",
                    analysistype = "dfs", recurrence = "recurrence", recurrenceLevel = "Yes")
  r_ttp <- .val_run(data = outcomeorganizer_pfs, outcome = "vital_status", outcomeLevel = "Dead",
                    analysistype = "ttp", recurrence = "progression", recurrenceLevel = "Yes")
  r_ms  <- .val_run(data = outcomeorganizer_multistate, outcome = "current_state", multievent = TRUE, analysistype = "multistate",
                    dod = "Dead", dooc = "Metastatic", awd = "Local_Recurrence", awod = "Disease_Free")

  expect_identical(r_os$myoutcome,  .val_oracle(outcomeorganizer_basic$vital_status, analysistype = "os", outcomeLevel = "Dead")$status)
  expect_identical(r_css$myoutcome, .val_oracle(outcomeorganizer_multievent$detailed_outcome, multievent = TRUE, analysistype = "cause",
                                                dod = "Dead_Disease", dooc = "Dead_Other_Causes", awd = "Alive_With_Disease", awod = "Alive_Disease_Free")$status)
  expect_identical(r_cmp$myoutcome, .val_oracle(outcomeorganizer_multievent$detailed_outcome, multievent = TRUE, analysistype = "compete",
                                                dod = "Dead_Disease", dooc = "Dead_Other_Causes", awd = "Alive_With_Disease", awod = "Alive_Disease_Free")$status)
  expect_identical(r_rfs$myoutcome, .val_oracle(outcomeorganizer_rfs$vital, analysistype = "rfs", outcomeLevel = "Dead",
                                                recurrence = outcomeorganizer_rfs$recurrence, recurrenceLevel = "Yes")$status)
  expect_identical(r_pfs$myoutcome, .val_oracle(outcomeorganizer_pfs$vital_status, analysistype = "pfs", outcomeLevel = "Dead",
                                                recurrence = outcomeorganizer_pfs$progression, recurrenceLevel = "Yes")$status)
  expect_identical(r_dfs$myoutcome, r_rfs$myoutcome)
  expect_identical(r_ttp$myoutcome, .val_oracle(outcomeorganizer_pfs$vital_status, analysistype = "ttp", outcomeLevel = "Dead",
                                                recurrence = outcomeorganizer_pfs$progression, recurrenceLevel = "Yes")$status)
  expect_identical(r_ms$myoutcome,  .val_oracle(outcomeorganizer_multistate$current_state, multievent = TRUE, analysistype = "multistate",
                                                dod = "Dead", dooc = "Metastatic", awd = "Local_Recurrence", awod = "Disease_Free")$status)
})

test_that("E2: Competing risks + administrative censoring resets both code 1 and code 2 beyond cut-off to 0", {
  d_e2 <- data.frame(
    status = factor(c("DOD","DOD","DOOC","DOOC","AWD","AWOD","DOD","DOOC","AWOD","AWD"),
                    levels = c("AWOD","AWD","DOD","DOOC")),
    fu     = c(10, 80, 20, 90, 30, 40, 50, 55, 70, 85),
    cut    = rep(60, 10)
  )
  res <- .val_run(data = d_e2, outcome = "status", multievent = TRUE, analysistype = "compete",
                  dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
                  adminCensoring = TRUE, followupTime = "fu", adminDate = "cut", outputTable = TRUE)
  expect_identical(res$myoutcome, c(1L, 0L, 2L, 0L, 0L, 0L, 1L, 2L, 0L, 0L))
  expect_identical(res$out_df$count, c(6L, 2L, 2L))
  expect_identical(res$admin_time, c(10, 60, 20, 60, 30, 40, 50, 55, 60, 60))
})

test_that("E3: All 5 conditional visibility toggles control result visibility and populate content", {
  r_off <- run_outcomeorganizer(
    data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Dead",
    outputTable = FALSE, diagnostics = FALSE, visualization = FALSE,
    showNaturalSummary = FALSE, showGlossary = FALSE
  )
  r_on <- run_outcomeorganizer(
    data = outcomeorganizer_basic, outcome = "vital_status", outcomeLevel = "Dead",
    outputTable = TRUE, diagnostics = TRUE, visualization = TRUE,
    showNaturalSummary = TRUE, showGlossary = TRUE
  )
  expect_false(r_off$outputTable$visible)
  expect_false(r_off$diagnosticsTable$visible)
  expect_false(r_off$outcomeViz$visible)
  expect_false(r_off$naturalSummary$visible)
  expect_false(r_off$glossary$visible)

  expect_true(r_on$outputTable$visible)
  expect_true(r_on$diagnosticsTable$visible)
  expect_true(r_on$outcomeViz$visible)
  expect_true(r_on$naturalSummary$visible)
  expect_true(r_on$glossary$visible)
  expect_gte(r_on$diagnosticsTable$rowCount, 1L)
  expect_true(nzchar(.val_html(r_on$naturalSummary)))
  expect_true(nzchar(.val_html(r_on$glossary)))
})

# =============================================================================
# BATTERY F: Monte Carlo Simulation Agreement across 5 Families
# =============================================================================

test_that("F1 & F2: Monte Carlo simulated cohorts across all 5 recoding families match independent oracle", {
  set.seed(20261025)
  families <- rep(c("os_bin", "compete_me", "multistate_me", "rfs_comp", "os_admin"), each = 10L)
  for (fam in families) {
    n_row <- sample(35L:65L, 1L)
    if (fam == "os_bin") {
      raw <- factor(sample(c("Alive", "Dead"), n_row, replace = TRUE), levels = c("Alive", "Dead"))
      raw[1:5] <- "Dead"; raw[6:10] <- "Alive"
      res <- .val_run(data = data.frame(y = raw), outcome = "y", outcomeLevel = "Dead", analysistype = "os", outputTable = TRUE)
      ora <- .val_oracle(raw, analysistype = "os", outcomeLevel = "Dead")
    } else if (fam == "compete_me") {
      lvls <- c("AWOD", "AWD", "DOD", "DOOC")
      raw <- factor(sample(lvls, n_row, replace = TRUE), levels = lvls)
      raw[1:4] <- lvls
      res <- .val_run(data = data.frame(y = raw), outcome = "y", multievent = TRUE, analysistype = "compete",
                      dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD", outputTable = TRUE)
      ora <- .val_oracle(raw, multievent = TRUE, analysistype = "compete",
                         dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD")
    } else if (fam == "multistate_me") {
      lvls <- c("Remission", "Relapsed", "Dead_Ca", "Dead_Oth")
      raw <- factor(sample(lvls, n_row, replace = TRUE), levels = lvls)
      raw[1:4] <- lvls
      res <- .val_run(data = data.frame(y = raw), outcome = "y", multievent = TRUE, analysistype = "multistate",
                      dod = "Dead_Ca", dooc = "Dead_Oth", awd = "Relapsed", awod = "Remission", outputTable = TRUE)
      ora <- .val_oracle(raw, multievent = TRUE, analysistype = "multistate",
                         dod = "Dead_Ca", dooc = "Dead_Oth", awd = "Relapsed", awod = "Remission")
    } else if (fam == "rfs_comp") {
      vital <- factor(sample(c("Alive", "Dead"), n_row, replace = TRUE), levels = c("Alive", "Dead"))
      rec   <- factor(sample(c("No", "Yes"), n_row, replace = TRUE), levels = c("No", "Yes"))
      vital[1:4] <- c("Alive", "Dead", "Alive", "Dead")
      rec[1:4]   <- c("No", "No", "Yes", "Yes")
      res <- .val_run(data = data.frame(vital = vital, rec = rec), outcome = "vital", outcomeLevel = "Dead",
                      analysistype = "rfs", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)
      ora <- .val_oracle(vital, analysistype = "rfs", outcomeLevel = "Dead",
                         recurrence = rec, recurrenceLevel = "Yes")
    } else if (fam == "os_admin") {
      vital <- factor(sample(c("Alive", "Dead"), n_row, replace = TRUE), levels = c("Alive", "Dead"))
      fu    <- round(runif(n_row, min = 5, max = 100), 2)
      cut   <- rep(60, n_row)
      vital[1:3] <- "Dead"; fu[1:3] <- c(10, 20, 30)
      vital[4:5] <- "Dead"; fu[4:5] <- c(75, 90)
      res <- .val_run(data = data.frame(vital = vital, fu = fu, cut = cut), outcome = "vital", outcomeLevel = "Dead",
                      analysistype = "os", adminCensoring = TRUE, followupTime = "fu", adminDate = "cut", outputTable = TRUE)
      ora <- .val_oracle(vital, analysistype = "os", outcomeLevel = "Dead",
                         adminCensoring = TRUE, followupTime = fu, adminDate = cut)
      expect_identical(res$admin_time, ora$admin_time)
    }
    expect_identical(res$myoutcome, ora$status)
    expect_identical(res$out_df$count, ora$summary$count)
    expect_equal(res$out_df$percentage, ora$summary$percentage, tolerance = 0)
  }
})

# =============================================================================
# BATTERY G, H, I, J: Cross-Output Consistency, HTML Escaping, Plot, Citations
# =============================================================================

test_that("G1: Cross-output conservation holds across outputTable, outcomeViz, naturalSummary, and eventRecodeInfo", {
  res <- .val_run(
    data = outcomeorganizer_compete, outcome = "outcome_status",
    multievent = TRUE, analysistype = "compete",
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED",
    outputTable = TRUE, diagnostics = TRUE, visualization = TRUE, showNaturalSummary = TRUE
  )
  tbl   <- res$out_df
  state <- res$results$outcomeViz$state$table
  nat   <- .val_html(res$results$naturalSummary)
  rec   <- .val_html(res$results$eventRecodeInfo)

  expect_identical(as.character(tbl$outcome), names(state))
  expect_identical(tbl$count, as.integer(state))
  expect_identical(sum(tbl$count), sum(!is.na(res$myoutcome)))
  expect_lte(abs(sum(tbl$percentage) - 100), 0.2)
  expect_match(nat, sprintf("out of %d coded records", sum(tbl$count)), fixed = TRUE)
  expect_match(rec, sprintf("<td align='right'>%d</td>", tbl$count[tbl$outcome == "1"]), fixed = TRUE)
  expect_match(rec, sprintf("<td align='right'>%d</td>", tbl$count[tbl$outcome == "2"]), fixed = TRUE)
  expect_match(rec, sprintf("<td align='right'>%d</td>", tbl$count[tbl$outcome == "0"]), fixed = TRUE)
})

test_that("H1: Malicious HTML/XSS payloads in variable names and factor levels are escaped across all Html outputs", {
  d_h1 <- data.frame(
    check.names = FALSE,
    "<script>alert(1)</script>" = factor(
      c(rep("<img src=x onerror=alert(2)>", 15), rep("<b>Alive & Well</b>", 15)),
      levels = c("<b>Alive & Well</b>", "<img src=x onerror=alert(2)>")
    )
  )
  res <- run_outcomeorganizer(
    data = d_h1, outcome = "<script>alert(1)</script>",
    outcomeLevel = "<img src=x onerror=alert(2)>",
    analysistype = "os", outputTable = TRUE, diagnostics = TRUE,
    showNaturalSummary = TRUE, showGlossary = TRUE
  )
  all_html <- paste(
    .val_html(res$eventRecodeInfo), .val_html(res$errors),
    .val_html(res$strongWarnings), .val_html(res$warnings),
    .val_html(res$infoMessages), .val_html(res$summary),
    .val_html(res$naturalSummary), .val_html(res$glossary),
    collapse = "\n"
  )
  expect_false(grepl("<script>alert(1)</script>", all_html, fixed = TRUE))
  expect_false(grepl("<img src=x onerror=alert(2)>", all_html, fixed = TRUE))
  expect_match(all_html, "&lt;script&gt;alert(1)&lt;/script&gt;", fixed = TRUE)
  expect_match(all_html, "&lt;img src=x onerror=alert(2)&gt;", fixed = TRUE)
  expect_false(grepl("\\b(NaN|Inf|-Inf|NULL)\\b", all_html))
})

test_that("I1: outcomeViz renders valid PNGs and redraws from image$state alone when private$.data is NULL", {
  a_plot <- run_outcomeorganizer_obj(
    data = outcomeorganizer_compete, outcome = "outcome_status",
    multievent = TRUE, analysistype = "compete",
    dod = "Dead_Disease", dooc = "Dead_Other",
    awd = "Alive_Disease", awod = "Alive_NED",
    outputTable = TRUE, visualization = TRUE
  )
  p1 <- tempfile(fileext = ".png")
  p2 <- tempfile(fileext = ".png")
  a_plot$results$outcomeViz$saveAs(p1)
  expect_true(file.exists(p1) && file.info(p1)$size > 1000)

  a_plot$.__enclos_env__$private$.data <- NULL
  a_plot$results$outcomeViz$saveAs(p2)
  expect_true(file.exists(p2) && file.info(p2)$size > 1000)
})

test_that("J1: Methodological citations AustinLeeFine2016 and AalenJohansen1978 resolve in 00refs.yaml", {
  skip_if_not_installed("yaml")
  refs_yaml <- yaml::read_yaml(file.path(.val_root, "jamovi/00refs.yaml"))
  expect_equal(as.integer(refs_yaml$refs$AustinLeeFine2016$year), 2016L)
  expect_match(refs_yaml$refs$AustinLeeFine2016$doi, "10.1161/CIRCULATIONAHA.115.017719", fixed = TRUE)
  expect_equal(as.integer(refs_yaml$refs$AalenJohansen1978$year), 1978L)
})

# =============================================================================
# OPEN DEFECTS (VAL-outcomeorganizer-01 .. VAL-outcomeorganizer-09)
# Written with the CORRECT expectation and guarded by skip() per Phase 6 of
# .claude/commands/validate-function.md. Remove the skip() when each fix lands.
# =============================================================================

test_that("VAL-outcomeorganizer-01: Unset ('') or non-matching recurrenceLevel in rfs/pfs/dfs/ttp halts instead of silently erasing events", {
  d_val01 <- data.frame(
    vital = factor(c("Alive","Alive","Alive","Alive","Alive","Dead","Dead","Dead","Dead","Dead"),
                   levels = c("Alive","Dead")),
    prog  = factor(c("Yes","Yes","No","No","No","Yes","Yes","No","No","No"),
                   levels = c("No","Yes"))
  )
  res_ttp <- .val_run(data = d_val01, outcome = "vital", outcomeLevel = "Dead",
                      analysistype = "ttp", recurrence = "prog", recurrenceLevel = "",
                      outputTable = TRUE)
  res_pfs <- .val_run(data = d_val01, outcome = "vital", outcomeLevel = "Dead",
                      analysistype = "pfs", recurrence = "prog", recurrenceLevel = "Relapse",
                      outputTable = TRUE)
  expect_equal(nrow(res_ttp$out_df), 0L)
  expect_equal(nrow(res_pfs$out_df), 0L)
})

test_that("VAL-outcomeorganizer-02: eventRecodeInfo updates after administrative censoring or event hierarchy modifies myoutcome", {
  d_a5 <- data.frame(
    vital   = factor(c("Dead","Dead","Dead","Dead","Dead","Alive","Alive","Alive","Alive","Alive"),
                     levels = c("Alive","Dead")),
    fu_num  = c(12, 24, 36, 48, 60, 18, 30, 36, 50, 72),
    cut_num = rep(36, 10)
  )
  res <- .val_run(data = d_a5, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "os", adminCensoring = TRUE,
                  followupTime = "fu_num", adminDate = "cut_num", outputTable = TRUE)
  rec_html <- .val_html(res$results$eventRecodeInfo)
  expect_match(rec_html, "Event level</td><td>\"Dead\"</td><td align='right'>3</td>", fixed = TRUE)
  expect_match(rec_html, "Censored</td><td>\"Alive\"</td><td align='right'>7</td>", fixed = TRUE)
})

test_that("VAL-outcomeorganizer-03: Input column named 'myoutcome' is not overwritten before recurrence/secondary variable read", {
  d_val03 <- data.frame(
    vital     = factor(c("Alive","Alive","Alive","Alive","Alive","Dead","Dead","Dead","Dead","Dead"),
                       levels = c("Alive","Dead")),
    myoutcome = factor(c("Yes","Yes","No","No","No","Yes","Yes","No","No","No"),
                       levels = c("No","Yes"))
  )
  res <- .val_run(data = d_val03, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "pfs", recurrence = "myoutcome", recurrenceLevel = "Yes",
                  outputTable = TRUE)
  expect_identical(res$out_df$count, c(3L, 7L))
})

test_that("VAL-outcomeorganizer-04: Zero-death cohorts (all 'Alive' factor with Dead level or RFS with recurrences) recode without error", {
  d_rfs0 <- data.frame(
    vital = factor(rep("Alive", 20), levels = c("Alive", "Dead")),
    rec   = factor(c(rep("Yes", 6), rep("No", 14)), levels = c("No", "Yes"))
  )
  res <- .val_run(data = d_rfs0, outcome = "vital", outcomeLevel = "Dead",
                  analysistype = "rfs", recurrence = "rec", recurrenceLevel = "Yes", outputTable = TRUE)
  expect_identical(res$out_df$count, c(14L, 6L))
})

test_that("VAL-outcomeorganizer-05: Factor with explicit NA level (addNA) is treated as missing in multievent and single-event modes", {
  d_val05 <- data.frame(
    vital_na = addNA(factor(c(rep("Alive", 10), rep("Dead", 10), rep(NA, 5)), levels = c("Alive", "Dead")))
  )
  res_me <- .val_run(data = d_val05, outcome = "vital_na", multievent = TRUE,
                     dod = "Dead", awod = "Alive", analysistype = "os", outputTable = TRUE)
  res_se <- .val_run(data = d_val05, outcome = "vital_na", multievent = FALSE,
                     outcomeLevel = "Dead", analysistype = "os", outputTable = TRUE)
  expect_identical(res_me$out_df$count, c(10L, 10L))
  expect_false(grepl("\"NA\"", .val_html(res_se$results$eventRecodeInfo), fixed = TRUE))
  expect_match(.val_html(res_se$results$warnings), "Large amount of missing outcome data", fixed = TRUE)
})

test_that("VAL-outcomeorganizer-06: Event hierarchy keeps 1 event per patient when priority and non-priority rows tie at earliest time", {
  d_val06 <- data.frame(
    pid    = c(1, 1,  2, 2,  3, 3,  4, 4,  5, 5),
    status = factor(c("Dead","Alive",  "Alive","Dead",  "Alive","Alive",  "Dead","Dead",  "Alive","Dead"),
                    levels = c("Alive", "Dead")),
    time   = c(10, 10,  5, 10,  8, 12,  6, 15,  4, 9)
  )
  res <- .val_run(data = d_val06, outcome = "status", multievent = TRUE,
                  dod = "Dead", awod = "Alive", analysistype = "os",
                  useHierarchy = TRUE, eventPriority = 1,
                  patientID = "pid", followupTime = "time", outputTable = TRUE)
  expect_identical(res$out_df$count, c(6L, 4L))
})

test_that("VAL-outcomeorganizer-07: Initial state with outcome = NULL and no other Variable options shows todo welcome without crashing", {
  res <- run_outcomeorganizer(data = outcomeorganizer_basic, outcome = NULL)
  expect_match(.val_html(res$todo), "Welcome to Enhanced Outcome Organizer", fixed = TRUE)
})

test_that("VAL-outcomeorganizer-08: Few-events (<5) warning checks post-recode event count rather than raw min(table(outcome))", {
  d_fp <- data.frame(num01 = c(rep(0, 3), rep(1, 17)))
  d_fn <- data.frame(status3 = factor(c(rep("DOD", 2), rep("AWD", 10), rep("AWOD", 18)),
                                      levels = c("AWOD", "AWD", "DOD")))
  r_fp <- run_outcomeorganizer(data = d_fp, outcome = "num01", outcomeLevel = NULL)
  r_fn <- run_outcomeorganizer(data = d_fn, outcome = "status3", multievent = TRUE,
                               dod = "DOD", awd = "AWD", awod = "AWOD", analysistype = "os")
  expect_false(grepl("Few events", .val_html(r_fp$warnings), fixed = TRUE))
  expect_match(.val_html(r_fn$warnings), "Few events", fixed = TRUE)
})

test_that("VAL-outcomeorganizer-09: outcomeorganizer.r.yaml declares only used package refs", {
  skip_if_not_installed("yaml")
  r_yaml <- yaml::read_yaml(file.path(.val_root, "jamovi/outcomeorganizer.r.yaml"))
  b_src  <- paste(readLines(file.path(.val_root, "R/outcomeorganizer.b.R"), warn = FALSE), collapse = "\n")
  unused <- Filter(function(ref) ref %in% c("survminer", "finalfit") && !grepl(ref, b_src, fixed = TRUE), r_yaml$refs)
  expect_length(unused, 0L)
})
