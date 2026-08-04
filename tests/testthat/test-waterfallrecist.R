# Tests for waterfallrecist (RECIST v1.1 compliant response analysis).
#
# Checked against RECIST v1.1 as published (Eisenhauer et al., Eur J Cancer
# 2009;45:228-247), not against the module's own comments. The previous version
# of this file was a generated stub whose synthetic data could not work: it
# sampled lesionType and isNewLesion as "A"/"B" (the backend requires
# "Target"/"Non-Target" and a logical), never produced a visit at the baseline
# timepoint, and asserted class "jmvcoreClass". It has been replaced.

# Build lesion-level data: one row per lesion per visit, with the per-visit
# target-lesion SUM split evenly across `n_lesions`.
recist_data <- function(patient, sums, visits = c(0, 6, 12), n_lesions = 2,
                        type = "Target", new_at = NA_real_) {
  do.call(rbind, lapply(seq_along(sums), function(i) {
    data.frame(
      patientID   = patient,
      lesionID    = paste0("L", seq_len(n_lesions)),
      visitTime   = visits[i],
      lesionType  = type,
      location    = rep(c("Liver", "Lung", "Node"), length.out = n_lesions),
      diameter    = rep(sums[i] / n_lesions, n_lesions),
      isNewLesion = !is.na(new_at) & visits[i] >= new_at,
      stringsAsFactors = FALSE
    )
  }))
}

wr_private <- function(df, ...) {
  defaults <- list(patientID = "patientID", lesionID = "lesionID",
                   visitTime = "visitTime", lesionType = "lesionType",
                   location = "location", diameter = "diameter",
                   isNewLesion = "isNewLesion")
  opts <- do.call(waterfallrecistOptions$new, utils::modifyList(defaults, list(...)))
  waterfallrecistClass$new(options = opts, data = df)$.__enclos_env__$private
}

target_sums_for <- function(df, ...) {
  p <- wr_private(df, ...)
  p$.calculateTargetLesionSums(p$.prepareLesionData())
}


test_that("progression is referenced to the nadir, not to baseline", {
  # THE discriminating case. Sum 100 -> 60 -> 78.
  #   78 vs baseline 100 = -22%  -> a baseline-referenced rule sees no progression
  #   78 vs nadir    60  = +30% and +18mm -> RECIST v1.1: PD
  ts <- target_sums_for(recist_data("PT1", c(100, 60, 78)))
  v <- ts[ts$patientID == "PT1", ]
  v <- v[order(v$visitTime), ]

  expect_equal(round(v$percent_change, 1), c(0, -40, -22))            # vs baseline
  expect_equal(v$nadir_sum, c(100, 60, 60))                            # running minimum
  expect_equal(round(v$percent_change_from_nadir, 1), c(0, 0, 30))
  # The baseline scan is the reference, not an assessment, so it is labelled
  # "Baseline" rather than being scored as stable disease.
  expect_equal(v$target_response, c("Baseline", "PR", "PD"))
})

test_that("PD requires BOTH a 20% relative and a 5mm absolute increase", {
  # 8mm -> 10mm is +25% but only +2mm: not progression.
  small <- rbind(
    data.frame(patientID = "PT9", lesionID = "L1", visitTime = 0, lesionType = "Target",
               location = "Liver", diameter = 8, isNewLesion = FALSE),
    data.frame(patientID = "PT9", lesionID = "L1", visitTime = 6, lesionType = "Target",
               location = "Liver", diameter = 10, isNewLesion = FALSE))
  ts <- target_sums_for(small)
  late <- ts[ts$visitTime == 6, ]

  expect_equal(round(late$percent_change_from_nadir, 1), 25)
  expect_equal(late$absolute_change_from_nadir, 2)
  expect_equal(late$target_response, "SD")   # 5mm rule blocks PD

  # 100 -> 121 clears both thresholds (+21%, +21mm)
  big <- target_sums_for(recist_data("PT3", c(100, 100, 121)))
  expect_equal(big$target_response[big$visitTime == 12], "PD")
})

test_that("partial response stays referenced to the baseline sum", {
  # PR is defined against baseline even after the nadir moves.
  ts <- target_sums_for(recist_data("PT2", c(100, 50, 55)))
  v <- ts[order(ts$visitTime), ]

  expect_equal(round(v$percent_change, 1), c(0, -50, -45))
  expect_equal(v$target_response, c("Baseline", "PR", "PR"))  # -45% is still >=30% down
  # ...and +10% / +5mm from nadir does not reach PD
  expect_equal(round(v$percent_change_from_nadir[3], 1), 10)
})

test_that("nadir is monotone non-increasing and never exceeds baseline", {
  ts <- target_sums_for(rbind(
    recist_data("PT1", c(100, 60, 78)),
    recist_data("PT2", c(100, 50, 48)),
    recist_data("PT4", c(100, 98, 102))))

  expect_true(all(ts$nadir_sum <= ts$baseline_sum + 1e-9))
  for (pt in unique(ts$patientID)) {
    d <- ts[ts$patientID == pt, ]
    d <- d[order(d$visitTime), ]
    expect_true(all(diff(d$nadir_sum) <= 1e-9), info = pt)
  }
})

test_that("a marginal increase below both thresholds stays stable disease", {
  ts <- target_sums_for(recist_data("PT4", c(100, 98, 102)))
  late <- ts[ts$visitTime == 12, ]
  expect_lt(late$percent_change_from_nadir, 20)
  expect_lt(late$absolute_change_from_nadir, 5)
  expect_equal(late$target_response, "SD")
})

test_that("a new lesion forces PD regardless of target shrinkage", {
  # Target lesions L1/L2 keep shrinking; a genuinely NEW lesion L3 appears at
  # visit 12. (Flagging an existing target lesion as new instead would remove it
  # from the target sum, which is correct but tests something else.)
  d <- rbind(
    recist_data("PT5", c(100, 60, 40)),
    data.frame(patientID = "PT5", lesionID = "L3", visitTime = 12,
               lesionType = "Non-Target", location = "Bone", diameter = 8,
               isNewLesion = TRUE, stringsAsFactors = FALSE))
  p <- wr_private(d)
  ld <- p$.prepareLesionData()
  ts <- p$.calculateTargetLesionSums(ld)
  overall <- p$.determineOverallResponse(ts, p$.detectNewLesions(ld),
                                         p$.assessNonTargetProgression(ld))

  late <- overall[overall$visitTime == 12, ]
  expect_true(late$new_lesion_present)
  expect_equal(late$overall_response_unconfirmed, "PD")
  # target lesions alone would have read PR
  expect_equal(ts$target_response[ts$visitTime == 12], "PR")
})

test_that("the analysis runs end to end and reports best overall response", {
  d <- rbind(
    recist_data("PT1", c(100, 60, 78)),   # responds then progresses off the nadir
    recist_data("PT2", c(100, 50, 48)),   # sustained response
    recist_data("PT3", c(100, 100, 130))) # progresses

  result <- waterfallrecist(
    data = d, patientID = "patientID", lesionID = "lesionID",
    visitTime = "visitTime", lesionType = "lesionType", location = "location",
    diameter = "diameter", isNewLesion = "isNewLesion",
    baselineTimepoint = 0, confirmationInterval = 4)

  expect_s3_class(result, "waterfallrecistResults")
  expect_equal(result$targetSumTable$rowCount, 9)   # 3 patients x 3 visits
  expect_equal(result$bestResponseTable$rowCount, 3)

  bor <- as.data.frame(result$bestResponseTable$asDF)
  # PT1's PR at visit 6 is never confirmed before it progresses, and best overall
  # response is truncated at progression, so its BOR is PD -- not the "SD" that a
  # scored baseline visit used to manufacture.
  expect_equal(bor$bestOverallResponse[bor$patientID == "PT1"], "PD")
  expect_equal(bor$progressionOccurred[bor$patientID == "PT1"], "Yes")
  # PT2 responds and stays responding
  expect_equal(bor$bestOverallResponse[bor$patientID == "PT2"], "PR")
  expect_equal(bor$progressionOccurred[bor$patientID == "PT2"], "No")
})

test_that("a cohort with no baseline visit is refused with an explanation", {
  # baselineTimepoint defaults to 0; these visits start at 6. jamovi analyses
  # report this through a result element rather than by throwing.
  d <- recist_data("PT6", c(100, 60, 78), visits = c(6, 12, 18))
  result <- waterfallrecist(
    data = d, patientID = "patientID", lesionID = "lesionID",
    visitTime = "visitTime", lesionType = "lesionType", location = "location",
    diameter = "diameter", isNewLesion = "isNewLesion", baselineTimepoint = 0)

  expect_equal(result$targetSumTable$rowCount, 0)
  notices <- gsub("<[^>]*>", " ", paste(as.character(result$notices$content), collapse = " "))
  expect_match(notices, "Baseline Timepoint Not Found")
})


# ---------------------------------------------------------------------------
# Defects surfaced by the multi-reviewer pass and reproduced before fixing.
# ---------------------------------------------------------------------------

wr_notices <- function(result) {
  gsub("[[:space:]]+", " ", paste(as.character(result$notices$content), collapse = " "))
}
wr_run <- function(d, ...) {
  args <- utils::modifyList(list(
    data = d, patientID = "patientID", lesionID = "lesionID", visitTime = "visitTime",
    lesionType = "lesionType", location = "location", diameter = "diameter",
    isNewLesion = "isNewLesion", baselineTimepoint = 0, confirmationInterval = 4), list(...))
  suppressWarnings(do.call(waterfallrecist, args))
}

test_that("complete response is reachable", {
  # A true CR has a percent change of -100, which also satisfies the PR mask.
  # Assigning PR after CR therefore overwrote every CR, so the CR row of the
  # response distribution was structurally always zero.
  d <- recist_data("P1", c(40, 0, 0), visits = c(0, 8, 16))
  ts <- target_sums_for(d)
  expect_equal(ts$target_response, c("Baseline", "CR", "CR"))

  bor <- as.data.frame(wr_run(d)$bestResponseTable$asDF)
  expect_equal(bor$bestOverallResponse[1], "CR")
})

test_that("a patient who only progresses is PD, not stable disease", {
  # The baseline row was scored as a real SD assessment, and SD outranks PD in
  # the BOR hierarchy, so every progressor was reported as disease control.
  d <- recist_data("P1", c(40, 60, 70), visits = c(0, 4, 8))
  bor <- as.data.frame(wr_run(d)$bestResponseTable$asDF)

  expect_equal(bor$bestOverallResponse[1], "PD")
  expect_equal(bor$progressionOccurred[1], "Yes")
})

test_that("a patient with only a baseline scan is Not Evaluable", {
  d <- rbind(recist_data("P1", c(40), visits = 0),
             recist_data("P2", c(40, 20, 18), visits = c(0, 8, 16)))
  result <- wr_run(d)
  bor <- as.data.frame(result$bestResponseTable$asDF)

  expect_equal(bor$bestOverallResponse[bor$patientID == "P1"], "Not Evaluable")
  expect_equal(bor$bestOverallResponse[bor$patientID == "P2"], "PR")
  expect_match(wr_notices(result), "Not Response-Evaluable")
})

test_that("an unmeasured target lesion does not fabricate a partial response", {
  # 3 target lesions at baseline (30mm); only 2 measured later (20mm). Summing
  # what was measured reads -33% = PR. The visit is not comparable, so it is NE.
  d <- rbind(
    data.frame(patientID = "P1", lesionID = c("L1", "L2", "L3"), visitTime = 0,
               lesionType = "Target", location = c("Liver", "Lung", "Node"),
               diameter = c(10, 10, 10), isNewLesion = FALSE, stringsAsFactors = FALSE),
    data.frame(patientID = "P1", lesionID = c("L1", "L2"), visitTime = 8,
               lesionType = "Target", location = c("Liver", "Lung"),
               diameter = c(10, 10), isNewLesion = FALSE, stringsAsFactors = FALSE))
  result <- wr_run(d)
  ts <- as.data.frame(result$targetSumTable$asDF)

  expect_equal(ts$targetResponse[ts$visitTime == 8], "NE")
  expect_equal(as.data.frame(result$bestResponseTable$asDF)$bestOverallResponse[1],
               "Not Evaluable")
  expect_match(wr_notices(result), "Incomplete Target Lesion")
})

test_that("a dataset with no target lesion reports a notice instead of crashing", {
  d <- recist_data("P1", c(40, 20), visits = c(0, 8), type = "Non-Target")
  expect_no_error(result <- wr_run(d))
  expect_match(wr_notices(result), "No Target Lesions")
})

test_that("lesion type is recognised however it is spelled", {
  # Only the exact strings "target"/"nontarget"/"new" matched after tolower(), so
  # the hyphenated spelling the documentation uses fell through and every
  # non-target lesion was silently ignored.
  for (spelling in c("Non-Target", "non target", "NONTARGET", "Non-target")) {
    d <- rbind(
      recist_data("P1", c(40, 20), visits = c(0, 8)),
      data.frame(patientID = "P1", lesionID = "N1", visitTime = c(0, 8),
                 lesionType = spelling, location = "Bone", diameter = 12,
                 isNewLesion = FALSE, stringsAsFactors = FALSE))
    p <- wr_private(d)
    types <- unique(p$.prepareLesionData()$lesionType)
    expect_true("NonTarget" %in% types, info = spelling)
  }
})

test_that("confirmation cannot reach across an intervening progression", {
  # PR at t=8, PD at t=16, PR again at t=24. The late PR belongs to a different
  # disease course and must not confirm the early one.
  d <- recist_data("P1", c(100, 60, 130, 60), visits = c(0, 8, 16, 24))
  bor <- as.data.frame(wr_run(d)$bestResponseTable$asDF)

  expect_equal(bor$bestOverallResponse[1], "PD")
  expect_equal(bor$progressionVisit[1], "Visit 16")
})

test_that("a genuinely confirmed partial response is still reported", {
  d <- recist_data("P1", c(100, 60, 58), visits = c(0, 8, 16))
  bor <- as.data.frame(wr_run(d)$bestResponseTable$asDF)

  expect_equal(bor$bestOverallResponse[1], "PR")
  expect_equal(bor$borConfirmed[1], "Yes")
})

test_that("new lesions are detected however the indicator is coded", {
  # jmvcore::toNumeric() on a factor of "Yes"/"No" returns the LABELS, so the
  # `== 1` test was never TRUE and a new lesion coded that way was silently
  # missed -- losing an automatic Progressive Disease.
  for (coding in list(c(no = "No", yes = "Yes"), c(no = "0", yes = "1"),
                      c(no = "FALSE", yes = "TRUE"), c(no = "N", yes = "Y"))) {
    d <- rbind(
      data.frame(patientID = "P1", lesionID = c("L1", "L2"),
                 visitTime = rep(c(0, 8), each = 2), lesionType = "Target",
                 location = "Liver", diameter = c(50, 50, 30, 30),
                 isNewLesion = coding[["no"]], stringsAsFactors = FALSE),
      data.frame(patientID = "P1", lesionID = "N1", visitTime = 8,
                 lesionType = "Non-Target", location = "Bone", diameter = 9,
                 isNewLesion = coding[["yes"]], stringsAsFactors = FALSE))

    bor <- as.data.frame(wr_run(d)$bestResponseTable$asDF)
    expect_equal(bor$bestOverallResponse[1], "PD", info = coding[["yes"]])
    expect_equal(bor$progressionOccurred[1], "Yes", info = coding[["yes"]])
  }
})

test_that("non-target assessments are normalised onto the RECIST categories", {
  # RECIST defines non-target PD as "unequivocal progression" -- a radiologist's
  # qualitative judgement that no lesion count can establish. The optional
  # Non-Target Response variable lets the reader record it, overriding the
  # count-based heuristic. Spelling must not be a barrier to that.
  p <- wr_private(data.frame(id = 1))
  n <- function(x) p$.normaliseNonTargetStatus(x)

  expect_equal(n("CR"), "CR")
  expect_equal(n("complete response"), "CR")
  expect_equal(n("PD"), "PD")
  expect_equal(n("progression"), "PD")
  expect_equal(n("Unequivocal progression"), "PD")
  # the compound label must not be swallowed by the bare "PD" rule
  expect_equal(n("Non-CR/Non-PD"), "Non-CR/Non-PD")
  expect_equal(n("non cr non pd"), "Non-CR/Non-PD")
  expect_equal(n("NonCRNonPD"), "Non-CR/Non-PD")
  expect_equal(n("stable"), "Non-CR/Non-PD")
  expect_equal(n("NE"), "NE")
  expect_equal(n("not evaluable"), "NE")
  # anything unrecognised must be NA so the caller reports it and falls back
  expect_true(is.na(n("bogus")))
  expect_true(is.na(n("")))
  expect_true(is.na(n(NA)))
  # vectorised
  expect_equal(n(c("CR", "PD", "junk")), c("CR", "PD", NA))
})

test_that("the lesion-count heuristic announces itself as an estimate", {
  # Without a radiologist assessment the analysis infers non-target progression
  # from lesion counts, which is NOT the RECIST criterion. It must say so.
  d <- rbind(
    recist_data("P1", c(100, 60), visits = c(0, 8)),
    data.frame(patientID = "P1", lesionID = "N1", visitTime = c(0, 8),
               lesionType = "Non-Target", location = "Bone", diameter = c(10, 10),
               isNewLesion = FALSE, stringsAsFactors = FALSE))
  p <- wr_private(d)
  p$.assessNonTargetProgression(p$.prepareLesionData())

  titles <- vapply(p$.noticeList, function(n) n$title, character(1))
  expect_true("Non-Target Progression Is Estimated, Not Assessed" %in% titles)
})

test_that("the radiologist's non-target assessment overrides the count heuristic", {
  # RECIST defines non-target PD as unequivocal progression, a qualitative call.
  # Here the target lesions shrink to a PR and only ONE non-target lesion is
  # present at both visits, so the lesion-count heuristic sees no progression --
  # but the reader judges unequivocal progression at visit 8.
  d <- rbind(
    do.call(rbind, lapply(c(0, 8), function(v) data.frame(
      patientID = "P1", lesionID = c("L1", "L2"), visitTime = v, lesionType = "Target",
      location = c("Liver", "Lung"),
      diameter = if (v == 0) c(50, 50) else c(30, 30),
      isNewLesion = "No", ntr = NA_character_, stringsAsFactors = FALSE))),
    data.frame(patientID = "P1", lesionID = "N1", visitTime = c(0, 8),
               lesionType = "Non-Target", location = "Bone", diameter = c(10, 10),
               isNewLesion = "No", ntr = c("Non-CR/Non-PD", "PD"),
               stringsAsFactors = FALSE))

  # NOTE: the generated wrapper resolves variable arguments with
  # jmvcore::enquo()/resolveQuo(), i.e. non-standard evaluation. Passing a local
  # variable whose NAME matches a column (e.g. a helper argument called `ntr`)
  # captures the symbol and silently resolves it to that column, so the two calls
  # are written out in full rather than driven from a shared helper.
  without <- suppressWarnings(waterfallrecist(
    data = d, patientID = "patientID", lesionID = "lesionID", visitTime = "visitTime",
    lesionType = "lesionType", location = "location", diameter = "diameter",
    isNewLesion = "isNewLesion",
    baselineTimepoint = 0, confirmationInterval = 4))
  with_it <- suppressWarnings(waterfallrecist(
    data = d, patientID = "patientID", lesionID = "lesionID", visitTime = "visitTime",
    lesionType = "lesionType", location = "location", diameter = "diameter",
    isNewLesion = "isNewLesion", nonTargetResponseVar = "ntr",
    baselineTimepoint = 0, confirmationInterval = 4))

  # Without the assessment the count heuristic sees no progression, and the
  # single unconfirmed PR leaves the patient not evaluable.
  expect_equal(as.data.frame(without$bestResponseTable$asDF)$bestOverallResponse[1],
               "Not Evaluable")
  expect_match(wr_notices(without), "Estimated, Not Assessed")

  # With it, visit 8 is progression (PD needs no confirmation).
  bor <- as.data.frame(with_it$bestResponseTable$asDF)
  expect_equal(bor$bestOverallResponse[1], "PD")
  expect_equal(bor$progressionOccurred[1], "Yes")
  expect_match(wr_notices(with_it), "Non-Target Assessment Supplied")
  # ...and the heuristic caveat is no longer raised, because it was not used
  expect_false(grepl("Estimated, Not Assessed", wr_notices(with_it)))
})

test_that("getting-started guidance shows until the analysis is configured", {
  d <- data.frame(PatientID = c("P1", "P1"), LesionID = c("L1", "L2"),
                  VisitTime = c(0, 0), LesionType = "Target",
                  Location = c("Liver", "Lung"), Diameter = c(50, 40),
                  IsNew = "No", stringsAsFactors = FALSE)
  txt <- function(r) paste(as.character(r$instructions$content), collapse = "")

  partial <- suppressWarnings(waterfallrecist(data = d, patientID = "PatientID"))
  expect_gt(nchar(txt(partial)), 0)
  expect_match(txt(partial), "ONE ROW PER LESION PER VISIT")

  complete <- suppressWarnings(waterfallrecist(
    data = d, patientID = "PatientID", lesionID = "LesionID",
    visitTime = "VisitTime", diameter = "Diameter"))
  expect_equal(nchar(txt(complete)), 0)
})
