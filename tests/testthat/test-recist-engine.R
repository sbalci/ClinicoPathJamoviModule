# Tests for the shared RECIST v1.1 engine (R/recist_engine.R).
#
# These call the engine directly, with no analysis object anywhere: that is the
# point of the extraction. If a second analysis is ever wired up to these
# functions, this file is what says the criteria still hold.
#
# Ground truth is RECIST v1.1 as published (Eisenhauer et al., Eur J Cancer
# 2009;45:228-247), not the module's own comments.

# One row per lesion per visit, with the per-visit target SUM split across lesions.
eng_data <- function(patient, sums, visits = c(0, 6, 12), n_lesions = 2,
                     type = "Target", new_at = NA_real_) {
  do.call(rbind, lapply(seq_along(sums), function(i) {
    data.frame(
      patientID   = patient,
      lesionID    = paste0("L", seq_len(n_lesions)),
      visitTime   = visits[i],
      lesionType  = type,
      location    = rep(c("Liver", "Lung", "Node"), length.out = n_lesions),
      diameter    = rep(sums[i] / n_lesions, n_lesions),
      isNewLesion = as.numeric(!is.na(new_at) & visits[i] >= new_at),
      isBaseline  = visits[i] == 0,
      stringsAsFactors = FALSE
    )
  }))
}


test_that("the engine runs with no analysis object and no notice sink", {
  # recist_context() defaults must be enough to compute a result: a caller with
  # no jamovi around it (a script, a test, another package) is the target case.
  ctx <- recist_context()
  expect_null(ctx$notify)
  expect_equal(ctx$baselineTimepoint, 0)

  ts <- recist_target_sums(eng_data("P1", c(100, 60, 78)), ctx)
  expect_equal(nrow(ts), 3)
  expect_equal(ts$target_response, c("Baseline", "PR", "PD"))
})

test_that("thresholds live in one place", {
  expect_equal(.RECIST$PR, -30)
  expect_equal(.RECIST$PD, 20)
  expect_equal(.RECIST$PD_ABS_MM, 5)
  expect_equal(.RECIST$CR, -100)
})

test_that("progression is referenced to the nadir, with the 5mm rule", {
  ctx <- recist_context()

  # 100 -> 60 -> 78: -22% from baseline but +30% and +18mm over the nadir -> PD
  ts <- recist_target_sums(eng_data("P1", c(100, 60, 78)), ctx)
  expect_equal(round(ts$percent_change_from_nadir, 1), c(0, 0, 30))
  expect_equal(ts$target_response[3], "PD")

  # 8mm -> 10mm is +25% but only +2mm: the absolute rule blocks PD
  small <- rbind(
    data.frame(patientID = "P9", lesionID = "L1", visitTime = 0, lesionType = "Target",
               location = "Liver", diameter = 8, isNewLesion = 0, isBaseline = TRUE),
    data.frame(patientID = "P9", lesionID = "L1", visitTime = 6, lesionType = "Target",
               location = "Liver", diameter = 10, isNewLesion = 0, isBaseline = FALSE))
  ts2 <- recist_target_sums(small, ctx)
  expect_equal(ts2$target_response[2], "SD")
})

test_that("complete response is reachable and outranks partial response", {
  ts <- recist_target_sums(eng_data("P1", c(40, 0, 0), visits = c(0, 8, 16)),
                           recist_context())
  expect_equal(ts$target_response, c("Baseline", "CR", "CR"))
})

test_that("the baseline visit is a reference, never a scored assessment", {
  ts <- recist_target_sums(eng_data("P1", c(100, 130, 150), visits = c(0, 4, 8)),
                           recist_context())
  expect_equal(ts$target_response[1], "Baseline")
  expect_true(all(ts$is_baseline_visit == c(TRUE, FALSE, FALSE)))
})

test_that("a new lesion is progression whatever the target lesions do", {
  ctx <- recist_context()
  d <- rbind(
    eng_data("P1", c(100, 60, 40)),
    data.frame(patientID = "P1", lesionID = "N1", visitTime = 12,
               lesionType = "NonTarget", location = "Bone", diameter = 8,
               isNewLesion = 1, isBaseline = FALSE, stringsAsFactors = FALSE))

  ts <- recist_target_sums(d, ctx)
  overall <- recist_overall_response(ts, recist_detect_new_lesions(d),
                                     recist_assess_nontarget(d, ctx), ctx)
  late <- overall[overall$visitTime == 12, ]
  expect_true(late$new_lesion_present)
  expect_equal(late$overall_response_unconfirmed, "PD")
  expect_equal(ts$target_response[ts$visitTime == 12], "PR")  # targets alone said PR
})

test_that("non-target assessments are normalised without a context", {
  expect_equal(recist_normalise_nontarget("CR"), "CR")
  expect_equal(recist_normalise_nontarget("Non-CR/Non-PD"), "Non-CR/Non-PD")
  expect_equal(recist_normalise_nontarget("non cr non pd"), "Non-CR/Non-PD")
  expect_equal(recist_normalise_nontarget("unequivocal progression"), "PD")
  expect_equal(recist_normalise_nontarget(c("CR", "PD", "junk")), c("CR", "PD", NA))
})

test_that("the notify callback is optional and receives the notice", {
  seen <- list()
  ctx <- recist_context(notify = function(type, title, content) {
    seen[[length(seen) + 1]] <<- list(type = type, title = title)
  })

  d <- rbind(
    eng_data("P1", c(100, 60), visits = c(0, 8)),
    data.frame(patientID = "P1", lesionID = "N1", visitTime = c(0, 8),
               lesionType = "NonTarget", location = "Bone", diameter = c(10, 10),
               isNewLesion = 0, isBaseline = c(TRUE, FALSE), stringsAsFactors = FALSE))
  recist_assess_nontarget(d, ctx)

  titles <- vapply(seen, function(x) x$title, character(1))
  expect_true("Non-Target Progression Is Estimated, Not Assessed" %in% titles)

  # ...and the same call is silent when no sink is supplied
  expect_silent(recist_assess_nontarget(d, recist_context()))
})

test_that("confirmation honours the interval and cannot span a progression", {
  ctx <- recist_context(confirmationInterval = 4)
  d <- eng_data("P1", c(100, 60, 130, 60), visits = c(0, 8, 16, 24))

  ts <- recist_target_sums(d, ctx)
  overall <- recist_overall_response(ts, recist_detect_new_lesions(d),
                                     recist_assess_nontarget(d, ctx), ctx)
  confirmed <- recist_confirm_responses(overall, ctx)
  bor <- recist_best_overall_response(confirmed)

  # the PR at t=24 lies beyond the progression at t=16 and cannot confirm t=8
  expect_equal(bor$bestOverallResponse, "PD")
  expect_equal(bor$progressionVisit, "Visit 16")
})

test_that("best overall response is truncated at progression", {
  ctx <- recist_context(confirmationInterval = 4)
  d <- eng_data("P1", c(100, 60, 58), visits = c(0, 8, 16))  # sustained PR

  ts <- recist_target_sums(d, ctx)
  overall <- recist_overall_response(ts, recist_detect_new_lesions(d),
                                     recist_assess_nontarget(d, ctx), ctx)
  bor <- recist_best_overall_response(recist_confirm_responses(overall, ctx))

  expect_equal(bor$bestOverallResponse, "PR")
  expect_equal(bor$borConfirmed, "Yes")
})

test_that("engine results match the analysis that delegates to it", {
  # The wrappers in waterfallrecist.b.R must add nothing: same inputs, same output.
  d <- eng_data("P1", c(100, 60, 78))
  d$isNewLesion <- "No"
  d$isBaseline <- NULL   # let .prepareLesionData derive it

  via_analysis <- {
    opts <- waterfallrecistOptions$new(
      patientID = "patientID", lesionID = "lesionID", visitTime = "visitTime",
      lesionType = "lesionType", location = "location", diameter = "diameter",
      isNewLesion = "isNewLesion", baselineTimepoint = 0)
    p <- waterfallrecistClass$new(options = opts, data = d)$.__enclos_env__$private
    p$.calculateTargetLesionSums(p$.prepareLesionData())
  }
  via_engine <- {
    opts <- waterfallrecistOptions$new(
      patientID = "patientID", lesionID = "lesionID", visitTime = "visitTime",
      lesionType = "lesionType", location = "location", diameter = "diameter",
      isNewLesion = "isNewLesion", baselineTimepoint = 0)
    p <- waterfallrecistClass$new(options = opts, data = d)$.__enclos_env__$private
    recist_target_sums(p$.prepareLesionData(), recist_context())
  }

  expect_equal(via_analysis$target_response, via_engine$target_response)
  expect_equal(via_analysis$percent_change_from_nadir, via_engine$percent_change_from_nadir)
  expect_equal(via_analysis$nadir_sum, via_engine$nadir_sum)
})
