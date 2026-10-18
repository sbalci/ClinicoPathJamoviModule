wf_id_analysis <- function(data, ...) {
  waterfallClass$new(
    data = data, options = waterfallOptions$new(patientID = "id", responseVar = "response", ...)
  )
}

test_that("one numeric patient's progression does not truncate another patient's response", {
  d <- data.frame(id = rep(1e15 + 0:1, each = 2), time = c(0, 1, 0, 2),
                  response = c(100, 150, 100, 50))
  result <- waterfall(d, patientID = "id", responseVar = "response",
                      timeVar = "time", inputType = "raw")
  expect_identical(result$clinicalMetrics$asDF$value[1], "2")
  expect_identical(result$enhancedClinicalMetrics$asDF$value[1:2],
                   c("50.0%", "50.0%"))
  patients <- result$waterfallplot$state$data$waterfall
  expect_setequal(patients$id, unique(d$id))
  expect_identical(as.character(patients$recist_category[match(unique(d$id), patients$id)]),
                   c("PD", "PR"))
})

test_that("time-to-event and person-time retain each original numeric ID", {
  ids <- 1e15 + 0:1
  d <- data.frame(id = rep(ids, each = 3), time = c(0, 1, 3, 0, 2, 4),
                  response = c(0, -50, -20, 0, -40, -50))
  private <- wf_id_analysis(d, timeVar = "time")$.__enclos_env__$private
  categories <- data.frame(id = ids, category = c("PR", "PR"))
  tte <- private$.calculateTimeToEventMetrics(d, "id", "time", "response", categories)
  expect_setequal(tte$all_patients$id, ids)
  ordered <- tte$all_patients[match(ids, tte$all_patients$id), ]
  expect_equal(ordered$time_to_first_response, c(1, 2))
  expect_equal(ordered$duration_of_response, c(2, 2))
  expect_equal(ordered$duration_censored, c(1, 0))
  pt <- private$.calculatePersonTimeMetrics(d, "id", "time", "response", categories, tte)
  expect_equal(pt$summary$total_patients, 2)
  expect_equal(pt$summary$total_person_time, 7)
  expect_equal(pt$summary$total_response_time, 4)
  expect_equal(pt$summary$response_rate_per_100, 400 / 7)
})

test_that("overrides and annotations join using original patient keys", {
  ids <- 1e15 + 0:3
  d <- data.frame(id = ids, response = c(-40, 0, 40, -100),
                  category = c("SD", "PD", "CR", "PR"),
                  confirmation = paste0("confirm ", 1:4), ongoing = c(TRUE, FALSE, TRUE, FALSE))
  result <- waterfall(d, patientID = "id", responseVar = "response",
                      responseCategoryVar = "category", confirmationVar = "confirmation",
                      ongoingVar = "ongoing")
  patients <- result$waterfallplot$state$data$waterfall
  patients <- patients[match(ids, patients$id), ]
  expect_identical(as.character(patients$recist_category), d$category)
  expect_identical(patients$confirm_status, d$confirmation)
  expect_identical(patients$ongoing_flag, d$ongoing)

  doubled <- d[rep(1:4, each = 2), ]
  private <- wf_id_analysis(doubled)$.__enclos_env__$private
  field <- private$.resolvePatientField(doubled, "id", "category")
  expect_length(field$conflicts, 0)
  expect_identical(field$ids, ids)
  doubled$category[2] <- "PR"
  field <- private$.resolvePatientField(doubled, "id", "category")
  expect_identical(field$conflicts, sprintf("%.0f", ids[1]))
})

test_that("an invalid raw measurement affects only its own numeric patient", {
  ids <- 1e15 + 0:1
  d <- data.frame(id = rep(ids, each = 2), time = rep(c(0, 1), 2),
                  response = c(100, -1, 100, 50), category = c("CR", "CR", "PR", "PR"))
  result <- waterfall(d, patientID = "id", responseVar = "response", timeVar = "time",
                      inputType = "raw", responseCategoryVar = "category")
  patients <- result$waterfallplot$state$data$waterfall
  expect_identical(as.character(patients$recist_category[match(ids, patients$id)]),
                   c("Unknown", "PR"))
})

test_that("capped-patient notices count distinct numeric IDs", {
  result <- waterfall(data.frame(id = 1e15 + 0:1, response = c(-150, -120)),
                      patientID = "id", responseVar = "response")
  expect_match(result$notices$content, "2 patient(s) had a change below -100%", fixed = TRUE)
  expect_match(result$notices$content, "1000000000000000", fixed = TRUE)
  expect_match(result$notices$content, "1000000000000001", fixed = TRUE)
})

test_that("spider trajectories and endpoint labels distinguish numeric patients", {
  ids <- 1e15 + 0:1
  d <- data.frame(id = rep(ids, each = 2), time = rep(c(0, 1), 2),
                  response = c(0, -50, 0, 30))
  analysis <- wf_id_analysis(d, timeVar = "time", showSpiderPlot = TRUE,
                             showSpiderLabels = TRUE, showThresholds = FALSE)
  suppressWarnings(analysis$run())
  theme <- jmvcore:::getGlobalTheme("default", "jmv")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_true(analysis$.__enclos_env__$private$.spiderplot(
    analysis$results$spiderplot, theme$ggtheme, theme$theme
  ))
  built <- ggplot2::ggplot_build(ggplot2::last_plot())
  expect_length(unique(built$data[[1]]$group), 2)
  labels <- unlist(lapply(built$data, function(layer) layer$label))
  expect_setequal(as.character(labels), sprintf("%.0f", ids))
})
