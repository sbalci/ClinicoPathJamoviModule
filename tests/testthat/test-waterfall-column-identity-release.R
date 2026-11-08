# A column's spelling must not change patient identity or the response analysis.
wf_identity_data <- function() {
  data.frame(id = rep(LETTERS[1:8], each = 3), visit = rep(c(0, 2, 4), 8),
    size = c(50, 30, 40, 50, 65, 20, 20, 24, 22, 100, 0, 10,
             50, 45, 44, 100, 50, 40, 100, 90, 85, 100, 70, 50),
    arm = rep(rep(c("A", "B"), each = 4), each = 3),
    supplied = rep(c("PR", "PD", "SD", "CR", "SD", "PR", "SD", "PR"), each = 3),
    confirmed = rep(c("yes", "no"), 12), ongoing = rep(c(1, 0), each = 12),
    marker = rep(c("mutant", "wild type"), each = 12), check.names = FALSE)
}
wf_identity_run <- function(d, role = NULL, name = NULL, input = "raw") {
  args <- list(patientID = "id", timeVar = "visit", responseVar = "size", groupVar = "arm",
    responseCategoryVar = "supplied", confirmationVar = "confirmed", ongoingVar = "ongoing",
    annotationVars = "marker", inputType = input, measurementUnit = "mm",
    showSpiderPlot = TRUE, showResponseDuration = TRUE, generateCopyReadyReport = TRUE)
  if (!is.null(role)) {
    old <- args[[role]]
    names(d)[match(old, names(d))] <- name
    args[[role]] <- name
  }
  do.call(ClinicoPath::waterfall, c(list(data = d), args))
}
wf_identity_tables <- function(x) lapply(c("summaryTable", "clinicalMetrics",
    "enhancedClinicalMetrics", "responseDurationTable", "personTimeTable",
    "groupComparisonTable", "groupComparisonTest"), function(n) {
  it <- tryCatch(x[[n]], error = function(e) NULL)
  if (is.null(it)) NULL else it$asDF
})

test_that("reserved column names preserve counts, durations and groups in both input modes", {
  reserved <- c("response", "min_abs_pct", "recist_category", ".cat_override_norm",
    "patient_group", "confirm_status", "ongoing_flag", "time", "responder_status")
  roles <- c("patientID", "timeVar", "responseVar", "groupVar", "responseCategoryVar",
             "confirmationVar", "ongoingVar", "annotationVars")
  for (input in c("raw", "percentage")) {
    d <- wf_identity_data()
    if (input == "percentage") d$size <- 100 * (d$size / rep(d$size[d$visit == 0], each = 3) - 1)
    ref <- wf_identity_tables(wf_identity_run(d, input = input))
    for (role in roles) for (name in reserved) {
      x <- wf_identity_run(d, role, name, input)
      expect_equal(wf_identity_tables(x), ref, info = paste(input, role, name))
      st <- x$waterfallplot$state
      expect_setequal(as.character(st$data$waterfall[[st$options$patientID]]), LETTERS[1:8])
    }
  }
})

test_that("aliases cannot collide with an existing source column or depend on row order", {
  d <- wf_identity_data()
  names(d)[1] <- "response"
  d$.waterfall.source.1 <- "existing unrelated column"
  ref <- wf_identity_tables(wf_identity_run(wf_identity_data()))
  for (i in list(seq_len(nrow(d)), rev(seq_len(nrow(d))), c(2:3, seq_len(nrow(d))))) {
    x <- ClinicoPath::waterfall(d[i, ], patientID = "response", timeVar = "visit", responseVar = "size",
      groupVar = "arm", responseCategoryVar = "supplied", inputType = "raw", measurementUnit = "mm",
      showSpiderPlot = TRUE, showResponseDuration = TRUE)
    expect_equal(wf_identity_tables(x), ref)
  }
})

test_that("missing and filtered rows retain identities with a reserved patient key", {
  d <- wf_identity_data()
  d$size[6] <- NA_real_
  d <- d[-c(1:3), ]
  ref <- wf_identity_tables(wf_identity_run(d))
  x <- wf_identity_run(d, "patientID", "response")
  expect_equal(wf_identity_tables(x), ref)
  st <- x$waterfallplot$state
  expect_setequal(st$data$waterfall[[st$options$patientID]], LETTERS[2:8])
})
