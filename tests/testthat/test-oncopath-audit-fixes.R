audit_ihc_data <- function() {
  data.frame(ref = 10:29, b1 = (10:29) + rep(c(-1, 1), 10),
             b2 = (10:29) + rep(c(1, 0, -1, 0), 5))
}
audit_notes <- function(table) paste(vapply(table$notes, function(n) n$note, ""), collapse = " ")

test_that("Deeks table and renderer agree with an independent zero-cell reference", {
  d <- data.frame(study = paste0("S", 1:12), tp = c(0, 21:31), fn = 10:21,
                  fp = 5:16, tn = seq(15, 125, 10))
  a <- diagnosticmetaClass$new(data = d, options = diagnosticmetaOptions$new(
    study = "study", true_positives = "tp", false_negatives = "fn",
    false_positives = "fp", true_negatives = "tn", publication_bias = TRUE, funnel_plot = TRUE))
  a$run()
  ess <- 4 * (d$tp + d$fn) * (d$fp + d$tn) / rowSums(d[c("tp", "fn", "fp", "tn")])
  corrected <- d[c("tp", "fn", "fp", "tn")] + 0.5
  logdor <- with(corrected, log(tp * tn / (fp * fn)))
  cf <- summary(lm(logdor ~ I(1 / sqrt(ess)), weights = ess))$coefficients
  row <- a$results$publicationbias$asDF
  expect_equal(row$statistic[1], unname(cf[2, 3]))
  expect_equal(row$p_value[1], unname(cf[2, 4]))
  prepared <- a$.__enclos_env__$private$.prepareDeeksData(a$results$funnelplot$state)
  expect_equal(prepared$ess, ess)
  expect_equal(prepared$log_dor, logdor)
  expect_equal(prepared$ess[1], 80 / 3)
  # Render on a real graphics device as well as checking the underlying vectors.
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({grDevices::dev.off(); unlink(path)}, add = TRUE)
  expect_silent(a$.__enclos_env__$private$.funnelplot(a$results$funnelplot, NULL, "default"))
})

test_that("waterfall rejects infinite responses and times before capping or classification", {
  for (input in c("percentage", "raw")) for (variable in c("response", "time")) {
    for (value in c(Inf, -Inf)) {
      d <- data.frame(id = rep(c("A", "B"), each = 2), response = c(50, 25, 60, 45),
                      time = c(0, 1, 0, 1))
      d[[variable]][2] <- value
      a <- waterfallClass$new(data = d, options = waterfallOptions$new(
        patientID = "id", responseVar = "response", timeVar = "time", inputType = input))
      expect_error(a$run(), paste0("Infinite response or time values in variable '", variable, "'"), fixed = TRUE)
      a$run(noThrow = TRUE)
      expect_true(a$errored)
    }
  }
})

test_that("swimmer rejects infinite endpoints in both summary paths and display modes", {
  for (n in c(4, 1001)) for (display in c("relative", "absolute")) {
    for (variable in c("start", "end")) for (value in c(Inf, -Inf)) {
      d <- data.frame(id = seq_len(n), start = 0, end = 10)
      d[[variable]][2] <- value
      a <- swimmerplotClass$new(data = d, options = swimmerplotOptions$new(
        patientID = "id", startTime = "start", endTime = "end", timeDisplay = display))
      expect_error(a$run(), paste0("Infinite time values in variable '", variable, "'"), fixed = TRUE)
      expect_null(a$results$plot$state)
    }
  }
})

test_that("partial IHC selection guides the user and clears a previous result", {
  d <- audit_ihc_data()
  o <- ihcheterogeneityOptions$new(biopsy1 = "b1", biopsy2 = "b2", showInterpretation = TRUE)
  a <- ihcheterogeneityClass$new(data = d, options = o)
  a$run()
  original <- a$results$reproducibilitytable$asDF
  expect_gt(nrow(original), 0)
  changed <- o$option("biopsy2"); changed$value <- NULL
  expect_silent(a$run())
  expect_false(a$errored)
  expect_match(a$results$notices$content, "second distinct regional measurement", fixed = TRUE)
  expect_equal(a$results$reproducibilitytable$rowCount, 0)
  expect_null(a$results$biopsyplot$state)
  expect_identical(a$results$interpretation$content, "")
  changed <- o$option("biopsy2"); changed$value <- "b2"
  a$run()
  expect_equal(a$results$reproducibilitytable$asDF, original)
})

test_that("report controls default off and disabling clears existing narrative content", {
  d <- audit_ihc_data()
  o <- ihcheterogeneityOptions$new(wholesection = "ref", biopsy1 = "b1", biopsy2 = "b2")
  a <- ihcheterogeneityClass$new(data = d, options = o)
  a$run()
  expect_identical(a$results$interpretation$content, "")
  changed <- o$option("showInterpretation"); changed$value <- TRUE
  a$run()
  expect_match(a$results$interpretation$content, "Study Design", fixed = TRUE)
  changed <- o$option("showInterpretation"); changed$value <- FALSE
  a$run()
  expect_identical(a$results$interpretation$content, "")
  d <- data.frame(id = 1:4, response = c(-50, 0, 20, -100))
  o <- waterfallOptions$new(patientID = "id", responseVar = "response")
  a <- waterfallClass$new(data = d, options = o)
  suppressWarnings(a$run())
  expect_identical(a$results$clinicalSummary$content, "")
  expect_identical(a$results$aboutAnalysis$content, "")
  changed <- o$option("showClinicalSummary"); changed$value <- TRUE
  changed <- o$option("showAboutAnalysis"); changed$value <- TRUE
  suppressWarnings(a$run())
  expect_gt(nchar(a$results$clinicalSummary$content), 100)
  expect_gt(nchar(a$results$aboutAnalysis$content), 100)
  changed <- o$option("showClinicalSummary"); changed$value <- FALSE
  changed <- o$option("showAboutAnalysis"); changed$value <- FALSE
  suppressWarnings(a$run())
  expect_identical(a$results$clinicalSummary$content, "")
  expect_identical(a$results$aboutAnalysis$content, "")
})

test_that("sparse warning wording matches its zero-or-one predicate", {
  for (small in c(1, 3, 5)) {
    d <- data.frame(study = paste0("S", 1:6), tp = 20:25, fn = 10:15,
                    fp = c(small, small, 10:13), tn = 40:45)
    r <- diagnosticmeta(d, study = "study", true_positives = "tp", false_negatives = "fn",
                        false_positives = "fp", true_negatives = "tn")
    expect_identical(grepl("2 of 6 studies have a cell of 0 or 1", r$notices$content, fixed = TRUE), small == 1)
  }
})

test_that("real Fisher workspace failures are distinct from absent outcome contrasts", {
  counts <- cbind(nonresponse = seq(20, 400, 20), response = seq(300, 15, -15))
  expect_error(stats::fisher.test(counts), "FEXACT")
  group <- rep(paste0("G", 1:20), rowSums(counts))
  status <- unlist(lapply(1:20, function(i) c(rep("PD", counts[i, 1]), rep("PR", counts[i, 2]))))
  d <- data.frame(id = seq_along(group), start = 0, end = 1, group = group, status = status,
                  response = ifelse(status == "PR", -50, 30))
  sw <- swimmerplot(d, patientID = "id", startTime = "start", endTime = "end",
                    responseVar = "status", groupVar = "group")
  wf <- suppressWarnings(waterfall(d, patientID = "id", responseVar = "response", groupVar = "group"))
  for (r in list(sw, wf)) {
    expect_match(r$notices$content, "for ORR could not be computed", fixed = TRUE)
    expect_match(r$notices$content, "for DCR could not be computed", fixed = TRUE)
    expect_match(audit_notes(r$groupComparisonTest), "FEXACT", fixed = TRUE)
    expect_false(grepl("every patient", audit_notes(r$groupComparisonTest), fixed = TRUE))
    expect_equal(r$groupComparisonTest$rowCount, 0)
  }
})

test_that("narrative switches are valid result-cache dependencies", {
  folder <- file.path("..", "..", "jamovi")
  skip_if_not(file.exists(file.path(folder, "waterfall.a.yaml")), "source YAML is unavailable")
  for (fn in c("ihcheterogeneity", "waterfall")) {
    options <- yaml::read_yaml(file.path(folder, paste0(fn, ".a.yaml")))$options
    results <- yaml::read_yaml(file.path(folder, paste0(fn, ".r.yaml")))$items
    option_names <- vapply(options, function(x) x$name, "")
    for (result in results) {
      expect_true(all(unlist(result$clearWith) %in% option_names), info = result$name)
    }
    toggles <- if (fn == "waterfall") c(clinicalSummary = "showClinicalSummary", aboutAnalysis = "showAboutAnalysis")
               else c(interpretation = "showInterpretation")
    for (name in names(toggles)) {
      result <- results[[which(vapply(results, function(x) identical(x$name, name), TRUE))]]
      expect_true(toggles[[name]] %in% result$clearWith)
      # waterfall: "Analysis explanations" shows the same two panels (2026-10-19, R89)
      expect_identical(result$visible, if (fn == "waterfall")
        paste0("(", toggles[[name]], " || showExplanations)") else paste0("(", toggles[[name]], ")"))
    }
  }
})
