test_that("diagnostic marginal tests and fixed-effect uncertainty are described accurately", {
  d <- data.frame(study = paste0("S", 1:8), tp = 60:67, fn = 20:27, fp = 10:17, tn = 80:87)
  x <- ClinicoPath::diagnosticmeta(d, study = "study", true_positives = "tp",
    false_negatives = "fn", false_positives = "fp", true_negatives = "tn", method = "fixed")
  notes <- paste(vapply(x$bivariateresults$notes, function(n) n$note, ""), collapse = "\n")
  expect_match(notes, "equals 50% for that margin", fixed = TRUE)
  expect_false(grepl("no better than chance", notes, fixed = TRUE))
  expect_false(grepl("narrower than", notes, fixed = TRUE))
  expect_match(notes, "non-significant heterogeneity test does not establish", fixed = TRUE)
  expect_false(grepl("narrower than", x$notices$content, fixed = TRUE))
  expect_match(x$notices$content, "not merely because a heterogeneity test is non-significant", fixed = TRUE)
})
