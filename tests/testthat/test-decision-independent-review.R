# Regression tests for the independent review of decision, 2026-09-25
# (development-ideas/decision-independent-review-2026-09-25.md): findings F1-F4 and the
# reference clean-up. Oracles: Jaeschke, Guyatt & Sackett (1994) likelihood-ratio bands;
# DOR = LR+/LR- on a single 2x2 table (Glas et al. 2003); expected values are hand
# arithmetic in the comments, never the implementation's own formatting.

ir_mk <- function(TP, FP, FN, TN, tl = c("pos", "neg"), gl = c("dis", "non"))
  data.frame(test = factor(rep(c(tl[1], tl[1], tl[2], tl[2]), c(TP, FP, FN, TN)), levels = tl),
             ref  = factor(rep(c(gl[1], gl[2], gl[1], gl[2]), c(TP, FP, FN, TN)), levels = gl))
ir_run <- function(d, gp = "dis", tp = "pos", ...)
  suppressWarnings(decision(data = d, gold = "ref", goldPositive = gp, newtest = "test",
                            testPositive = tp, goldNegative = NULL, testNegative = NULL, ...))
# Inline tags vanish (they sit inside a sentence); block tags become a space.
ir_text <- function(h) trimws(gsub("[[:space:]]+", " ",
  gsub("<[^>]+>", " ", gsub("</?(em|strong|b|i|span)( [^>]*)?>", "", paste(h, collapse = " ")))))

test_that("F1 LR- 0.0969 is banded large (Jaeschke: < 0.1) and printed as the table holds it", {
  # LR- = (19/200)/(98/100) = 0.09694; LR+ = (181/200)/(2/100) = 45.25
  r <- ir_run(ir_mk(181, 2, 19, 98), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE,
              showReportTemplate = TRUE)
  expect_equal(r$ratioTable$asDF$LRN, (19 / 200) / (98 / 100), tolerance = 1e-12)
  summary <- ir_text(r$naturalLanguageSummary$content)
  expect_match(summary, "Negative LR: 0.0969 (Strong evidence against disease)", fixed = TRUE)
  expect_match(summary, "Positive LR: 45.2 (Strong evidence for disease)", fixed = TRUE)
  expect_match(ir_text(r$clinicalInterpretation$content),
               "Negative LR (0.0969): Large and often conclusive decrease", fixed = TRUE)
  expect_match(ir_text(r$reportTemplate$content), "positive likelihood ratio of 45.2 (95% CI", fixed = TRUE)
})

test_that("F1 the mirror fixture reads large in both codings (away from the rounding windows)", {
  # Both levels swapped: LR+' = 1/LR- = 10.316 (large), LR-' = 1/LR+ = 0.0221 (large)
  r <- ir_run(ir_mk(181, 2, 19, 98), gp = "non", tp = "neg", showNaturalLanguage = TRUE)
  summary <- ir_text(r$naturalLanguageSummary$content)
  expect_match(summary, "Positive LR: 10.3 (Strong evidence for disease)", fixed = TRUE)
  expect_match(summary, "Negative LR: 0.0221 (Strong evidence against disease)", fixed = TRUE)
})

test_that("F1 large likelihood ratios print in fixed notation, three significant figures", {
  # Sens 99/100, Spec 999/1000: LR+ = 0.99 / 0.001 = 990
  r <- ir_run(ir_mk(99, 1, 1, 999), showNaturalLanguage = TRUE, showReportTemplate = TRUE)
  expect_equal(r$ratioTable$asDF$LRP, 990, tolerance = 1e-9)
  expect_match(ir_text(r$naturalLanguageSummary$content), "Positive LR: 990 (Strong evidence for disease)", fixed = TRUE)
  expect_no_match(ir_text(r$reportTemplate$content), "e\\+|990\\.")
})

test_that("F1 a comma decimal mark (OutDec) does not break the narrative panels", {
  # formatC() follows getOption("OutDec"); "0,0969" would be NA to the band classifier and
  # every panel would fall back to its "unable to generate" text.
  withr::local_options(OutDec = ",")
  r <- ir_run(ir_mk(181, 2, 19, 98), showNaturalLanguage = TRUE, showClinicalInterpretation = TRUE)
  expect_match(ir_text(r$naturalLanguageSummary$content), "Negative LR: 0.0969 (Strong evidence against disease)", fixed = TRUE)
  expect_match(ir_text(r$clinicalInterpretation$content), "Negative LR (0.0969): Large and often conclusive decrease", fixed = TRUE)
})

test_that("F2 a zero cell with ONE corrected likelihood ratio says DOR need not equal LR+/LR-", {
  # TP 20, FP 0, FN 5, TN 15: LR+ corrected (20.5/26)/(0.5/16) = 25.23, LR- observed 0.20,
  # DOR corrected 20.5*15.5/(0.5*5.5) = 115.5, which is not 25.23/0.20 = 126.2.
  r <- ir_run(ir_mk(20, 0, 5, 15), ci = TRUE, fnote = TRUE)
  expect_equal(r$epirTable_number$asDF$est[3], 20.5 * 15.5 / (0.5 * 5.5), tolerance = 1e-9)
  expect_match(r$epirTable_number$notes$continuity$note, "need not equal LR+ / LR- as displayed", fixed = TRUE)
  dor_foot <- paste(unlist(r$epirTable_number$getCell(rowNo = 3, col = "statsnames")$footnotes), collapse = " ")
  expect_match(dor_foot, "need not satisfy that identity", fixed = TRUE)
})

test_that("F2 the mirror case, only LR- corrected, says the same", {
  # TP 20, FP 5, FN 0, TN 15: LR+ observed (20/20)/(5/20) = 4.00; LR- corrected
  # (0.5/21)/(15.5/21) = 0.0323; DOR corrected 20.5*15.5/(5.5*0.5) = 115.5, not 4.00/0.0323 = 124.
  r <- ir_run(ir_mk(20, 5, 0, 15), ci = TRUE, fnote = TRUE)
  rt <- r$ratioTable$asDF
  expect_equal(c(rt$LRP, rt$LRN), c(4, 0.5 / 15.5), tolerance = 1e-12)
  expect_equal(r$epirTable_number$asDF$est[3], 20.5 * 15.5 / (5.5 * 0.5), tolerance = 1e-9)
  expect_match(r$epirTable_number$notes$continuity$note, "need not equal LR+ / LR- as displayed", fixed = TRUE)
  dor_foot <- paste(unlist(r$epirTable_number$getCell(rowNo = 3, col = "statsnames")$footnotes), collapse = " ")
  expect_match(dor_foot, "need not satisfy that identity", fixed = TRUE)
})

test_that("F2 when both likelihood ratios are corrected the identity holds and is stated plainly", {
  # FP = FN = 0: every ratio comes from the corrected table, so DOR = LR+/LR- exactly.
  r <- ir_run(ir_mk(10, 0, 0, 10), ci = TRUE, fnote = TRUE)
  rt <- r$ratioTable$asDF
  expect_equal(r$epirTable_number$asDF$est[3], rt$LRP / rt$LRN, tolerance = 1e-9)
  expect_no_match(r$epirTable_number$notes$continuity$note, "need not", fixed = TRUE)
  dor_foot <- paste(unlist(r$epirTable_number$getCell(rowNo = 3, col = "statsnames")$footnotes), collapse = " ")
  expect_match(dor_foot, "equal to LR+ / LR- (Glas et al. 2003)", fixed = TRUE)
  # No zero cell: no continuity note at all.
  expect_null(ir_run(ir_mk(231, 32, 27, 54), ci = TRUE)$epirTable_number$notes$continuity)
})

test_that("F3 the one-row-per-patient assumption is stated with default options and in About", {
  r <- ir_run(ir_mk(231, 32, 27, 54), showAboutAnalysis = TRUE)
  expect_match(r$nTable$notes$independence$note, "Each row must be a different, independent patient", fixed = TRUE)
  expect_match(ir_text(r$aboutAnalysis$content), "Each row must be a different, independent patient", fixed = TRUE)
})

test_that("F4 the default output names the disease-present and test-positive levels", {
  r <- ir_run(ir_mk(231, 32, 27, 54, tl = c("AtypiaPos", "AtypiaNeg"), gl = c("Malignant", "Benign")),
              gp = "Malignant", tp = "AtypiaPos")
  note <- r$cTable$notes$levels$note
  expect_match(note, 'Reference Positive is "Malignant" and Reference Negative is "Benign" (ref)', fixed = TRUE)
  expect_match(note, 'Test Positive is "AtypiaPos" and Test Negative is "AtypiaNeg" (test)', fixed = TRUE)
  # Inverting both choices changes the note, which is the only visible sign of the mirror.
  inv <- ir_run(ir_mk(231, 32, 27, 54, tl = c("AtypiaPos", "AtypiaNeg"), gl = c("Malignant", "Benign")),
                gp = "Benign", tp = "AtypiaNeg")
  expect_match(inv$cTable$notes$levels$note, 'Reference Positive is "Benign"', fixed = TRUE)
})

test_that("F4 labels with markup characters reach the note as jamovi 28.3 renders them", {
  # The note renderer escapes "&" itself and shows "<20%" as written, so Ki-67 style labels
  # pass through unchanged; only a "<" that could open a tag is broken apart.
  r <- ir_run(ir_mk(30, 5, 6, 40, tl = c(">=20%", "<20%")), tp = ">=20%")
  expect_match(r$cTable$notes$levels$note, 'Test Positive is ">=20%" and Test Negative is "<20%"', fixed = TRUE)
  rb <- ir_run(ir_mk(30, 5, 6, 40, tl = c("<b>pos", "neg")), tp = "<b>pos")
  expect_match(rb$cTable$notes$levels$note, 'Test Positive is "< b>pos"', fixed = TRUE)
})

test_that("F4 a bracketed variable or level name does not truncate the note", {
  # setNote() re-translates the text and cuts an untranslated " [...]" as a msgctxt.
  d <- ir_mk(30, 5, 6, 40, tl = c("Ki67 high [>=20%]", "Ki67 low"), gl = c("Malignant", "Benign"))
  names(d) <- c("Ki67 [%]", "Histology [final]")
  r <- suppressWarnings(decision(data = d, gold = "Histology [final]", goldPositive = "Malignant",
                                 newtest = "Ki67 [%]", testPositive = "Ki67 high [>=20%]",
                                 goldNegative = NULL, testNegative = NULL))
  note <- r$cTable$notes$levels$note
  expect_match(note, 'Test Positive is "Ki67 high (>=20%)" and Test Negative is "Ki67 low" (Ki67 (%))', fixed = TRUE)
  expect_match(note, 'Sensitivity is the proportion of "Malignant" cases that the test calls "Ki67 high (>=20%)".', fixed = TRUE)
})

test_that("references: the suspicious ones are gone from decision and every remaining one resolves", {
  ryaml <- testthat::test_path("..", "..", "jamovi", "decision.r.yaml")
  refs_yaml <- testthat::test_path("..", "..", "jamovi", "00refs.yaml")
  skip_if_not(file.exists(ryaml) && file.exists(refs_yaml))
  # readLines(encoding = "UTF-8") marks the text without re-encoding it: yaml::read_yaml()
  # converts to the native encoding and silently truncates 00refs.yaml under a C locale.
  read_utf8 <- function(f) yaml::yaml.load(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
  spec <- read_utf8(ryaml)
  item_refs <- function(name) unlist(Filter(function(i) identical(i$name, name), spec$items)[[1]]$refs)
  expect_false("DiagnosticTests" %in% unlist(spec$refs))        # SARS-CoV-2 review as the general reference
  expect_false("Fagan2" %in% item_refs("plot1"))                 # web page; Fagan1975 + nomogrammer remain
  expect_false("bandolier1996" %in% item_refs("epirTable_number"))  # web page; Linn & Grunau 2006 remains
  expect_false("Glas2003" %in% item_refs("ratioTable"))          # that table shows no DOR
  expect_true("Glas2003" %in% item_refs("epirTable_number"))
  expect_true(all(c("Fagan1975", "Fagan") %in% item_refs("plot1")))
  # Added 2026-09-26 (reference-gap review, each attachment accepted by two skeptics): every
  # claim an item shows carries its source on that item.
  expect_true(all(c("ClopperPearson1934", "youden1950", "UsherSmith2016") %in% item_refs("notices")))
  expect_true("Ying2020" %in% item_refs("nTable"))                # independence note
  expect_true("Alberg2004" %in% item_refs("ratioTable"))          # sample-accuracy note
  expect_true(all(c("haldane1956", "anscombe1956") %in% item_refs("epirTable_number")))
  expect_true(all(c("ClopperPearson1934", "Simel1991", "AgrestiCaffo2000", "Ying2020", "Valenstein1990")
                  %in% item_refs("aboutAnalysis")))
  all_refs <- unique(c(unlist(lapply(spec$items, `[[`, "refs")), unlist(spec$refs)))
  known <- names(read_utf8(refs_yaml)$refs)
  expect_gt(length(known), 400)                                  # the whole file was read
  expect_length(setdiff(all_refs, known), 0)
})

test_that("references listed at run time follow what the output shows", {
  # The notices item is always visible, so its references are set per run from the notices
  # actually raised; Haldane and Anscombe are cited only when a zero cell was corrected.
  clean <- ir_run(ir_mk(231, 32, 27, 54))       # n 344, smallest cell 27: no notice at all
  expect_length(clean$notices$getRefs(), 0)
  expect_false(any(c("haldane1956", "anscombe1956") %in% clean$ratioTable$getRefs()))

  zero <- ir_run(ir_mk(20, 0, 5, 15))           # FP = 0: continuity correction applied
  expect_true(all(c("haldane1956", "anscombe1956") %in% zero$notices$getRefs()))
  expect_true(all(c("haldane1956", "anscombe1956") %in% zero$ratioTable$getRefs()))

  small <- ir_run(ir_mk(5, 1, 2, 3))            # n 11 with a cell of 1
  expect_true(all(c("Buderer1996", "ClopperPearson1934") %in% small$notices$getRefs()))
  expect_false("Schuetz2012" %in% small$notices$getRefs())   # no level was excluded
})
