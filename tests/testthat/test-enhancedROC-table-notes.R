ertn_data <- function() {
    data.frame(
        y = factor(rep(c("neg", "pos"), each = 15), levels = c("neg", "pos")),
        m = c(1:15, 8:22),
        perfect = 1:30
    )
}

ertn_object <- function(data = ertn_data(), ...) {
    args <- utils::modifyList(list(outcome = "y", positiveClass = "pos",
        predictors = c("m", "perfect"), rocCurve = FALSE), list(...), keep.null = TRUE)
    options <- do.call(ClinicoPath:::enhancedROCOptions$new, args)
    analysis <- ClinicoPath:::enhancedROCClass$new(options = options, data = data)
    suppressWarnings(analysis$run())
    analysis
}

ertn_tables <- function(analysis) {
    group <- analysis$results$results
    Filter(function(item) inherits(item, "Table"), lapply(group$itemNames, group$get))
}

# Kept with table notes off besides the seed (enhancedROC .keptTableNotes, production pass
# 2026-10-11): the notes without which a number is misread. Excluded here; the seed is below.
ertn_kept <- c("rule", "cutpoint_optimism", "venkatraman", "multiplicity")
# A blanked cell keeps the footnote saying why it is blank (review 2026-10-11): an empty
# statistic with no reason anywhere reads as a missing result. Excluded here like the kept notes.
ertn_notes <- function(table) {
    notes <- vapply(table$notes[setdiff(names(table$notes), ertn_kept)], function(note) note$note, character(1))
    for (column in table$columns) {
        for (row in seq_len(table$rowCount)) {
            cell <- column$getCell(row)
            if (identical(cell$value, "")) next
            notes <- c(notes, cell$footnotes)
        }
    }
    unname(notes)
}
ertn_blank_footnotes <- function(table) {
    notes <- character()
    for (column in table$columns) {
        for (row in seq_len(table$rowCount)) {
            cell <- column$getCell(row)
            if (identical(cell$value, "")) notes <- c(notes, cell$footnotes)
        }
    }
    unique(notes)
}

# The random seed is never hidden: a resampled result always shows the seed that produced it.
ertn_nonseed <- function(notes) notes[!startsWith(notes, "Random seed: ")]

test_that("enhancedROC table notes are opt-in across its result branches", {
    scenarios <- list(
        list(analysisType = "comprehensive", clinicalMetrics = TRUE, detectImbalance = TRUE,
             partialAuc = TRUE, crocAnalysis = TRUE, pairwiseComparisons = TRUE,
             showMetricsDiff = TRUE, statisticalComparison = TRUE, calibrationAnalysis = TRUE,
             hosmerLemeshow = TRUE, clinicalImpact = TRUE),
        list(data = data.frame(y = factor(rep(c("A", "B", "C"), each = 15)), m = 1:45),
             predictors = "m", positiveClass = NULL, multiClassROC = TRUE),
        list(useBootstrap = TRUE, bootstrapSamples = 100),
        list(validationPanel = TRUE, validationNegative = "neg", modelVersion = "test-1.0")
    )
    for (args in scenarios) {
        off <- do.call(ertn_object, args)
        on <- do.call(ertn_object, c(args, list(showTableNotes = TRUE)))
        expect_identical(off$options$showTableNotes, FALSE)
        expect_length(ertn_nonseed(unlist(lapply(ertn_tables(off), ertn_notes))), 0L)
        expect_gt(length(unlist(lapply(ertn_tables(on), ertn_notes))), 0L)
        expect_equal(lapply(ertn_tables(off), function(table) table$asDF),
                     lapply(ertn_tables(on), function(table) table$asDF))
    }
})

test_that("enhancedROC keeps the cut-off rule note when table notes are off", {
    off <- ertn_object()
    expect_match(off$results$results$optimalCutoffSummary$notes$rule$note, "at or above the cutoff", fixed = TRUE)
})

test_that("enhancedROC keeps the random seed note when table notes are off", {
    off <- ertn_object(useBootstrap = TRUE, bootstrapSamples = 100)
    expect_identical(off$results$results$aucSummary$notes$seed$note, paste0("Random seed: ", off$options$seed))
    expect_identical(names(off$results$results$aucSummary$notes), "seed")
})

test_that("enhancedROC removes and restores notes when the checkbox changes", {
    analysis <- ertn_object(showTableNotes = TRUE)
    table <- analysis$results$results$aucSummary
    notes <- ertn_notes(table)
    expect_gt(length(table$getCell("auc_lower", rowKey = "perfect")$footnotes), 0L)
    expect_gt(length(table$footnotes), 0L)

    option <- analysis$options$option("showTableNotes")
    option$value <- FALSE
    suppressWarnings(analysis$run())
    expect_length(unlist(lapply(ertn_tables(analysis), ertn_notes)), 0L)
    expect_setequal(unlist(table$footnotes), ertn_blank_footnotes(table))
    expect_gt(length(ertn_blank_footnotes(table)), 0L)   # the perfect marker's blank interval says why

    option$value <- TRUE
    suppressWarnings(analysis$run())
    expect_identical(ertn_notes(table), notes)
})

test_that("enhancedROC does not restore saved table notes after the checkbox changes", {
    saved <- list(stale = FALSE, state = raw(), table = list(
        columns = list(), rowNames = character(),
        notes = list(list(key = "saved", note = "Previously displayed note", init = FALSE))))
    after <- ertn_object()
    tables <- ertn_tables(after)
    for (i in seq_along(tables)) {
        tables[[i]]$fromProtoBuf(saved, oChanges = "showTableNotes", vChanges = character())
    }
    expect_length(unlist(lapply(tables, ertn_notes)), 0L)
})

test_that("enhancedROC clears earlier table notes when required input is removed", {
    analysis <- ertn_object(showTableNotes = TRUE)
    option <- analysis$options$option("showTableNotes")
    option$value <- FALSE
    predictors <- analysis$options$option("predictors")
    predictors$value <- character()
    suppressWarnings(analysis$run())
    expect_length(unlist(lapply(ertn_tables(analysis), ertn_notes)), 0L)
})
