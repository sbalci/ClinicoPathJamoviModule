# Regression test for OO-01: the column outcomeorganizer writes back to the
# jamovi spreadsheet must agree with the status the analysis itself reports.
#
# The exported column used to be a snapshot (private$.causeFactor) taken right
# after the initial recode, before administrative censoring and the event
# hierarchy mutated the status. A competing-risks run with administrative
# censoring therefore reported a patient as censored in its own tables while
# writing "Event" into the spreadsheet. That is not cosmetic: survival and
# multisurvival re-decode the Censored/Event/Competing hand-off and map "Event"
# to 1, so the patient re-entered downstream analysis as an event, at
# untruncated follow-up, silently inflating the event count.
#
# The `type: Output` element is only populated by jamovi itself, so the exported
# vector cannot be read back in this harness. These tests therefore pin the two
# things that ARE observable: the analysis's own post-truncation status counts,
# and the absence of the stale snapshot at the export site.

.oo_ns <- NULL
for (.p in c("ClinicoPath", "jsurvival")) {
    if (.p %in% loadedNamespaces() || requireNamespace(.p, quietly = TRUE)) {
        .cand <- asNamespace(.p)
        if (exists("outcomeorganizer", envir = .cand, inherits = FALSE)) {
            .oo_ns <- .cand
            break
        }
    }
}
skip_if(is.null(.oo_ns), "outcomeorganizer not available in this distribution")

quiet <- function(expr) { sink(tempfile()); on.exit(sink()); suppressWarnings(force(expr)) }

test_that("administrative censoring truncates the analysed status in a competing-risks run", {
    # Half died of disease, half alive without disease; half of each group was
    # followed to 30 months and half to 5. The administrative cut-off is 20, so
    # the 12 patients with follow-up 30 are truncated, and the 6 of those who
    # were events must revert to censored. (At least two distinct outcome values
    # are required -- a single-valued outcome is correctly rejected.)
    d <- data.frame(
        pid = paste0("p", 1:24),
        out = factor(rep(c("DOD", "AWOD"), each = 12),
                     levels = c("DOD", "DOOC", "AWD", "AWOD")),
        fu  = rep(c(rep(5, 6), rep(30, 6)), 2),
        cut = rep(20, 24),
        stringsAsFactors = FALSE)

    r <- quiet(get("outcomeorganizer", envir = .oo_ns)(
        data = d, outcome = "out", outcomeLevel = "DOD", recurrenceLevel = NULL,
        multievent = TRUE, analysistype = "compete",
        dod = "DOD", dooc = "DOOC", awd = "AWD", awod = "AWOD",
        followupTime = "fu", adminCensoring = TRUE, adminDate = "cut",
        outputTable = TRUE, diagnostics = TRUE))

    diag_txt <- paste(unlist(as.data.frame(r$diagnosticsTable)), collapse = " ")
    expect_match(diag_txt, "Administrative censoring applied")
    expect_match(diag_txt, "truncated for 12")   # the 12 with fu = 30 > cut = 20
    expect_match(diag_txt, "6 event")           # of which 6 were events
})

test_that("the exported outcome column is derived from the final status, not a stale snapshot", {
    gen <- get("outcomeorganizerClass", envir = .oo_ns)
    src <- paste(vapply(gen$private_methods,
                        function(f) paste(deparse(f), collapse = " "),
                        character(1)), collapse = " ")
    src <- gsub("\\s+", " ", src)

    # The defect, precisely: the Output element fed with the pre-mutation
    # snapshot. Any reappearance of this pattern is the bug coming back.
    expect_false(grepl("setValues(as.character(private$.causeFactor))", src, fixed = TRUE))

    # ... and the export must reference the post-mutation status vector.
    expect_true(grepl("addOutcome$setValues", src, fixed = TRUE))
    expect_true(grepl("df_outcome$myoutcome", src, fixed = TRUE))
})

test_that(".causeFactor is still captured, so competing-risks runs keep their labelled form", {
    # It remains the "this was a competing-risks run" flag -- the fix must not
    # have removed the distinction between the labelled and 0/1 export forms.
    gen <- get("outcomeorganizerClass", envir = .oo_ns)
    src <- paste(vapply(gen$private_methods,
                        function(f) paste(deparse(f), collapse = " "),
                        character(1)), collapse = " ")
    expect_true(grepl("causeFactor", src, fixed = TRUE))
    expect_true(grepl("Competing", src, fixed = TRUE))
})
