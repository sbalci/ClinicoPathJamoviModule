# Every file that belongs to an analysis must carry the analysis's canonical name -- the
# `name:` field in jamovi/<fn>.a.yaml -- with EXACT case.
#
# macOS is case-insensitive, so a mis-cased file is invisible locally and fatal elsewhere:
#   * DESCRIPTION Collate: 'nomogrammer.R' against a tracked 'nomogrammer.r' makes
#     R CMD build fail with "files in 'Collate' field missing from 'R'" on any Linux builder.
#   * A .b.R defining <Name>Class while the generated .h.R calls <name>Class makes the
#     analysis die with "object '<name>Class' not found" -- treatmentswitching shipped that
#     way and could not run at all, in the GUI or from R.
#   * jamovi/js/<n>.events.js referenced from a .u.yaml resolves by exact name.
#
# Data files, docs/, vignettes/ and data-raw/ are deliberately NOT checked: a dataset name
# such as `enhancedroc_biomarker` is its own identifier, not the function name.

skip_if_not(nzchar(Sys.which("git")))
ROOT <- normalizePath("../..")

tracked <- local({
    t <- system2("git", c("-C", shQuote(ROOT), "ls-files"), stdout = TRUE)
    t[nzchar(t)]
})
skip_if(length(tracked) == 0)

analyses <- local({
    out <- list()
    for (f in list.files(file.path(ROOT, "jamovi"), pattern = "\\.a\\.yaml$", full.names = TRUE)) {
        txt <- readLines(f, warn = FALSE)
        nm <- sub("^name:\\s*", "", grep("^name:\\s*\\S+\\s*$", txt, value = TRUE)[1])
        if (!is.na(nm) && nzchar(nm)) out[[nm]] <- basename(f)
    }
    out
})


test_that("every analysis file carries the .a.yaml name with exact case", {
    skip_if(length(analyses) == 0)
    patterns <- c("jamovi/%s.a.yaml", "jamovi/%s.r.yaml", "jamovi/%s.u.yaml",
                  "R/%s.b.R", "R/%s.h.R", "man/%s.Rd", "man/%sClass.Rd",
                  "jamovi/js/%s.events.js", "tests/testthat/test-%s.R")
    lower <- tolower(tracked)
    bad <- character(0)
    for (nm in names(analyses)) {
        for (p in patterns) {
            want <- sprintf(p, nm)
            if (want %in% tracked) next
            hit <- tracked[lower == tolower(want)]
            if (length(hit)) bad <- c(bad, sprintf("%s (tracked as %s)", want, hit[1]))
        }
    }
    expect_equal(bad, character(0))
})


test_that("every .b.R defines the class its generated wrapper instantiates", {
    skip_if(length(analyses) == 0)
    bad <- character(0)
    for (nm in names(analyses)) {
        b <- file.path(ROOT, "R", paste0(nm, ".b.R"))
        h <- file.path(ROOT, "R", paste0(nm, ".h.R"))
        if (!file.exists(b) || !file.exists(h)) next
        hs <- paste(readLines(h, warn = FALSE), collapse = "\n")
        want <- regmatches(hs, regexpr("analysis <- \\w+\\$new\\(", hs))
        want <- if (length(want)) sub("analysis <- (\\w+)\\$new\\($", "\\1", want) else paste0(nm, "Class")
        bs <- readLines(b, warn = FALSE)
        if (!any(grepl(paste0("^", want, "\\s*<-"), bs)))
            bad <- c(bad, sprintf("%s: wrapper calls %s, .b.R defines %s", nm, want,
                                  paste(sub("\\s*<-.*$", "", grep("^\\w+Class\\s*<-", bs, value = TRUE)),
                                        collapse = "/")))
    }
    expect_equal(bad, character(0))
})


test_that("no two tracked paths differ only by case", {
    # such a pair cannot both survive a checkout on macOS or Windows
    dup <- tracked[duplicated(tolower(tracked))]
    expect_equal(dup, character(0))
})
