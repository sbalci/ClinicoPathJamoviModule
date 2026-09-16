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


# ---------------------------------------------------------------------------
# R/ file naming. The module updater ships a helper file wherever a shipped analysis
# uses what it defines (_updateModules_plan.R), so names do not route anything; they
# tell a reader who owns the code, and these tests keep the names honest:
#   <analysis>.b.R, <analysis>.h.R        the analysis
#   <analysis>-<topic>.R                  used by exactly that one analysis
#   utils.R, utils-<topic>.R              used by two or more analyses
#   00jmv.R, zzz.R, ClinicoPath-package.R, data-<topic>.R / data_<x>.R   infrastructure
# Checked against R/ on disk (not git), so a rename is covered before it is committed.

naming_plan <- file.path(ROOT, "_updateModules_plan.R")
naming_env <- new.env(parent = globalenv())
if (file.exists(naming_plan)) sys.source(naming_plan, envir = naming_env)
r_on_disk <- list.files(file.path(ROOT, "R"), "\\.[Rr]$")
analysis_stems <- sub("\\.a\\.yaml$", "", list.files(file.path(ROOT, "jamovi"), "\\.a\\.yaml$"))

# Files that break the rules for a reason still to be resolved. The test fails when an
# entry no longer exists, so this list can only shrink.
naming_pending <- c(
  "enhanced_wrapper_example.R"   # dead, but exports enhanced_ttest(): removal is an API change
)

file_kind <- function(f) {
  if (grepl("\\.(b|h)\\.R$", f)) return("analysis")
  if (f %in% c("00jmv.R", "zzz.R", "ClinicoPath-package.R") || grepl("^data[-_]", f)) return("infra")
  if (grepl("^utils(-[a-z0-9]+)*\\.R$", f)) return("shared")
  if (grepl("^[A-Za-z0-9]+(-[a-z0-9]+)+\\.R$", f) && sub("-.*$", "", f) %in% analysis_stems) return("single")
  NA_character_
}

test_that("every R/ file follows the naming scheme", {
  expect_equal(intersect(analysis_stems, c("utils", "data", "zzz")), character(0))
  expect_equal(setdiff(naming_pending, r_on_disk), character(0), info = "stale naming_pending entry")
  kinds <- vapply(r_on_disk, file_kind, "")
  expect_equal(setdiff(names(kinds)[is.na(kinds)], naming_pending), character(0))
})

test_that("helper file names agree with the analyses that actually use them", {
  skip_if_not(exists("resolve_helpers", envir = naming_env), "updater planner not available")
  index <- naming_env$index_r_sources(ROOT)
  owners <- list()
  for (a in analysis_stems) {
    seed <- paste0(a, ".b.R")
    if (is.null(index[[seed]])) next
    for (f in naming_env$resolve_helpers(index, seed, ignore = character())$files)
      owners[[f]] <- c(owners[[f]], a)
  }
  bad <- character(0)
  for (f in setdiff(r_on_disk, naming_pending)) {
    k <- file_kind(f)
    o <- sort(unique(owners[[f]]))
    if (identical(k, "single") && !identical(o, sub("-.*$", "", f)))
      bad <- c(bad, sprintf("%s: named for one analysis, used by: %s", f, if (length(o)) paste(o, collapse = ", ") else "none"))
    if (identical(k, "shared") && length(o) < 2L)
      bad <- c(bad, sprintf("%s: shared name, used by: %s", f, if (length(o)) paste(o, collapse = ", ") else "none"))
  }
  expect_equal(bad, character(0))
})

test_that("no top-level name is defined in two R/ files", {
  skip_if_not(exists(".plan_top_defs", envir = naming_env), "updater planner not available")
  defs <- lapply(file.path(ROOT, "R", r_on_disk), function(p)
    tryCatch(naming_env$.plan_top_defs(parse(p, keep.source = FALSE)), error = function(e) character()))
  owner <- split(rep(r_on_disk, lengths(defs)), unlist(defs))
  dup <- owner[lengths(owner) > 1L]
  expect_equal(vapply(dup, paste, "", collapse = " + "), setNames(character(0), character(0)))
})

test_that("no Collate, no @include, and no R/ file is build-ignored", {
  expect_false(desc::desc_has_fields("Collate", file = file.path(ROOT, "DESCRIPTION")))
  includes <- unlist(lapply(file.path(ROOT, "R", r_on_disk), function(p)
    grep("^#'\\s*@include", readLines(p, warn = FALSE), value = TRUE)))
  expect_equal(includes, character(0))
  rules <- readLines(file.path(ROOT, ".Rbuildignore"), warn = FALSE)
  rules <- rules[nzchar(rules) & !startsWith(rules, "#")]
  ignored <- r_on_disk[vapply(paste0("R/", r_on_disk), function(p)
    any(vapply(rules, function(r) isTRUE(tryCatch(grepl(r, p, perl = TRUE, ignore.case = TRUE), error = function(e) FALSE)), NA)), NA)]
  expect_equal(ignored, character(0))
})
