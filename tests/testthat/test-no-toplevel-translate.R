# Guard test for GitHub issue #122 ("object 'self' not found").
#
# jmvcore's translation helper `.()` reads `self` from the CALLER's frame:
#   function(text, n = 1) { self <- eval.parent(str2lang("self")); self$options$translate(text, n) }
# That only works inside an R6 analysis method. If a *top-level* package helper
# function (defined at file scope, no `self` in its frame) calls `.()`, it throws
# "object 'self' not found" -- but only when the branch containing the `.()` call
# actually executes, so the crash is data-dependent and easy to miss in review
# (issue #122: it fired only for outcomes with an event rate < 5%).
#
# This test statically scans every R/*.R source file and fails if any `.()` call
# sits OUTSIDE an R6Class(...) span, i.e. in a top-level helper. It guards the
# whole bug class, not just the one branch that was fixed.
#
# Implementation note: we parse with getParseData() rather than grep so that
# `.()` inside comments or strings is ignored automatically.

test_that("no top-level helper calls the .() translate function (issue #122)", {
  skip_on_cran()

  r_dir <- test_path("..", "..", "R")
  # Under R CMD check the raw R/ sources are not shipped (they are serialized
  # into the installed package), so this scan only runs against the source tree.
  skip_if_not(dir.exists(r_dir), "package R/ source not available")

  # Return a data.frame of dangerous `.()` sites (file, line) for one file.
  scan_file <- function(path) {
    parsed <- tryCatch(parse(path, keep.source = TRUE), error = function(e) NULL)
    if (is.null(parsed)) return(NULL)
    pd <- utils::getParseData(parsed)
    if (is.null(pd) || nrow(pd) == 0) return(NULL)

    # Line spans of every R6Class(...) call -- `.()` inside these has `self`.
    r6_rows <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == "R6Class")
    spans <- lapply(r6_rows, function(r) {
      head_expr_id <- pd$parent[r]                     # expr wrapping the symbol
      call_expr_id <- pd$parent[pd$id == head_expr_id] # the enclosing call expr
      call_row <- which(pd$id == call_expr_id)
      if (length(call_row) == 1) c(pd$line1[call_row], pd$line2[call_row]) else NULL
    })
    spans <- Filter(Negate(is.null), spans)
    in_r6 <- function(ln) any(vapply(spans, function(s) ln >= s[1] && ln <= s[2], logical(1)))

    dot_rows <- which(pd$token == "SYMBOL_FUNCTION_CALL" & pd$text == ".")
    if (!length(dot_rows)) return(NULL)
    dlines <- pd$line1[dot_rows]
    bad <- dlines[!vapply(dlines, in_r6, logical(1))]
    if (!length(bad)) return(NULL)
    data.frame(file = basename(path), line = bad, stringsAsFactors = FALSE)
  }

  files <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)
  violations <- do.call(rbind, lapply(files, scan_file))

  msg <- if (is.null(violations) || nrow(violations) == 0) {
    ""
  } else {
    paste0(
      "Found .() translate calls in top-level helper(s) -- these throw ",
      "\"object 'self' not found\" when hit (issue #122). Use plain string ",
      "literals in file-level helpers instead:\n",
      paste(sprintf("  %s:%d", violations$file, violations$line), collapse = "\n")
    )
  }

  expect_true(is.null(violations) || nrow(violations) == 0, info = msg)
})
