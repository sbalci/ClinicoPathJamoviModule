# Module-wide contract for how results are rendered.
#
# Every check here encodes a CLASS of finding raised by the jamovi library
# reviewer (see jamovi-library-audit/ and vignettes/jamovi_library_review_guide.md).
# They are written at the level of the class, not the instance, so that fixing
# four venn renderers becomes "this cannot come back anywhere in the module".

contract_root <- normalizePath(
  testthat::test_path("..", ".."),
  winslash = "/",
  mustWork = TRUE
)

contract_source_available <- dir.exists(file.path(contract_root, "R"))

backend_files <- function() {
  list.files(file.path(contract_root, "R"), pattern = "\\.b\\.R$", full.names = TRUE)
}

all_r_files <- function() {
  list.files(file.path(contract_root, "R"), pattern = "\\.R$", full.names = TRUE)
}


test_that("no render function reads image$state without a NULL guard", {
  skip_if_not(contract_source_available, "package source tree not available")

  # A render function can run when .run() did NOT set the state: .run() returned
  # early on a validation failure, jamovi re-invoked the renderer on resize, or
  # jmvcore's .load() restored a saved .omv without re-running. An unguarded read
  # then hands NULL to a plotting library and the user sees a raw library error
  # instead of a clean empty panel.
  #
  # House pattern (R/agepyramid.b.R, R/benford.b.R, R/dataquality.b.R):
  #     plotData <- image$state
  #     if (is.null(plotData))
  #         return(FALSE)

  offenders <- character()

  for (path in backend_files()) {
    lines <- readLines(path, warn = FALSE)
    for (i in seq_along(lines)) {
      plain <- regmatches(
        lines[i],
        regexec("^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*(image[A-Za-z0-9_.]*)\\$state\\s*$", lines[i])
      )[[1]]
      nested <- regmatches(
        lines[i],
        regexec("^\\s*([A-Za-z_.][A-Za-z0-9_.]*)\\s*<-\\s*(image[A-Za-z0-9_.]*)\\$state\\$", lines[i])
      )[[1]]
      if (!length(plain) && !length(nested)) next

      hit <- if (length(plain)) plain else nested
      variable <- hit[2]
      element <- hit[3]

      backward <- paste(lines[max(1L, i - 12L):max(1L, i - 1L)], collapse = "\n")
      if (grepl(paste0("is\\.null\\(\\s*", element, "\\$state\\b"), backward)) next

      # NULL$field is NULL, so a guard on the extracted value covers a missing
      # parent state too.
      forward <- paste(lines[min(length(lines), i + 1L):min(length(lines), i + 7L)],
                       collapse = "\n")
      guarded <- grepl(paste0("is\\.null\\(\\s*", variable, "\\b"), forward) ||
        grepl(paste0("length\\(\\s*", variable, "\\s*\\)\\s*(==|<)\\s*[01]"), forward)
      if (guarded) next

      # an unguarded sub-field read needs the PARENT guarded before the read
      offenders <- c(offenders, paste0(basename(path), ":", i))
    }
  }

  expect_identical(
    offenders, character(),
    info = paste0(
      "image$state read without a NULL guard at: ", paste(offenders, collapse = ", "),
      "\nRun: python3 tools/check_state_guards.py"
    )
  )
})


test_that("HTML output does not paint opaque light-theme backgrounds", {
  skip_if_not(contract_source_available, "package source tree not available")

  # jamovi has a dark theme. A pale hex background with no explicit foreground is
  # unreadable there, because the inherited text colour is light. A translucent
  # rgba() tint composites over whatever pane is behind it, so one declaration is
  # correct in both themes -- and, chosen as in tools/theme_safe_html.py, it is
  # pixel-identical to the old pastel over a white pane.
  #
  # Saturated opaque fills (badges/chips) are allowed, but must set a foreground.
  #
  # This scans CSS DECLARATIONS, not whole `style="..."` attributes. An earlier
  # attribute-level regex silently missed 200 backgrounds: style strings split
  # across R string concatenation, `style =` passed as an htmltools argument, or
  # held in a variable. Do not "simplify" this back to matching style attributes.

  # HSL lightness, matching tools/theme_safe_html.py. It separates pale panel
  # tints from deliberate saturated chips far better than relative luminance,
  # which drags hue-heavy pastels like #ffcdd2 below any sensible threshold.
  lightness <- function(hex) {
    if (nchar(hex) == 4L)
      hex <- paste0("#", paste(rep(substring(hex, 2:4, 2:4), each = 2L), collapse = ""))
    channels <- strtoi(substring(hex, c(2, 4, 6), c(3, 5, 7)), 16L)
    (max(channels) + min(channels)) / 2 / 255
  }

  bg_re <- "background(-color)?\\s*:\\s*(#[0-9a-fA-F]{3,8})"
  fg_re <- "(^|[^-[:alnum:]])color\\s*:\\s*(#[0-9a-fA-F]{3,8})"
  # a keyword colour (`white`), `inherit` or `var(...)` satisfies a chip just as
  # well as a hex one, so the chip check uses this looser pattern
  any_fg_re <- "(^|[^-[:alnum:]])color\\s*:\\s*[^;'\"}]+"

  pale_offenders <- character()
  chip_offenders <- character()
  override_offenders <- character()

  for (path in all_r_files()) {
    lines <- readLines(path, warn = FALSE)
    text <- paste(lines, collapse = "\n")

    # --- opaque backgrounds -------------------------------------------------
    for (i in seq_along(lines)) {
      m <- gregexpr(bg_re, lines[i], perl = TRUE)[[1]]
      if (m[1] == -1L) next
      decls <- regmatches(lines[i], gregexpr(bg_re, lines[i], perl = TRUE))[[1]]
      for (d in decls) {
        hex <- sub(paste0(".*", "(#[0-9a-fA-F]{3,8})$"), "\\1", d)
        if (nchar(hex) == 9L) next          # #rrggbbaa already carries alpha
        where <- paste0(basename(path), ":", i)
        if (lightness(hex) >= 0.80) {
          pale_offenders <- c(pale_offenders, where)
        } else {
          # a chip must declare a foreground somewhere in its own attribute
          at <- regexpr(d, lines[i], fixed = TRUE)
          near <- substr(lines[i], max(1L, at - 220L), at + attr(at, "match.length") + 220L)
          if (!grepl(any_fg_re, near, perl = TRUE))
            chip_offenders <- c(chip_offenders, where)
        }
      }
    }

    # --- dark foreground overriding an inserted `color: inherit` -------------
    # CSS is last-wins, so a dark color: after a translucent background in the
    # same attribute re-breaks dark mode.
    starts <- gregexpr("background(-color)?:\\s*rgba\\(", text, perl = TRUE)[[1]]
    if (starts[1] != -1L) {
      lens <- attr(starts, "match.length")
      for (k in seq_along(starts)) {
        from <- starts[k] + lens[k]
        window <- substr(text, from, from + 260L)
        cut <- regexpr("['\"}]", window)   # `}` ends a rule inside a <style> block
        if (cut != -1L) window <- substr(window, 1L, cut - 1L)
        fgs <- regmatches(window, gregexpr(fg_re, window, perl = TRUE))[[1]]
        for (g in fgs) {
          hex <- sub(".*(#[0-9a-fA-F]{3,8}).*", "\\1", g)
          if (nchar(hex) == 9L) next
          if (lightness(hex) < 0.50) {
            ln <- sum(strsplit(substr(text, 1L, from), "")[[1]] == "\n") + 1L
            override_offenders <- c(override_offenders, paste0(basename(path), ":", ln))
          }
        }
      }
    }
  }

  expect_identical(
    unique(pale_offenders), character(),
    info = paste0(
      "opaque pale background (unreadable in jamovi's dark theme) at: ",
      paste(unique(pale_offenders), collapse = ", "),
      "\nRun: python3 tools/theme_safe_html.py --apply"
    )
  )
  expect_identical(
    unique(chip_offenders), character(),
    info = paste0(
      "saturated background with no explicit text colour at: ",
      paste(unique(chip_offenders), collapse = ", ")
    )
  )
  expect_identical(
    unique(override_offenders), character(),
    info = paste0(
      "dark `color:` overrides the `color: inherit` on a translucent panel at: ",
      paste(unique(override_offenders), collapse = ", ")
    )
  )
})


test_that("only the five structural HTML entities appear in R source", {
  skip_if_not(contract_source_available, "package source tree not available")

  # &lt; &gt; &amp; &quot; &apos; stand for characters with special meaning in
  # HTML and must be escaped. Every other named entity works only because
  # jamovi's Html renderer currently happens to expand arbitrary named entities;
  # a documented upcoming jamovi fix makes them render LITERALLY, and they
  # already fail non-HTML export today. Use a \u{} escape for the real character.

  structural <- c("&lt;", "&gt;", "&amp;", "&quot;", "&apos;")
  # R/jwaffle.b.R strips entities with gsub("&nbsp;", " ", x) -- correct as is.
  stripper <- "gsub\\(\\s*[\"']&[a-zA-Z]+;"

  offenders <- character()
  for (path in all_r_files()) {
    lines <- readLines(path, warn = FALSE)
    for (i in seq_along(lines)) {
      if (grepl(stripper, lines[i])) next
      found <- regmatches(lines[i], gregexpr("&[a-zA-Z][a-zA-Z0-9]{1,12};", lines[i]))[[1]]
      found <- setdiff(found, structural)
      if (length(found))
        offenders <- c(offenders, paste0(basename(path), ":", i, " ", paste(found, collapse = " ")))
    }
  }

  expect_identical(
    offenders, character(),
    info = paste0("non-structural HTML entity at: ", paste(offenders, collapse = "; "))
  )
})


test_that("an analysis never hides an element and then writes its explanation to it", {
  skip_if_not(contract_source_available, "package source tree not available")

  # jamovi already has a presentation for a failed analysis -- it greys the pane
  # and shows an analysis-level error -- and that presentation depends on the
  # results STAYING IN PLACE. Hiding them makes the pane collapse and re-expand
  # as the user types through invalid intermediate states. setVisible() is for
  # option-driven visibility only; use jmvcore::reject() or an Html notice.
  #
  # The precise, unambiguous bug shape is: hide an element, write NON-EMPTY
  # content or a note TO THAT SAME ELEMENT, then return. The note is never
  # rendered, so the explanation the developer wrote is never shown -- the user
  # sees the output silently disappear. (meddecide audit, R/agreement.b.R.)
  #
  # Deliberately NOT flagged: hide-then-clear (`setContent("")`) and
  # hide-then-repopulate-later, which are the legitimate "reset stale output at
  # the top of .run()" idiom. For the broader, noisier sweep of setVisible(FALSE)
  # on failure paths, see the grep in vignettes/jamovi_library_review_guide.md.

  offenders <- character()
  for (path in backend_files()) {
    lines <- readLines(path, warn = FALSE)
    hits <- grep("self\\$results\\$([A-Za-z0-9_]+)\\$setVisible\\(FALSE\\)", lines)
    for (i in hits) {
      element <- sub(".*self\\$results\\$([A-Za-z0-9_]+)\\$setVisible\\(FALSE\\).*", "\\1", lines[i])
      window <- lines[min(length(lines), i + 1L):min(length(lines), i + 12L)]

      # stop at the point the element is shown again -- that is a reset, not a hide
      shown <- grep(paste0("self\\$results\\$", element, "\\$setVisible\\(TRUE\\)"), window)
      if (length(shown)) window <- window[seq_len(shown[1] - 1L)]
      if (!length(window)) next

      writes <- grep(paste0("self\\$results\\$", element, "\\$set(Note|Content)\\("), window)
      if (!length(writes)) next

      # a write that only clears the element is the legitimate reset idiom
      payload <- paste(window[writes[1]:min(length(window), writes[1] + 4L)], collapse = " ")
      if (grepl('setContent\\(\\s*(""|\'\')\\s*\\)', payload)) next

      # the failure shape is hide -> write -> return, in that order. A `return()`
      # that comes BEFORE the write means the write belongs to a later branch,
      # not to this one (a line window cannot see the brace structure).
      bail <- grep("^\\s*return\\(", window)
      if (!length(bail) || bail[1] < writes[1]) next

      offenders <- c(offenders, paste0(basename(path), ":", i, " (", element, ")"))
    }
  }

  expect_identical(
    offenders, character(),
    info = paste0(
      "element hidden, then handed the explanation the user will never see, at: ",
      paste(offenders, collapse = ", "),
      "\nLeave the element in place and use jmvcore::reject() or an Html notice."
    )
  )
})
