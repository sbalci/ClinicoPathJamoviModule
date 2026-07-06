#!/usr/bin/env Rscript
# Build the pkgdown site with dev/scratch root .md files hidden, so pkgdown
# never renders (or hard-fails on) files like AGENTS.md / CLAUDE.md / GEMINI.md
# / TODO.md / sonograph_log.md.
#
# Why this wrapper: pkgdown 2.x renders EVERY non-standard root .md into the
# site (build_home -> package_mds), offers no config to exclude them, and
# ignores .Rbuildignore. Worse, it HARD-FAILS the whole build when one of those
# files has content pandoc parses as invalid YAML (e.g. TODO.md's `---` line
# followed by text containing colons).
#
# Approach: hide every root .md EXCEPT the legitimate site pages (pkgdown builds
# README / NEWS / LICENSE itself; CONTRIBUTING is real content). Hiding renames
# each file IN PLACE with a suffix -- a same-directory rename that cannot fail
# across filesystems and needs no temp dir. Every rename is checked and the
# build ABORTS loudly if a file can't be hidden, so pkgdown is never handed a
# scratch file. Files are restored on exit, and any leftovers from a previously
# crashed run are restored on start.
#
# Use this instead of `Rscript -e "pkgdown::build_site()"`:
#   Rscript scripts/build_site.R

SUFFIX <- ".pkgdown-hidden"

# Root .md files that ARE site content and must stay visible to pkgdown.
KEEP <- c("README.md", "NEWS.md", "LICENSE.md", "LICENCE.md",
          "CONTRIBUTING.md", "index.md", "CODE_OF_CONDUCT.md",
          "SUPPORT.md", "cran-comments.md")

# Recover any files left hidden by a previous run that crashed mid-build.
restore_leftovers <- function() {
  all <- list.files(".")
  for (h in all[endsWith(all, SUFFIX)]) {
    orig <- substr(h, 1, nchar(h) - nchar(SUFFIX))
    if (!file.exists(orig)) file.rename(h, orig)
  }
}

build_site_clean <- function() {
  restore_leftovers()

  root_mds <- list.files(".", pattern = "[.]md$")
  to_hide  <- setdiff(root_mds, KEEP)

  hidden <- character(0)
  restore <- function() {
    for (f in hidden) {
      h <- paste0(f, SUFFIX)
      if (file.exists(h)) file.rename(h, f)
    }
  }

  for (f in to_hide) {
    if (isTRUE(file.rename(f, paste0(f, SUFFIX)))) {
      hidden <- c(hidden, f)
    } else {
      restore()
      stop("Could not hide dev page '", f, "'; aborting so pkgdown is never ",
           "handed a scratch .md file.", call. = FALSE)
    }
  }

  on.exit(restore(), add = TRUE)

  if (length(hidden) > 0) {
    message("Hid ", length(hidden), " dev page(s) during build: ",
            paste(hidden, collapse = ", "))
  }

  pkgdown::build_site()
}

build_site_clean()
