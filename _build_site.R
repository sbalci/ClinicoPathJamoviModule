#!/usr/bin/env Rscript
# Build the pkgdown site for this package (umbrella or submodule).
#
# Usage:  Rscript _build_site.R [--force] [path]
#
# Release rules -- the same ladder .github/workflows/release.yaml and pkgdown.yaml
# apply, so a site is never published for a version those workflows would not ship.
# A leading "v" is accepted anywhere and is not part of the version.
#
#   DESCRIPTION Version: and jamovi/0000.yaml version: must agree -> else ERROR.
#
#   1.0.2, 10.0.2, 1.10.2   -> release       three parts, single-digit last part
#   1.0.12, 1.0.52          -> pre-release   multi-digit last part
#   1.0.2-beta, 0.0.3test   -> pre-release   alpha / beta / test marker
#   0.0.37.07, 0.0.2.0006   -> SKIPPED       four or more parts = development build
#   banana, 1.0.2-rc1       -> ERROR         malformed, fix DESCRIPTION
#
# Releases and pre-releases build. Development builds skip with exit status 0 --
# every working version in this project is a four-part dev string, so pass --force
# to preview the site locally.

# pkgdown renders every root .md into a site page; these are developer notes.
DEV_NOTES <- c("AGENTS.md", "CLAUDE.md", "GEMINI.md", "TODO.md", "sonograph_log.md")

read_version <- function(path, pattern) {
  if (!file.exists(path)) return(NA_character_)
  hit <- grep(pattern, readLines(path, warn = FALSE), value = TRUE)
  if (!length(hit)) return(NA_character_)
  trimws(gsub("[\"']", "", sub(pattern, "", hit[[1]])))
}

classify_version <- function(v) {
  v <- sub("^v", "", v)
  if (grepl("alpha|beta|test", v, ignore.case = TRUE))  return("pre-release")
  if (grepl("^[0-9]+\\.[0-9]+\\.[0-9]$", v))            return("release")
  if (grepl("^[0-9]+\\.[0-9]+\\.[0-9]{2,}$", v))        return("pre-release")
  if (!nzchar(v) || grepl("[^0-9.]", v))
    stop("unusable version string '", v, "' - fix DESCRIPTION", call. = FALSE)
  "development"
}

# A killed build (or a crashed R session) leaves the notes renamed. Put any back
# before doing anything else -- otherwise the repo silently keeps losing them.
unstash_notes <- function() {
  for (s in list.files(all.files = TRUE, pattern = "\\.pkgdown-hide$")) {
    target <- sub("^\\.(.*)\\.pkgdown-hide$", "\\1", s)
    if (!file.exists(target)) file.rename(s, target)
  }
}

build_pages <- function() {
  hidden  <- intersect(DEV_NOTES, list.files())
  stashed <- paste0(".", hidden, ".pkgdown-hide")
  file.rename(hidden, stashed)
  on.exit(file.rename(stashed, hidden), add = TRUE)
  # lazy = TRUE: skip articles whose source is older than the built HTML (the
  # stagemigration vignette alone takes many minutes). To force a full rebuild of
  # one article, delete its docs/articles/<name>.html first.
  pkgdown::build_site(lazy = TRUE)
}

main <- function(args) {
  force <- "--force" %in% args
  rest  <- setdiff(args, "--force")
  root  <- if (length(rest)) rest[[1]] else "."

  if (!file.exists(file.path(root, "DESCRIPTION")))
    stop("no DESCRIPTION in ", normalizePath(root, mustWork = FALSE), call. = FALSE)
  owd <- setwd(root)
  on.exit(setwd(owd), add = TRUE)

  desc <- read_version("DESCRIPTION", "^Version:[[:space:]]*")
  yml  <- read_version(file.path("jamovi", "0000.yaml"), "^version:[[:space:]]*")
  if (is.na(desc)) stop("DESCRIPTION has no Version: field", call. = FALSE)
  if (!is.na(yml) && sub("^v", "", desc) != sub("^v", "", yml))
    stop("version mismatch - DESCRIPTION is '", desc, "' but jamovi/0000.yaml is '",
         yml, "'. Bump both together.", call. = FALSE)

  unstash_notes()
  pkg  <- read.dcf("DESCRIPTION", "Package")[[1]]
  kind <- classify_version(desc)
  cat(sprintf("%s %s -> %s\n", pkg, desc, kind))

  if (kind == "development" && !force) {
    cat("development build - not published; pass --force to build the site locally\n")
    return(invisible(FALSE))
  }
  if (!file.exists("_pkgdown.yml"))
    stop("no _pkgdown.yml in ", getwd(), call. = FALSE)

  build_pages()
  invisible(TRUE)
}

main(commandArgs(trailingOnly = TRUE))
