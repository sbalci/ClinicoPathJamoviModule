# Build pkgdown site with exclusions
# pkgdown doesn't support excluding .md files from home page processing,
# so we temporarily hide them during the build.

exclude_from_home <- c("TODO.md", "AGENTS.md", "CLAUDE.md", "GEMINI.md", "sonograph_log.md")

# Temporarily rename files so pkgdown won't find them
hidden <- character(0)
for (f in exclude_from_home) {
  if (file.exists(f)) {
    hidden_name <- paste0(".", f, ".pkgdown-hide")
    file.rename(f, hidden_name)
    hidden <- c(hidden, setNames(hidden_name, f))
  }
}

# Restore function
restore_files <- function() {
  for (i in seq_along(hidden)) {
    file.rename(hidden[[i]], names(hidden)[[i]])
  }
}

# Build, then always restore.
# lazy = TRUE: skip re-rendering articles/reference pages whose source is older
# than the built HTML (the stagemigration vignette alone takes many minutes).
# To force a full rebuild of one article, delete its docs/articles/<name>.html.
tryCatch(
  pkgdown::build_site(lazy = TRUE),
  finally = restore_files()
)
